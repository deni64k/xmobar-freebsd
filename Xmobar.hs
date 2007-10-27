{-# OPTIONS -fglasgow-exts #-}
-----------------------------------------------------------------------------
-- |
-- Module      :  Xmobar
-- Copyright   :  (c) Andrea Rossato
-- License     :  BSD-style (see LICENSE)
-- 
-- Maintainer  :  Andrea Rossato <andrea.rossato@unibz.it>
-- Stability   :  unstable
-- Portability :  unportable
--
-- A status bar for the Xmonad Window Manager 
--
-----------------------------------------------------------------------------

module Xmobar (-- * Main Stuff
               -- $main
               X, XConf (..), runX
              , eventLoop
              -- * Program Execution
              -- $command
              , startCommand
              -- * Window Management
              -- $window
              , createWin, updateWin
              -- * Printing
              -- $print
              , drawInWin, printStrings
              -- * Unmamaged Windows
              -- $unmanwin
              , mkUnmanagedWindow
              -- * Useful Utilities
              , initColor, io, nextEvent', fi
              ) where

import Prelude hiding (catch)
import Graphics.X11.Xlib
import Graphics.X11.Xlib.Extras
import Graphics.X11.Xinerama

import Control.Arrow ((&&&))
import Control.Monad.Reader
import Control.Concurrent
import Control.Concurrent.STM
import Control.Exception hiding (handle)
import Data.Bits
import System.Posix.Types (Fd(..))

import Config
import Parsers
import Commands
import Runnable

-- $main
--
-- The Xmobar data type and basic loops and functions.

-- | The X type is a ReaderT
type X = ReaderT XConf IO

-- | The ReaderT inner component
data XConf =
    XConf { display :: Display
          , window  :: Window
          , config  :: Config
         }

-- | Runs the ReaderT
runX :: Config -> Display -> Window -> X () -> IO ()
runX c d w f = runReaderT f (XConf d w c)

-- | The event loop
eventLoop :: Config -> [(Maybe ThreadId, TVar String)] -> Display -> Window -> IO ()
eventLoop c v d w = block $ do
    tv <- atomically $ newTVar []
    t <- myThreadId
    ct <- forkIO (checker t tv "" `catch` \_ -> return ())
    go tv ct   
 where
    -- interrupt the drawing thread every time a var is updated
    checker t tvar ov = do
      nval <- atomically $ do
              nv <- fmap concat $ mapM readTVar (map snd v)
              guard (nv /= ov)
              writeTVar tvar nv
              return nv
      throwDynTo t ()
      checker t tvar nval

    -- Continuously wait for a timer interrupt or an expose event
    go tv ct = do
      catchDyn (unblock $ allocaXEvent $ \e ->
                    handle tv ct =<< (nextEvent' d e >> getEvent e))
               (\() -> runX c d w (updateWin tv) >> return ())
      go tv ct

    -- event hanlder
    handle _ ct (ConfigureEvent {ev_window = win}) = do
      rootw <- rootWindow d (defaultScreen d)
      when (win == rootw) $ block $ do
                      (Rectangle _ _ wid _):_ <- getScreenInfo d
                      let nw = min wid $ fi (width c)
                      killThread ct
                      destroyWindow d w
                      w' <- createWin d (c {width = fi nw})
                      eventLoop (c {width = fi nw}) v d w'

    handle tvar _ (ExposeEvent {}) = runX c d w (updateWin tvar)

    handle _ _ _  = return ()

-- $command

-- | Runs a command as an independent thread and returns its thread id
-- and the TVar the command will be writing to.
startCommand :: (Runnable,String,String) -> IO (Maybe ThreadId, TVar String)
startCommand (com,s,ss)
    | alias com == ""  = do var <- atomically $ newTVar is
                            atomically $ writeTVar var "Could not parse the template"
                            return (Nothing,var)
    | otherwise        = do var <- atomically $ newTVar is
                            let cb str = atomically $ writeTVar var (s ++ str ++ ss)
                            h <- forkIO $ start com cb
                            return (Just h,var)
    where is = "Updating... "

-- $window

-- | The function to create the initial window
createWin :: Display -> Config -> IO Window
createWin d c = do
  let dflt = defaultScreen d
  (Rectangle _ _ wid _):_ <- getScreenInfo d
  rootw <- rootWindow d dflt
  w     <- mkUnmanagedWindow d (defaultScreenOfDisplay d) rootw 
           (fi $ xPos   c) 
           (fi $ yPos   c) 
           (min wid $ fi $ width  c)
           (fi $ height c) True
  selectInput d w (exposureMask .|. structureNotifyMask)
  mapWindow   d w
  setProperties c d w
  return w

setProperties :: Config -> Display -> Window -> IO ()
setProperties c d w = do
  a1 <- internAtom d "_NET_WM_STRUT"            False
  c1 <- internAtom d "CARDINAL"                 False
  a2 <- internAtom d "_NET_WM_WINDOW_TYPE"      False
  c2 <- internAtom d "ATOM"                     False
  v  <- internAtom d "_NET_WM_WINDOW_TYPE_DOCK" False
  changeProperty32 d w a1 c1 propModeReplace $ map fi $ getStrutValues c
  changeProperty32 d w a2 c2 propModeReplace [v]

getStrutValues :: Config -> [Int]
getStrutValues c
    | yPos c == 0 = [0,0,height c,0]
    | yPos c  > 0 = [0,0,0,height c]
    | otherwise   = [0,0,height c,0]

updateWin :: TVar String -> X ()
updateWin v = do
  c  <- asks config
  i  <- io $ atomically $ readTVar v
  ps <- io $ parseString c i
  drawInWin ps

-- $print

-- | Draws in and updates the window
drawInWin :: [(String, String)] -> X ()
drawInWin str = do
  r <- ask
  let (conf,(d,w)) = (config &&& display &&& window) r
  bgcolor <- io $ initColor d $ bgColor conf
  gc      <- io $ createGC  d w
  --let's get the fonts
  let lf c = loadQueryFont d (font c)
  fontst  <- io $ catch (lf conf) (const $ lf defaultConfig)
  io $ setFont d gc (fontFromFontStruct fontst)
  -- create a pixmap to write to and fill it with a rectangle
  p <- io $ createPixmap d w 
            (fi $ width  conf)
            (fi $ height conf)
            (defaultDepthOfScreen (defaultScreenOfDisplay d))
  -- the fgcolor of the rectangle will be the bgcolor of the window
  io $ setForeground d gc bgcolor
  io $ fillRectangle d p gc 0 0 
       (fi $ width  conf) 
       (fi $ height conf)
  -- write to the pixmap the new string
  let strWithLenth = map (\(s,c) -> (s,c,textWidth fontst s)) str
  printStrings p gc fontst 1 strWithLenth 
  -- copy the pixmap with the new string to the window
  io $ copyArea   d p w gc 0 0 (fi (width conf)) (fi (height conf)) 0 0
  -- free up everything (we do not want to leak memory!)
  io $ freeFont   d fontst
  io $ freeGC     d gc
  io $ freePixmap d p
  -- resync
  io $ sync d True

-- | An easy way to print the stuff we need to print
printStrings :: Drawable -> GC -> FontStruct -> Position
             -> [(String, String, Position)] -> X ()
printStrings _ _ _ _ [] = return ()
printStrings dr gc fontst offs sl@((s,c,l):xs) = do
  r <- ask
  let (conf,d)    = (config &&& display) r
      (_,asc,dsc,_) = textExtents fontst s
      totSLen     = foldr (\(_,_,len) -> (+) len) 0 sl
      valign      = (fi (height conf) + fi (asc) - fi dsc) `div` 2
      remWidth    = fi (width conf) - fi totSLen
      offset      = case (align conf) of
                      "center" -> (remWidth + offs) `div` 2
                      "right"  -> remWidth - 1
                      "left"   -> offs
                      _        -> offs
  (fc,bc) <- case (break (==',') c) of
               (f,',':b) -> do
                 fgc <- io $ initColor d f
                 bgc <- io $ initColor d b
                 return (fgc,bgc) 
               (f,_) -> do
                 fgc <- io $ initColor d f
                 bgc <- io $ initColor d (bgColor conf)
                 return (fgc,bgc) 
  io $ setForeground d gc fc
  io $ setBackground d gc bc
  io $ drawImageString d dr gc offset valign s
  printStrings dr gc fontst (offs + l) xs

{- $unmanwin

This is a way to create unmamaged window.

-}

-- | Creates a window with the attribute override_redirect set to True.
-- Windows Managers should not touch this kind of windows.
mkUnmanagedWindow :: Display
                  -> Screen
                  -> Window
                  -> Position
                  -> Position
                  -> Dimension
                  -> Dimension
                  -> Bool
                  -> IO Window
mkUnmanagedWindow dpy scr rw x y w h o = do
  let visual   = defaultVisualOfScreen scr
      attrmask = cWOverrideRedirect
  allocaSetWindowAttributes $ 
         \attributes -> do
           set_override_redirect attributes o
           createWindow dpy rw x y w h 0 (defaultDepthOfScreen scr) 
                        inputOutput visual attrmask attributes                                

{- $utility
Utilities
-}

-- | Get the Pixel value for a named color: if an invalid name is
-- given the black pixel will be returned.
initColor :: Display -> String -> IO Pixel
initColor dpy c =
    catch (initColor' dpy c) (const . return . blackPixel dpy $ (defaultScreen dpy))

initColor' :: Display -> String -> IO Pixel
initColor' dpy c = (color_pixel . fst) `liftM` allocNamedColor dpy colormap c
    where colormap = defaultColormap dpy (defaultScreen dpy)

-- | A version of nextEvent that does not block in foreign calls.
nextEvent' :: Display -> XEventPtr -> IO ()
nextEvent' d p = do
    pend <- pending d
    if pend /= 0
        then nextEvent d p
        else do
            threadWaitRead (Fd fd)
            nextEvent' d p
 where
    fd = connectionNumber d

-- | Short-hand for lifting in the IO monad
io :: IO a -> X a
io = liftIO

-- | Short-hand for 'fromIntegral'
fi :: (Integral a, Num b) => a -> b
fi = fromIntegral
