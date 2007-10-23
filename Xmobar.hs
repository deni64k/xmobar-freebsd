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
import Graphics.X11.Xlib.Misc
import Graphics.X11.Xlib.Event

import Control.Arrow ((&&&))
import Control.Monad.Reader
import Control.Concurrent
import Control.Concurrent.STM
import Control.Exception
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
eventLoop c v d w = do
    b  <- newEmptyMVar
    tv <- atomically $ newTVar []
    t  <- forkIO (block $ do putMVar b (); go tv)
    takeMVar b
    checker t tv ""
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
    go tvar = do
      runX c d w (updateWin tvar)
      catchDyn (unblock $ allocaXEvent $ nextEvent' d) (\() -> return ())
      go tvar

-- $command

-- | Runs a command as an independent thread and returns its thread id
-- and the TVar the command will be writing to.
startCommand :: (Runnable,String,String) -> IO (Maybe ThreadId, TVar String)
startCommand (com,s,ss)
    | alias com == ""  = do var <- atomically $ newTVar is
                            atomically $ writeTVar var ("Could not parse the template")
                            return (Nothing,var)
    | otherwise        = do var <- atomically $ newTVar is
                            let cb str = atomically $ writeTVar var (s ++ str ++ ss)
                            h <- forkIO $ start com cb
                            return (Just h,var)
    where is = "Updating... "

-- $window

-- | The function to create the initial window
createWin :: Config -> IO (Display, Window)
createWin conf = do
  dpy   <- openDisplay ""
  let dflt = defaultScreen dpy
  rootw <- rootWindow dpy dflt
  win   <- mkUnmanagedWindow dpy (defaultScreenOfDisplay dpy) rootw 
           (fi $ xPos   conf) 
           (fi $ yPos   conf) 
           (fi $ width  conf) 
           (fi $ height conf)
  selectInput dpy win exposureMask
  mapWindow   dpy win
  return     (dpy,win)

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
            (fi (width  conf)) 
            (fi (height conf)) 
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
                  -> IO Window
mkUnmanagedWindow dpy scr rw x y w h = do
  let visual   = defaultVisualOfScreen scr
      attrmask = cWOverrideRedirect
  allocaSetWindowAttributes $ 
         \attributes -> do
           set_override_redirect attributes True
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
