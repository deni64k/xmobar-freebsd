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
          , rect    :: Rectangle
          , window  :: Window
          , fontS   :: FontStruct
          , config  :: Config
         }

-- | Runs the ReaderT
runX :: XConf -> X () -> IO ()
runX xc f = runReaderT f xc

-- | The event loop
eventLoop :: XConf -> [(Maybe ThreadId, TVar String)] -> IO ()
eventLoop xc@(XConf d _ w fs c) v = block $ do
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
               (\() -> runX xc (updateWin tv) >> return ())
      go tv ct

    -- event hanlder
    handle _ ct (ConfigureEvent {ev_window = win}) = do
      rootw <- rootWindow d (defaultScreen d)
      when (win == rootw) $ block $ do
                      killThread ct
                      destroyWindow d w
                      (r',w') <- createWin d fs c
                      eventLoop (XConf d r' w' fs c) v

    handle tvar _ (ExposeEvent {}) = runX xc (updateWin tvar)

    handle _ _ _  = return ()

-- $command

-- | Runs a command as an independent thread and returns its thread id
-- and the TVar the command will be writing to.
startCommand :: (Runnable,String,String) -> IO (Maybe ThreadId, TVar String)
startCommand (com,s,ss)
    | alias com == "" = do var <- atomically $ newTVar is
                           atomically $ writeTVar var "Could not parse the template"
                           return (Nothing,var)
    | otherwise       = do var <- atomically $ newTVar is
                           let cb str = atomically $ writeTVar var (s ++ str ++ ss)
                           h <- forkIO $ start com cb
                           return (Just h,var)
    where is = s ++ "Updating..." ++ ss

-- $window

-- | The function to create the initial window
createWin :: Display -> FontStruct -> Config -> IO (Rectangle,Window)
createWin d fs c = do
  let dflt = defaultScreen d
  Rectangle rx ry rw rh:_ <- getScreenInfo d
  rootw <- rootWindow d dflt
  let (_,as,ds,_) = textExtents fs []
      ht          = as + ds + 2
      (x,y,w,h,o) = case position c of
                     Top                -> (rx,ry             ,rw,fi ht,True)
                     Bottom             -> (rx,ry + fi rh - ht,rw,fi ht,True)
                     Static cx cy cw ch -> (fi cx,fi cy,fi cw,fi ch,True)
  win <- mkUnmanagedWindow d (defaultScreenOfDisplay d) rootw x y w h o
  selectInput       d win (exposureMask .|. structureNotifyMask)
  mapWindow         d win
  setProperties h c d win
  return (Rectangle x y w h,win)

setProperties :: Dimension -> Config -> Display -> Window -> IO ()
setProperties h c d w = do
  a1 <- internAtom d "_NET_WM_STRUT"            False
  c1 <- internAtom d "CARDINAL"                 False
  a2 <- internAtom d "_NET_WM_WINDOW_TYPE"      False
  c2 <- internAtom d "ATOM"                     False
  v  <- internAtom d "_NET_WM_WINDOW_TYPE_DOCK" False
  changeProperty32 d w a1 c1 propModeReplace $ map fi $ getStrutValues h c
  changeProperty32 d w a2 c2 propModeReplace [v]

getStrutValues :: Dimension -> Config -> [Int]
getStrutValues h c
    | position c == Top    = [0, 0, fi h, 0   ]
    | position c == Bottom = [0, 0, 0   , fi h]
    | otherwise            = [0, 0, 0   , 0   ]

updateWin :: TVar String -> X ()
updateWin v = do
  xc <- ask
  let (conf,rec) = (config &&& rect) xc
      [lc,rc]    = if length (alignSep conf) == 2 
                   then alignSep conf 
                   else alignSep defaultConfig
  i <- io $ atomically $ readTVar v
  let [l,c,r] = if (lc `elem` i && rc `elem` i)
                then let (le,_:re) = break (==lc) i
                         (ce,_:ri) = break (==rc) re
                     in [le,ce,ri]
                else [i,[],[]]
  ps <- io $ mapM (parseString conf) [l,c,r]
  drawInWin rec ps

-- $print

data Align = C | L | R

-- | Draws in and updates the window
drawInWin :: Rectangle -> [[(String, String)]] -> X ()
drawInWin (Rectangle _ _ wid ht) ~[left,center,right] = do
  r <- ask
  let (c,d ) = (config &&& display) r
      (w,fs) = (window &&& fontS  ) r
  bgcolor <- io $ initColor d $ bgColor c
  gc      <- io $ createGC  d w
  --let's get the fonts
  io $ setFont d gc (fontFromFontStruct fs)
  -- create a pixmap to write to and fill it with a rectangle
  p <- io $ createPixmap d w wid ht
            (defaultDepthOfScreen (defaultScreenOfDisplay d))
  -- the fgcolor of the rectangle will be the bgcolor of the window
  io $ setForeground d gc bgcolor
  io $ fillRectangle d p gc 0 0 wid ht
  -- write to the pixmap the new string
  let strWithLenth str = map (\(s,cl) -> (s,cl,textWidth fs s)) str
  printStrings p gc fs 1 L $ strWithLenth left
  printStrings p gc fs 1 R $ strWithLenth right
  printStrings p gc fs 1 C $ strWithLenth center
  -- copy the pixmap with the new string to the window
  io $ copyArea   d p w gc 0 0 wid ht 0 0
  -- free up everything (we do not want to leak memory!)
  io $ freeGC     d gc
  io $ freePixmap d p
  -- resync
  io $ sync       d True

-- | An easy way to print the stuff we need to print
printStrings :: Drawable -> GC -> FontStruct -> Position
             -> Align -> [(String, String, Position)] -> X ()
printStrings _ _ _ _ _ [] = return ()
printStrings dr gc fontst offs a sl@((s,c,l):xs) = do
  r <- ask
  let (conf,d)    = (config &&& display) r
      (Rectangle _ _ wid ht ) = rect r
      (_,as,ds,_) = textExtents fontst s
      totSLen     = foldr (\(_,_,len) -> (+) len) 0 sl
      valign      = (fi ht + fi as - fi ds) `div` 2
      remWidth    = fi wid - fi totSLen
      offset      = case a of
                      C -> (remWidth + offs) `div` 2
                      R -> remWidth - 1
                      L -> offs
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
  printStrings dr gc fontst (offs + l) a xs

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
