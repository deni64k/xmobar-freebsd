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

module Xmobar
    ( -- * Main Stuff
      -- $main
      X , XConf (..), runX
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
    ) where

import Prelude hiding (catch)
import Graphics.X11.Xlib hiding (textExtents, textWidth)
import Graphics.X11.Xlib.Extras
import Graphics.X11.Xinerama

import Control.Arrow ((&&&))
import Control.Monad.Reader
import Control.Concurrent
import Control.Concurrent.STM
import Control.Exception hiding (handle)
import Data.Bits
import Data.Char


import Config
import Parsers
import Commands
import Runnable
import XUtil

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
          , fontS   :: XFont
          , config  :: Config
          }

-- | Runs the ReaderT
runX :: XConf -> X () -> IO ()
runX xc f = runReaderT f xc

-- | The event loop
eventLoop :: XConf -> [(Maybe ThreadId, TVar String)] -> IO ()
eventLoop xc@(XConf d _ w fs c) v = block $ do
    tv <- atomically $ newTVar []
    t  <- myThreadId
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
createWin :: Display -> XFont -> Config -> IO (Rectangle,Window)
createWin d fs c = do
  let dflt = defaultScreen d
  srs     <- getScreenInfo d
  rootw   <- rootWindow d dflt
  (as,ds) <- textExtents fs "0"
  let ht       = as + ds + 4
      (r,o) = setPosition (position c) srs (fi ht)
  win <- newWindow  d (defaultScreenOfDisplay d) rootw r o
  selectInput       d win (exposureMask .|. structureNotifyMask)
  setProperties r c d win
  mapWindow         d win
  return (r,win)

setPosition :: XPosition -> [Rectangle] -> Dimension -> (Rectangle,Bool)
setPosition p rs ht =
    case p' of
    Top                -> (Rectangle rx          ry      rw      h     , True)
    TopW L i           -> (Rectangle rx          ry     (nw i)   h     , True)
    TopW R i           -> (Rectangle (right  i)  ry     (nw i)   h     , True)
    TopW C i           -> (Rectangle (center i)  ry     (nw i)   h     , True)
    Bottom             -> (Rectangle rx          ny      rw      h     , True)
    BottomW L i        -> (Rectangle rx          ny     (nw i)   h     , True)
    BottomW R i        -> (Rectangle (right  i)  ny     (nw i)   h     , True)
    BottomW C i        -> (Rectangle (center i)  ny     (nw i)   h     , True)
    Static cx cy cw ch -> (Rectangle (fi cx   ) (fi cy) (fi cw) (fi ch), True)
    OnScreen _ _       -> error "Nested OnScreen positions are not allowed"
    where
      (Rectangle rx ry rw rh, p') = case p of
                                        OnScreen i x -> (rs !! i, x)
                                        _            -> (head rs, p)
      ny       = ry + fi (rh - ht)
      center i = rx + (fi $ div (remwid i) 2)
      right  i = rx + (fi $ remwid i)
      remwid i = rw - pw (fi i)
      pw i     = rw * (min 100 i) `div` 100
      nw       = fi . pw . fi
      h        = fi ht

setProperties :: Rectangle -> Config -> Display -> Window -> IO ()
setProperties r c d w = do
  a1 <- internAtom d "_NET_WM_STRUT_PARTIAL"    False
  c1 <- internAtom d "CARDINAL"                 False
  a2 <- internAtom d "_NET_WM_WINDOW_TYPE"      False
  c2 <- internAtom d "ATOM"                     False
  v  <- internAtom d "_NET_WM_WINDOW_TYPE_DOCK" False
  changeProperty32 d w a1 c1 propModeReplace $ map fi $ getStrutValues r c
  changeProperty32 d w a2 c2 propModeReplace [fromIntegral v]

getStrutValues :: Rectangle -> Config -> [Int]
getStrutValues (Rectangle x _ w h) c =
    case position c of
    Top         -> [0, 0, nh,  0, 0, 0, 0, 0, nx, nw,  0,  0]
    TopW    _ _ -> [0, 0, nh,  0, 0, 0, 0, 0, nx, nw,  0,  0]
    Bottom      -> [0, 0,  0, nh, 0, 0, 0, 0,  0,  0, nx, nw]
    BottomW _ _ -> [0, 0,  0, nh, 0, 0, 0, 0,  0,  0, nx, nw]
    _           -> [0, 0,  0,  0, 0, 0, 0, 0,  0,  0,  0,  0]
    where nh = fi h
          nx = fi x
          nw = fi (x + fi w - 1)

updateWin :: TVar String -> X ()
updateWin v = do
  xc <- ask
  let (conf,rec) = (config &&& rect) xc
      [lc,rc]    = if length (alignSep conf) == 2
                   then alignSep conf
                   else alignSep defaultConfig
  i <- io $ atomically $ readTVar v
  let def     = [i,[],[]]
      [l,c,r] = case break (==lc) i of
                  (le,_:re) -> case break (==rc) re of
                                 (ce,_:ri) -> [le,ce,ri]
                                 _         -> def
                  _         -> def
  ps <- io $ mapM (parseString conf) [l,c,r]
  drawInWin rec ps

-- $print

-- | Draws in and updates the window
drawInWin :: Rectangle -> [[(String, String)]] -> X ()
drawInWin (Rectangle _ _ wid ht) ~[left,center,right] = do
  r <- ask
  let (c,d ) = (config &&& display) r
      (w,fs) = (window &&& fontS  ) r
      strLn  = io . mapM (\(s,cl) -> textWidth d fs s >>= \tw -> return (s,cl,fi tw))
  bgcolor <- io $ initColor d $ bgColor c
  gc      <- io $ createGC  d w
  -- create a pixmap to write to and fill it with a rectangle
  p <- io $ createPixmap d w wid ht
            (defaultDepthOfScreen (defaultScreenOfDisplay d))
  -- the fgcolor of the rectangle will be the bgcolor of the window
  io $ setForeground d gc bgcolor
  io $ fillRectangle d p gc 0 0 wid ht
  -- write to the pixmap the new string
  printStrings p gc fs 1 L =<< strLn left
  printStrings p gc fs 1 R =<< strLn right
  printStrings p gc fs 1 C =<< strLn center
  -- copy the pixmap with the new string to the window
  io $ copyArea   d p w gc 0 0 wid ht 0 0
  -- free up everything (we do not want to leak memory!)
  io $ freeGC     d gc
  io $ freePixmap d p
  -- resync
  io $ sync       d True

-- | An easy way to print the stuff we need to print
printStrings :: Drawable -> GC -> XFont -> Position
             -> Align -> [(String, String, Position)] -> X ()
printStrings _ _ _ _ _ [] = return ()
printStrings dr gc fontst offs a sl@((s,c,l):xs) = do
  r <- ask
  (as,ds) <- io $ textExtents fontst s
  let (conf,d)             = (config &&& display) r
      Rectangle _ _ wid _  = rect r
      totSLen              = foldr (\(_,_,len) -> (+) len) 0 sl
      valign               = fi $ as + ds
      remWidth             = fi wid - fi totSLen
      offset               = case a of
                               C -> (remWidth + offs) `div` 2
                               R -> remWidth - 1
                               L -> offs
      (fc,bc)              = case (break (==',') c) of
                               (f,',':b) -> (f, b           )
                               (f,    _) -> (f, bgColor conf)
  io $ printString d dr fontst gc fc bc offset valign s
  printStrings dr gc fontst (offs + l) a xs
