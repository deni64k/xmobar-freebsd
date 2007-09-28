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
               Xbar
              , runXbar
              , eventLoop
              , createWin
              , updateWin
              -- * Printing
              -- $print
              , drawInWin
              , printStrings
              -- * Program Execution
              -- $commands
              , execCommand
              , readVariables
              -- * Unmamaged Windows
              -- $unmanwin
              , mkUnmanagedWindow
              -- * Useful Utilities
              , initColor
              , io
              ) where

import Prelude hiding (catch)
import Graphics.X11.Xlib
import Graphics.X11.Xlib.Misc
import Graphics.X11.Xlib.Event

import Control.Monad.State
import Control.Monad.Reader
import Control.Concurrent
import Control.Exception

import System.Posix.Types (Fd(..))

import Config
import Parsers
import Commands
import Runnable

-- $main
--
-- The Xmobar data type and basic loops and functions.

-- | This is copied from XMonad.
newtype Xbar a = X (ReaderT Config (StateT XState IO) a)
    deriving (Functor, Monad, MonadIO, MonadState XState, MonadReader Config)

-- | The State component of StateT
data XState = 
    XState { display :: Display
           , window  :: Window
           , vars    :: [(Maybe ThreadId, MVar String)]
           }

-- | We use get to get the state and ask to get the configuration: whis way 
-- functions requires less arguments.
runXbar :: Config -> [(Maybe ThreadId, MVar String)] -> Display -> Window -> Xbar () -> IO ()
runXbar c v d w (X f) = 
    do runStateT (runReaderT f c) (XState d w v)
       return ()

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

-- | The event loop
eventLoop :: Config -> [(Maybe ThreadId, MVar String)] -> Display -> Window -> IO ()
eventLoop c v d w = do
    t <- forkIO (block go)
    timer t
 where
    -- interrupt the drawing thread every so often
    timer t = do
        tenthSeconds (refresh c)
        throwTo t (ErrorCall "Xmobar.eventLoop: yield")
        timer t
    -- Continuously wait for a timer interrupt or an expose event
    go = do
        runXbar c v d w updateWin
        catch (unblock $ allocaXEvent $ nextEvent' d) (const $ return ())
        go

-- | The function to create the initial window
createWin :: Config -> IO (Display, Window)
createWin config =
  do dpy   <- openDisplay ""
     let dflt = defaultScreen dpy
     rootw <- rootWindow dpy dflt
     win   <- mkUnmanagedWindow dpy (defaultScreenOfDisplay dpy) rootw 
            (fi $ xPos config) 
            (fi $ yPos config) 
            (fi $ width config) 
            (fi $ height config)
     selectInput dpy win exposureMask
     mapWindow   dpy win
     return (dpy,win)

updateWin :: Xbar ()
updateWin =
    do c  <- ask
       s  <- get
       i  <- io $ readVariables (vars s)
       ps <- io $ parseString c i
       drawInWin ps

-- $print

-- | Draws in and updates the window
drawInWin :: [(String, String)] -> Xbar ()
drawInWin str = 
    do config  <- ask
       st      <- get
       let (dpy,win) = (display st, window st)
       bgcolor <- io $ initColor dpy $ bgColor config
       gc      <- io $ createGC dpy win
       --let's get the fonts
       let lf c = loadQueryFont dpy (font c)
       fontst  <- io $ catch (lf config) (const $ lf defaultConfig)
       io $ setFont dpy gc (fontFromFontStruct fontst)
       -- create a pixmap to write to and fill it with a rectangle
       p       <- io $ createPixmap dpy win 
                         (fi (width  config)) 
                         (fi (height config)) 
                         (defaultDepthOfScreen (defaultScreenOfDisplay dpy))
       -- the fgcolor of the rectangle will be the bgcolor of the window
       io $ setForeground dpy gc bgcolor
       io $ fillRectangle dpy p gc 0 0 
              (fi $ width  config) 
              (fi $ height config)
       -- write to the pixmap the new string
       let strWithLenth = map (\(s,c) -> (s,c,textWidth fontst s)) str
       printStrings p gc fontst 1 strWithLenth 
       -- copy the pixmap with the new string to the window
       io $ copyArea dpy p win gc 0 0 (fi (width config)) (fi (height config)) 0 0
       -- free up everything (we do not want to leak memory!)
       io $ freeFont   dpy fontst
       io $ freeGC     dpy gc
       io $ freePixmap dpy p
       -- resync
       io $ sync dpy True

-- | An easy way to print the stuff we need to print
printStrings :: Drawable 
             -> GC
             -> FontStruct
             -> Position
             -> [(String, String, Position)]
             -> Xbar ()
printStrings _ _ _ _ [] = return ()
printStrings d gc fontst offs sl@((s,c,l):xs) =
    do config <- ask
       st <- get
       let (_,asc,_,_) = textExtents fontst s
           totSLen     = foldr (\(_,_,len) -> (+) len) 0 sl
           valign      = (fi (height config) + fi asc) `div` 2
           remWidth    = fi (width config) - fi totSLen
           offset      = case (align config) of
                           "center" -> (remWidth + offs) `div` 2
                           "right"  -> remWidth - 1
                           "left"   -> offs
                           _        -> offs
       fgcolor <- io $ initColor (display st) c
       bgcolor <- io $ initColor (display st) (bgColor config)
       io $ setForeground (display st) gc fgcolor
       io $ setBackground (display st) gc bgcolor
       io $ drawImageString (display st) d gc offset valign s
       printStrings d gc fontst (offs + l) xs

-- $commands

-- | Runs a command as an independent thread and returns its thread id
-- and the MVar the command will be writing to.
execCommand :: (Runnable,String,String) -> IO (Maybe ThreadId, MVar String)
execCommand (com,s,ss)
    | alias com == ""  = do var <- newMVar "Updating..."
                            modifyMVar_ var (const $ return $ "Could not parse the template")
                            return (Nothing,var)
    | otherwise        = do var <- newMVar "Updating..."
                            let cb str = modifyMVar_ var (\_ -> return $ s ++ str ++ ss)
                            h <- forkIO $ start com cb
                            return (Just h,var)

-- | Reads MVars set by 'runCommandLoop'
readVariables :: [(Maybe ThreadId, MVar String)] -> IO String
readVariables [] = return ""
readVariables ((_,v):xs) =
    do f  <- readMVar v
       fs <- readVariables xs
       return $! f ++ fs

{- $unmanwin

This is a way to create unmamaged window. It was a mistery in Haskell. 
Till I've found out...;-)

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
  let visual = defaultVisualOfScreen scr
      attrmask = cWOverrideRedirect
  win <- allocaSetWindowAttributes $ 
         \attributes -> do
           set_override_redirect attributes True
           createWindow dpy rw x y w h 0 (defaultDepthOfScreen scr) 
                        inputOutput visual attrmask attributes                                
  return win

{- $utility
Utilities, aka stollen without givin' credit stuff.
-}

-- | Get the Pixel value for a named color: if an invalid name is
-- given the black pixel will be returned.
initColor :: Display -> String -> IO Pixel
initColor dpy c =
    catch (initColor' dpy c) (const $ return $ blackPixel dpy (defaultScreen dpy))

initColor' :: Display -> String -> IO Pixel
initColor' dpy c = (color_pixel . fst) `liftM` allocNamedColor dpy colormap c
    where colormap = defaultColormap dpy (defaultScreen dpy)

-- | Short-hand for lifting in the IO monad
io :: IO a -> Xbar a
io = liftIO

-- | Short-hand for 'fromIntegral'
fi :: (Integral a, Num b) => a -> b
fi = fromIntegral
