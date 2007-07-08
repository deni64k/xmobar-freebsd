{-# OPTIONS -fglasgow-exts #-}
-----------------------------------------------------------------------------
-- |
-- Module      :  XMobar
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

module XMobar (-- * Main Stuff
               -- $main
               Xbar
              , runXMobar
              , eventLoop
              , createWin
              -- * Printing
              -- $print
              , drawInWin
              , printStrings
              -- * Program Execution
              -- $commands
              , execCommands
              , execCommand
              , runCommandLoop
              , readVariables
              -- * Unmamaged Windows
              -- $unmanwin
              , mkUnmanagedWindow
              -- * Useful Utilities
              , initColor
              , io
              ) where

import Graphics.X11.Xlib
import Graphics.X11.Xlib.Misc

import Control.Monad.State
import Control.Monad.Reader
import Control.Concurrent

import Config
import Parsers
import Commands

-- $main
--
-- The XMobar data type and basic loops and functions.

-- | This is just esthetics, stolen from XMonad: see 'runXMobar'
newtype Xbar a = X (ReaderT Config (StateT XState IO) a)
    deriving (Functor, Monad, MonadIO, MonadState XState, MonadReader Config)

-- | The State component of StateT
data XState = 
    XState { display :: Display
           , window :: Window
           , vars :: [(ThreadId, MVar String)]
           }

-- | Totally useless: but it is nice to be able to use get to get the
-- state and ask to get the configuration: functions requires less
-- arguments, after all.
runXMobar :: Config -> [(ThreadId, MVar String)] -> Display -> Window -> Xbar () -> IO ()
runXMobar c v d w (X f) = 
    do runStateT (runReaderT f c) (XState d w v)
       return ()

-- | The event loop
eventLoop :: Xbar ()
eventLoop =
    do c <- ask
       s <- get
       i <- io $ readVariables (vars s)
       ps <- io $ parseString c i
       drawInWin ps
       -- back again: we are never ending
       io $ tenthSeconds (refresh c)
       eventLoop

-- | The function to create the initial window
createWin :: Config -> IO (Display, Window)
createWin config =
  do dpy   <- openDisplay ""
     let dflt = defaultScreen dpy
     rootw  <- rootWindow dpy dflt
     win <- mkUnmanagedWindow dpy (defaultScreenOfDisplay dpy) rootw 
            (fi $ xPos config) 
            (fi $ yPos config) 
            (fi $ width config) 
            (fi $ height config)
     mapWindow dpy win
     return (dpy,win)


-- $print

-- | Draws in and updates the window
drawInWin :: [(String, String)] -> Xbar ()
drawInWin str = 
    do config <- ask
       st <- get
       let (dpy,win) = (display st, window st)
       bgcolor  <-  io $ initColor dpy $ bgColor config
       gc <- io $ createGC dpy win
       --let's get the fonts
       fontst <-  io $ loadQueryFont dpy (font config)
       io $ setFont dpy gc (fontFromFontStruct fontst)
       -- create a pixmap to write to and fill it with a rectangle:
       p <- io $ createPixmap dpy win 
            (fi (width config)) 
            (fi (height config)) 
            (defaultDepthOfScreen (defaultScreenOfDisplay dpy)) 
       io $ setForeground dpy gc bgcolor
       io $ fillRectangle dpy p gc 0 0 
              (fi $ width config) 
              (fi $ height config)
       -- write to the pixmap the new string:
       let strWithLenth = map (\(s,c) -> (s,c,textWidth fontst s)) str
       p' <- printStrings p gc fontst 1 strWithLenth 
       -- copy the pixmap with the new string to the window.
       io $ copyArea dpy p' win gc 
              (fi (xPos config)) 
              (fi (yPos config)) 
              (fi (width config)) 
              (fi (height config)) 0 0
       -- free up everything (we do not want to leak memory!)
       io $ freeFont dpy fontst
       io $ freeGC dpy gc
       io $ freePixmap dpy p'
       -- resync
       io $ sync dpy True

-- | An easy way to print the stuff we need to print
printStrings :: Drawable 
             -> GC
             -> FontStruct
             -> Position
             -> [(String, String, Position)]
             -> Xbar Pixmap --()
printStrings p _ _ _ [] = return p --()
printStrings p gc fontst offs sl@((s,c,l):xs) =
    do config <- ask
       st <- get
       let (_,asc,_,_) = textExtents fontst s
           totSLen = foldr (\(_,_,len) -> (+) len) 0 sl
           valign = (fi (height config) + fi asc) `div` 2
           remWidth = fi (width config) - fi totSLen
           offset = case (align config) of
                      "center" -> (remWidth + offs) `div` 2
                      "right" -> remWidth - 1
                      "left" -> offs
                      _ -> offs
       fgcolor <- io $ initColor (display st) c
       bgcolor <- io $ initColor (display st) (bgColor config)
       io $ setForeground (display st) gc fgcolor
       io $ setBackground (display st) gc bgcolor
       io $ drawImageString (display st) p gc offset valign s
       p' <- printStrings p gc fontst (offs + l) xs
       return p'

-- $commands

-- | Gets the refresh rate set in configuration for a given command.
getRefRate :: Config -> Command -> Int
getRefRate c com =
    let l = commands c
        p = filter (\(s,_) -> s == com) l
    in case p of
         [(_,int)] -> int
         _ -> refresh c

-- | Runs a list of programs as independent threads and returns their thread id
-- and the MVar they will be writing to.
execCommands :: Config -> [(Command,String,String)] -> IO [(ThreadId, MVar String)]
execCommands _ [] = return []
execCommands c (x:xs) =
    do i <- execCommand c x
       is <- execCommands c xs
       return $ i : is

execCommand :: Config -> (Command,String,String) -> IO (ThreadId, MVar String)
execCommand c com = 
    do var <- newMVar "Updating..."
       h <- forkIO $ runCommandLoop var c com
       return (h,var)

runCommandLoop :: MVar String -> Config -> (Command,String,String) -> IO ()
runCommandLoop var conf c@(com,s,ss)
    | show com == "" = 
        do modifyMVar_ var (\_ -> return $ "Could not parse the template")
           tenthSeconds (refresh conf)
           runCommandLoop var conf c
    | otherwise =
        do str <- run com
           modifyMVar_ var (\_ -> return $ s ++ str ++ ss)
           tenthSeconds (getRefRate conf com)
           runCommandLoop var conf c

-- | Reads MVars set by 'runCommandLoop'
readVariables :: [(ThreadId, MVar String)] -> IO String
readVariables [] = return ""
readVariables ((_,v):xs) =
    do f <- readMVar v
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

-- | Get the Pixel value for a named color
initColor :: Display -> String -> IO Pixel
initColor dpy c = (color_pixel . fst) `liftM` allocNamedColor dpy colormap c
    where colormap = defaultColormap dpy (defaultScreen dpy)

-- | Short-hand for lifting in the IO monad
io :: IO a -> Xbar a
io = liftIO

-- | Work arount to the Int max bound: since threadDelay takes an Int, it
-- is not possible to set a thread delay grater than about 45 minutes.
-- With a little recursion we solve the problem.
tenthSeconds :: Int -> IO ()
tenthSeconds s | s >= x = do threadDelay y
                             tenthSeconds (x - s)
               | otherwise = threadDelay (s * 100000)
               where y = (maxBound :: Int)
                     x = y `div` 100000

-- | Short-hand for 'fromIntegral'
fi :: (Integral a, Num b) => a -> b
fi = fromIntegral
