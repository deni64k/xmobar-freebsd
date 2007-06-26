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
              , getOptions
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

import System.Process
import System.Exit
import System.IO (hClose, hGetLine)

import Config
import Parsers

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
       io $ threadDelay $ 100000 * refresh c
       eventLoop

-- | The function to create the initial window
createWin :: Config -> IO (Display, Window)
createWin config =
  do dpy   <- openDisplay ""
     let dflt = defaultScreen dpy
     rootw  <- rootWindow dpy dflt
     win <- mkUnmanagedWindow dpy (defaultScreenOfDisplay dpy) rootw 
            (fromIntegral $ xPos config) 
            (fromIntegral $ yPos config) 
            (fromIntegral $ width config) 
            (fromIntegral $ hight config)
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
       fontst <-  io $ loadQueryFont dpy (fonts config)
       io $ setFont dpy gc (fontFromFontStruct fontst)

       -- set window background 
       io $ setForeground dpy gc bgcolor
       io $ fillRectangle dpy win gc 0 0 
              (fromIntegral $ width config) 
              (fromIntegral $ hight config)
       -- write
       let strWithLenth = map (\(s,c) -> (s,c,textWidth fontst s)) str
       printStrings gc fontst 1 strWithLenth 
       -- free everything
       io $ freeFont dpy fontst
       io $ freeGC dpy gc
       io $ flush dpy

-- | An easy way to print the stuff we need to print
printStrings :: GC
             -> FontStruct
             -> Position
             -> [(String, String, Position)]
             -> Xbar ()
printStrings _ _ _ [] = return ()
printStrings gc fontst offs sl@((s,c,l):xs) =
    do config <- ask
       st <- get
       let (_,asc,_,_) = textExtents fontst s
           totSLen = foldr (\(_,_,len) -> (+) len) 0 sl
           valign = (fromIntegral (hight config) + fromIntegral asc) `div` 2
           offset = case (align config) of
                      "center" -> (fromIntegral (width config) - fromIntegral totSLen) `div` 2
                      "right" -> fromIntegral (width config) - fromIntegral totSLen
                      "left" -> offs
                      _ -> offs
       color <- io $ initColor (display st) c
       io $ setForeground (display st) gc color
       io $ drawString (display st) (window st) gc offset valign s
       printStrings gc fontst (offs + l) xs

-- $commands

-- | Gets the command options set in configuration.
getOptions :: Config -> String -> [String]
getOptions c com =
    let l = commands c
        p = filter (\(s,_,_) -> s == com) l
    in case p of
         [(_,_,opts)] -> opts
         _ -> []

-- | Gets the command options set in configuration.
getRefRate :: Config -> String -> Int
getRefRate c com =
    let l = commands c
        p = filter (\(s,_,_) -> s == com) l
    in case p of
         [(_,int,_)] -> int
         _ -> refresh c

-- | Runs a list of programs
execCommands :: Config -> [(String,String,String)] -> IO [(ThreadId, MVar String)]
execCommands _ [] = return []
execCommands c (x:xs) =
    do i <- execCommand c x
       is <- execCommands c xs
       return $ i : is

execCommand :: Config -> (String,String,String) -> IO (ThreadId, MVar String)
execCommand c com = 
    do var <- newMVar "Updating..."
       h <- forkIO $ runCommandLoop var c com
       return (h,var)

-- | Runs the external program
runCommandLoop :: MVar String -> Config -> (String,String,String) -> IO ()
runCommandLoop var conf c@(s,com,ss) 
    | com == "" = 
        do modifyMVar_ var (\_ -> return $ "Could not parse the template")
           threadDelay (100000 * (refresh conf))
           runCommandLoop var conf c
    | otherwise =
        do (i,o,e,p) <- runInteractiveCommand (com ++ concat (map (' ':) $ getOptions conf com))
           -- the followinf leaks memory
           --(i,o,e,p) <- runInteractiveProcess com (getOptions c com) Nothing Nothing
           exit <- waitForProcess p
           let closeHandles = do hClose o
                                 hClose i
                                 hClose e
           case exit of
             ExitSuccess -> do str <- hGetLine o
                               closeHandles
                               modifyMVar_ var (\_ -> return $ s ++ str ++ ss)
                               threadDelay (100000 * (getRefRate conf com))
                               runCommandLoop var conf c
             _ -> do closeHandles
                     modifyMVar_ var $ \_ -> return $ "Could not execute command " ++ com
                     threadDelay (100000 * (getRefRate conf com))
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
