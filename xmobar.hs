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

module Main ( -- * Configuration
              -- $config
              Config (..),
              -- * Main Stuff
              -- $main
              main
            , eventLoop
            , createWin
            , drawInWin
              -- * Printing
              -- $print
            , printStrings
              -- * Parsing
              -- $parser
            , stringParse 
            , stringParser
            , defaultColors
            , colorsAndText
              -- * Unmamaged Windows
              -- $unmanwin
            , mkUnmanagedWindow
              -- * Useful Utilities
            , readConfig
            , initColor
            ) where

import Graphics.X11.Xlib
import Graphics.X11.Xlib.Misc

import Text.ParserCombinators.Parsec

import Control.Monad
import System.Environment

-- $config
-- Configuration data type and default configuration

-- | The configuration data type
data Config = 
    Config { fonts :: String   -- ^ Fonts
           , bgColor :: String -- ^ Backgroud color
           , fgColor :: String -- ^ Default font color
           , xPos :: Int       -- ^ x Window position (origin in the upper left corner) 
           , yPos :: Int       -- ^ y Window position 
           , width :: Int      -- ^ Window width
           , hight :: Int      -- ^ Window hight
           } deriving (Eq, Show, Read, Ord)

defaultConfig :: Config
defaultConfig =
    Config { fonts = "-misc-fixed-*-*-*-*-*-*-*-*-*-*-*-*" 
           , bgColor = "#000000"
           , fgColor = "#ffffff"
           , xPos = 0
           , yPos = 0
           , width = 1024
           , hight = 15
           }

-- $main

-- | The main entry point
main :: IO ()
main = 
    do args <- getArgs
       config <-
           if length args /= 1
              then do putStrLn ("No configuration file specified. Using default settings.")
                      return defaultConfig
              else readConfig (args!!0)
       eventLoop config

-- | The event loop
eventLoop :: Config -> IO ()
eventLoop c =
    do i <- getLine
       ps <- stringParse c i
       w <- createWin c
       drawInWin c w ps

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

-- | Draws and updates the window
drawInWin :: Config -> (Display, Window) -> [(String, String)] -> IO ()
drawInWin config (dpy, win) str = do
  -- get win bgcolor
  bgcolor  <- initColor dpy $ bgColor config

  -- set window background 
  gc <- createGC dpy win
  setForeground dpy gc bgcolor
  fillRectangle dpy win gc 0 0 
                    (fromIntegral $ width config) 
                    (fromIntegral $ hight config)

  -- let's get the fonts
  fontst <- loadQueryFont dpy (fonts config)
  setFont dpy gc (fontFromFontStruct fontst)
  
  -- print what you need to print
  let strWithLenth = map (\(s,c) -> (s,c,textWidth fontst s)) str
  printStrings dpy win gc fontst 1 strWithLenth 

  -- refreesh, fre, resync... do what you gotta do
  freeGC dpy gc
  sync dpy True
  -- back again: we are never ending
  eventLoop config


-- $print

-- | An easy way to print the stuff we need to print
printStrings :: Display
             -> Drawable
             -> GC
             -> FontStruct
             -> Position
             -> [(String, String, Position)]
             -> IO ()
printStrings _ _ _ _ _ [] = return ()
printStrings dpy win gc fontst offset ((s,c,l):xs) =
    do let (_,asc,_,_) = textExtents fontst s
       color  <- initColor dpy c
       setForeground dpy gc color
       drawString dpy win gc offset asc s
       printStrings dpy win gc fontst (offset + l) xs

{- $parser
This is suppose do be a parser. Don't trust him.
-}

-- | Run the actual parsers
stringParse :: Config -> String -> IO [(String, String)]
stringParse config s = 
    case (parse (stringParser config) "" s) of
      Left _ -> return [("Sorry, if I were a decent parser you now would be starring at something meaningful..."
                        , (fgColor config))]
      Right x  -> return x

-- | Get the string and combine the needed parsers
stringParser :: Config -> Parser [(String, String)]
stringParser c = manyTill (choice [colorsAndText c,defaultColors c]) eof

-- | parses a string with default color (no color set)
defaultColors :: Config -> Parser (String, String)
defaultColors config = 
    do { s <- many $ noneOf "^"
       ; notFollowedBy (char '#')
       ; return (s,(fgColor config))
       }
    <|> colorsAndText config

-- | parses a string with a color set
colorsAndText :: Config -> Parser (String, String) 
colorsAndText config = 
    do { string "^#"
       ; n <- count 6 hexDigit
       ; s <- many $ noneOf "^"
       ; notFollowedBy (char '#') 
       ; return (s,"#"++n)
       }
    <|> defaultColors config


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
  window <- allocaSetWindowAttributes $ 
            \attributes -> do
              set_override_redirect attributes True
              createWindow dpy rw x y w h 0 (defaultDepthOfScreen scr) 
                           inputOutput visual attrmask attributes                                
  return window

{- $utility
Utilities, aka stollen without givin' credit stuff.
-}

-- | Reads the configuration files or quits with an error
readConfig :: FilePath -> IO Config
readConfig f = 
    do s <- readFile f
       case reads s of
         [(config,_)] -> return config
         [] -> error ("Corrupt config file: " ++ f)
         _ -> error ("Some problem occured. Aborting...")


-- | Get the Pixel value for a named color
initColor :: Display -> String -> IO Pixel
initColor dpy c = (color_pixel . fst) `liftM` allocNamedColor dpy colormap c
    where colormap = defaultColormap dpy (defaultScreen dpy)


