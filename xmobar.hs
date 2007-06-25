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
              Config (..)
              -- * Main Stuff
              -- $main
            , Xbar
            , main
            , runXMobar
            , eventLoop
            , createWin
            , drawInWin
              -- * Printing
              -- $print
            , printStrings
              -- * Program Execution
              -- $commands
            , getOptions
            , execCommands
            , runCom
              -- * Parsing
              -- $parser
            , parseString
            , stringParser
            , defaultColors
            , colorsAndText
            , templateStringParser
            , templateCommandParser
            , templateParser
            , parseTemplate
              -- * Unmamaged Windows
              -- $unmanwin
            , mkUnmanagedWindow
              -- * Useful Utilities
            , readConfig
            , initColor
            , io
            ) where

import Graphics.X11.Xlib
import Graphics.X11.Xlib.Misc

import Text.ParserCombinators.Parsec

import Control.Monad.Reader
import Control.Concurrent
import System.Environment

import System.Process
import System.Exit
import System.IO (hClose, hGetLine)

-- $config
-- Configuration data type and default configuration

-- | The configuration data type
data Config = 
    Config { fonts          :: String   -- ^ Fonts
           , bgColor        :: String   -- ^ Backgroud color
           , fgColor        :: String   -- ^ Default font color
           , xPos           :: Int      -- ^ x Window position (origin in the upper left corner) 
           , yPos           :: Int      -- ^ y Window position 
           , width          :: Int      -- ^ Window width
           , hight          :: Int      -- ^ Window hight
           , align          :: String   -- ^ text alignment
           , refresh        :: Int      -- ^ Refresh rate in tenth of seconds
           , commands       :: [(String,[String])]   -- ^ For setting the options of the programs to run (optionals)
           , sepChar        :: String     -- ^ The character to be used for indicating 
                                        --   commands in the output template (default '%')
           , template       :: String   -- ^ The output template 
           } deriving (Eq, Show, Read, Ord)

defaultConfig :: Config
defaultConfig =
    Config { fonts = "-misc-fixed-*-*-*-*-10-*-*-*-*-*-*-*"
           , bgColor = "#000000"
           , fgColor = "#BFBFBF"
           , xPos = 0
           , yPos = 0
           , width = 1024
           , hight = 15
           , align = "left"
           , refresh = 10
           , commands = []
           , sepChar = "%"
           , template = "Uptime: <fc=#00FF00>%uptime%</fc> ** <fc=#FF0000>%date%</fc>"
           }

-- | This is just esthetics: see 'runXMobar'
type Xbar a = ReaderT Config IO a

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
       runReaderT runXMobar config

-- | Totally useless: since most of the operations are done in the IO
-- monad it is ofter simpler to pass Config instead of lifting everytime.
-- But we like the mtl library...;-)
runXMobar :: Xbar ()
runXMobar =
    do config <- ask
       dwgf <- io $ createWin config
       eventLoop dwgf    

-- | The event loop
eventLoop :: (Display, Window) -> Xbar ()
eventLoop (d,w) =
    do c <- ask
       cl <- io $ parseTemplate c (template c)
       i <- io $ execCommands c cl
       ps <- io $ parseString c i
       io $ drawInWin c (d,w) ps
       -- back again: we are never ending
       io $ threadDelay $ 100000 * refresh c
       eventLoop (d,w)

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

-- | Draws in and updates the window
drawInWin :: Config -> (Display, Window) -> [(String, String)] -> IO ()
drawInWin config (dpy, win) str = 
     do bgcolor  <- initColor dpy $ bgColor config
        gc <- createGC dpy win
        --let's get the fonts
        fontst <- loadQueryFont dpy (fonts config)
        setFont dpy gc (fontFromFontStruct fontst)

        -- set window background 
        setForeground dpy gc bgcolor
        fillRectangle dpy win gc 0 0 
                          (fromIntegral $ width config) 
                          (fromIntegral $ hight config)
        -- write
        let strWithLenth = map (\(s,c) -> (s,c,textWidth fontst s)) str
        printStrings config dpy win gc fontst 1 strWithLenth 
        -- free everything
        freeFont dpy fontst
        freeGC dpy gc
        flush dpy

-- $print

-- | An easy way to print the stuff we need to print
printStrings :: Config 
             -> Display
             -> Drawable
             -> GC
             -> FontStruct
             -> Position
             -> [(String, String, Position)]
             -> IO ()
printStrings _ _ _ _ _ _ [] = return ()
printStrings config dpy win gc fontst offs sl@((s,c,l):xs) =
    do let (_,asc,_,_) = textExtents fontst s
           totSLen = foldr (\(_,_,len) -> (+) len) 0 sl
           valign = (fromIntegral (hight config) + fromIntegral asc) `div` 2
           offset = case (align config) of
                      "center" -> (fromIntegral (width config) - fromIntegral totSLen) `div` 2
                      "right" -> fromIntegral (width config) - fromIntegral totSLen
                      "left" -> offs
                      _ -> offs
       color <- initColor dpy c
       setForeground dpy gc color
       drawString dpy win gc offset valign s
       printStrings config dpy win gc fontst (offs + l) xs

-- $commands

-- | Gets the command options set in configuration.
getOptions :: Config -> String -> [String]
getOptions c com =
    let l = commands c
        p = filter (\(s,_) -> s == com) l
    in case p of
         [(_,opts)] -> opts
         _ -> []

-- | Runs a list of programs
execCommands :: Config -> [(String,String,String)] -> IO String
execCommands _ [] = return ""
execCommands c ((s,com,ss):xs) =
    do i <- runCom c com
       is <- execCommands c xs
       return $ s ++ i ++ ss ++ is

-- | Runs the external program
runCom :: Config -> String -> IO String
runCom c com =
    do (i,o,e,p) <- runInteractiveCommand (com ++ concat (map (' ':) $ getOptions c com))
       -- the followinf leaks memory
       --(i,o,e,p) <- runInteractiveProcess com (getOptions c com) Nothing Nothing
       exit <- waitForProcess p
       let closeHandles = do hClose o
                             hClose i
                             hClose e
       case exit of
         ExitSuccess -> do str <- hGetLine o
                           closeHandles
                           return str
         _ -> do closeHandles
                 return $ "Could not execute command " ++ com

{- $parser
These are the neede parsers. Don't trust them too much.

There are parsers for the commands output and parsers for the
formatting template.
 -}

-- | Runs the actual string parsers
parseString :: Config -> String -> IO [(String, String)]
parseString config s = 
    case (parse (stringParser config) "" s) of
      Left _ -> return [("Sorry, if I were a decent parser you now would be starring at something meaningful..."
                        , (fgColor config))]
      Right x  -> return x

-- | Gets the string and combines the needed parsers
stringParser :: Config -> Parser [(String, String)]
stringParser c = manyTill (colorsAndText c <|> defaultColors c) eof

-- | Parses a string with the default color (no color set)
defaultColors :: Config -> Parser (String, String)
defaultColors config = 
    do { s <- many $ noneOf "<"
       ; return (s,(fgColor config))
       }
    <|> colorsAndText config

-- | Parses a string with a color set
colorsAndText :: Config -> Parser (String, String) 
colorsAndText config = 
    do { string "<fc=#"
       ; n <- count 6 hexDigit
       ; string ">"
       ; s <- many $ noneOf "<"
       ; string "</fc>"
       ; return (s,"#"++n)
       }
    <|> defaultColors config

-- | Parses the output template string
templateStringParser :: Config -> Parser (String,String,String)
templateStringParser c =
    do{ s <- many $ noneOf "%"
      ; (_,com,_) <- templateCommandParser c
      ; ss <- many $ noneOf "%"
      ; return (s, com, ss)
      } 

-- | Parses the command part of the template string
templateCommandParser :: Config -> Parser (String,String,String)
templateCommandParser c =
    do { let chr = head $ sepChar c
       ; char chr
       ; com <- many $ noneOf (sepChar c)
       ; char chr
       ; return $ ("",com,"")
       }
-- | Combines the template parsers
templateParser :: Config -> Parser [(String,String,String)]
templateParser c = many (templateStringParser c <|> templateCommandParser c)

-- | Actually runs the template parsers
parseTemplate :: Config -> String -> IO [(String,String,String)]
parseTemplate config s = 
    case (parse (templateParser config) "" s) of
      Left _ -> return [("Could not parse templete","","")]
      Right x  -> return x

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

-- | Short-hand for lifting in the IO monad
io :: IO a -> Xbar a
io = liftIO
