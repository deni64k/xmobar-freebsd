-----------------------------------------------------------------------------
-- |
-- Module      :  Xmobar.Main
-- Copyright   :  (c) Andrea Rossato
-- License     :  BSD-style (see LICENSE)
-- 
-- Maintainer  :  Andrea Rossato <andrea.rossato@unibz.it>
-- Stability   :  unstable
-- Portability :  unportable
--
-- The main module of Xmobar, a text based status bar 
--
-----------------------------------------------------------------------------

module Main ( -- * Main Stuff
              -- $main
              main
            , readConfig
            , readDefaultConfig
            ) where

import Xmobar
import Parsers
import Config

import Data.IORef
import System.Console.GetOpt
import System.Exit
import System.Environment
import System.Posix.Files

-- $main
 
-- | The main entry point
main :: IO ()
main = 
    do args <- getArgs
       (o,file) <- getOpts args
       conf <- case file of
                 [cfgfile] -> readConfig cfgfile
                 _         -> readDefaultConfig
       c <- newIORef conf 
       doOpts c o
       config <- readIORef c
       cl <- parseTemplate config (template config)
       var <- execCommands config cl
       (d,w) <- createWin config
       eventLoop config var d w
       return ()

-- | Reads the configuration files or quits with an error
readConfig :: FilePath -> IO Config
readConfig f = 
    do s <- readFile f
       case reads s of
         [(config,_)] -> return config
         [] -> error ("Corrupt config file: " ++ f)
         _ -> error ("Some problem occured. Aborting...")

-- | Read default configuration file or load the default config
readDefaultConfig :: IO Config
readDefaultConfig = 
    do home <- getEnv "HOME"
       let path = home ++ "/.xmobarrc"
       f <- fileExist path
       if f then readConfig path else return defaultConfig

data Opts = Help
          | Version 
          | Font String
          | BgColor String
          | FgColor String
          | XPos String
          | YPos String
          | Width String
          | Height String
          | Align String
          | Refresh String
          | Commands String
          | SepChar String
          | Template String 
       deriving Show
    
options :: [OptDescr Opts]
options =
    [ Option ['h','?'] ["help"] (NoArg Help) "This help"
    , Option ['V'] ["version"] (NoArg Version) "Show version information"
    , Option ['f'] ["font"] (ReqArg Font "font name") "The font name"
    , Option ['B'] ["bgcolor"] (ReqArg BgColor "backgorund color") "The background color. Default balck"
    , Option ['F'] ["fgcolor"] (ReqArg FgColor "foregorund color") "The foreground color. Default grey"
    , Option ['x'] ["xpos"] (ReqArg XPos "x position") "The x position. Default 0"
    , Option ['y'] ["ypos"] (ReqArg YPos "y position") "The y position. Default 0"
    , Option ['W'] ["width"] (ReqArg Width "width") "The status bar width. Default 1024"
    , Option ['H'] ["height"] (ReqArg Height "height") "The status bar heigth. Default 15"
    , Option ['a'] ["align"] (ReqArg Align "text alignement") "The text alignment: center, left or right. Default: left"
    , Option ['r'] ["refresh"] (ReqArg Refresh "default rate") "The refresh rate in tenth of seconds: dafault 1 sec."
    , Option ['s'] ["sepchar"] (ReqArg SepChar "separation character") "The charater used to separate commands in the output templae. Default '%'"
    , Option ['t'] ["template"] (ReqArg Template "tempate") "The output template"
    , Option ['c'] ["commands"] (ReqArg Commands  "commands")  "The list of commands to be executed"
    ]

getOpts :: [String] -> IO ([Opts], [String])
getOpts argv = 
    case getOpt Permute options argv of
      (o,n,[]) -> return (o,n)
      (_,_,errs) -> error (concat errs ++ usage)

usage :: String
usage = usageInfo header options
    where header = "Usage: xmobar [OPTION...] [FILE]"

version :: String
version = "Xmobar 0.7 (c) 2007 Andrea Rossato <andrea.rossato@unibz.it>"

doOpts :: IORef Config -> [Opts] -> IO ()
doOpts _  [] = return ()
doOpts conf (o:oo) =
    case o of
      Help -> putStr usage >> exitWith ExitSuccess
      Version -> putStrLn version >> exitWith ExitSuccess
      Font s -> modifyIORef conf (\c -> c { font = s }) >> doOpts conf oo
      BgColor s -> modifyIORef conf (\c -> c { bgColor = s }) >> doOpts conf oo
      FgColor s -> modifyIORef conf (\c -> c { fgColor = s }) >> doOpts conf oo
      XPos s -> modifyIORef conf (\c -> c { xPos = readInt s c xPos}) >> doOpts conf oo
      YPos s -> modifyIORef conf (\c -> c { yPos = readInt s c yPos }) >> doOpts conf oo
      Width s -> modifyIORef conf (\c -> c { width = readInt s c width }) >> doOpts conf oo
      Height s -> modifyIORef conf (\c -> c { height = readInt s c height }) >> doOpts conf oo
      Align s -> modifyIORef conf (\c -> c { align = s }) >> doOpts conf oo
      Refresh s -> modifyIORef conf (\c -> c { refresh = readInt s c refresh }) >> doOpts conf oo
      SepChar s -> modifyIORef conf (\c -> c { sepChar = s }) >> doOpts conf oo
      Template s -> modifyIORef conf (\c -> c { template = s }) >> doOpts conf oo
      Commands s -> do case readCom s of
                         Right x -> modifyIORef conf ((\v c -> c { commands = v }) x) >> doOpts conf oo 
                         Left e -> putStr (e ++ usage) >> exitWith (ExitFailure 1)
    where readCom :: Read a => String -> Either String a
          readCom str = case readStr str of
	                  [x] -> Right x
	                  _  -> Left "xmobar: cannot read list of commands specified with the -c option\n"
          readInt str c f = case readStr str of
	                      [x] -> x
	                      _  -> f c
          readStr str = [x | (x,t) <- reads str, ("","") <- lex t]
