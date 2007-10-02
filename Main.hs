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
    do args     <- getArgs
       (o,file) <- getOpts args
       conf     <- case file of
                     [cfgfile] -> readConfig cfgfile
                     _         -> readDefaultConfig
       c        <- newIORef conf
       doOpts c o
       config   <- readIORef c
       cl       <- parseTemplate config (template config)
       vars     <- mapM startCommand cl
       (d,w)    <- createWin config
       eventLoop config vars d w
       return ()

-- | Reads the configuration files or quits with an error
readConfig :: FilePath -> IO Config
readConfig f = 
    do file <- fileExist f
       s    <- if file then readFile f else error $ f ++ ": file not found!\n" ++ usage
       case reads s of
         [(config,_)] -> return config
         [] -> error $ f ++ ": configuration file contains errors!\n" ++ usage
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
          | Font     String
          | BgColor  String
          | FgColor  String
          | XPos     String
          | YPos     String
          | Width    String
          | Height   String
          | Align    String
          | Commands String
          | SepChar  String
          | Template String 
       deriving Show
    
options :: [OptDescr Opts]
options =
    [ Option ['h','?' ] ["help"     ] (NoArg  Help                 ) "This help"
    , Option ['V'     ] ["version"  ] (NoArg  Version              ) "Show version information"
    , Option ['f'     ] ["font"     ] (ReqArg Font "font name"     ) "The font name"
    , Option ['B'     ] ["bgcolor"  ] (ReqArg BgColor "bg color"   ) "The background color. Default black"
    , Option ['F'     ] ["fgcolor"  ] (ReqArg FgColor "fg color"   ) "The foreground color. Default grey"
    , Option ['x'     ] ["xpos"     ] (ReqArg XPos "x pos"         ) "The x position. Default 0"
    , Option ['y'     ] ["ypos"     ] (ReqArg YPos "y pos"         ) "The y position. Default 0"
    , Option ['W'     ] ["width"    ] (ReqArg Width "width"        ) "The status bar width. Default 1024"
    , Option ['H'     ] ["height"   ] (ReqArg Height "height"      ) "The status bar height. Default 15"
    , Option ['a'     ] ["align"    ] (ReqArg Align "align"        ) "The text alignment: center, left or right.\nDefault: left"
    , Option ['s'     ] ["sepchar"  ] (ReqArg SepChar "char"       ) "The character used to separate commands in\nthe output template. Default '%'"
    , Option ['t'     ] ["template" ] (ReqArg Template "tempate"   ) "The output template"
    , Option ['c'     ] ["commands" ] (ReqArg Commands  "commands" ) "The list of commands to be executed"
    ]

getOpts :: [String] -> IO ([Opts], [String])
getOpts argv = 
    case getOpt Permute options argv of
      (o,n,[])   -> return (o,n)
      (_,_,errs) -> error (concat errs ++ usage)

usage :: String
usage = (usageInfo header options) ++ footer
    where header = "Usage: xmobar [OPTION...] [FILE]\nOptions:"
          footer = "\nMail bug reports and suggestions to " ++ mail

version :: String
version = "Xmobar 0.7 (C) 2007 Andrea Rossato " ++ mail ++ license

mail :: String
mail = "<andrea.rossato@unibz.it>\n"

license :: String
license = "\nThis program is distributed in the hope that it will be useful,\n" ++
          "but WITHOUT ANY WARRANTY; without even the implied warranty of\n" ++
          "MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.\n" ++
          "See the License for more details."

doOpts :: IORef Config -> [Opts] -> IO ()
doOpts _  [] = return ()
doOpts conf (o:oo) =
    case o of
      Help       -> putStr   usage   >> exitWith ExitSuccess
      Version    -> putStrLn version >> exitWith ExitSuccess
      Font     s -> modifyIORef conf (\c -> c { font     = s                   }) >> go
      BgColor  s -> modifyIORef conf (\c -> c { bgColor  = s                   }) >> go
      FgColor  s -> modifyIORef conf (\c -> c { fgColor  = s                   }) >> go
      XPos     s -> modifyIORef conf (\c -> c { xPos     = readInt s c xPos    }) >> go
      YPos     s -> modifyIORef conf (\c -> c { yPos     = readInt s c yPos    }) >> go
      Width    s -> modifyIORef conf (\c -> c { width    = readInt s c width   }) >> go
      Height   s -> modifyIORef conf (\c -> c { height   = readInt s c height  }) >> go
      Align    s -> modifyIORef conf (\c -> c { align    = s                   }) >> go
      SepChar  s -> modifyIORef conf (\c -> c { sepChar  = s                   }) >> go
      Template s -> modifyIORef conf (\c -> c { template = s                   }) >> go
      Commands s -> case readCom s of
                      Right x -> modifyIORef conf (\c -> c { commands = x })>> go 
                      Left e  -> putStr (e ++ usage) >> exitWith (ExitFailure 1)
    where readCom str =
              case readStr str of
	        [x] -> Right x
	        _  -> Left "xmobar: cannot read list of commands specified with the -c option\n"
          readInt str c f =
              case readStr str of
	        [x] -> x
	        _  -> f c
          readStr str =
              [x | (x,t) <- reads str, ("","") <- lex t]
          go = doOpts conf oo
