-----------------------------------------------------------------------------
-- |
-- Module      :  XMobar.Main
-- Copyright   :  (c) Andrea Rossato
-- License     :  BSD-style (see LICENSE)
-- 
-- Maintainer  :  Andrea Rossato <andrea.rossato@unibz.it>
-- Stability   :  unstable
-- Portability :  unportable
--
-- The main module of XMobar, a status bar for the Xmonad Window Manager 
--
-----------------------------------------------------------------------------

module Main ( -- * Main Stuff
              -- $main
              main
            , readConfig
            , readDefaultConfig
            ) where

import XMobar
import Parsers
import Config
import System.Environment
import System.IO.Error

-- $main
 
-- | The main entry point
main :: IO ()
main = 
    do args <- getArgs
       config <- case args of
           [cfgfile] -> readConfig cfgfile
           _         -> readDefaultConfig
       cl <- parseTemplate config (template config)
       var <- execCommands config cl
       (d,w) <- createWin config
       runXMobar config var d w eventLoop

-- | Reads the configuration files or quits with an error
readConfig :: FilePath -> IO Config
readConfig f = 
    do s <- readFile f
       case reads s of
         [(config,_)] -> return config
         [] -> error ("Corrupt config file: " ++ f)
         _ -> error ("Some problem occured. Aborting...")

-- | Read default configuration or quit with an error
readDefaultConfig :: IO Config
readDefaultConfig = 
    do home <- getEnv "HOME"
       let path = home ++ "/.xmobarrc"
       catch (readConfig path)
             (\e -> if isUserError e then ioError e else return defaultConfig)

