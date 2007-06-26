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
            ) where

import XMobar
import Parsers
import Config
import System.Environment

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


