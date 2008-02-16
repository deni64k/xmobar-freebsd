-----------------------------------------------------------------------------
-- |
-- Module      :  Plugins.Monitors.CoreCommon
-- Copyright   :  (c) Juraj Hercek
-- License     :  BSD-style (see LICENSE)
--
-- Maintainer  :  Juraj Hercek <juhe_haskell@hck.sk>
-- Stability   :  unstable
-- Portability :  unportable
--
-- The common part for cpu core monitors (e.g. cpufreq, coretemp)
--
-----------------------------------------------------------------------------

module Plugins.Monitors.CoreCommon where

import Plugins.Monitors.Common
import System.Posix.Files (fileExist)
import System.Directory
import Data.Char (isDigit)
import Data.List (isPrefixOf)

-- |
-- Function checks the existence of first file specified by pattern and if the
-- file doesn't exists failure message is shown, otherwise the data retrieval
-- is performed.
checkedDataRetrieval :: String -> String -> String -> String -> Double -> Monitor String
checkedDataRetrieval failureMessage dir file pattern divisor = do
    exists <- io $ fileExist $ foldl (++) dir ["/", pattern, "0/", file]
    case exists of
         False  -> return failureMessage
         True   -> retrieveData dir file pattern divisor

-- |
-- Function retrieves data from files in directory dir specified by pattern.
-- String values are converted to double and adjusted with divisor. Final array
-- is processed by template parser function and returned as monitor string.
retrieveData :: String -> String -> String -> Double -> Monitor String
retrieveData dir file pattern divisor = do
    count <- io $ dirCount dir pattern
    contents <- io $ mapM readFile $ files count
    values <- mapM (showWithColors show) $ map conversion contents
    parseTemplate values
    where
        dirCount path str = getDirectoryContents path
                            >>= return . length
                                       . filter (\s -> str `isPrefixOf` s
                                                       && isDigit (last s))
        files count = [ foldl (++) dir [ "/", pattern, show i, "/", file ]
                      | i <- [0 .. count - 1] ]
        conversion = flip (/) divisor . (read :: String -> Double)

