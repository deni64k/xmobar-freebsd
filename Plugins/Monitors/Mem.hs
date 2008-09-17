-----------------------------------------------------------------------------
-- |
-- Module      :  Plugins.Monitors.Mem
-- Copyright   :  (c) Andrea Rossato
-- License     :  BSD-style (see LICENSE)
-- 
-- Maintainer  :  Andrea Rossato <andrea.rossato@unibz.it>
-- Stability   :  unstable
-- Portability :  unportable
--
-- A memory monitor for Xmobar
--
-----------------------------------------------------------------------------

module Plugins.Monitors.Mem where

import Plugins.Monitors.Common

memConfig :: IO MConfig
memConfig = mkMConfig
       "Mem: <usedratio>% (<cache>M)" -- template
       ["total", "free", "buffer",    -- available replacements
        "cache", "rest", "used", "usedratio"]

fileMEM :: IO String
fileMEM = readFile "/proc/meminfo"

parseMEM :: IO [Float]
parseMEM =
    do file <- fileMEM 
       let content = map words $ take 4 $ lines file
           [total, free, buffer, cache] = map (\line -> (read $ line !! 1 :: Float) / 1024) content
           rest = free + buffer + cache
           used = total - rest
           usedratio = used * 100 / total
       return [total, free, buffer, cache, rest, used, usedratio]

formatMem :: [Float] -> Monitor [String]
formatMem x =
    do let f n = showDigits 0 n
       mapM (showWithColors f) x

runMem :: [String] -> Monitor String
runMem _ =
    do m <- io $ parseMEM
       l <- formatMem m
       parseTemplate l 
