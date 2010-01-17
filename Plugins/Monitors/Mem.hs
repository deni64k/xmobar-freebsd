{-# OPTIONS -cpp #-}
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

import Data.List
import Data.Maybe

import Plugins.Monitors.Common

memConfig :: IO MConfig
memConfig = mkMConfig
       "Mem: <usedratio>% (<cache>M)" -- template
       ["total", "free", "buffer",    -- available replacements
        "cache", "rest", "used", "usedratio"]

fileMEM :: IO String
fileMEM = readFile "/proc/meminfo"

#if defined (__freebsd__)
parseMEM :: IO [Float]
parseMEM =
    do file <- fileMEM
       let content   = filter (\e -> isInteger $ e !! 1) $ map (take 2 . words) $ lines file
           pairs     = map (\e -> (delete ':' (e !! 0), (read $ e !! 1 :: Float) / 1024)) content
           total     = fromJust $ lookup "MemTotal" pairs
           free      = fromJust $ lookup "MemFree" pairs
           buffer    = fromJust $ lookup "Buffers" pairs
           cache     = fromJust $ lookup "Cached" pairs
           rest      = free + buffer + cache
           used      = total - rest
           usedratio = used * 100 / total
       return [total, free, buffer, cache, rest, used, usedratio]
    where isInteger = \s -> all isDigit s
          isDigit = \c -> c >= '0' && c <= '9'
#else
parseMEM :: IO [Float]
parseMEM =
    do file <- fileMEM
       let content = map words $ take 4 $ lines file
           [total, free, buffer, cache] = map (\line -> (read $ line !! 1 :: Float) / 1024) content
           rest = free + buffer + cache
           used = total - rest
           usedratio = used * 100 / total
       return [total, free, buffer, cache, rest, used, usedratio]
#endif

formatMem :: [Float] -> Monitor [String]
formatMem x =
    do let f n = showDigits 0 n
       mapM (showWithColors f) x

runMem :: [String] -> Monitor String
runMem _ =
    do m <- io $ parseMEM
       l <- formatMem m
       parseTemplate l
