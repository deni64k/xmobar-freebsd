-----------------------------------------------------------------------------
-- |
-- Module      :  Plugins.Monitors.Swap
-- Copyright   :  (c) Andrea Rossato
-- License     :  BSD-style (see LICENSE)
-- 
-- Maintainer  :  Andrea Rossato <andrea.rossato@unibz.it>
-- Stability   :  unstable
-- Portability :  unportable
--
-- A  swap usage monitor for Xmobar
--
-----------------------------------------------------------------------------

module Plugins.Monitors.Swap where

import Plugins.Monitors.Common

import qualified Data.ByteString.Lazy.Char8 as B

swapConfig :: IO MConfig
swapConfig = mkMConfig
        "Swap: <usedratio>"                    -- template
        ["total", "used", "free", "usedratio"] -- available replacements

fileMEM :: IO B.ByteString
fileMEM = B.readFile "/proc/meminfo"

parseMEM :: IO [Float]
parseMEM =
    do file <- fileMEM
       let p x y = flip (/) 1024 . read . stringParser x $ y
           tot = p (1,11) file
           free = p (1,12) file
       return [tot, (tot - free), free, (tot - free) / tot]

formatSwap :: [Float] -> Monitor [String] 
formatSwap x =
    do let f1 n = showDigits 2 n
           f2 n = floatToPercent n
           (hd, tl) = splitAt 3 x
       firsts <- mapM (showWithColors f1) hd
       lasts <- mapM (showWithColors f2) tl
       return $ firsts ++ lasts

runSwap :: [String] -> Monitor String
runSwap _ =
    do m <- io $ parseMEM
       l <- formatSwap m
       parseTemplate l 
