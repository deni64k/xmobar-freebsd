-----------------------------------------------------------------------------
-- |
-- Module      :  Monitors.Swap
-- Copyright   :  (c) Andrea Rossato
-- License     :  BSD-style (see LICENSE)
-- 
-- Maintainer  :  Andrea Rossato <andrea.rossato@unibz.it>
-- Stability   :  unstable
-- Portability :  unportable
--
-- A  swap usage monitor for XMobar
--
-----------------------------------------------------------------------------

module Monitors.Swap where

import Monitors.Common

import qualified Data.ByteString.Lazy.Char8 as B

swapConfig :: IO MConfig
swapConfig = newConfig
        "Swap: <usedratio>"                    -- template
        package                                -- package
        ""                                     -- usage tail?
        []                                     -- added args
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
    do let f1 n = show (takeDigits 2 n)
           f2 n = floatToPercent n
           (hd, tl) = splitAt 3 x
       firsts <- mapM (showWithColors f1) hd
       lasts <- mapM (showWithColors f2) tl
       return $ firsts ++ lasts

package :: String
package = "xmb-swap"

runSwap :: [String] -> Monitor String
runSwap _ =
    do m <- io $ parseMEM
       l <- formatSwap m
       parseTemplate l 
    
{-
main :: IO ()
main =
    do let af = runSwap []
       runMonitor swapConfig af runSwap
-}
