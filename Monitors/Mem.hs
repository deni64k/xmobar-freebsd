-----------------------------------------------------------------------------
-- |
-- Module      :  Monitors.Mem
-- Copyright   :  (c) Andrea Rossato
-- License     :  BSD-style (see LICENSE)
-- 
-- Maintainer  :  Andrea Rossato <andrea.rossato@unibz.it>
-- Stability   :  unstable
-- Portability :  unportable
--
-- A memory monitor for XMobar
--
-----------------------------------------------------------------------------

module Monitors.Mem where

import Monitors.Common

import Data.IORef

memConfig :: IO MConfig
memConfig = 
    do lc <- newIORef "#BFBFBF"
       l <- newIORef 300
       nc <- newIORef "#00FF00"
       h <- newIORef 500
       hc <- newIORef "#FF0000"
       t <- newIORef "Mem: <usedratio>% (<cache>M)"
       p <- newIORef package
       u <- newIORef ""
       a <- newIORef []
       e <- newIORef ["total", "free", "buffer", "cache", "rest", "used", "usedratio"]
       return $ MC nc l lc h hc t p u a e

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
    do let f n = show (takeDigits 2 n)
       mapM (showWithColors f) x

package :: String
package = "xmb-mem"

runMem :: [String] -> Monitor String
runMem _ =
    do m <- io $ parseMEM
       l <- formatMem m
       parseTemplate l 
    
{-
main :: IO ()
main =
    do let af = runMem []
       runMonitor monitorConfig af runMem
-}