-----------------------------------------------------------------------------
-- |
-- Module      :  Monitors.Cpu
-- Copyright   :  (c) Andrea Rossato
-- License     :  BSD-style (see LICENSE)
-- 
-- Maintainer  :  Andrea Rossato <andrea.rossato@unibz.it>
-- Stability   :  unstable
-- Portability :  unportable
--
-- A cpu monitor for XMobar
--
-----------------------------------------------------------------------------

module Monitors.Cpu where

import Monitors.Common
import qualified Data.ByteString.Lazy.Char8 as B
import Data.IORef

cpuConfig :: IO MConfig
cpuConfig = 
    do lc <- newIORef "#BFBFBF"
       l <- newIORef 2
       nc <- newIORef "#00FF00"
       h <- newIORef 60
       hc <- newIORef "#FF0000"
       t <- newIORef "Cpu: <total>"
       p <- newIORef package
       u <- newIORef ""
       a <- newIORef []
       e <- newIORef ["total","user","nice","system","idle"]
       return $ MC nc l lc h hc t p u a e

cpuData :: IO [Float]
cpuData = do s <- B.readFile "/proc/stat"
             return $ cpuParser s

cpuParser :: B.ByteString -> [Float]
cpuParser =
    map read . map B.unpack . tail . B.words . flip (!!) 0 . B.lines

parseCPU :: IO [Float]
parseCPU = 
    do (a,b) <- doActionTwiceWithDelay 750000 cpuData
       let dif = zipWith (-) b a
           tot = foldr (+) 0 dif
           percent = map (/ tot) dif
       return percent

formatCpu :: [Float] -> Monitor [String] 
formatCpu [] = return [""]
formatCpu x =
    do let f s = floatToPercent (s / 100)
           t = foldr (+) 0 $ take 3 x
           list = t:x
       mapM (showWithColors f) . map (* 100) $ list

package :: String
package = "xmb-cpu"

runCpu :: [String] -> Monitor String
runCpu _ =
    do c <- io $ parseCPU
       l <- formatCpu c
       parseTemplate l 
    
{-
main :: IO ()
main =
    do let af = runCpu []
       runMonitor cpuConfig af runCpu
-}