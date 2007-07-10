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

cpuConfig :: IO MConfig
cpuConfig = newConfig
       "Cpu: <total>"                           -- template
       package                                  -- package
       ""                                       -- usage tail?
       []                                       -- added args
       ["total","user","nice","system","idle"]  -- available replacements

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
