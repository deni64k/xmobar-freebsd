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

module Main where

import Numeric
import Control.Concurrent
import Control.Concurrent.MVar

import qualified Data.ByteString.Lazy.Char8 as B

data Config = 
    Config { intervall :: Int
           , cpuNormal :: Integer
           , cpuNormalColor :: String
           , cpuCritical :: Integer
           , cpuCriticalColor :: String
           } deriving (Show)

defaultConfig :: Config
defaultConfig = 
    Config { intervall = 500000
           , cpuNormal = 2
           , cpuNormalColor = "#00FF00" 
           , cpuCritical = 60
           , cpuCriticalColor = "#FF0000"  
           }

config :: Config
config = defaultConfig

-- Utilities

floatToPercent :: Float -> String
floatToPercent n = 
    showFFloat (Just 2) (n*100) "%" 

setColor :: Show a => String -> a -> (a -> String) -> String
setColor str conf ty =
    "<fc=" ++ ty conf ++ ">" ++
    str ++ "</fc>"
    
fileCPU :: IO B.ByteString
fileCPU = B.readFile "/proc/stat"

getData :: MVar [Float] -> Int -> IO ()
getData var d =
    do threadDelay d
       s <- fileCPU
       modifyMVar_ var (\_ -> return $! cpuParser s)

cpuParser :: B.ByteString -> [Float]
cpuParser =
    map read . map B.unpack . tail . B.words . flip (!!) 0 . B.lines

parseCPU :: IO [Float]
parseCPU = 
    do v1 <- newMVar []
       forkIO $! getData v1 0
       v2 <- newMVar []
       forkIO $! getData v2 500000
       threadDelay 750000
       a <- readMVar v1
       b <- readMVar v2
       let dif = zipWith (-) b a
           tot = foldr (+) 0 dif
           percent = map (/ tot) dif
       return percent

formatCpu :: [Float] -> String 
formatCpu (us:ni:sy:_)
    | x >= c = setColor y config cpuCriticalColor
    | x >= n  = setColor y config cpuNormalColor
    | otherwise = y
    where x = (us * 100) + (sy * 100) + (ni * 100)
          y = floatToPercent $ us + sy + ni
          c = fromInteger (cpuCritical config)
          n = fromInteger (cpuNormal config)
formatCpu _ = ""

main :: IO ()
main =
    do s <- parseCPU
       putStrLn $ "Cpu: " ++ formatCpu s
