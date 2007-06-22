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
import Text.ParserCombinators.Parsec


data Config = 
    Config { intervall :: Int
           , cpuNormal :: Integer
           , cpuNormalColor :: String
           , cpuCritical :: Integer
           , cpuCriticalColor :: String
           }

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

interSec :: IO ()
interSec = threadDelay  (intervall config)

takeDigits :: Int -> Float -> Float
takeDigits d n = 
    read $ showFFloat (Just d) n ""

floatToPercent :: Float -> String
floatToPercent n = 
    showFFloat (Just 2) (n*100) "%" 


run :: Parser [a] -> IO String -> IO [a]
run p input
        = do a <- input
             case (parse p "" a) of
               Left _ -> return []
               Right x  -> return x

fileCPU :: IO String
fileCPU = readFile "/proc/stat"


getNumbers :: Parser Float
getNumbers = skipMany space >> many1 digit >>= \n -> return $ read n

parserCPU :: Parser [Float]
parserCPU = string "cpu" >> count 4 getNumbers

parseCPU :: IO [Float]
parseCPU = 
    do a <- run parserCPU fileCPU
       interSec
       b <- run parserCPU fileCPU
       let dif = zipWith (-) b a
           tot = foldr (+) 0 dif
           percent = map (/ tot) dif
       return percent

formatCpu :: [Float] -> String 
formatCpu [] = ""
formatCpu (us:ni:sy:_)
    | x >= c = setColor z cpuCriticalColor
    | x >= n  = setColor z cpuNormalColor
    | otherwise = floatToPercent y
    where x = (us * 100) + (sy * 100) + (ni * 100)
          y = us + sy + ni
          z = floatToPercent y
          c = fromInteger (cpuCritical config)
          n = fromInteger (cpuNormal config)
formatCpu _ = ""

setColor :: String -> (Config -> String) -> String
setColor str ty =
    "<fc=" ++ ty config ++ ">" ++
    str ++ "</fc>"
    
cpu :: IO String
cpu = 
    do l <- parseCPU
       return $ "Cpu: " ++ formatCpu l

main :: IO ()
main =
    do c <- cpu
       putStrLn c
