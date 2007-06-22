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

module Main where

import Numeric

data Config = 
    Config { memNormal :: Integer
           , memNormalColor :: String
           , memCritical :: Integer
           , memCriticalColor :: String
           , swapNormal :: Integer
           , swapNormalColor :: String
           , swapCritical :: Integer
           , swapCriticalColor :: String
           }

defaultConfig :: Config
defaultConfig = 
    Config { memNormal = 80
           , memNormalColor =  "#00FF00" 
           , memCritical = 90
           , memCriticalColor =  "#FF0000"
           , swapNormal = 15
           , swapNormalColor = "#00FF00" 
           , swapCritical = 50
           , swapCriticalColor = "#FF0000" 
           }
config :: Config
config = defaultConfig

-- Utilities

takeDigits :: Int -> Float -> Float
takeDigits d n = 
    read $ showFFloat (Just d) n ""

floatToPercent :: Float -> String
floatToPercent n = 
    showFFloat (Just 2) (n*100) "%" 

fileMEM :: IO String
fileMEM = readFile "/proc/meminfo"

parseMEM :: IO [Float]
parseMEM = 
    do file <- fileMEM 
       let content = map words $ take 13 $ lines file
           [total, free, buffer, cache,_,_,_,_,_,_,_,swapTotal,swapFree] = map (\line -> (read $ line !! 1 :: Float) / 1024) content
           rest = free + buffer + cache
           used = total - rest
           usedratio = used * 100 / total
           swapRatio = 100 - (swapFree / swapTotal * 100)
       return [total, free, buffer, cache, rest, used, usedratio, swapFree, swapRatio]


formatMem :: [Float] -> String 
formatMem [] = ""
formatMem (total:_:buffer:cach:_:used:_:_:swapRatio:_) =
    "Ram: " ++ ram ++ " cached: " ++ cache ++ " Swap: " ++ swap
        where (memN,memC,swapN,swapC) = (fromIntegral $ memNormal config,fromIntegral $ memCritical config
                                        , fromIntegral $ swapNormal config, fromIntegral $ swapCritical config)
              m = floatToPercent ((used + buffer + cach) / total)
              sw = show (takeDigits 2 swapRatio) ++ "%"
              cache = show (takeDigits 2 cach) ++ "Mb"
              ram | (used / total * 100) >= memC = setColor m memCriticalColor
                  | (used / total * 100) >= memN = setColor m memNormalColor
                  | otherwise = floatToPercent (used / total)
              swap | swapRatio >= swapC = setColor sw swapCriticalColor
                   | swapRatio >= swapN = setColor sw swapNormalColor
                   | otherwise = sw
formatMem _ = ""

setColor :: String -> (Config -> String) -> String
setColor str ty =
    "<fc=" ++ ty config ++ ">" ++
    str ++ "</fc>"
    
mem :: IO String
mem =
    do m <- parseMEM
       return $ formatMem m

main :: IO ()
main =
    do m <- mem
       putStrLn m