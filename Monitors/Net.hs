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
-- A net device monitor for XMobar
--
-----------------------------------------------------------------------------

module Main where

import Numeric
import Control.Concurrent
import Text.ParserCombinators.Parsec
import System.Environment

data Config = 
    Config { intervall :: Int
           , netDevice :: String
           , netNormal :: Integer
           , netNormalColor :: String
           , netCritical :: Integer
           , netCriticalColor :: String
           }

defaultConfig :: Config
defaultConfig = 
    Config { intervall = 500000
           , netDevice = "eth1"
           , netNormal = 0
           , netNormalColor = "#00FF00" 
           , netCritical = 50
           , netCriticalColor = "#FF0000" 
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

fileNET :: IO String
fileNET = 
    do f <- readFile "/proc/net/dev"
       return $ unlines $ drop 2 $ lines f

-- CPU

getNumbers :: Parser Float
getNumbers = skipMany space >> many1 digit >>= \n -> return $ read n

-- Net Devices

data NetDev = NA
            | ND { netDev :: String
                 , netRx :: Float
                 , netTx :: Float
                 } deriving (Eq,Read)

instance Show NetDev where
    show NA = "N/A"
    show (ND nd rx tx) =
        nd ++ ": " ++ (formatNet rx) ++ "|" ++ formatNet tx          

formatNet :: Float -> String
formatNet d | d > fromInteger (netCritical config) = setColor str netCriticalColor 
            | d > fromInteger (netNormal config) = setColor str netNormalColor
            | otherwise = str
            where str = show d ++ "Kb"

pNetDev :: Parser NetDev
pNetDev = 
    do { skipMany1 space
       ; dn <- manyTill alphaNum $ char ':'
       ; [rx] <- count 1 getNumbers
       ; _ <- count 7 getNumbers
       ; [tx] <- count 1 getNumbers
       ; _ <- count 7 getNumbers
       ; char '\n'
       ; return $ ND dn (rx / 1024) (tx / 1024)
       } 

parserNet :: Parser [NetDev]
parserNet = manyTill pNetDev eof

parseNET :: String -> IO [NetDev]
parseNET nd = 
    do a <- run parserNet fileNET
       interSec
       b <- run parserNet fileNET
       let netRate f da db = takeDigits 2 $ ((f db) - (f da)) * fromIntegral (1000000 `div` (intervall config))
           diffRate (da,db) = ND (netDev da) 
                              (netRate netRx da db)
                              (netRate netTx da db)
       return $ filter (\d -> netDev d == nd) $ map diffRate $ zip a b

-- Formattings

setColor :: String -> (Config -> String) -> String
setColor str ty =
    "<fc=" ++ ty config ++ ">" ++
    str ++ "</fc>"
    
net :: String -> IO String
net nd = 
    do pn <- parseNET nd
       n <- case pn of
              [x] -> return x
              _ -> return $ NA
       return $ show n

main :: IO ()
main =
    do args <- getArgs
       n <-
           if length args /= 1
              then error "No device specified.\nUsage: net dev"
              else net (args!!0)
       putStrLn n
