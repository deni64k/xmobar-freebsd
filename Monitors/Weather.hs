-----------------------------------------------------------------------------
-- |
-- Module      :  Monitors.Weather
-- Copyright   :  (c) Andrea Rossato
-- License     :  BSD-style (see LICENSE)
-- 
-- Maintainer  :  Andrea Rossato <andrea.rossato@unibz.it>
-- Stability   :  unstable
-- Portability :  unportable
--
-- A weather monitor for XMobar
--
-----------------------------------------------------------------------------

module Main where

import Monitors.Common

import Data.IORef

import System.Process
import System.Exit
import System.IO

import Text.ParserCombinators.Parsec


monitorConfig :: IO MConfig
monitorConfig = 
    do lc <- newIORef "#BFBFBF"
       l <- newIORef 15
       nc <- newIORef "#00FF00"
       h <- newIORef 27
       hc <- newIORef "#FF0000"
       t <- newIORef "<station>: <tempC>C, rh <rh>% (<hour>)"
       p <- newIORef package
       u <- newIORef "station ID"
       a <- newIORef []
       e <- newIORef ["station"
                     , "stationState"
                     , "year"
                     , "month"
                     , "day"
                     , "hour"
                     , "wind"
                     , "visibility"
                     , "skyCondition"
                     , "tempC"
                     , "tempF"
                     , "dewPoint"
                     , "rh"
                     ,"pressure"
                     ]
       return $ MC nc l lc h hc t p u a e


data WeatherInfo = 
    WI { stationPlace :: String
       , stationState :: String
       , year :: String
       , month :: String
       , day :: String
       , hour :: String
       , wind :: String
       , visibility :: String
       , skyCondition :: String
       , temperature :: Float
       , dewPoint :: String
       , humidity :: Float
       , pressure :: String
       } deriving (Show)



pTime :: Parser (String, String, String, String)
pTime = do y <- getNumbersAsString
           char '.'
           m <- getNumbersAsString
           char '.'
           d <- getNumbersAsString
           char ' '
           (h:hh:mi:mimi) <- getNumbersAsString
           char ' '
           return (y, m, d ,([h]++[hh]++":"++[mi]++mimi))

pTemp :: Parser Float
pTemp = do string ": "
           manyTill anyChar $ char '('
           s <- manyTill digit $ (char ' ' <|> char '.')
           skipRestOfLine
           return $read s

pRh :: Parser Float
pRh = do string ": "
         s <- manyTill digit $ (char '%' <|> char '.')
         return $ read s

parseData :: Parser [WeatherInfo]
parseData = 
    do st <- getAllBut "," 
       space
       ss <- getAllBut "("
       skipRestOfLine >> getAllBut "/"
       (y,m,d,h) <- pTime
       w <- getAfterString "Wind: "
       v <- getAfterString "Visibility: "
       sk <- getAfterString "Sky conditions: "
       skipTillString "Temperature"
       temp <- pTemp
       dp <- getAfterString "Dew Point: "
       skipTillString "Relative Humidity"
       rh <- pRh
       p <- getAfterString "Pressure (altimeter): "
       manyTill skipRestOfLine eof
       return $ [WI st ss y m d h w v sk temp dp rh p]

defUrl :: String
defUrl = "http://weather.noaa.gov/pub/data/observations/metar/decoded/"

getData :: String -> IO String
getData url=
        do (i,o,e,p) <- runInteractiveCommand ("curl " ++ defUrl ++ url ++ ".TXT")
           exit <- waitForProcess p
           let closeHandles = do hClose o
                                 hClose i
                                 hClose e
           case exit of
             ExitSuccess -> do str <- hGetContents o
                               return str
             _ -> do closeHandles
                     return "Could not retrieve data"

formatWeather :: [WeatherInfo] -> Monitor String
formatWeather [(WI st ss y m d h w v sk temp dp r p)] =
    do cel <- showWithColors show temp
       far <- showWithColors (show . takeDigits 1) (((9 / 5) * temp) + 32)
       rh <- showWithColors show r
       parseTemplate [st, ss, y, m, d, h, w, v, sk, cel, far, dp, rh , p ]
formatWeather _ = return "N/A"

runWeather :: [String] -> Monitor String
runWeather str =
    do d <- io $ getData $ head str
       i <- io $ runP parseData d
       formatWeather i

package :: String
package = "xmb-weather"

main :: IO ()
main =
    do let af = return "No station ID specified"
       runMonitor monitorConfig af runWeather
