module Main where

import Text.ParserCombinators.Parsec
import System.Environment

import System.Process
import System.Exit
import System.IO



data Config = 
    Config { weatherNormal :: Integer
           , weatherNormalColor :: String
           , weatherCritical :: Integer
           , weatherCriticalColor :: String
           }

defaultConfig :: Config
defaultConfig = 
    Config { weatherNormal = 0
           , weatherNormalColor = "#00FF00" 
           , weatherCritical = 50
           , weatherCriticalColor = "#FF0000" 
           }

config :: Config
config = defaultConfig


data WeatherInfo = Fail String
                 | WI { station :: String
                      , time :: String
                      , temperature :: Int
                      , humidity :: Int
                      } 
                   
instance Show WeatherInfo where
    show (Fail x) = "N/A " ++ x
    show (WI st t temp rh) =
        st ++ ": " ++ (formatWeather temp) ++ "C, rh " ++ formatWeather rh ++
        "% (" ++ t ++ ")"

parseData :: Parser WeatherInfo
parseData = 
    do { st <- manyTill anyChar $ char '('
       ; pNL
       ; manyTill anyChar $ char '/'
       ; space
       ; t <- manyTill anyChar newline
       ; manyTill pNL (string "Temperature")
       ; temp <- pTemp
       ; manyTill pNL (string "Relative Humidity")
       ; rh <- pRh
       ; manyTill pNL eof
       ; return $ WI st t temp rh
       } 

pTemp :: Parser Int
pTemp = do string ": "
           manyTill anyChar $ char '('
           s <- manyTill digit $ (char ' ' <|> char '.')
           pNL
           return $read s

pRh :: Parser Int
pRh = do string ": "
         s <- manyTill digit $ (char '%' <|> char '.')
         return $read s

pNL :: Parser Char
pNL = do many $ noneOf "\n\r"
         newline


runP :: Parser WeatherInfo -> String -> IO WeatherInfo
runP p i =
    do case (parse p "" i) of
         Left err -> return $ Fail $ show err
         Right x  -> return x

formatWeather :: Int -> String
formatWeather d | d > fromInteger (weatherCritical config) = setColor str weatherCriticalColor 
                | d > fromInteger (weatherNormal config) = setColor str weatherNormalColor
                | otherwise = str
                where str = show d


setColor :: String -> (Config -> String) -> String
setColor str ty =
    "<fc=" ++ ty config ++ ">" ++
    str ++ "</fc>"

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

main :: IO ()
main =
    do args <- getArgs
       str <- if length args /= 1
                 then error $ "No Station ID specified.\nUsage: weather STATION_ID" ++
                      "\nExample: xmb-weather LIPB"
                 else getData (args !! 0)
       i <- runP parseData str 
       putStrLn $ show i
       
