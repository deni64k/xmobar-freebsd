-----------------------------------------------------------------------------
-- |
-- Module      :  Plugins.Monitors.Common
-- Copyright   :  (c) Andrea Rossato
-- License     :  BSD-style (see LICENSE)
-- 
-- Maintainer  :  Andrea Rossato <andrea.rossato@unibz.it>
-- Stability   :  unstable
-- Portability :  unportable
--
-- Utilities for creating monitors for Xmobar
--
-----------------------------------------------------------------------------

module Plugins.Monitors.Common ( 
                       -- * Monitors
                       -- $monitor
                         Monitor
                       , MConfig (..)
                       , Opts (..)
                       , setConfigValue
                       , getConfigValue
                       , mkMConfig
                       , runM
                       , io
                       -- * Parsers
                       -- $parsers
                       , runP
                       , skipRestOfLine
                       , getNumbers
                       , getNumbersAsString
                       , getAllBut
                       , getAfterString
                       , skipTillString
                       , parseTemplate
                       -- ** String Manipulation
                       -- $strings
                       , showWithColors
                       , takeDigits
                       , showDigits
                       , floatToPercent
                       , stringParser
                       -- * Threaded Actions
                       -- $thread
                       , doActionTwiceWithDelay
                       ) where


import Control.Concurrent
import Control.Monad.Reader

import qualified Data.ByteString.Lazy.Char8 as B
import Data.IORef
import qualified Data.Map as Map 
import Data.List

import Numeric

import Text.ParserCombinators.Parsec

import System.Console.GetOpt

-- $monitor

type Monitor a = ReaderT MConfig IO a

data MConfig =
    MC { normalColor :: IORef (Maybe String)
       , low :: IORef Int
       , lowColor :: IORef (Maybe String)
       , high :: IORef Int
       , highColor :: IORef (Maybe String)
       , template :: IORef String
       , export :: IORef [String]
       } 

-- | from 'http:\/\/www.haskell.org\/hawiki\/MonadState'
type Selector a = MConfig -> IORef a

sel :: Selector a -> Monitor a
sel s = 
    do hs <- ask
       liftIO $ readIORef (s hs)

mods :: Selector a -> (a -> a) -> Monitor ()
mods s m = 
    do v <- ask
       io $ modifyIORef (s v) m

setConfigValue :: a -> Selector a -> Monitor ()
setConfigValue v s =
       mods s (\_ -> v)

getConfigValue :: Selector a -> Monitor a
getConfigValue s =
    sel s

mkMConfig :: String
          -> [String]
          -> IO MConfig
mkMConfig tmpl exprts =
    do lc <- newIORef Nothing
       l <- newIORef 33
       nc <- newIORef Nothing
       h <- newIORef 66
       hc <- newIORef Nothing
       t <- newIORef tmpl
       e <- newIORef exprts
       return $ MC nc l lc h hc t e

data Opts = HighColor String
          | NormalColor String
          | LowColor String
          | Low String
          | High String
          | Template String

options :: [OptDescr Opts]
options =
    [ Option ['H']  ["High"]  (ReqArg High "number") "The high threshold"
    , Option ['L']  ["Low"]  (ReqArg Low "number") "The low threshold"
    , Option ['h']  ["high"]  (ReqArg HighColor "color number") "Color for the high threshold: ex \"#FF0000\""
    , Option ['n']  ["normal"]  (ReqArg NormalColor "color number") "Color for the normal threshold: ex \"#00FF00\""
    , Option ['l']  ["low"]  (ReqArg LowColor "color number") "Color for the low threshold: ex \"#0000FF\""
    , Option ['t']  ["template"]  (ReqArg Template "output template") "Output template."
    ]

doArgs :: [String] 
       -> ([String] -> Monitor String)
       -> Monitor String
doArgs args action =
    do case (getOpt Permute options args) of
         (o, n, []) -> do doConfigOptions o
                          action n
         (_, _, errs) -> return (concat errs)

doConfigOptions :: [Opts] -> Monitor ()
doConfigOptions [] = io $ return ()
doConfigOptions (o:oo) =
    do let next = doConfigOptions oo
       case o of
         High h -> setConfigValue (read h) high >> next
         Low l -> setConfigValue (read l) low >> next
         HighColor hc -> setConfigValue (Just hc) highColor >> next
         NormalColor nc -> setConfigValue (Just nc) normalColor >> next
         LowColor lc -> setConfigValue (Just lc) lowColor >> next
         Template t -> setConfigValue t template >> next

runM :: [String] -> IO MConfig -> ([String] -> Monitor String) -> IO String
runM args conf action =
    do c <- conf
       let ac = doArgs args action
       runReaderT ac c

io :: IO a -> Monitor a
io = liftIO



-- $parsers

runP :: Parser [a] -> String -> IO [a]
runP p i = 
    do case (parse p "" i) of
         Left _ -> return []
         Right x  -> return x

getAllBut :: String -> Parser String
getAllBut s =
    manyTill (noneOf s) (char $ head s)

getNumbers :: Parser Float
getNumbers = skipMany space >> many1 digit >>= \n -> return $ read n

getNumbersAsString :: Parser String
getNumbersAsString = skipMany space >> many1 digit >>= \n -> return n

skipRestOfLine :: Parser Char
skipRestOfLine =
    do many $ noneOf "\n\r"
       newline

getAfterString :: String -> Parser String
getAfterString s =
    do { try $ manyTill skipRestOfLine $ string s
       ; v <- manyTill anyChar $ newline
       ; return v
       } <|> return ("<" ++ s ++ " not found!>")

skipTillString :: String -> Parser String
skipTillString s = 
    manyTill skipRestOfLine $ string s

-- | Parses the output template string
templateStringParser :: Parser (String,String,String)
templateStringParser =
    do{ s <- many $ noneOf "<"
      ; (_,com,_) <- templateCommandParser
      ; ss <- many $ noneOf "<"
      ; return (s, com, ss)
      } 

-- | Parses the command part of the template string
templateCommandParser :: Parser (String,String,String)
templateCommandParser =
    do { char '<'
       ; com <- many $ noneOf ">"
       ; char '>'
       ; return $ ("",com,"")
       }

-- | Combines the template parsers
templateParser :: Parser [(String,String,String)]
templateParser = many templateStringParser --"%")

-- | Takes a list of strings that represent the values of the exported
-- keys. The strings are joined with the exported keys to form a map
-- to be combined with 'combine' to the parsed template. Returns the
-- final output of the monitor.
parseTemplate :: [String] -> Monitor String
parseTemplate l =
    do t <- getConfigValue template
       s <- io $ runP templateParser t
       e <- getConfigValue export
       let m = Map.fromList . zip e $ l 
       return $ combine m s 

-- | Given a finite "Map" and a parsed templatet produces the
-- | resulting output string.
combine :: Map.Map String String -> [(String, String, String)] -> String
combine _ [] = []
combine m ((s,ts,ss):xs) = 
    s ++ str ++ ss ++ combine m xs
        where str = Map.findWithDefault err ts m
              err = "<" ++ ts ++ " not found!>"

-- $strings

type Pos = (Int, Int)

takeDigits :: Int -> Float -> Float
takeDigits d n = 
    fromIntegral ((round (n * fact)) :: Int) / fact
  where fact = 10 ^ d

showDigits :: Int -> Float -> String
showDigits d n =
    showFFloat (Just d) n ""

floatToPercent :: Float -> String
floatToPercent n = 
    showDigits 2 (n*100) ++ "%"

stringParser :: Pos -> B.ByteString -> String
stringParser (x,y) =
     tk x . words . tk y . lines . B.unpack
    where tk i l | length l >= i = flip (!!) i $ l
                 | otherwise = []

setColor :: String -> Selector (Maybe String) -> Monitor String
setColor str s =
    do a <- getConfigValue s
       case a of
            Nothing -> return str
            Just c -> return $
                "<fc=" ++ c ++ ">" ++ str ++ "</fc>"

showWithColors :: (Float -> String) -> Float -> Monitor String
showWithColors f x =
    do h <- getConfigValue high
       l <- getConfigValue low
       let col = setColor $ f x
           [ll,hh] = map fromIntegral $ sort [l, h] -- consider high < low 
       head $ [col highColor   | x > hh ] ++
              [col normalColor | x > ll ] ++
              [col lowColor    | True]

-- $threads

doActionTwiceWithDelay :: Int -> IO [a] -> IO ([a], [a])
doActionTwiceWithDelay delay action = 
    do v1 <- newMVar []
       forkIO $! getData action v1 0
       v2 <- newMVar []
       forkIO $! getData action v2 delay
       threadDelay (delay `div` 3 * 4)
       a <- readMVar v1
       b <- readMVar v2
       return (a,b)

getData :: IO a -> MVar a -> Int -> IO ()
getData action var d =
    do threadDelay d
       s <- action
       modifyMVar_ var (\_ -> return $! s)
