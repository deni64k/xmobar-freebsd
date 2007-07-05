-----------------------------------------------------------------------------
-- |
-- Module      :  Monitors.Common
-- Copyright   :  (c) Andrea Rossato
-- License     :  BSD-style (see LICENSE)
-- 
-- Maintainer  :  Andrea Rossato <andrea.rossato@unibz.it>
-- Stability   :  unstable
-- Portability :  unportable
--
-- Utilities for creating monitors for XMobar
--
-----------------------------------------------------------------------------

module Monitors.Common ( 
                       -- * Monitors
                       -- $monitor
                         Monitor
                       , MConfig (..)
                       , Opts (..)
                       , setConfigValue
                       , getConfigValue
                       , runMonitor
                       , io
                       -- * Parsers
                       -- $parsers
                       , runP
                       , skipRestOfLine
                       , getNumbers
                       , getNumbersAsString
                       , getAllBut
                       , parseTemplate
                       -- ** String Manipulation
                       -- $strings
                       , showWithColors
                       , takeDigits
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

import Numeric

import Text.ParserCombinators.Parsec

import System.Console.GetOpt
import System.Environment
import System.Exit

-- $monitor

type Monitor a = ReaderT MConfig IO a

data MConfig =
    MC { normalColor :: IORef String
       , low :: IORef Int
       , lowColor :: IORef String
       , high :: IORef Int
       , highColor :: IORef String
       , template :: IORef String
       , packageName :: IORef String
       , usageTail :: IORef String
       , addedArgs :: IORef [OptDescr Opts]
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


data Opts = Help
          | Version
          | HighColor String
          | NormalColor String
          | LowColor String
          | Low String
          | High String
          | Template String
          | Others String

options :: Monitor [OptDescr Opts]
options =
    do t <- getConfigValue export
       ao <- getConfigValue addedArgs 
       tmpl <- getConfigValue template
       return $ [ Option ['h']  ["help"]    (NoArg Help)    "Show this help"
                , Option ['V']  ["version"] (NoArg Version) "Show version information"
                , Option ['H']  ["High"]  (ReqArg High "number") "The high threshold"
                , Option ['L']  ["Low"]  (ReqArg Low "number") "The low threshold"
                , Option []  ["high"]  (ReqArg HighColor "color number") "Color for the high threshold: es \"#FF0000\""
                , Option []  ["normal"]  (ReqArg NormalColor "color number") "Color for the normal threshold: es \"#00FF00\""
                , Option []  ["low"]  (ReqArg LowColor "color number") "Color for the low threshold: es \"#0000FF\""
                , Option ['t']  ["template"]  (ReqArg Template "output template") 
                             ("Output template.\nAvaliable variables: " ++ show t ++ "\nDefault template: " ++ show tmpl)
                ] ++ ao

usage :: Monitor ()
usage =
    do pn <- io $ getProgName
       u <- getConfigValue usageTail
       opts <- options
       io $ putStr $ usageInfo ("Usage: " ++ pn ++ " [OPTIONS...] " ++ u) opts

version :: String
version = "0.4"

versinfo :: String -> String -> IO ()
versinfo p v = putStrLn $ p ++" " ++ v

doArgs :: [String] 
       -> Monitor String 
       -> ([String] -> Monitor String)
       -> Monitor String
doArgs args actionFail action =
    do opts <- options
       case (getOpt Permute opts args) of
         (o, n, []) -> do
           doConfigOptions o
           case n of
             []   -> actionFail
             nd   -> action nd
         (_, _, errs) -> io $ error (concat errs)

doConfigOptions :: [Opts] -> Monitor ()
doConfigOptions [] = io $ return ()
doConfigOptions (o:oo) =
    do pn <- getConfigValue packageName
       let next = doConfigOptions oo
       case o of
         Help -> usage >> io (exitWith ExitSuccess)
         Version -> io $ versinfo pn version >> exitWith ExitSuccess
         High h -> setConfigValue (read h) high >> next
         Low l -> setConfigValue (read l) low >> next
         HighColor hc -> setConfigValue hc highColor >> next
         NormalColor nc -> setConfigValue nc normalColor >> next
         LowColor lc -> setConfigValue lc lowColor >> next
         Template t -> setConfigValue t template >> next
         _ -> next

runMonitor ::  IO MConfig -> Monitor String -> ([String] -> Monitor String) -> IO ()
runMonitor conf actionFail action =
    do c <- conf
       args <- getArgs
       let ac = doArgs args actionFail action
       putStrLn =<< runReaderT ac c

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
    read $ showFFloat (Just d) n ""

floatToPercent :: Float -> String
floatToPercent n = 
    showFFloat (Just 2) (n*100) "%" 

stringParser :: Pos -> B.ByteString -> String
stringParser (x,y) =
     flip (!!) x . map B.unpack . B.words . flip (!!) y . B.lines

setColor :: String -> Selector String -> Monitor String
setColor str s =
    do a <- getConfigValue s
       return $ "<fc=" ++ a ++ ">" ++
              str ++ "</fc>"

showWithColors :: (Float -> String) -> Float -> Monitor String
showWithColors f x =
    do h <- getConfigValue high
       l <- getConfigValue low
       let col = setColor $ f x
       head $ [col highColor | x > fromIntegral h ] ++
              [col normalColor | x > fromIntegral l ] ++
              [col lowColor | True]

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
