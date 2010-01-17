{-# LANGUAGE CPP #-}
-----------------------------------------------------------------------------
-- |
-- Module      :  Plugins.Monitors.Net
-- Copyright   :  (c) Andrea Rossato
-- License     :  BSD-style (see LICENSE)
--
-- Maintainer  :  Andrea Rossato <andrea.rossato@unibz.it>
-- Stability   :  unstable
-- Portability :  unportable
--
-- A net device monitor for Xmobar
--
-----------------------------------------------------------------------------

module Plugins.Monitors.Net where

#if defined (FREEBSD)
import System.BSD.Sysctl
import System.Process (readProcess)
import qualified Control.Exception as E
import Control.Monad (liftM, mapM)
import Text.Printf
#else
import qualified Data.ByteString.Lazy.Char8 as B
#endif

import Plugins.Monitors.Common

data NetDev = NA
            | ND { netDev :: String
                 , netRx :: Float
                 , netTx :: Float
                 } deriving (Eq,Show,Read)

interval :: Int
interval = 1000000

netConfig :: IO MConfig
netConfig = mkMConfig
    "<dev>: <rx>|<tx>"      -- template
    ["dev", "rx", "tx"]     -- available replacements

#if defined (FREEBSD)
-- Insert dot between device name and its number.
--
-- > dotize "ale0"
--
-- will become @"ale.0"@
dotize :: String -> String
dotize [] = []
dotize s@(c:cs) = if isDigit c
                  then '.' : s
                  else c : dotize cs
    where isDigit d = d >= '0' && d <= '9'

readNetDev :: String -> IO NetDev
readNetDev devName =
    do oids  <- mapM (\ctlName -> sysctlNameToOid $ printf ctlName (dotized::String)) ctlNames
       quads <- mapM sysctlReadQuad oids
       let [rx, tx] = map (\quad -> quadToFloat quad / 1024) quads
       return $ ND devName rx tx
    where dotized     = dotize devName
          ctlNames    = ["dev.%s.stats.rx.good_octets", "dev.%s.stats.tx.good_octets"]
          quadToFloat = fromInteger . toInteger :: Integral a => a -> Float

fileNET :: IO [NetDev]
fileNET =
    do devices <- liftM words (readProcess "ifconfig" ["-l", "-u"] [])
       nds     <- mapM (\dev -> readNetDev dev `E.catch` handler) devices
       return $ filter onlyND nds
    where handler = (\_ -> return $ NA) :: E.SomeException -> IO NetDev
          onlyND NA = False
          onlyND _  = True
#else
-- Given a list of indexes, take the indexed elements from a list.
getNElements :: [Int] -> [a] -> [a]
getNElements ns as = map (as!!) ns
-- Split into words, with word boundaries indicated by the given predicate.
-- Drops delimiters.  Duplicates 'Data.List.Split.wordsBy'.
--
-- > map (wordsBy (`elem` " :")) ["lo:31174097 31174097", "eth0:  43598 88888"]
--
-- will become @[["lo","31174097","31174097"], ["eth0","43598","88888"]]@
wordsBy :: (a -> Bool) -> [a] -> [[a]]
wordsBy f s = case dropWhile f s of
    [] -> []
    s' -> w : wordsBy f s'' where (w, s'') = break f s'

readNetDev :: [String] -> NetDev
readNetDev [] = NA
readNetDev xs =
    ND (xs !! 0) (r (xs !! 1)) (r (xs !! 2))
       where r s | s == "" = 0
                 | otherwise = read s / 1024

fileNET :: IO [NetDev]
fileNET =
    do f <- B.readFile "/proc/net/dev"
       return $ netParser f

netParser :: B.ByteString -> [NetDev]
netParser =
    map (readNetDev . getNElements [0,1,9] . wordsBy (`elem` " :") . B.unpack) . drop 2 . B.lines
#endif

formatNet :: Float -> Monitor String
formatNet d =
    showWithColors f d
        where f s = showDigits 1 s ++ "Kb"

printNet :: NetDev -> Monitor String
printNet nd =
    case nd of
         ND d r t -> do rx <- formatNet r
                        tx <- formatNet t
                        parseTemplate [d,rx,tx]
         NA -> return "N/A"

parseNET :: String -> IO [NetDev]
parseNET nd =
    do (a,b) <- doActionTwiceWithDelay interval fileNET
       let netRate f da db = takeDigits 2 $ (f db - f da) * fromIntegral (1000000 `div` interval)
           diffRate (da,db) = ND (netDev da)
                              (netRate netRx da db)
                              (netRate netTx da db)
       return $ filter (\d -> netDev d == nd) $ map diffRate $ zip a b

runNet :: [String] -> Monitor String
runNet nd =
    do pn <- io $ parseNET $ head nd
       n <- case pn of
              [x] -> return x
              _ -> return NA
       printNet n
