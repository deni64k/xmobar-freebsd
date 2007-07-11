-----------------------------------------------------------------------------
-- |
-- Module      :  Monitors.Net
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

module Monitors.Net where

import Monitors.Common
import Text.ParserCombinators.Parsec

data NetDev = NA
            | ND { netDev :: String
                 , netRx :: Float
                 , netTx :: Float
                 } deriving (Eq,Show,Read)

interval :: Int
interval = 500000

netConfig :: IO MConfig
netConfig = mkMConfig
    "<dev>: <rx>|<tx>"      -- template
    ["dev", "rx", "tx"]     -- available replacements

fileNET :: IO String
fileNET = 
    do f <- readFile "/proc/net/dev"
       return $ unlines $ drop 2 $ lines f

formatNet :: Float -> Monitor String
formatNet d =
    showWithColors f d
        where f s = show s ++ "Kb"

printNet :: NetDev -> Monitor String
printNet nd =
    do case nd of
         ND d r t -> do rx <- formatNet r
                        tx <- formatNet t
                        parseTemplate [d,rx,tx]
         NA -> return "N/A"

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
    do (a',b') <- doActionTwiceWithDelay interval fileNET
       a <- runP parserNet a'
       b <- runP parserNet b'
       let netRate f da db = takeDigits 2 $ ((f db) - (f da)) * fromIntegral (1000000 `div` interval)
           diffRate (da,db) = ND (netDev da) 
                              (netRate netRx da db)
                              (netRate netTx da db)
       return $ filter (\d -> netDev d == nd) $ map diffRate $ zip a b

runNet :: [String] -> Monitor String
runNet nd = 
    do pn <- io $ parseNET $ head nd
       n <- case pn of
              [x] -> return x
              _ -> return $ NA
       printNet n
