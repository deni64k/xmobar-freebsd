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
import qualified Data.ByteString.Lazy.Char8 as B

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


-- takes to element of a list given their indexes
getTwoElementsAt :: Int -> Int -> [a] -> [a]
getTwoElementsAt x y xs =
    z : [zz]
      where z = xs !! x
            zz = xs !! y

-- split a list of strings returning a list with: 1. the first part of
-- the split; 2. the second part of the split without the Char; 3. the
-- rest of the list. For instance: 
--
-- > splitAtChar ':' ["lo:31174097","31174097"] 
--
-- will become ["lo","31174097","31174097"]
splitAtChar :: Char ->  [String] -> [String]
splitAtChar c xs =
    first : (rest xs)
        where rest = map $ \x -> if (c `elem` x) then (tail $ dropWhile (/= c) x) else x
              first = head $ map (takeWhile (/= c)) . filter (\x -> (c `elem` x)) $ xs

readNetDev :: [String] -> NetDev               
readNetDev [] = NA
readNetDev xs =
    ND (xs !! 0) (r (xs !! 1)) (r (xs !! 2))
       where r s | s == "" = 0
                 | otherwise = (read s) / 1024

fileNET :: IO [NetDev]
fileNET = 
    do f <- B.readFile "/proc/net/dev"
       return $ netParser f

netParser :: B.ByteString -> [NetDev]
netParser =
    map readNetDev . map (splitAtChar ':') . map (getTwoElementsAt 0 8) . map (words . B.unpack) . drop 2 . B.lines

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

parseNET :: String -> IO [NetDev]
parseNET nd = 
    do (a,b) <- doActionTwiceWithDelay interval fileNET
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
