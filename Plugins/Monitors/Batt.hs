-----------------------------------------------------------------------------
-- |
-- Module      :  Plugins.Monitors.Batt
-- Copyright   :  (c) Andrea Rossato
-- License     :  BSD-style (see LICENSE)
--
-- Maintainer  :  Andrea Rossato <andrea.rossato@unibz.it>
-- Stability   :  unstable
-- Portability :  unportable
--
-- A battery monitor for Xmobar
--
-----------------------------------------------------------------------------

module Plugins.Monitors.Batt where

import qualified Data.ByteString.Lazy.Char8 as B
import Plugins.Monitors.Common
import System.Posix.Files (fileExist)

data Batt = Batt Float
          | NA

battConfig :: IO MConfig
battConfig = mkMConfig
       "Batt: <left>" -- template
       ["left"]       -- available replacements

file2batfile :: String -> (String, String)
file2batfile s = ("/proc/acpi/battery/"++ s ++ "/info", "/proc/acpi/battery/"++ s ++ "/state")

readFileBatt :: (String, String) -> IO (B.ByteString, B.ByteString)
readFileBatt (i,s) =
    do a <- rf i
       b <- rf s
       return (a,b)
    where rf file = do
            f <- fileExist file
            if f then B.readFile file else return B.empty

parseBATT :: [(String, String)] -> IO Batt
parseBATT bfs =
    do [(a0,b0),(a1,b1),(a2,b2)] <- mapM readFileBatt (take 3 $ bfs ++ repeat ("",""))
       let sp p s = case stringParser p s of
                      [] -> 0
                      x -> read x
           (f0, p0) = (sp (3,2) a0, sp (2,4) b0)
           (f1, p1) = (sp (3,2) a1, sp (2,4) b1)
           (f2, p2) = (sp (3,2) a2, sp (2,4) b2)
           left = (p0 + p1 + p2) / (f0 + f1 + f2) --present / full
       return $ if isNaN left then NA else Batt left

formatBatt :: Float -> Monitor [String]
formatBatt x =
    do let f s = floatToPercent (s / 100)
       l <- showWithColors f (x * 100)
       return [l]

runBatt :: [String] -> Monitor String
runBatt = runBatt' ["BAT0","BAT1","BAT2"]

runBatt' :: [String] -> [String] -> Monitor String
runBatt' bfs _ = do
  c <- io $ parseBATT (map file2batfile bfs)
  case c of
    Batt x -> do l <- formatBatt x
                 parseTemplate l
    NA -> return "N/A"
