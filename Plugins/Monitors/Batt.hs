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

fileB0 :: (String, String)
fileB0 = ("/proc/acpi/battery/BAT0/info", "/proc/acpi/battery/BAT0/state")

fileB1 :: (String, String)
fileB1 = ("/proc/acpi/battery/BAT1/info", "/proc/acpi/battery/BAT1/state")

fileB2 :: (String, String)
fileB2 = ("/proc/acpi/battery/BAT2/info", "/proc/acpi/battery/BAT2/state")

readFileBatt :: (String, String) -> IO (B.ByteString, B.ByteString)
readFileBatt (i,s) = 
    do a <- rf i
       b <- rf s
       return (a,b)
    where rf file = do
            f <- fileExist file
            if f then B.readFile file else return B.empty

parseBATT :: IO Batt
parseBATT =
    do (a0,b0) <- readFileBatt fileB0
       (a1,b1) <- readFileBatt fileB1
       (a2,b2) <- readFileBatt fileB2
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
runBatt _ =
    do c <- io $ parseBATT
       case c of
         Batt x -> do l <- formatBatt x
                      parseTemplate l 
         NA -> return "N/A"
