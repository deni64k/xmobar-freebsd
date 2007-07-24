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

battConfig :: IO MConfig
battConfig = mkMConfig
       "Batt: <left>" -- template
       ["left"]       -- available replacements

fileB1 :: (String, String)
fileB1 = ("/proc/acpi/battery/BAT1/info", "/proc/acpi/battery/BAT1/state")

fileB2 :: (String, String)
fileB2 = ("/proc/acpi/battery/BAT2/info", "/proc/acpi/battery/BAT2/state")

readFileBatt :: (String, String) -> IO (B.ByteString, B.ByteString)
readFileBatt (i,s) = 
    do a <- catch (B.readFile i) (const $ return B.empty)
       b <- catch (B.readFile s) (const $ return B.empty)
       return (a,b)

parseBATT :: IO Float
parseBATT =
    do (a1,b1) <- readFileBatt fileB1
       (a2,b2) <- readFileBatt fileB2
       let sp p s = case stringParser p s of
                      [] -> 0
                      x -> read x
           (f1, p1) = (sp (3,2) a1, sp (2,4) b1)
           (f2, p2) = (sp (3,2) a2, sp (2,4) b2)
       return $ (p1 + p2) / (f1 + f2) --present / full
      
formatBatt :: Float -> Monitor [String] 
formatBatt x =
    do let f s = floatToPercent (s / 100)
       l <- showWithColors f (x * 100)
       return [l]

runBatt :: [String] -> Monitor String
runBatt _ =
    do c <- io $ parseBATT
       l <- formatBatt c
       parseTemplate l 
