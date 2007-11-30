-----------------------------------------------------------------------------
-- |
-- Module      :  Plugins.Monitors.CpuFreq
-- Copyright   :  (c) Juraj Hercek
-- License     :  BSD-style (see LICENSE)
--
-- Maintainer  :  Juraj Hercek <juhe_haskell@hck.sk>
-- Stability   :  unstable
-- Portability :  unportable
--
-- A cpu frequency monitor for Xmobar
--
-----------------------------------------------------------------------------

module Plugins.Monitors.CpuFreq where

import Plugins.Monitors.Common
import Plugins.Monitors.CoreCommon

cpuFreqConfig :: IO MConfig
cpuFreqConfig = mkMConfig
       "Freq: <core0>GHz" -- template
       (zipWith (++) (repeat "core") (map show [0 :: Int ..])) -- available
                                                               -- replacements
runCpuFreq :: [String] -> Monitor String
runCpuFreq _ = do
    let dir = "/sys/devices/system/cpu"
        file = "cpufreq/scaling_cur_freq"
        pattern = "cpu"
        divisor = 1e6 :: Double
        failureMessage = "CpuFreq: N/A"
    checkedDataRetrieval failureMessage dir file pattern divisor

