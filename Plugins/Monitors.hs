-----------------------------------------------------------------------------
-- |
-- Module      :  Xmobar.Plugins.Monitors
-- Copyright   :  (c) Andrea Rossato
-- License     :  BSD-style (see LICENSE)
-- 
-- Maintainer  :  Andrea Rossato <andrea.rossato@unibz.it>
-- Stability   :  unstable
-- Portability :  unportable
--
-- The 'Exec' class and the 'Command' data type.
--
-- The 'Exec' class rappresents the executable types, whose constructors may
-- appear in the 'Config.commands' field of the 'Config.Config' data type.
--
-- The 'Command' data type stores the monitors to be run internally by
-- Xmobar.
--
-----------------------------------------------------------------------------

module Plugins.Monitors where

import Plugins

import Plugins.Monitors.Common ( runM )
import Plugins.Monitors.Weather
import Plugins.Monitors.Net
import Plugins.Monitors.Mem
import Plugins.Monitors.Swap
import Plugins.Monitors.Cpu
import Plugins.Monitors.Batt

data Monitors = Weather Station Args Rate
              | Network Interface Args Rate
              | Memory Args Rate
              | Swap Args Rate
              | Cpu Args Rate
              | Battery Args Rate
                deriving (Show,Read,Eq)

type Args = [String]
type Program = String
type Alias = String
type Station = String
type Interface = String
type Rate = Int

instance Exec Monitors where
    alias (Weather s _ _) = s
    alias (Network i _ _) = i
    alias (Memory _ _) = "memory"
    alias (Swap _ _) = "swap"
    alias (Cpu _ _) = "cpu"
    alias (Battery _ _) = "battery"
    rate (Weather _ _ r) = r
    rate (Network _ _ r) = r
    rate (Memory _ r) = r
    rate (Swap _ r) = r
    rate (Cpu _ r) = r
    rate (Battery _ r) = r
    run (Weather s a _) = runM (a ++ [s]) weatherConfig runWeather 
    run (Network i a _) = runM (a ++ [i]) netConfig runNet
    run (Memory args _) = runM args memConfig runMem
    run (Swap args _) = runM args swapConfig runSwap
    run (Cpu args _) = runM args cpuConfig runCpu
    run (Battery args _) = runM args battConfig runBatt
