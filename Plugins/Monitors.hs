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
-- The system monitor plugin for Xmobar.
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

type Args      = [String]
type Program   = String
type Alias     = String
type Station   = String
type Interface = String
type Rate      = Int

instance Exec Monitors where
    alias (Weather s _ _) = s
    alias (Network i _ _) = i
    alias (Memory    _ _) = "memory"
    alias (Swap      _ _) = "swap"
    alias (Cpu       _ _) = "cpu"
    alias (Battery   _ _) = "battery"
    start (Weather s a r) = runM (a ++ [s]) weatherConfig runWeather r
    start (Network i a r) = runM (a ++ [i]) netConfig     runNet     r
    start (Memory    a r) = runM a          memConfig     runMem     r
    start (Swap      a r) = runM a          swapConfig    runSwap    r
    start (Cpu       a r) = runM a          cpuConfig     runCpu     r
    start (Battery   a r) = runM a          battConfig    runBatt    r
