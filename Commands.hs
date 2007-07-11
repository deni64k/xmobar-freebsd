-----------------------------------------------------------------------------
-- |
-- Module      :  XMobar.Commands
-- Copyright   :  (c) Andrea Rossato
-- License     :  BSD-style (see LICENSE)
-- 
-- Maintainer  :  Andrea Rossato <andrea.rossato@unibz.it>
-- Stability   :  unstable
-- Portability :  unportable
--
-- A Command datatype for XMobar status bar for the Xmonad Window Manager 
--
-----------------------------------------------------------------------------

module Commands where

import System.Process
import System.Exit
import System.IO (hClose, hGetLine)

import Monitors.Common ( runM )
import Monitors.Weather
import Monitors.Net
import Monitors.Mem
import Monitors.Swap
import Monitors.Cpu
import Monitors.Batt

data Command = Exec Program Args Alias 
             | Weather Station Args 
             | Network Interface Args
             | Memory Args
             | Swap Args
             | Cpu Args
             | Battery Args
               deriving (Read,Eq)

type Args = [String]
type Program = String
type Alias = String
type Station = String
type Interface = String

instance Show Command where
    show (Weather s _) = s
    show (Network i _) = i
    show (Memory _) = "memory"
    show (Swap _) = "swap"
    show (Cpu _) = "cpu"
    show (Battery _) = "battery"
    show (Exec p _ a) | p /= "" = if  a == "" then p else a
                      | otherwise = ""
class Exec e where
    run :: e -> IO String

instance Exec Command where
    run (Weather s a) = runM (a ++ [s]) weatherConfig runWeather 
    run (Network i a) = runM (a ++ [i]) netConfig runNet
    run (Memory args) = runM args memConfig runMem
    run (Swap args) = runM args swapConfig runSwap
    run (Cpu args) = runM args cpuConfig runCpu
    run (Battery args) = runM args battConfig runBatt
    run (Exec prog args _) = do (i,o,e,p) <- runInteractiveCommand (prog ++ concat (map (' ':) args))
                                exit <- waitForProcess p
                                let closeHandles = do hClose o
                                                      hClose i
                                                      hClose e
                                case exit of
                                  ExitSuccess -> do str <- hGetLine o
                                                    closeHandles
                                                    return str
                                  _ -> do closeHandles
                                          return $ "Could not execute command " ++ prog
