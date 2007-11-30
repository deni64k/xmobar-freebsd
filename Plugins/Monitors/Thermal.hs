-----------------------------------------------------------------------------
-- |
-- Module      :  Plugins.Monitors.Thermal
-- Copyright   :  (c) Juraj Hercek
-- License     :  BSD-style (see LICENSE)
--
-- Maintainer  :  Juraj Hercek <juhe_haskell@hck.sk>
-- Stability   :  unstable
-- Portability :  unportable
--
-- A thermal monitor for Xmobar
--
-----------------------------------------------------------------------------

module Plugins.Monitors.Thermal where

import qualified Data.ByteString.Lazy.Char8 as B
import Plugins.Monitors.Common
import System.Posix.Files (fileExist)

thermalConfig :: IO MConfig
thermalConfig = mkMConfig
       "Thm: <temp>C" -- template
       ["temp"]       -- available replacements

runThermal :: [String] -> Monitor String
runThermal _ = do
    let file = "/proc/acpi/thermal_zone/THM/temperature"
    exists <- io $ fileExist file
    case exists of
         False  -> return "Thermal: N/A"
         True   -> do number <- io $ B.readFile file
                                     >>= return . (read :: String -> Int)
                                                . stringParser (1, 0)
                      thermal <- showWithColors show number
                      parseTemplate [  thermal ]

