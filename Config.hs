-----------------------------------------------------------------------------
-- |
-- Module      :  XMobar.Config
-- Copyright   :  (c) Andrea Rossato
-- License     :  BSD-style (see LICENSE)
-- 
-- Maintainer  :  Andrea Rossato <andrea.rossato@unibz.it>
-- Stability   :  unstable
-- Portability :  unportable
--
-- The configuration module of XMobar, a status bar for the Xmonad Window Manager 
--
-----------------------------------------------------------------------------

module Config ( -- * Configuration
                -- $config
                Config (..)
              , defaultConfig
              , runnableTypes
              ) where

import Commands
import {-# SOURCE #-} Runnable

-- $config
-- Configuration data type and default configuration

-- | The configuration data type
data Config = 
    Config { font           :: String   -- ^ Font
           , bgColor        :: String   -- ^ Backgroud color
           , fgColor        :: String   -- ^ Default font color
           , xPos           :: Int      -- ^ x Window position (origin in the upper left corner) 
           , yPos           :: Int      -- ^ y Window position 
           , width          :: Int      -- ^ Window width
           , height         :: Int      -- ^ Window height
           , align          :: String   -- ^ text alignment
           , refresh        :: Int      -- ^ Refresh rate in tenth of seconds
           , commands       :: [Runnable] -- ^ For setting the command, the command argujments 
                                               -- and refresh rate for the programs to run (optional)
           , sepChar        :: String -- ^ The character to be used for indicating
                                      -- commands in the output template (default '%')
           , template       :: String -- ^ The output template 
           } deriving (Read)

-- | The default configuration values
defaultConfig :: Config
defaultConfig =
    Config { font = "-misc-fixed-*-*-*-*-10-*-*-*-*-*-*-*"
           , bgColor = "#000000"
           , fgColor = "#BFBFBF"
           , xPos = 0
           , yPos = 0
           , width = 1024
           , height = 15
           , align = "left"
           , refresh = 10
           , commands = [Run $ Memory [] 10]
           , sepChar = "%"
           , template = "Uptime: <fc=#00FF00>%uptime%</fc> ** <fc=#FF0000>%date%</fc> %memory%"
           }

-- | This is the list of types that can be hidden inside
-- 'Runnable.Runnable', the existential type that stores all commands
-- to be executed by XMobar. It is used by 'Runnable.readRunnable' in
-- the 'Runnable.Runnable' Read instance. To install a plugin just add
-- the plugin's type to the list of types appearing in this function's type
-- signature.
runnableTypes :: (Command,())
runnableTypes = undefined
