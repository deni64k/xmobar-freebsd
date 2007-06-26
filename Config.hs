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
              ) where

-- $config
-- Configuration data type and default configuration

-- | The configuration data type
data Config = 
    Config { fonts          :: String   -- ^ Fonts
           , bgColor        :: String   -- ^ Backgroud color
           , fgColor        :: String   -- ^ Default font color
           , xPos           :: Int      -- ^ x Window position (origin in the upper left corner) 
           , yPos           :: Int      -- ^ y Window position 
           , width          :: Int      -- ^ Window width
           , hight          :: Int      -- ^ Window hight
           , align          :: String   -- ^ text alignment
           , refresh        :: Int      -- ^ Refresh rate in tenth of seconds
           , commands       :: [(String, Int, [String])]   -- ^ For setting the refresh rate and 
                                                           -- options for the programs to run (optionals)
           , sepChar        :: String     -- ^ The character to be used for indicating 
                                        --   commands in the output template (default '%')
           , template       :: String   -- ^ The output template 
           } deriving (Eq, Show, Read, Ord)

-- | The default configuration values
defaultConfig :: Config
defaultConfig =
    Config { fonts = "-misc-fixed-*-*-*-*-10-*-*-*-*-*-*-*"
           , bgColor = "#000000"
           , fgColor = "#BFBFBF"
           , xPos = 0
           , yPos = 0
           , width = 1024
           , hight = 15
           , align = "left"
           , refresh = 10
           , commands = [("date", 10, [])]
           , sepChar = "%"
           , template = "Uptime: <fc=#00FF00>%uptime%</fc> ** <fc=#FF0000>%date%</fc>"
           }

