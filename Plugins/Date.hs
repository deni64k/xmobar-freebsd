-----------------------------------------------------------------------------
-- |
-- Module      :  Plugins.Date
-- Copyright   :  (c) Andrea Rossato
-- License     :  BSD-style (see LICENSE)
-- 
-- Maintainer  :  Andrea Rossato <andrea.rossato@unibz.it>
-- Stability   :  unstable
-- Portability :  unportable
--
-- A date plugin for Xmobar
--
-- Usage example: in template put 
-- Run Date "%a %b %_d %Y <fc=#ee9a00> %H:%M:%S</fc>" "mydate" 10
--
-----------------------------------------------------------------------------

module Plugins.Date where

import Plugins
import System.Locale
import System.Time


data Date = Date String String Int
    deriving (Read)

instance Exec Date where
    run (Date f _ _) = date f
    rate (Date _ _ r) = r
    alias (Date _ a _) = a

date :: String -> IO String
date format = do
 t <- toCalendarTime =<< getClockTime
 return $ formatCalendarTime defaultTimeLocale format t

