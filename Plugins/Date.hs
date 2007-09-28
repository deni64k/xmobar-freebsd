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
    deriving (Read, Show)

instance Exec Date where
    start (Date f _ r) cb = date f r cb
    alias (Date _ a _)    = a

date :: String -> Int -> (String -> IO ()) -> IO ()
date format r cb = do go
    where go = do
            t <- toCalendarTime =<< getClockTime
            cb $ formatCalendarTime defaultTimeLocale format t
            tenthSeconds r >> go
  