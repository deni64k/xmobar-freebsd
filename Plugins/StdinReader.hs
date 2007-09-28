-----------------------------------------------------------------------------
-- |
-- Module      :  Plugins.StdinReader
-- Copyright   :  (c) Andrea Rossato
-- License     :  BSD-style (see LICENSE)
-- 
-- Maintainer  :  Andrea Rossato <andrea.rossato@unibz.it>
-- Stability   :  unstable
-- Portability :  unportable
--
-- A plugin for reading from stdin
--
-----------------------------------------------------------------------------

module Plugins.StdinReader where

import System.IO
import Plugins

data StdinReader = StdinReader
    deriving (Read, Show)

instance Exec StdinReader where 
    start StdinReader cb = do
        forever (hGetLine stdin >>= cb)
        where forever a = a >> forever a
