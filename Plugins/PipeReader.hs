-----------------------------------------------------------------------------
-- |
-- Module      :  Plugins.PipeReader
-- Copyright   :  (c) Andrea Rossato
-- License     :  BSD-style (see LICENSE)
-- 
-- Maintainer  :  Andrea Rossato <andrea.rossato@unibz.it>
-- Stability   :  unstable
-- Portability :  unportable
--
-- A plugin for reading from named pipes
--
-----------------------------------------------------------------------------

module Plugins.PipeReader where

import System.IO
import Plugins

data PipeReader = PipeReader String
    deriving (Read, Show)

instance Exec PipeReader where 
    alias (PipeReader p)    = p
    start (PipeReader p) cb = do
        h <- openFile p ReadMode
        forever (hGetLine h >>= cb)
        where forever a = a >> forever a
