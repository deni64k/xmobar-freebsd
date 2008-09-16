-----------------------------------------------------------------------------
-- |
-- Module      :  Plugins.CommandReader
-- Copyright   :  (c) John Goerzen
-- License     :  BSD-style (see LICENSE)
--
-- Maintainer  :  Andrea Rossato <andrea.rossato@unibz.it>
-- Stability   :  unstable
-- Portability :  unportable
--
-- A plugin for reading from external commands
-- note: stderr is lost here
--
-----------------------------------------------------------------------------

module Plugins.CommandReader where

import System.IO
import Plugins

data CommandReader = CommandReader String String
    deriving (Read, Show)

instance Exec CommandReader where
    alias (CommandReader _ a)    = a
    start (CommandReader p _) cb = do
        (hstdin, hstdout, hstderr) <- runInteractiveCommand p
        hClose hstdin
        hClose hstderr
        forever (hGetLineSafe hstdout >>= cb)
        where forever a = a >> forever a
