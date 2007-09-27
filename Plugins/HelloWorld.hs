-----------------------------------------------------------------------------
-- |
-- Module      :  Plugins.HelloWorld
-- Copyright   :  (c) Andrea Rossato
-- License     :  BSD-style (see LICENSE)
-- 
-- Maintainer  :  Andrea Rossato <andrea.rossato@unibz.it>
-- Stability   :  unstable
-- Portability :  unportable
--
-- A plugin example for Xmobar, a text based status bar 
--
-----------------------------------------------------------------------------

module Plugins.HelloWorld where

import Plugins

data HelloWorld = HelloWorld
    deriving (Read, Show)

instance Exec HelloWorld where
    rate HelloWorld = 0
    alias HelloWorld = "helloWorld"
    start HelloWorld cb = cb "<fc=red>Hello World!!</fc>"
