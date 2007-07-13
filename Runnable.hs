{-# OPTIONS -fglasgow-exts #-}
-----------------------------------------------------------------------------
-- |
-- Module      :  XMobar.Runnable
-- Copyright   :  (c) Andrea Rossato
-- License     :  BSD-style (see LICENSE)
-- 
-- Maintainer  :  Andrea Rossato <andrea.rossato@unibz.it>
-- Stability   :  unstable
-- Portability :  unportable
--
-- The existential type to store the list of commands to be executed.
-- I must thank Claus Reinke for the help in understanding the mysteries of
-- reading existential types. The Read instance of Runnable must be credited to 
-- him. 
-- 
-- See here:
-- http:\/\/www.haskell.org\/pipermail\/haskell-cafe\/2007-July\/028227.html
--
-----------------------------------------------------------------------------

module Runnable where

import Control.Monad
import Text.Read
import Text.ParserCombinators.ReadPrec
import Config (runnableTypes)
import Commands

data Runnable = forall r . (Exec r, Read r) => Run r

instance Exec Runnable where
     run (Run a) = run a
     rate (Run a) = rate a
     alias (Run a) = alias a

instance Read Runnable where
    readPrec = readRunnable

class ReadAsAnyOf ts ex where
    -- | Reads an existential type as any of hidden types ts
    readAsAnyOf :: ts -> ReadPrec ex

instance ReadAsAnyOf () ex where 
    readAsAnyOf ~() = mzero

instance (Read t, Exec t, ReadAsAnyOf ts Runnable) => ReadAsAnyOf (t,ts) Runnable where 
    readAsAnyOf ~(t,ts) = r t `mplus` readAsAnyOf ts
              where r ty = do { m <- readPrec; return (Run (m `asTypeOf` ty)) }

readRunnable :: ReadPrec Runnable
readRunnable = prec 10 $ do
                 Ident "Run" <- lexP
                 parens $ readAsAnyOf runnableTypes
