{-# OPTIONS -fglasgow-exts #-}
module Runnable where
import Commands

data Runnable = forall r . (Exec r,Read r) => Run r

instance Read Runnable
instance Exec Runnable



