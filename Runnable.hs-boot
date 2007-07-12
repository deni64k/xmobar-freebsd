{-# OPTIONS -fglasgow-exts #-}
module Runnable where
import Commands

data Runnable = forall r . (Exec r,Show r, Read r) => Run r

instance Read Runnable
instance Exec Runnable
instance Show Runnable


