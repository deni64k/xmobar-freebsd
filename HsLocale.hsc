{-# OPTIONS -fglasgow-exts #-}
-----------------------------------------------------------------------------
-- |
-- Module      :  HsLocale
-- Copyright   :  (C) 2007 Andrea Rossato
-- License     :  BSD3
--
-- Maintainer  :  andrea.rossato@unibz.it
-- Stability   :  unstable
-- Portability :  unportable
--
-----------------------------------------------------------------------------

module HsLocale
    ( setupLocale
    ) where

import Foreign.C

#include <locale.h>
foreign import ccall unsafe "locale.h setlocale"
    setlocale :: CInt -> CString -> IO CString

setupLocale :: IO ()
setupLocale = withCString "" $ \s -> do
                setlocale (#const LC_ALL) s
                return ()
