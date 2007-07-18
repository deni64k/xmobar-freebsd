-----------------------------------------------------------------------------
-- |
-- Module      :  Xmobar.Commands
-- Copyright   :  (c) Andrea Rossato
-- License     :  BSD-style (see LICENSE)
-- 
-- Maintainer  :  Andrea Rossato <andrea.rossato@unibz.it>
-- Stability   :  unstable
-- Portability :  unportable
--
-- The 'Exec' class and the 'Command' data type.
--
-- The 'Exec' class rappresents the executable types, whose constructors may
-- appear in the 'Config.commands' field of the 'Config.Config' data type.
--
-- The 'Command' data type stores the monitors to be run internally by
-- Xmobar.
--
-----------------------------------------------------------------------------

module Commands where

import System.Process
import System.Exit
import System.IO (hClose, hGetLine)

class Exec e where
    run :: e -> IO String
    rate :: e -> Int
    alias :: e -> String

data Command = Com Program Args Alias Rate
               deriving (Show,Read,Eq)

type Args = [String]
type Program = String
type Alias = String
type Rate = Int

instance Exec Command where
    alias (Com p _ a _) | p /= "" = if  a == "" then p else a
                        | otherwise = ""
    rate (Com _ _ _ r) = r
    run (Com prog args _ _) = do (i,o,e,p) <- runInteractiveCommand (prog ++ concat (map (' ':) args))
                                 exit <- waitForProcess p
                                 let closeHandles = do hClose o
                                                       hClose i
                                                       hClose e
                                 case exit of
                                   ExitSuccess -> do str <- hGetLine o
                                                     closeHandles
                                                     return str
                                   _ -> do closeHandles
                                           return $ "Could not execute command " ++ prog


