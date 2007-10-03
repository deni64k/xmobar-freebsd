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
-- The 'Command' data type is for OS commands to be run by Xmobar 
--
-----------------------------------------------------------------------------

module Commands where

import Control.Concurrent
import Data.Char
import System.Process
import System.Exit
import System.IO (hClose, hGetLine)

class Show e => Exec e where
    alias :: e -> String
    alias e = takeWhile (not . isSpace) $ show e
    run :: e -> IO String
    run _ = return ""
    start :: e -> (String -> IO ()) -> IO ()
    start e cb = go
        where go = do
                run e >>= cb
                tenthSeconds 10 >> go

data Command = Com Program Args Alias Rate
               deriving (Show,Read,Eq)

type Args    = [String]
type Program = String
type Alias   = String
type Rate    = Int

instance Exec Command where
    alias (Com p    _    a _)
        | p /= ""             = if a == "" then p else a
        | otherwise           = ""
    start (Com prog args _ r) cb = do go
        where go = do
                (i,o,e,p) <- runInteractiveCommand (prog ++ concat (map (' ':) args))
                exit <- waitForProcess p
                let closeHandles = do 
                        hClose o
                        hClose i
                        hClose e
                case exit of
                  ExitSuccess -> do 
                            str <- hGetLine o
                            closeHandles
                            cb str
                  _ -> do closeHandles
                          cb $ "Could not execute command " ++ prog
                tenthSeconds r >> go

-- | Work arount to the Int max bound: since threadDelay takes an Int, it
-- is not possible to set a thread delay grater than about 45 minutes.
-- With a little recursion we solve the problem.
tenthSeconds :: Int -> IO ()
tenthSeconds s | s >= x = do threadDelay y
                             tenthSeconds (x - s)
               | otherwise = threadDelay (s * 100000)
               where y = (maxBound :: Int)
                     x = y `div` 100000
