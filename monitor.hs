#! /usr/bin/env runhaskell


import Data.Time (getZonedTime)
import Text.Printf (printf)
import System.Process (runInteractiveCommand)
import Data.List
import System.IO (hGetContents)


getOutput :: String -> IO String
getOutput cmd = do
    (_, out, _, _) <- runInteractiveCommand cmd
    hGetContents out 

memParse :: String -> String
memParse file = 
    let content = map words $ take 4 $ lines file
        [total, free, buffer, cache] = map (\line -> (read $ line !! 1 :: Float) / 1024) content
        rest = free + buffer + cache
        used = total - rest
        usedratio = used * 100 / total
    in
        printf "MEM: %sM %.1f%% used   %.0fM rest" used usedratio rest


mem :: IO String
mem = do
    file <- readFile "/proc/meminfo"
    return $ memParse file


time :: IO String
time = do
    now <- getZonedTime
    return $ take 16 $ show now


temp :: IO String
temp = do
    file <- readFile "/proc/acpi/thermal_zone/THRM/temperature"
    let t = (words file) !! 1
    return $ "TEMP: " ++ t ++ "C"


takeTail :: Int -> [a] -> [a]
takeTail n xs =
    let len = length xs in
        drop (len-n) xs


load :: IO String
load = do
    content <- getOutput "uptime"
    let l = map (delete ',') $ takeTail 3 $ words content
    return $ unwords $ "LOAD:" : l


sep :: IO String
sep = return "        "


main = do 
    putStr ""
    mapM_ (>>=putStr) $ intersperse sep [load, temp, mem, time]
    putChar '\n'