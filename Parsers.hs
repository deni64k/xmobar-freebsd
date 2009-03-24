-----------------------------------------------------------------------------
-- |
-- Module      :  Xmobar.Parsers
-- Copyright   :  (c) Andrea Rossato
-- License     :  BSD-style (see LICENSE)
--
-- Maintainer  :  Andrea Rossato <andrea.rossato@unibz.it>
-- Stability   :  unstable
-- Portability :  unportable
--
-- Parsers needed for Xmobar, a text based status bar
--
-----------------------------------------------------------------------------

module Parsers
    ( parseString
    , parseTemplate
    ) where

import Config
import Commands
import Runnable
import Text.ParserCombinators.Parsec
import qualified Data.Map as Map

-- | Runs the string parser
parseString :: Config -> String -> IO [(String, String)]
parseString c s =
    case parse (stringParser c) "" s of
      Left  _ -> return [("Could not parse string: " ++ s, fgColor c)]
      Right x -> return (concat x)

-- | Gets the string and combines the needed parsers
stringParser :: Config -> Parser [[(String, String)]]
stringParser = flip manyTill eof . colorParser

-- | Parses a string with the default color (no color set)
colorParser :: Config -> Parser [(String, String)]
colorParser c = do
  s <- manyTill anyChar (tryString "<fc" <|> endOfLine "")
  n <- colorsAndText <|> endOfLine ("","")
  return [(s,fgColor c),n]

-- | Parses a string with a color set
colorsAndText :: Parser (String, String)
colorsAndText = do
  c <- inside (string "=") colors (string ">")
  s <- manyTill anyChar (tryString "</fc>")
  return (s,c)

-- | Parses a color specification (hex or named)
colors :: Parser String
colors = many1 (alphaNum <|> char ',' <|> char '#')

-- | Parses the output template string
templateStringParser :: Config -> Parser (String,String,String)
templateStringParser c = do
  s   <- allTillSep c
  com <- templateCommandParser c
  ss  <- allTillSep c
  return (com, s, ss)

-- | Parses the command part of the template string
templateCommandParser :: Config -> Parser String
templateCommandParser c =
  let chr = char . head . sepChar
  in  between (chr c) (chr c) (allTillSep c) 

-- | Combines the template parsers
templateParser :: Config -> Parser [(String,String,String)]
templateParser = many . templateStringParser

-- | Actually runs the template parsers
parseTemplate :: Config -> String -> IO [(Runnable,String,String)]
parseTemplate c s =
    do str <- case parse (templateParser c) "" s of
                Left _  -> return [("","","")]
                Right x -> return x
       let cl = map alias (commands c)
           m  = Map.fromList $ zip cl (commands c)
       return $ combine c m str

-- | Given a finite "Map" and a parsed templatet produces the
-- | resulting output string.
combine :: Config -> Map.Map String Runnable -> [(String, String, String)] -> [(Runnable,String,String)]
combine _ _ [] = []
combine c m ((ts,s,ss):xs) = (com, s, ss) : combine c m xs
    where com  = Map.findWithDefault dflt ts m
          dflt = Run $ Com ts [] [] 10

endOfLine :: a -> Parser a
endOfLine r = eof >> return r

tryString :: String -> Parser String
tryString = try . string

allTillSep :: Config -> Parser String
allTillSep = many . noneOf . sepChar
