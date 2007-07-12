-----------------------------------------------------------------------------
-- |
-- Module      :  XMobar.Parsers
-- Copyright   :  (c) Andrea Rossato
-- License     :  BSD-style (see LICENSE)
-- 
-- Maintainer  :  Andrea Rossato <andrea.rossato@unibz.it>
-- Stability   :  unstable
-- Portability :  unportable
--
-- Parsers needed for XMobar, a status bar for the Xmonad Window Manager 
--
-----------------------------------------------------------------------------

module Parsers (
               -- * Parsing
               -- $parser
               parseString
               , stringParser
               , defaultColors
               , colorsAndText
               , templateStringParser
               , templateCommandParser
               , templateParser
               , parseTemplate
               ) where

import Config
import Commands
import Runnable
import Text.ParserCombinators.Parsec
import qualified Data.Map as Map


{- $parser
These are the neede parsers. Don't trust them too much.

There are parsers for the commands output and parsers for the
formatting template.
 -}

-- | Runs the actual string parsers
parseString :: Config -> String -> IO [(String, String)]
parseString config s = 
    case (parse (stringParser config) "" s) of
      Left _ -> return [("Could not parse string: " ++ s
                        , (fgColor config))]
      Right x  -> return x

-- | Gets the string and combines the needed parsers
stringParser :: Config -> Parser [(String, String)]
stringParser c = manyTill (colorsAndText c <|> defaultColors c) eof

-- | Parses a string with the default color (no color set)
defaultColors :: Config -> Parser (String, String)
defaultColors config = 
    do { s <- many $ noneOf "<"
       ; return (s,(fgColor config))
       }
    <|> colorsAndText config

-- | Parses a string with a color set
colorsAndText :: Config -> Parser (String, String) 
colorsAndText config = 
    do { string "<fc="
       ; c <- colorSpec
       ; string ">"
       ; s <- many $ noneOf "<"
       ; string "</fc>"
       ; return (s,c)
       }
    <|> defaultColors config

-- | Parses a color specification (hex or named)
colorSpec :: Parser String
colorSpec =
    do { c <- char '#'
       ; s <- count 6 hexDigit
       ; return $ c:s
       }
    <|> many1 alphaNum

-- | Parses the output template string
templateStringParser :: Config -> Parser (String,String,String)
templateStringParser c =
    do{ s <- many $ noneOf (sepChar c)
      ; (com,_,_) <- templateCommandParser c
      ; ss <- many $ noneOf (sepChar c)
      ; return (com, s, ss)
      } 

-- | Parses the command part of the template string
templateCommandParser :: Config -> Parser (String,String,String)
templateCommandParser c =
    do { let chr = head $ sepChar c
       ; char chr
       ; com <- many $ noneOf (sepChar c)
       ; char chr
       ; return $ (com,"","")
       } 

-- | Combines the template parsers
templateParser :: Config -> Parser [(String,String,String)]
templateParser c = many (templateStringParser c)

-- | Actually runs the template parsers
parseTemplate :: Config -> String -> IO [(Runnable,String,String)]
parseTemplate config s = 
    do str <- case (parse (templateParser config) "" s) of
                Left _ -> return [("","","")]
                Right x  -> return x
       let comList = map alias (commands config)
           m = Map.fromList $ zip comList (commands config)
       return $ combine config m str

-- | Given a finite "Map" and a parsed templatet produces the
-- | resulting output string.
combine :: Config -> Map.Map String Runnable -> [(String, String, String)] -> [(Runnable,String,String)]
combine _ _ [] = []
combine config m ((ts,s,ss):xs) = 
    [(com, s, ss)] ++ combine config m xs
        where com = Map.findWithDefault dflt ts m
              dflt = Run $ Com ts [] [] (refresh config) --"<" ++ ts ++ " not found!>"
