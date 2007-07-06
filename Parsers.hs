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
import Text.ParserCombinators.Parsec


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
      ; (_,com,_) <- templateCommandParser c
      ; ss <- many $ noneOf (sepChar c)
      ; return (s, com, ss)
      } 

-- | Parses the command part of the template string
templateCommandParser :: Config -> Parser (String,String,String)
templateCommandParser c =
    do { let chr = head $ sepChar c
       ; char chr
       ; com <- many $ noneOf (sepChar c)
       ; char chr
       ; return $ ("",com,"")
       }
-- | Combines the template parsers
templateParser :: Config -> Parser [(String,String,String)]
templateParser c = many (templateStringParser c)

-- | Actually runs the template parsers
parseTemplate :: Config -> String -> IO [(String,String,String)]
parseTemplate config s = 
    case (parse (templateParser config) "" s) of
      Left _ -> return [("","","")]
      Right x  -> return x

