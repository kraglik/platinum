module Platinum.Parser where

import Control.Monad (void)
import Data.Void
import Data.Maybe
import Data.Text hiding (map, empty, head, find, cons)

import Platinum.AST

import Text.Megaparsec
import Text.Megaparsec.Char
import Text.Megaparsec.Pos
import qualified Text.Megaparsec.Char.Lexer as L

type Parser = Parsec Void Text


keywords = [str kw
    | kw <- [
            "let",
            "mut",
            "class",
            "instance",
            "import",
            "module",
            "if",
            "else",
            "while",
            "for",
            "data",
            "record",
            "showing",
            "hiding",
            "=",
            "<-",
            "->",
            "=>",
            "when",
            "..."
        ]
    ]

keyword = choice keywords <?> "keyword"

hasType = string $ pack "::" :: Parser Text

symbols     =   [
    char x | 
        x <- [
            '_', 
            '-', 
            '+', 
            '=', 
            '?', 
            '!', 
            '|', 
            '<', 
            '>', 
            '*',
            '&',
            '^',
            '%',
            '$',
            '@'
        ]
    ]   :: [Parser Char]
digits      =   [char x | x <- ['0'..'9']] :: [Parser Char]
lowerABC    =   [char x | x <- ['a'..'z']] :: [Parser Char]
upperABC    =   [char x | x <- ['A'..'Z']] :: [Parser Char]

headIdent = lowerABC ++ upperABC ++ symbols
headUpper = upperABC
headLower = lowerABC ++ symbols
tailIdent = digits ++ lowerABC ++ upperABC ++ symbols


sc :: Parser ()
sc = L.space
  space1
  (L.skipLineComment $ pack "//")
  (L.skipBlockComment (pack "/*") (pack "*/"))

lexeme :: Parser a -> Parser a
lexeme = L.lexeme sc


symbol :: Text -> Parser Text
symbol = L.symbol sc

str :: String -> Parser Text
str c = string $ pack c

lstr :: String -> Parser Text
lstr c = lexeme $ lexeme $ string (pack c)
