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

