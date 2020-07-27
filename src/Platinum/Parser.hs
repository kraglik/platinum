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

charLiteral :: Parser Char
charLiteral = between (char '\'') (char '\'') L.charLiteral <?> "char literal"


escape :: Parser String
escape = do
    d <- char '\\'
    c <- oneOf "\\\"0nrvtbf" -- all the characters which can be escaped
    return [d, c]

nonEscape :: Parser Char
nonEscape = noneOf "\\\"\0\n\r\v\t\b\f"

character :: Parser String
character = fmap return nonEscape <|> escape


stringLiteral :: Parser Text
stringLiteral = (do
        char '"'
        strings <- many character
        char '"'
        return $ pack (Prelude.concat strings)
    ) <?> "string literal"


integer :: Parser Integer
integer = lexeme L.decimal <?> "integer literal"


float :: Parser Double
float = lexeme L.float <?> "float literal"


semicolon :: Parser ()
semicolon = (lexeme (str ";") >> return ()) <?> "semicolon (;)"

comma :: Parser ()
comma = (lexeme (str ",") >> return ()) <?> "comma (,)"


dot :: Parser ()
dot = (lexeme (str ".") >> return ()) <?> "dot (.)"

signedInteger :: Parser Integer
signedInteger = L.signed sc integer <?> "signed integer literal"

signedFloat :: Parser Double
signedFloat = L.signed sc float <?> "signed float literal"


pub :: Parser Bool
pub = do p <- optional $ str "pub"
         return $ case p of Nothing -> False
                            Just _  -> True


combinePos :: SourcePos -> SourcePos -> SrcPos
combinePos
    (SourcePos p ls cs)
    (SourcePos _ le ce) = SrcPos (unPos ls)
                                 (unPos cs)
                                 (unPos le)
                                 (unPos ce)
                                 (pack p)


commaSep :: Parser a -> Parser [a]
commaSep p = p `sepBy1` lstr ","


rndCommaSep :: Parser a -> Parser [a]
rndCommaSep p = do lstr "("
                   out <- commaSep p
                   lstr ")"
                   return out


consName :: Parser Text
consName = try (do
    try $ notFollowedBy keyword
    head <- choice headUpper
    tail <- many (choice tailIdent)
    return $ pack $ head : tail) <?> "constructor name"


varName :: Parser Text
varName = try (do
    notFollowedBy keyword
    head <- choice headLower
    tail <- many (choice tailIdent)

    return $ pack $ head : tail) <?> "constructor name"


name :: Parser Text
name = try (do try $ notFollowedBy keyword

               head <- choice headIdent
               tail <- many (choice tailIdent)

               return $ pack $ head : tail) <?> "name"


cons :: Parser Cons
cons = try (do p1 <- getSourcePos
               name <- lexeme consName
               Cons name . combinePos p1 <$> getSourcePos) <?> "identifier"


cls :: Parser Cls
cls = try (do p1 <- getSourcePos
              name <- lexeme consName
              Cls name . combinePos p1 <$> getSourcePos) <?> "identifier"


var :: Parser Var
var = try (do p1 <- getSourcePos
              name <- lexeme varName
              Var name . combinePos p1 <$> getSourcePos) <?> "identifier"


typevar :: Parser TypeVar
typevar = try (do p1 <- getSourcePos
                  name <- lexeme varName
                  TypeVar name . combinePos p1 <$> getSourcePos) <?> "identifier"


literal :: Parser Expression
literal = try (signedFloatLit
           <|> floatLit
           <|> signedIntLit
           <|> intLit
           <|> charLit
           <|> stringLit)
           <?> "literal"
    where
        signedIntLit = try (
            do p1 <- getSourcePos
               val <- signedInteger
               Literal (Integer val) . combinePos p1 <$> getSourcePos
          )
        intLit = try (
            do p1 <- getSourcePos
               val <- integer
               Literal (Integer val) . combinePos p1 <$> getSourcePos
          )
        signedFloatLit = try (
            do p1 <- getSourcePos
               val <- signedFloat
               Literal (Float val) . combinePos p1 <$> getSourcePos
          )
        floatLit = try (
            do p1 <- getSourcePos
               val <- float
               Literal (Float val) . combinePos p1 <$> getSourcePos
          )
        charLit = try (
            do p1 <- getSourcePos
               val <- charLiteral
               Literal (Char val) . combinePos p1 <$> getSourcePos
          )
        stringLit = try (
            do p1 <- getSourcePos
               val <- stringLiteral
               Literal (String val) . combinePos p1 <$> getSourcePos
          )


dottedName :: Parser [Text]
dottedName = name `sepBy1` dot


importItem :: Parser ImportItem
importItem = try (do
    p1 <- getSourcePos
    nm <- lexeme name
    als <- optional $ do lexeme $ str "as"
                         lexeme name
    ImportItem nm als . combinePos p1 <$> getSourcePos
    ) <?> "import item"


importStmt :: Parser Import
importStmt = try (simpleImport
              <|> importAs
              <|> importShowing
              <|> importHiding
            )
    where
        simpleImport = try (do
            p1 <- getSourcePos
            lstr "import"
            path <- dottedName
            lstr ";"
            p2 <- getSourcePos
            return $ Import path (combinePos p1 p2)) <?> "import"

        importAs = try (do
            p1 <- getSourcePos
            lstr "import"
            path <- dottedName
            lstr "as"
            alias <- lexeme $ name
            lstr ";"
            p2 <- getSourcePos
            return $ Import path (combinePos p1 p2)) <?> "import as"

        importShowing = try (do
            p1 <- getSourcePos
            lstr "import"
            path <- dottedName
            lstr "showing"
            items <- rndCommaSep importItem
            lstr ";"
            p2 <- getSourcePos
            return $ ImportShowing path items (combinePos p1 p2)) <?> "import showing"

        importHiding = try (do
            p1 <- getSourcePos
            lstr "import"
            path <- dottedName
            lstr "hiding"
            items <- rndCommaSep name
            lstr ";"
            p2 <- getSourcePos
            return $ ImportHiding path items (combinePos p1 p2)) <?> "import hiding"
