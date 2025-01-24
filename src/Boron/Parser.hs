module Boron.Parser where

import Boron.Eval
import Boron.AST
import Text.Megaparsec
import Text.Megaparsec.Char
import Text.Megaparsec.Char.Lexer qualified as L
import Data.Functor
import Data.Void
import Control.Applicative hiding (many, some)

type Parser = Parsec Void String


boolParser :: Parser Expr 
boolParser = fmap LiteralBool $ choice
                                [ symbol "#t" $> True
                                , symbol "#f" $> False]

-- Identifiers and numbers such
lexeme :: Parser a -> Parser a
lexeme = L.lexeme hspace

-- Verbatim strings
symbol :: String -> Parser String
symbol = L.symbol hspace

name :: Parser String
name = do
  prefix <- letterChar <|> oneOf validPunct
  inner  <- many $ alphaNumChar <|> oneOf validPunct
  suffix <- many $ char '\''
  pure $ prefix : (inner ++ suffix)
  where validPunct = "!#$%&/=*+-<>?@^_~"

identifier :: Parser Name
identifier = name

number :: Parser Expr
number = LiteralNum <$> choice [asBin, asHex, asDec]
  where asBin = string "0b" *> L.binary
        asHex = string "0h" *> L.hexadecimal
        asDec = L.decimal

ifthenelse :: Parser Expr
ifthenelse = do
  _if <- string "if"
  cond <- expr
  whenTrue <- block
  innerRest <- elifthenelse
  _else <- string "else"
  whenFalse <- block
  pure $ If cond whenTrue (innerRest : whenFalse)

  where elifthenelse = do 
          _elif <- string "elif"
          cond <- expr
          whenTrue <- block
          rest <- many elifthenelse
          pure $ If cond whenTrue rest
  


expr :: Parser Expr
expr = undefined

block :: Parser [Expr]
block = undefined
