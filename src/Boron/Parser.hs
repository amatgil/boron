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

identifier :: Parser String
identifier = allSymbols <|> allAlphaNum
  where
    validPunct = "!#$%&/=*+-<>?@^_~"
    allSymbols = do
      inner <- many $ oneOf validPunct
      suffix <- many $ char '\''
      pure $ inner ++ suffix
    allAlphaNum = do
      prefix <- letterChar <|> char '_' -- must start with a letter or underscore
      inner  <- many $ alphaNumChar <|> char '_'
      suffix <- many $ char '\''
      pure $ prefix : (inner ++ suffix)

number :: Parser Expr
number = LiteralNum <$> choice [asBin, asHex, asDec]
  where asBin = string "0b" *> L.binary
        asHex = string "0h" *> L.hexadecimal
        asDec = L.decimal

for :: Parser Expr
for = do
  _f <- symbol "for"
  iterVar <- identifier
  _in <- symbol "in"
  iterVals <- expr
  body <- block
  pure $ For iterVar iterVals body

while :: Parser Expr
while = do
  _w <- symbol "while"
  pred <- expr
  inner <- block
  pure $ While pred inner

ifthenelse :: Parser Expr
ifthenelse = do
  _if <- symbol "if"
  cond <- expr
  whenTrue <- block
  innerRest <- elifthenelse
  _else <- symbol "else"
  whenFalse <- block
  pure $ If cond whenTrue (innerRest : whenFalse)

  where elifthenelse = do 
          _elif <- symbol "elif"
          cond <- expr
          whenTrue <- block
          rest <- many elifthenelse
          pure $ If cond whenTrue rest


expr :: Parser Expr
expr = undefined

block :: Parser [Expr]
block = undefined
