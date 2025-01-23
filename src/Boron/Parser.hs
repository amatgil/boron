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

name :: Parser Name
name = do
  prefix <- letterChar <|> oneOf validPunct
  inner  <- many $ alphaNumChar <|> oneOf validPunct
  suffix <- many $ char '\''
  pure $ prefix : (inner ++ suffix)
  where validPunct = "!#$%&/=*+-<>?@^_~"


-- characters that 
isValidCharInIdent :: Char -> Bool 
isValidCharInIdent c = isAllAlphaNum ident || isAllPunct ident
    where isAllAlphaNum = undefined
          isAllPunct = undefined -- do not allow parens, brackets, the other brackets, commas, periods, semicolons, etc.
