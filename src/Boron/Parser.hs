module Boron.Parser where

import Text.Megaparsec
import Text.Megaparsec.Char
import Data.Functor

type Parser = Parsec Void String


boolParser :: Parser Expr 
boolParser = string "true" <|> string "false"




  
-- characters that 
isValidIdent :: String -> Bool 
isValidIdent ident = isAllAlphaNum ident || isAllPunct ident
    where isAllAlphaNum = undefined
          isAllPunct = undefined -- do not allow parens, brackets, the other brackets, commas, periods, semicolons, etc.
