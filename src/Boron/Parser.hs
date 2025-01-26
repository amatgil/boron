module Boron.Parser where

import Boron.Eval
import Boron.AST
import Text.Megaparsec
import Text.Megaparsec.Char
import Text.Megaparsec.Char.Lexer qualified as L
import Data.Functor
import Data.Void
import Data.Function
import qualified Data.Set as Set

type Parser = Parsec Void String

parens :: Parser a -> Parser a
parens = between (symbol "(") (symbol ")")

brackets :: Parser a -> Parser a
brackets = between (symbol "[") (symbol "]")

curlies:: Parser a -> Parser a
curlies = between (symbol "{") (symbol "}")

commaSeparated :: Parser a -> Parser [a]
commaSeparated = flip sepEndBy $ symbol ","


boolParser :: Parser Expr 
boolParser = LiteralBool <$> choice
                            [ try $ symbol "#t" $> True
                            , try $ symbol "#f" $> False]

-- Identifiers and numbers such
lexeme :: Parser a -> Parser a
lexeme = L.lexeme hspace

-- Verbatim strings
symbol :: String -> Parser String
symbol = L.symbol space

identifier :: Parser String
identifier = lexeme $ allSymbols <|> allAlphaNum
  where
    validPunct = "!#$%&/=*+-<>?@^_~"
    keywords = ["for", "while"]
    allSymbols = do
      inner <- some $ oneOf validPunct
      suffix <- many $ char '\''
      let result = inner ++ suffix
      if result `elem` keywords then fail "Is keyword" else pure result
    allAlphaNum = do
      prefix <- letterChar <|> char '_' -- must start with a letter or underscore
      inner  <- many (alphaNumChar <|> char '_')
      suffix <- many $ char '\''
      let result = prefix : (inner ++ suffix)
      if result `elem` keywords then fail "Is keyword" else pure result

literalInt :: Parser Int
literalInt = choice [try asDec, try asBin, try asHex]
  where asBin = string "0b" *> L.binary
        asHex = string "0x" *> L.hexadecimal
        asDec = L.decimal

literalNumber :: Parser Expr
literalNumber = LiteralNum . toEnum <$> literalInt

literalString :: Parser Expr
literalString = LiteralString <$> lexeme (char '"' *> manyTill L.charLiteral (char '"'))

literalTable :: Parser Expr
literalTable = LiteralTable <$> curlies (commaSeparated pair)
  where pair = do
          lhs <- expr
          _c <- symbol ":"
          rhs <- expr
          pure (lhs, rhs)

literalTuple :: Parser Expr
literalTuple = LiteralTuple <$> parens (commaSeparated expr)

for :: Parser Expr
for = do
  _f <- symbol "for"
  iterVar <- identifier
  _in <- symbol "in"
  iterVals <- expr
  For iterVar iterVals <$> block

while :: Parser Expr
while = do
  _w <- symbol "while"
  predicate <- expr
  While predicate <$> block


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

assignment :: Parser Expr
assignment = do
  _let <- symbol "let"
  lhs <- identifier
  _op <- symbol ":="
  rhs <- expr
  pure $ Assign lhs rhs 
  
reassignment :: Parser Expr
reassignment = do
  lhs <- identifier
  _op <- symbol "="
  rhs <- expr
  pure $ Reassign lhs rhs 

var :: Parser Expr
var = Var <$> identifier

atom :: Parser Expr
atom = choice $ try <$>
       [ boolParser
       , literalNumber
       , literalString
       , literalTuple 
       , literalTable
       , for
       , while
       , ifthenelse
       , assignment
       , reassignment
       , var
       ]

  
postfix :: Parser Expr
postfix = do
  lhs <- atom
  pf <- some $ choice [continueTableIndex
                      , continueTupleIndex
                      , continueCall]
  pure $ foldl (&) lhs pf
  
  where
    continueTableIndex :: Parser (Expr -> Expr)
    continueTableIndex = do
      indexed <- brackets expr
      pure $ flip TableIndexInto indexed

    continueTupleIndex :: Parser (Expr -> Expr)
    continueTupleIndex = do
      _t <- char '.' 
      index <- literalInt
      pure $ flip TupleIndexInto index

    continueCall :: Parser (Expr -> Expr)
    continueCall = do
      args <- parens $ commaSeparated expr
      pure $ flip Call args
      
      
  
expr :: Parser Expr
expr = try postfix <|> atom

block :: Parser [Expr]
block = curlies $ expr `sepEndBy` symbol ";"

--parseProgram :: String -> Either (Par [Expr]

parseProgram :: String -> Either String [Expr]
parseProgram p =  case parse block "" p of
  Right ast -> Right ast
  Left err -> Left $ errorBundlePretty err
