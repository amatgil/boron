module Boron.Parser where

import Boron.Eval
import Boron.AST
import Text.Megaparsec
import Text.Megaparsec.Char
import Text.Megaparsec.Char.Lexer qualified as L
import Data.Functor
import Data.Void

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
      inner  <- many (alphaNumChar <|> char '_')
      suffix <- many $ char '\''
      pure $ prefix : (inner ++ suffix)

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
literalTable = LiteralTable <$> (curlies $ commaSeparated pair)
  where pair = do
          lhs <- expr
          _c <- symbol ":"
          rhs <- expr
          pure (lhs, rhs)

literalTuple :: Parser Expr
literalTuple = LiteralTuple <$> (parens $ commaSeparated expr)

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
  predicate <- expr
  inner <- block
  pure $ While predicate inner

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

tableindex :: Parser Expr
tableindex = do
  table <- expr
  index <- brackets expr
  pure $ TableIndexInto table index

tupleindex :: Parser Expr
tupleindex = do
  tup <- expr
  _index <- char '.'
  pos <- literalInt
  pure $ TupleIndexInto tup pos
  
assignment :: Parser Expr
assignment = do
  lhs <- identifier
  _op <- symbol "="
  rhs <- expr
  pure $ Assign lhs rhs 
  
reassignment :: Parser Expr
reassignment = do
  _let <- symbol "let"
  lhs <- identifier
  _op <- symbol ":="
  rhs <- expr
  pure $ Reassign lhs rhs 

call :: Parser Expr
call = do
  name <- expr
  args <- parens $ commaSeparated expr
  pure $ Call name args

var :: Parser Expr
var = Var <$> identifier

expr :: Parser Expr
expr = choice
       [ try boolParser
       , try literalNumber
       , try literalString
       , try literalTuple 
       , try literalTable
       , try for
       , try while
       , try ifthenelse
       , try tableindex
       , try tupleindex
       , try assignment
       , try reassignment
       , try var
       -- , try call
       ]

block :: Parser [Expr]
block = curlies $ expr `sepEndBy` symbol ";"

parseProgram :: String -> Maybe [Expr]
parseProgram = parseMaybe block
