module Boron.Parser where

import Boron.AST
import Data.Function
import Data.Functor
import Data.Void
import Text.Megaparsec
import Text.Megaparsec.Char
import Text.Megaparsec.Char.Lexer qualified as L

type Parser = Parsec Void String

keywords :: [String]
keywords = ["for", "while", "lambda", "let"]

parens :: Parser a -> Parser a
parens = between (symbol "(") (symbol ")")

brackets :: Parser a -> Parser a
brackets = between (symbol "[") (symbol "]")

curlies :: Parser a -> Parser a
curlies = between (symbol "{") (symbol "}")

commaSeparated :: Parser a -> Parser [a]
commaSeparated = flip sepEndBy $ symbol ","

literalBool :: Parser Expr
literalBool = do
  _ <- char '#'
  LiteralBool <$> choice [symbol "t" $> True, symbol "f" $> False]

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
    allSymbols = do
      inner <- some $ oneOf validPunct
      suffix <- many $ char '\''
      let result = inner ++ suffix
      if result `elem` keywords then fail "Is keyword" else pure result
    allAlphaNum = do
      prefix <- letterChar <|> char '_' -- must start with a letter or underscore
      inner <- many (alphaNumChar <|> char '_')
      suffix <- many $ char '\''
      let result = prefix : (inner ++ suffix)
      if result `elem` keywords then fail "Is keyword" else pure result

literalInt :: Parser Int
literalInt = lexeme $ choice [try asBin, try asHex, asDec]
  where
    asBin = string "0b" *> L.binary
    asHex = string "0x" *> L.hexadecimal
    asDec = L.signed space L.decimal

literalNumber :: Parser Expr
literalNumber = LiteralNum . toEnum <$> literalInt

literalString :: Parser Expr
literalString = LiteralString <$> lexeme (char '"' *> manyTill L.charLiteral (char '"'))

literalTable :: Parser Expr
literalTable = LiteralTable <$> curlies (commaSeparated pair)
  where
    pair = do
      lhs <- expr
      _c <- symbol ":"
      rhs <- expr
      pure (lhs, rhs)

literalTuple :: Parser Expr
literalTuple = lexeme $ LiteralTuple <$> parens (commaSeparated expr)

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

xthenelse :: String -> Parser Expr
xthenelse word = do
  _w <- symbol word
  cond <- expr
  whenTrue <- block
  innerRest <- many $ xthenelse "elif"
  _else <- symbol "else"
  whenFalse <- block
  pure $ If cond whenTrue (innerRest ++ whenFalse)

ifthenelse :: Parser Expr
ifthenelse = xthenelse "if"

assignment :: Parser Expr
assignment = do
  _let <- symbol "let"
  lhs <- identifier
  _op <- symbol ":="
  Assign lhs <$> expr

reassignment :: Parser Expr
reassignment = do
  lhs <- identifier
  _op <- symbol "="
  Reassign lhs <$> expr

var :: Parser Expr
var = Var <$> identifier

lambda :: Parser Expr
lambda = do
  _lambda <- symbol "lambda"
  names <- parens $ commaSeparated identifier
  LambdaE names <$> block

atom :: Parser Expr
atom =
  choice
    [ literalBool,
      try literalNumber,
      literalString,
      literalTuple,
      literalTable,
      lambda,
      for,
      while,
      ifthenelse,
      assignment,
      try reassignment,
      var
    ]

postfix :: Parser Expr
postfix = do
  lhs <- atom
  pf <-
    some $
      choice
        [ continueTableIndex,
          continueTupleIndex,
          continueCall
        ]
  pure $ foldl (&) lhs pf
  where
    continueTableIndex :: Parser (Expr -> Expr)
    continueTableIndex = do
      indexed <- brackets expr
      pure $ flip TableIndexInto indexed

    continueTupleIndex :: Parser (Expr -> Expr)
    continueTupleIndex = do
      _t <- char '.'
      flip TupleIndexInto <$> literalInt

    continueCall :: Parser (Expr -> Expr)
    continueCall = do
      args <- parens $ commaSeparated expr
      pure $ flip Call args

expr :: Parser Expr
expr = try postfix <|> atom

exprs :: Parser [Expr]
exprs = expr `sepEndBy` symbol ";"

block :: Parser [Expr]
block = curlies exprs

parseProgram :: String -> Either String [Expr]
parseProgram p = case parse (exprs <* eof) "" p of
  Right ast -> Right ast
  Left err -> Left $ errorBundlePretty err
