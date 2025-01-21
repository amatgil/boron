module Boron.AST where

import qualified Data.Map as M

data Clojure = IDunno

type Name = String
type Block = [Expr]

-- What an Expr reduces down to
data Value
  = Int Int
  | Float Float
  | Lambda Clojure

-- AST proper
data Expr
  = LiteralNum Int
  | LiteralString String
  | LiteralTable [(Expr, Expr)]
  | LiteralTuple [Expr]
  | For Name Expr Block
  | If Expr Block Block
  | TableIndexInto Expr Expr
  | Assign Name Expr
  | Call Expr [Expr]
