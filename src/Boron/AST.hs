module Boron.AST where

import Numeric.Natural

type Name = String

type Block = [Expr]

type Nat = Natural

-- AST proper
-- Numbers are floats (see https://www.lua.org/pil/2.3.html)
data Expr
  = LiteralBool Bool
  | LiteralNum Double
  | LiteralString String
  | LiteralTable [(Expr, Expr)]
  | LiteralTuple [Expr]
  | For Name Expr Block
  | While Expr Block
  | If Expr Block Block
  | TableIndexInto Expr Expr
  | TupleIndexInto Expr Int
  | Assign Name Expr
  | Reassign Name Expr
  | Call Expr [Expr]
  | LambdaE [Name] Block
  | Var Name
  deriving (Show, Eq, Ord)
