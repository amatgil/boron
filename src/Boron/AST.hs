module Boron.AST where

type Name = String
type Block = [Expr]

-- AST proper
data Expr
  = LiteralBool Bool
  | LiteralNum Int
  | LiteralString String
  | LiteralTable [(Expr, Expr)]
  | LiteralTuple [Expr]
  | For Name Expr Block
  | If Expr Block Block
  | TableIndexInto Expr Expr
  | Assign Name Expr
  | Reassign Name Expr
  | Call Expr [Expr]
  | Var Name
  deriving (Show, Eq, Ord)
