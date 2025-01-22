module Boron.AST where
import Numeric.Natural

type Name = String
type Block = [Expr]
type Nat = Natural

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
  | TupleIndexInto Expr Nat
  | Assign Name Expr
  | Reassign Name Expr
  | Call Expr [Expr]
  | Var Name
  deriving (Show, Eq, Ord)
