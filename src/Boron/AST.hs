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
  | If Expr Block Block
  | TableIndexInto Expr Expr
  | TupleIndexInto Expr Nat
  | Assign Name Expr
  | Reassign Name Expr
  | Call Expr [Expr]
  | Var Name
  deriving (Show, Eq, Ord)
