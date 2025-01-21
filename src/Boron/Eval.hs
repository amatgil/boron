module Boron.Eval where
import Boron.AST

import qualified Data.Map as M

data Env = IDunnoYet

data Closure = Closure
  { params :: [Name],
    body :: [Expr],
    env :: Env
  }

-- What an Expr reduces down to
data Value
  = Bool Bool
  | Int Int
  | Float Float
  | String String
  | Table (M.Map Value Value)
  | Lambda Closure
  
