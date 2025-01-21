module Boron.Eval where
import Boron.AST

import qualified Data.Map as M
import Control.Monad.State.Lazy

data Env = Env [M.Map String Value]

data Closure = Closure
  { params :: [Name],
    body :: [Expr],
    env :: Env
  }

type Interpreter = StateT Env IO

-- What an Expr reduces down to
data Value
  = Bool Bool
  | Int Int
  | Float Float
  | String String
  | Tuple [Value]
  | Table (M.Map Value Value)
  | Lambda Closure
  

eval :: Expr -> Env -> Interpreter Value
eval expr env = case expr of
  LiteralBool b -> (Bool b, env)
  LiteralNum n -> (Int n, env)
  LiteralString s -> (String s, env)
  LiteralTuple tup -> (Tuple undefined, undefined)
  LiteralTable vs -> (Table $ M.fromList vs, env)
  For var values inner -> undefined
  If cond whenTrue whenFalse -> undefined
  TableIndexInto maybe_t index -> undefined
  Assign name value -> undefined
  Call f args -> undefined

  
