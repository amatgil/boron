module Boron.Eval where
import Boron.AST

import qualified Data.Map as M
import qualified Data.List.NonEmpty as NE
import Data.List.NonEmpty( NonEmpty( (:|) ) )


 

import Control.Monad.State.Lazy
import Data.Functor.Classes (eq1)
import qualified Data.List.NonEmpty as M

type Env = NE.NonEmpty (M.Map String Value)

data Closure = Closure
  { params :: [Name],
    body :: [Expr],
    env :: Env
  }
  deriving (Ord, Eq, Show)

type Interpreter = State Env

-- What an Expr reduces down to
data Value
  = Bool Bool
  | Int Int
  | Float Float
  | String String
  | Tuple [Value]
  | Table (M.Map Value Value)
  | Lambda Closure
  deriving (Ord, Eq, Show)
  

eval :: Expr -> Interpreter Value
eval expr = case expr of
  LiteralBool b -> pure $ Bool b
  LiteralNum n -> pure $ Int n
  LiteralString s -> pure $ String s
  LiteralTuple tup -> Tuple <$> traverse eval tup
  LiteralTable vs -> Table . M.fromList <$> traverse (\(e1, e2) -> liftA2 (,) (eval e1) (eval e2)) vs
  Assign name rhs -> do
    value <- eval rhs
    modify $ \(e :| es) -> M.insert name value e :| es
    pure $ Tuple []
  For var values inner -> undefined
  If condExpr whenTrue whenFalse -> do
    cond <- eval condExpr
    if cond == Bool True then evalBlock whenTrue else evalBlock whenFalse

  TableIndexInto maybe_t index -> undefined
  Call f args -> undefined

 
evalBlock :: Block -> Interpreter Value
evalBlock block = do
  modify (M.empty <|)
  retVal <- evalExprs block
  modify $ \(e:|es) -> NE.fromList es
  pure retVal


evalExprs :: [Expr] -> Interpreter Value
evalExprs []           = pure $ Tuple []
evalExprs [expr]       = eval expr
evalExprs (expr:exprs) = eval expr *> evalExprs exprs
