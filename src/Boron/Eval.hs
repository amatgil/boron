module Boron.Eval where
import Boron.AST

import qualified Data.Map as M
import qualified Data.List.NonEmpty as NE
import Data.Maybe
import Data.List.NonEmpty( NonEmpty( (:|) ), (<|) )
import Data.Traversable
import Data.Foldable
 
import Control.Monad.State.Lazy

type Scope = M.Map String Value
type Env = NE.NonEmpty Scope

bareEnv :: Env
bareEnv = NE.singleton $ M.fromList [ ("print", BuiltIn Print)
                                    , ("println", BuiltIn PrintLn)
                                    ]

data BuiltIn
    = Print
    | PrintLn
  deriving (Ord, Eq, Show)

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
  | Table (M.Map Value Value) (Maybe Value) -- Map and optional default
  | Lambda [Name] Block                     -- argument names, body
  | BuiltIn BuiltIn
  deriving (Ord, Eq, Show)
  
unit :: Value
unit = Tuple []

eval :: Expr -> Interpreter Value
eval expr = case expr of
  LiteralBool b -> pure $ Bool b
  LiteralNum n -> pure $ Int n
  LiteralString s -> pure $ String s
  LiteralTuple tup -> Tuple <$> traverse eval tup
  LiteralTable vs -> flip Table Nothing  . M.fromList <$> traverse (\(e1, e2) -> liftA2 (,) (eval e1) (eval e2)) vs
  Var name -> do
    env <- get
    ret <- getVar (NE.toList env) name
    pure $ case ret of
      Just val -> val
      Nothing -> error "Symbol does not exist in the current scope"
    
  Assign name rhs -> do
    value <- eval rhs
    modify $ \(e :| es) -> M.insert name value e :| es
    pure unit 
  For var valuesExpr inner -> do
    values <- evalIterable valuesExpr

    modify (M.empty <|)
    traverse_ (\v -> evalBody var v inner) values
    modify $ NE.fromList . NE.tail

    pure unit

  If condExpr whenTrue whenFalse -> do
    cond <- eval condExpr
    if cond == Bool True
      then evalBlock whenTrue
      else evalBlock whenFalse

  TableIndexInto maybeTExpr keyExpr -> do
    evaluated <- eval maybeTExpr 
    key <- eval keyExpr
    case evaluated of 
      Table t dflt -> pure $ case (M.lookup key t, dflt) of
        (Just val, _) -> val
        (Nothing, Just d) -> d
        (Nothing, Nothing) -> error "Key not found in table"
      _else -> error "*explosion noises*"
    
  Call fExpr argsExpr -> do
    fnMaybe <- eval fExpr
    args <- traverse eval argsExpr

    let (names, body) = (case fnMaybe of
                Lambda n b -> (n, b)
                _else -> error "Cannot call a function that isn't a function")

    globalScope <- gets NE.last
    let paramScope = M.fromList $ zip names args

    let (ret, env') = runState (evalExprs body) (paramScope :| [globalScope]) 
    let globalScope' = NE.last env'

    modify (\env -> NE.fromList $ NE.init env ++ [globalScope'])

    pure ret

  
evalBody :: Name -> Value -> Block -> Interpreter Value
evalBody name value block = do
  modify $ \(e :| es) -> M.insert name value e :| es
  _returned <- evalBlock block
  modify $ \(e :| es) -> M.delete name e :| es
  pure unit
  
evalBlock :: Block -> Interpreter Value
evalBlock block = do
  modify (M.empty <|)
  retVal <- evalExprs block
  modify $ NE.fromList . NE.tail
  pure retVal


evalExprs :: [Expr] -> Interpreter Value
evalExprs []           = pure unit
evalExprs [expr]       = eval expr
evalExprs (expr:exprs) = eval expr *> evalExprs exprs

evalIterable :: Expr -> Interpreter [Value]
evalIterable e = do
  v <- eval e
  case v of
    Tuple tup -> pure tup
    Table t _ -> pure $ map snd $ M.toList t
    _other -> error "Cannot iterate over non-tuple/table"
  

getVar :: [Scope] -> Name -> Interpreter (Maybe Value)
getVar [] _ = pure Nothing
getVar (s:ss) name = case M.lookup name s of
  Just v -> pure $ Just v
  Nothing -> getVar ss name
