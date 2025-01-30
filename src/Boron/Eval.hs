{-# OPTIONS_GHC -w -Werror -Wincomplete-patterns #-}

module Boron.Eval where

import Boron.AST
import Control.Monad.State.Lazy
import Data.Fixed (mod')
import Data.Foldable
import Data.List.NonEmpty (NonEmpty ((:|)), (<|))
import Data.List.NonEmpty qualified as NE
import Data.Map qualified as M
import Text.Printf (printf)

type Scope = M.Map String Value

type Env = NE.NonEmpty Scope

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
  | Number Double
  | String String
  | Tuple [Value]
  | Table (M.Map Value Value) (Maybe Value) -- Map and optional default
  | Lambda [Name] Block -- argument names, body
  | BuiltIn BuiltIn
  deriving (Ord, Eq, Show)

unit :: Value
unit = Tuple []

eval :: Expr -> Interpreter Value
eval expr = case expr of
  LiteralBool b -> pure $ Bool b
  LiteralNum x -> pure $ Number x
  LiteralString s -> pure $ String s
  LiteralTuple tup -> Tuple <$> traverse eval tup
  LiteralTable vs -> flip Table Nothing . M.fromList <$> traverse (\(e1, e2) -> liftA2 (,) (eval e1) (eval e2)) vs
  Var name -> do
    env <- get
    ret <- getVar (NE.toList env) name
    pure $ case ret of
      Just val -> val
      Nothing -> error $ printf "Symbol (%s) does not exist in the current scope" name
  Assign name rhs -> do
    value <- eval rhs
    modify $ \(e :| es) -> M.insert name value e :| es
    pure unit
  Reassign name rhs -> do
    value <- eval rhs
    modify $ \env -> NE.fromList $ updateVar name value (NE.toList env)
    pure unit
  For var valuesExpr inner -> do
    values <- evalIterable valuesExpr

    modify (M.empty <|)
    traverse_ (\v -> evalBody var v inner) values
    modify $ NE.fromList . NE.tail

    pure unit
  While predicate inner -> do
    p <- eval predicate
    if p == Bool True
      then do
        evalBlock inner
        eval $ While predicate inner
      else
        if p == Bool False
          then pure unit
          else error "While predicate must be bool"
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
  TupleIndexInto _ _ -> error "unimplemented"
  LambdaE varNames body -> Lambda varNames body
  Call fExpr argsExpr -> do
    fnMaybe <- eval fExpr
    args <- traverse eval argsExpr

    case fnMaybe of
      Lambda names body -> do
        globalScope <- gets NE.last
        let paramScope = M.fromList $ zip names args

        let (ret, env') = runState (evalExprs body) (paramScope :| [globalScope])
        let globalScope' = NE.last env'

        modify (\env -> NE.fromList $ NE.init env ++ [globalScope'])

        pure ret
      BuiltIn b -> evalBuiltIn b args
      _else -> error "Cannot call a function that isn't a function"

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
evalExprs [] = pure unit
evalExprs [expr] = eval expr
evalExprs (expr : exprs) = eval expr *> evalExprs exprs

evalIterable :: Expr -> Interpreter [Value]
evalIterable e = do
  v <- eval e
  case v of
    Tuple tup -> pure tup
    Table t _ -> pure $ map snd $ M.toList t
    _other -> error "Cannot iterate over non-tuple/table"

getVar :: [Scope] -> Name -> Interpreter (Maybe Value)
getVar [] _ = pure Nothing
getVar (s : ss) name = case M.lookup name s of
  Just v -> pure $ Just v
  Nothing -> getVar ss name

updateVar :: Name -> Value -> [Scope] -> [Scope]
updateVar name _ [] = error $ printf "%s does not exist in this env" name
updateVar name rhs (e : es) =
  if M.member name e
    then M.update (\_ -> Just rhs) name e : es
    else e : updateVar name rhs es

-- ======== PRIMITIVES =========
bareEnv :: Env
bareEnv =
  NE.singleton $
    M.fromList
      [ ("print", BuiltIn Print),
        ("println", BuiltIn PrintLn),
        ("range", BuiltIn Range),
        ("+", BuiltIn $ Arithmetic Add),
        ("-", BuiltIn $ Arithmetic Sub),
        (">", BuiltIn $ Comparison GreaterThan),
        ("<", BuiltIn $ Comparison LesserThan),
        ("=", BuiltIn $ Comparison EqualTo)
      ]

data BuiltIn
  = Print
  | PrintLn
  | Arithmetic ArithOp
  | Comparison CompOp
  | Eval
  | Range
  deriving (Ord, Eq, Show)

data CompOp
  = GreaterThan
  | LesserThan
  | EqualTo
  deriving (Ord, Eq, Show)

data ArithOp
  = Add
  | Sub
  | Mul
  | Div
  | Rem
  deriving (Ord, Eq, Show)

evalBuiltIn :: BuiltIn -> [Value] -> Interpreter Value
evalBuiltIn b args = case b of
  Print -> error "unimplemented"
  PrintLn -> error "unimplemented"
  Arithmetic op -> pure . Number $ foldl1 (computeArithOp op) (map coerceToNum args)
  Comparison op -> pure . Bool $ and $ stencil (computeCompOp op) (map coerceToNum args)
  Eval -> case args of
    [String code] -> error "unimplemented"
    _other -> error "Can only evaluate one string"
  Range -> pure . Tuple $ case args of
    [start, end] -> values start 1 end
    [start, end, stepIn] -> values start (coerceToNum stepIn) end
    _other -> error "unimplemented"
    where
      values start s end = Number <$> enumFromThenTo (coerceToNum start) s (coerceToNum end - 1)

computeArithOp :: ArithOp -> Double -> Double -> Double
computeArithOp op a b = case op of
  Add -> a + b
  Sub -> a - b
  Mul -> a * b
  Div -> a / b
  Rem -> a `mod'` b

computeCompOp :: CompOp -> Double -> Double -> Bool
computeCompOp op a b = case op of
  GreaterThan -> a > b
  LesserThan -> a < b
  EqualTo -> a == b

coerceToNum :: Value -> Double
coerceToNum (Bool b) = if b then 1.0 else 0.0
coerceToNum (Number x) = x
coerceToNum _ = error "Could not coerce to number" -- TODO: Return Maybe

coerceToBool :: Value -> Bool
coerceToBool (Bool b) = b
coerceToBool _ = error "Could not coerce to bool" -- TODO: Return Maybe

-- OVERLAPPING pairs
stencil :: (a -> a -> b) -> [a] -> [b]
stencil f [] = []
stencil f [x] = []
stencil f (x : y : xs) = f x y : stencil f (y : xs)
