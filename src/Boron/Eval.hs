{-# LANGUAGE LambdaCase #-}
{-# OPTIONS_GHC -w -Werror -Wincomplete-patterns #-}

module Boron.Eval where

import Boron.AST
import Control.Monad.State.Lazy
import Control.Monad.Except
import Data.Maybe
import Data.Fixed (mod')
import Data.Foldable
import Data.List
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

type Interpreter = StateT Env (ExceptT String IO)

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
    getVar (NE.toList env) name
  Assign name rhs -> do
    value <- eval rhs
    modify $ \(e :| es) -> M.insert name value e :| es
    pure unit
  Reassign name rhs -> do
    value <- eval rhs
    env <- get 
    -- This error comes from that 'newEnv' must be [Scope] but i'm trying to make it be Interpereter by returning throwError
    let newEnv = case updateVar name value (NE.toList env) of 
            Just newEnv -> newEnv
            Nothing -> throwError $ printf "Could not reassign: '%s' does not exist" name
    put $ NE.fromList newEnv
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
          else throwError $ printf "While predicate must be bool, but it was %s" $ stringify p
  If condExpr whenTrue whenFalse -> do
    cond <- eval condExpr
    if cond == Bool True
      then evalBlock whenTrue
      else evalBlock whenFalse
  TableIndexInto maybeTExpr keyExpr -> do
    evaluated <- eval maybeTExpr
    key <- eval keyExpr
    case evaluated of
      Table t dflt -> case (M.lookup key t, dflt) of
        (Just val, _) -> pure val
        (Nothing, Just d) -> pure d
        (Nothing, Nothing) -> throwError $ printf "Key '%s' not found in table that has no default" $ stringify key
      _else -> throwError "Cannot use table indexing into non-table"
  TupleIndexInto tup index -> do
    t <- eval tup
    case t of
      Tuple vals -> pure $ vals !! index -- TODO: make this `throwError` instead of panicking
      _other -> throwError "Cannot index value that isn't tuple as tuple"
  LambdaE varNames body -> pure $ Lambda varNames body
  Call fExpr argsExpr -> do
    fnMaybe <- eval fExpr
    args <- traverse eval argsExpr

    case fnMaybe of
      Lambda names body -> do
        globalScope <- gets NE.last
        let paramScope = M.fromList $ zip names args

        (ret, env') <-  lift $ runStateT (evalExprs body) (paramScope :| [globalScope])
        let globalScope' = NE.last env'

        modify (\env -> NE.fromList $ NE.init env ++ [globalScope'])

        pure ret
      BuiltIn b -> evalBuiltIn b args
      other -> throwError $ printf "Cannot call %s as a function" $ stringify other

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
    other -> throwError $ printf "Cannot iterate over non-tuple/table: %s" $ stringify other

getVar :: [Scope] -> Name -> Interpreter Value
getVar [] name = throwError $ printf "Symbol (%s) does not exist in the current scope" name
getVar (s : ss) name = case M.lookup name s of
  Just v -> pure v
  Nothing -> getVar ss name

updateVar :: Name -> Value -> [Scope] -> Maybe [Scope]
updateVar name _ [] = Nothing -- throwError $ printf "%s does not exist in this env" name
updateVar name rhs (e : es) =
  if M.member name e
    then Just $ M.update (\_ -> Just rhs) name e : es
    else updateVar name rhs es >>= \updated -> Just $ e : updated

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
        ("*", BuiltIn $ Arithmetic Mul),
        ("/", BuiltIn $ Arithmetic Div),
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
  Print -> builtinPrint args
  PrintLn -> builtinPrintLn args
  Arithmetic op -> case mapM coerceToNum args of
                        Just nums -> pure . Number $ foldl1 (computeArithOp op) nums
                        Nothing -> throwError "Could not arithmetic over non-number"
  Comparison op -> case mapM coerceToNum args of
                        Just nums -> pure . Bool $ and $ stencil (computeCompOp op) nums
                        Nothing -> throwError "Could not boolean over non-bools"
    
  Eval -> case args of
    [String code] -> error "unimplemented"
    other -> throwError "`eval` can only evaluate one string"
  Range -> case args of
    [start, end] -> valuesNoStep start end
    [start, end, stepIn] -> valuesWithStep start stepIn end
    _other -> throwError "`range` must be called with two or three arguments: (start, end) or (start, end, step)"
    where
      values from over to = Number <$> enumFromThenTo from over to
      valuesNoStep s e = case (coerceToNum s, coerceToNum e) of 
                       (Just startV, Just endV) -> pure . Tuple $ values startV 1 endV
                       _other -> throwError "Range must be called with numbers" -- TODO: improve message
      valuesWithStep s st e = case (coerceToNum s, coerceToNum st, coerceToNum e) of 
                       (Just startV, Just stepV, Just endV) -> pure . Tuple $ values startV stepV endV
                       _other -> throwError "Range must be called with numbers" -- TODO: improve message
        

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

coerceToNum :: Value -> Maybe Double
coerceToNum (Bool b) = Just $ if b then 1.0 else 0.0
coerceToNum (Number x) = Just x
coerceToNum x = Nothing

coerceToBool :: Value -> Maybe Bool
coerceToBool (Bool b) = Just b
coerceToBool x = Nothing

-- OVERLAPPING pairs
stencil :: (a -> a -> b) -> [a] -> [b]
stencil f [] = []
stencil f [x] = []
stencil f (x : y : xs) = f x y : stencil f (y : xs)

stringify :: Value -> String
stringify = \case
  Bool True -> "#t"
  Bool False -> "#f"
  Number n -> if toEnum (round n) == n then show $ fromEnum n else show n
  String s -> "\"" ++ s ++ "\""
  Tuple vals -> "(" ++ intercalate ", " (map stringify vals) ++ ")"
  Lambda _ _ -> "<lambda expr>"
  BuiltIn b -> "<builtin fn>"
  Table pairs dflt -> "{" ++ intercalate ", " prettypairs ++ "}"
    where
      prettypairs =
        map
          (\(k, v) -> stringify k ++ " -> " ++ stringify v)
          $ M.toList pairs

builtinPrint :: [Value] -> Interpreter Value
builtinPrint args = unit <$ liftIO (traverse (putStr . stringify) args)

builtinPrintLn :: [Value] -> Interpreter Value
builtinPrintLn args = unit <$ liftIO (traverse (putStrLn . stringify) args)
