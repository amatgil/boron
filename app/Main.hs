module Main where

import Boron.AST
import Boron.Eval
import Boron.Parser
import Control.Monad.State.Lazy
import Text.Megaparsec
import Debug.Trace
import Data.Either

-- evals:
-- y := 0
-- for x in 1..5 {
--   y += x
-- }
-- y

--main :: IO ()
--main = let thing = [
--             (Assign "y" (LiteralNum 0.0)),
--             (For "x" (LiteralTuple (map LiteralNum [1, 2, 3, 4])) [
--                 Reassign "y" (Call (Var "+") [Var "y", Var "x"])
--                                                  ]),
--             (Var "y")
--             ]
--  in print $ runState (evalExprs thing) bareEnv
        

-- evals:
-- y := 10
-- a := 10
-- while >(y, 7) {
--   y = -(y, 1)
--   a = +(a, 1)
-- }
-- (y, a)
         --  \  for x in range(1, 5) {\
         --  \    y = +(x, y);\
         --  \  }\

main :: IO ()
main = let
  input = "{ let y := 2; }"
  in putStrLn $ case parseProgram input of
                  Left err -> err
                  Right ast -> show ast

-- main :: IO ()
-- main = print $ parseMaybe literalTable "{ \"a\": 7}"

-- main :: IO ()
-- main = let thing = [
--              Assign "y" (LiteralNum 10.0),
--              Assign "a" (LiteralNum 0.0),
--              While (Call (Var ">") [Var "y", LiteralNum 7.0]) [
--                  Reassign "y" (Call (Var "-") [Var "y", LiteralNum 1.0]),
--                  Reassign "a" (Call (Var "+") [Var "a", LiteralNum 1.0])
--                  ],
--              LiteralTuple [Var "y", Var "a"]
--              ]
--   in print $ runState (evalExprs thing) bareEnv
        
