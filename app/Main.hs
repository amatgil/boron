module Main where

import Boron.AST
import Boron.Eval
import Control.Monad.State.Lazy

-- evals:
-- y := 0
-- for x in 1..5 {
--   y += x
-- }
-- y

main :: IO ()
main = let thing = [
             (Assign "y" (LiteralNum 0.0)),
             (For "x" (LiteralTuple (map LiteralNum [1, 2, 3, 4])) [
                 Reassign "y" (Call (Var "+") [Var "y", Var "x"])
                                                  ]),
             (Var "y")
             ]
  in print $ runState (evalExprs thing) bareEnv
        
