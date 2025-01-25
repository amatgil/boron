module Main where

import Boron.AST
import Boron.Eval
import Boron.Parser
import Control.Monad.State.Lazy
import Text.Megaparsec
import Debug.Trace
import Data.Either

main :: IO ()
main = let
  input = "{ let y := 0; for x in (1, 2, 3, 4, 5) { y = +(x, y); }; y; }"
  in putStrLn $ case parseProgram input of
                  Left err -> err
                  Right ast -> show $ evalState (evalBlock ast) bareEnv

