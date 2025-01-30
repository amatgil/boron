module Main where

import Boron.Eval
import Boron.Parser
import Control.Monad.State.Lazy

main :: IO ()
main = let
  input = "{ let y := 0; for x in (1, 2, 3, 4, 5) { y = +(x, y); }; y; }"
  fib = "{ let a := 0; let b := 1; let c := 0; for i in (0, 1, 2, 3, 4, 5, 6, 7, 8, 9, 10) {c = +(a, b); a = b; b = c;}; b;}"
  fib_range = "{ let nth := 3; let a := 0; let b := 1; let c := 0; for i in range(0, -(nth, 1)) {c = +(a, b); a = b; b = c;}; b;}"
  forloop = "{ let a := 0; let b := 1; let c := 0; for i in (1, 2, 3) { }; b; } "
  fizzbuzz = "{ let t := {}; t[7] = 52; } "
  in putStrLn $ case parseProgram fizzbuzz of
                  Left err -> err
                  Right ast -> show $ evalState (evalBlock ast) bareEnv
