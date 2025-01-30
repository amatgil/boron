module Main where

import Boron.Eval
import Boron.Parser
import Control.Monad.State.Lazy

input :: String
input = "{ let y := 0; for x in (1, 2, 3, 4, 5) { y = +(x, y); }; y; }"

fib :: String
fib = "{ let a := 0; let b := 1; let c := 0; for i in (0, 1, 2, 3, 4, 5, 6, 7, 8, 9, 10) {c = +(a, b); a = b; b = c;}; b;}"

fibRange :: String
fibRange = "{ let nth := 3; let a := 0; let b := 1; let c := 0; for i in range(0, -(nth, 1)) {c = +(a, b); a = b; b = c;}; b;}"

forloop :: String
forloop = "{ let a := 0; let b := 1; let c := 0; for i in (1, 2, 3) { }; b; } "

fizzbuzz :: String
fizzbuzz = "{ let t := {}; t[7] = 52; } "

fntest :: String
fntest = "{ let f := lambda (x) { +(x, 1) }; f(6) }"

fibProper :: String
fibProper = "let fib := lambda (n) { if <(n, 2) { 1 } else { +(fib(-(n, 1)), fib(-(n, 2))) } }; for i in range(0, 10) { println(fib(i))} "

main :: IO ()
main = case parseProgram fibProper of
  Left err -> putStrLn err
  Right ast -> putStrLn "" <* evalStateT (evalExprs ast) bareEnv
