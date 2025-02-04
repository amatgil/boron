module Main where

import Boron.Eval
import Boron.Parser
import Control.Monad.State.Lazy
import Control.Monad.Except

input :: String
input = "let y := 0; for x in (1, 2, 3, 4, 5) { y = +(x, y); }; y;"

fib :: String
fib = "let a := 0; let b := 1; let c := 0; for i in (0, 1, 2, 3, 4, 5, 6, 7, 8, 9, 10) {c = +(a, b); a = b; b = c;}; b"

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

tableTest :: String
tableTest = "let t := {1: 7, 2:42, \"hi\":420, lambda (n) { n }:\"yep!\", lambda (n) {n}:10, 8:inf }; println(/(-1,0));"

main :: IO ()
main = case parseProgram fibProper of
  Left err -> putStrLn err
  Right ast -> do
    ret <- runExceptT $ runStateT (evalExprs ast) bareEnv 
    case ret of
      Left except -> print $ "EXCEPTION: " ++ except
      Right val -> print $  "Result: " ++ stringify (fst val)

