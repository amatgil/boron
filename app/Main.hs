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
fibProper = "{ let f := lambda (x) { if #t { 1 } else { 3 } }; f(6) }"

-- fibProper :: String
-- fibProper = "{ let f := lambda (n) { 7 }; }"

--  +(-(n, 1), -(n, 2))

main :: IO ()
main = putStrLn $ case parseProgram fibProper of
  Left err -> err
  Right ast -> show $ evalState (evalBlock ast) bareEnv

-- \        if <(x, 2) {                \n\
-- \          1                         \n\
-- \        }                           \n\
-- \        else {                      \n\
-- \            +(fib(-(n, 1), -(n, 2)) \n\
-- \        }                           \n\
-- \    )};                             \n\
