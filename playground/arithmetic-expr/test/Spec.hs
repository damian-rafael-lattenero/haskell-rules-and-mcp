-- | Scaffolded test suite. Prints PASS/FAIL per case, exits 1 on
-- any failure. Add tests here and extend with QuickCheck / hspec
-- as the project grows.
module Main where

import ArithmeticExpr (greet)
import System.Exit (exitFailure, exitSuccess)

main :: IO ()
main = do
  let ok = greet "world" == "Hello, world!"
  putStrLn (if ok then "PASS  greet world" else "FAIL  greet world")
  if ok then exitSuccess else exitFailure
