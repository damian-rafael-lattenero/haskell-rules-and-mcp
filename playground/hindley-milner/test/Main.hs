module Main where

import Test.QuickCheck (quickCheckResult, isSuccess)
import System.Exit (exitSuccess, exitFailure)

import Properties (allProperties)

main :: IO ()
main = do
  results <- mapM runOne allProperties
  if all isSuccess results
    then do
      putStrLn $ "\nAll " ++ show (length results) ++ " properties passed."
      exitSuccess
    else do
      putStrLn "\nSome properties failed!"
      exitFailure
  where
    runOne (name, prop) = do
      putStr $ name ++ ": "
      quickCheckResult prop
