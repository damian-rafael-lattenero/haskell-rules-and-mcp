module Main where

import Test.QuickCheck
import TestLib (add)

main :: IO ()
main = quickCheck (\x y -> add x y == x + (y :: Int))
