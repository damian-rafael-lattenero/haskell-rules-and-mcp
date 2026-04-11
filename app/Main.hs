module Main where

import Lib (greet)

main :: IO ()
main = putStrLn (greet "World")
