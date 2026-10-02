module Main (main) where

import SpikeLib (answer)
import Test.QuickCheck

prop_double_nonneg :: Int -> Bool
prop_double_nonneg x = double' x >= 0
  where
    double' y = y * 2

main :: IO ()
main = do
  quickCheck prop_double_nonneg
  putStrLn ("answer = " ++ show answer)
