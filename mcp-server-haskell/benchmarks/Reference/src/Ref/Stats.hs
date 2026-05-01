-- | Basic statistics for the reference project.
module Ref.Stats
  ( mean
  , median
  , stdDev
  , scoreHistogram
  ) where

import Data.List (sort)
import qualified Data.Map.Strict as Map
import Data.Map.Strict (Map)

import Ref.Core (Score (..), unScore)
import Ref.Types (Item (..))

-- | Arithmetic mean of a list.  Returns 0 for empty list.
mean :: [Double] -> Double
mean [] = 0
mean xs = sum xs / fromIntegral (length xs)

-- | Median of a list.  Returns 0 for empty list.
-- Uses lower-median for even-length lists.
median :: [Double] -> Double
median [] = 0
median xs =
  let sorted = sort xs
      n      = length sorted
      mid    = n `div` 2
  in if odd n
       then sorted !! mid
       else (sorted !! (mid - 1) + sorted !! mid) / 2.0

-- | Population standard deviation.  Returns 0 for empty or singleton list.
stdDev :: [Double] -> Double
stdDev [] = 0
stdDev xs =
  let m  = mean xs
      sq = map (\x -> (x - m) ^ (2 :: Int)) xs
  in sqrt (sum sq / fromIntegral (length xs))

-- | Bucket item scores into a histogram with the given bucket width.
-- Keys are the lower bound of each bucket (e.g. 0, 10, 20 … for width=10).
scoreHistogram :: Double -> [Item] -> Map Double Int
scoreHistogram width =
  foldr (\i acc ->
    let bucket = fromIntegral (floor (unScore (itemScore i) / width) :: Int) * width
    in Map.insertWith (+) bucket 1 acc
  ) Map.empty
