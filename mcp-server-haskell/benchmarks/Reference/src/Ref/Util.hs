-- | General utility functions for the reference project.
module Ref.Util
  ( chunksOf
  , groupBy'
  , deduplicate
  , safeHead
  , safeLast
  ) where

import qualified Data.Map.Strict as Map

-- | Split a list into chunks of at most @n@ elements.
chunksOf :: Int -> [a] -> [[a]]
chunksOf _ [] = []
chunksOf n xs = take n xs : chunksOf n (drop n xs)

-- | Group a list of pairs into a map, collecting values under each key.
-- Ordering within each value list matches the original list.
groupBy' :: Ord k => [(k, v)] -> Map.Map k [v]
groupBy' = foldr (\(k, v) acc -> Map.insertWith (++) k [v] acc) Map.empty

-- | Remove duplicate elements, keeping the first occurrence.
-- O(n²) — suitable for small lists.
deduplicate :: Eq a => [a] -> [a]
deduplicate = foldr (\x acc -> if x `elem` acc then acc else x : acc) []

-- | Safe head: returns 'Nothing' for empty list.
safeHead :: [a] -> Maybe a
safeHead []    = Nothing
safeHead (x:_) = Just x

-- | Safe last: returns 'Nothing' for empty list.
safeLast :: [a] -> Maybe a
safeLast []  = Nothing
safeLast [x] = Just x
safeLast (_:xs) = safeLast xs
