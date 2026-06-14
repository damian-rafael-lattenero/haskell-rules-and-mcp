-- | Total replacements for the partial Prelude functions
-- (head, last, init, tail, !!, fromJust).
--
-- Use these at every site that would otherwise throw on empty input.
-- 'safeAt' is the primary workhorse; 'initLast' covers the common
-- pattern of splitting a known-nonempty list into its init and last
-- element (e.g. for parsing argument lists).
module HaskellFlows.Util.Safe
  ( safeAt
  , safeHead
  , safeLast
  , initLast
  ) where

import Data.List (unsnoc)
import Data.Maybe (listToMaybe)

-- | Total version of @(xs !! i)@. Returns @Nothing@ for negative or
-- out-of-bounds indices; never throws.
safeAt :: Int -> [a] -> Maybe a
safeAt i xs
  | i < 0    = Nothing
  | otherwise = listToMaybe (drop i xs)

-- | Total version of @head@. Returns @Nothing@ for an empty list.
safeHead :: [a] -> Maybe a
safeHead = listToMaybe

-- | Total version of @last@. Returns @Nothing@ for an empty list.
safeLast :: [a] -> Maybe a
safeLast = fmap snd . unsnoc

-- | Split a list into its init and last element. Returns @Nothing@
-- for an empty list. Equivalent to @(init xs, last xs)@ but total.
--
-- Example: @initLast [1,2,3] == Just ([1,2], 3)@
initLast :: [a] -> Maybe ([a], a)
initLast = unsnoc
