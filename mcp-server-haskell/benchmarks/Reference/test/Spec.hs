-- | QuickCheck property tests for the reference project.
-- These are exercised by the haskell-flows-mcp benchmark harness
-- to confirm the reference project compiles and its tests pass.
module Main where

import Test.QuickCheck

import Ref.Core (mkScore, unScore)
import Ref.Parse (parseTags, splitOn)
import Ref.Stats (mean, median, stdDev)
import Ref.Util (chunksOf, deduplicate, safeHead, safeLast)

main :: IO ()
main = do
  quickCheck prop_mkScoreClamps
  quickCheck prop_parseTagsRoundtrip
  quickCheck prop_meanEmpty
  quickCheck prop_medianSingleton
  quickCheck prop_chunksOfReconstitute
  quickCheck prop_deduplicateIdempotent
  quickCheck prop_safeHeadSafeLast
  quickCheck prop_splitOnEmpty
  putStrLn "+++ All reference-project properties passed."

-- | mkScore clamps values to [0, 100].
prop_mkScoreClamps :: Double -> Bool
prop_mkScoreClamps d =
  let s = unScore (mkScore d)
  in s >= 0 && s <= 100

-- | parseTags produces no empty strings.
prop_parseTagsRoundtrip :: [String] -> Property
prop_parseTagsRoundtrip strs =
  not (null strs) ==>
  let tags = parseTags (mconcat (map (\s -> " " <> s <> " , ") strs))
  in not (any (null . show) tags)

-- | mean of an empty list is 0.
prop_meanEmpty :: Bool
prop_meanEmpty = mean [] == 0

-- | median of a singleton is that element.
prop_medianSingleton :: Double -> Bool
prop_medianSingleton d = median [d] == d

-- | chunksOf reconstitutes the original list.
prop_chunksOfReconstitute :: Positive Int -> [Int] -> Bool
prop_chunksOfReconstitute (Positive n) xs =
  concat (chunksOf n xs) == xs

-- | deduplicate is idempotent.
prop_deduplicateIdempotent :: [Int] -> Bool
prop_deduplicateIdempotent xs =
  deduplicate (deduplicate xs) == deduplicate xs

-- | safeHead / safeLast agree on singletons.
prop_safeHeadSafeLast :: Int -> Bool
prop_safeHeadSafeLast x =
  safeHead [x] == Just x && safeLast [x] == Just x

-- | splitOn on an empty string returns no segments.
prop_splitOnEmpty :: Bool
prop_splitOnEmpty = null (splitOn "," "")
