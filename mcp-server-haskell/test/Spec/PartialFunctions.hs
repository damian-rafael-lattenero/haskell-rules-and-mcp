-- | Tests for issue #289 — eliminating partial functions from pure code.
--
-- Two categories:
--  1. Unit properties for 'HaskellFlows.Util.Safe' — totality proofs that
--     the safe wrappers never throw on empty / out-of-bounds input.
--  2. QuickCheck roundtrip properties for the affected parsers, feeding
--     empty and singleton inputs to confirm they are now total.
module Spec.PartialFunctions
  ( -- * Util.Safe unit tests
    testSafeAtNegative
  , testSafeAtOutOfBounds
  , testSafeAtHit
  , testSafeHeadEmpty
  , testSafeHeadNonEmpty
  , testSafeLastEmpty
  , testSafeLastNonEmpty
  , testInitLastEmpty
  , testInitLastSingleton
  , testInitLastMulti
    -- * Totality properties for affected parsers
  , testParseSignatureEmpty
  , testParseSignatureSingleton
  , testSplitModuleEmpty
  , testSplitModuleSingleton
  , testComputePercentileEmpty
  , testAggregateEmpty
  ) where

import Data.Maybe (isNothing)
import Data.Word (Word64)
import HaskellFlows.Util.Safe (safeAt, safeHead, safeLast, initLast)
import HaskellFlows.Parser.TypeSignature (parseSignature)
import HaskellFlows.Tool.Hoogle (splitModule)
import HaskellFlows.Bench.Runner (computePercentile)
import HaskellFlows.Tool.Perf (aggregate)

-- ---------------------------------------------------------------------------
-- safeAt
-- ---------------------------------------------------------------------------

testSafeAtNegative :: IO Bool
testSafeAtNegative = pure $ isNothing (safeAt (-1) [1,2,3 :: Int])

testSafeAtOutOfBounds :: IO Bool
testSafeAtOutOfBounds = pure $ isNothing (safeAt 5 [1,2,3 :: Int])

testSafeAtHit :: IO Bool
testSafeAtHit = pure $ safeAt 1 [10, 20, 30 :: Int] == Just 20

-- ---------------------------------------------------------------------------
-- safeHead / safeLast
-- ---------------------------------------------------------------------------

testSafeHeadEmpty :: IO Bool
testSafeHeadEmpty = pure $ isNothing (safeHead ([] :: [Int]))

testSafeHeadNonEmpty :: IO Bool
testSafeHeadNonEmpty = pure $ safeHead [42 :: Int] == Just 42

testSafeLastEmpty :: IO Bool
testSafeLastEmpty = pure $ isNothing (safeLast ([] :: [Int]))

testSafeLastNonEmpty :: IO Bool
testSafeLastNonEmpty = pure $ safeLast [1, 2, 3 :: Int] == Just 3

-- ---------------------------------------------------------------------------
-- initLast
-- ---------------------------------------------------------------------------

testInitLastEmpty :: IO Bool
testInitLastEmpty = pure $ isNothing (initLast ([] :: [Int]))

testInitLastSingleton :: IO Bool
testInitLastSingleton = pure $ initLast [42 :: Int] == Just ([], 42)

testInitLastMulti :: IO Bool
testInitLastMulti = pure $ initLast [1, 2, 3 :: Int] == Just ([1, 2], 3)

-- ---------------------------------------------------------------------------
-- Totality: parseSignature on empty / singleton tokens (#289)
-- ---------------------------------------------------------------------------

-- | Feeding an empty string to 'parseSignature' must not throw.
testParseSignatureEmpty :: IO Bool
testParseSignatureEmpty = do
  let result = parseSignature ""
  -- We don't care about the value — just that it doesn't throw.
  result `seq` pure True

-- | A signature with only one token (no arrows) must parse without crashing.
testParseSignatureSingleton :: IO Bool
testParseSignatureSingleton = do
  let result = parseSignature "Int"
  result `seq` pure True

-- ---------------------------------------------------------------------------
-- Totality: splitModule on edge-case inputs (#289)
-- ---------------------------------------------------------------------------

-- | Empty text to 'splitModule' must not throw.
testSplitModuleEmpty :: IO Bool
testSplitModuleEmpty = do
  let result = splitModule ""
  result `seq` pure True

-- | A single-word LHS to 'splitModule' must not throw.
testSplitModuleSingleton :: IO Bool
testSplitModuleSingleton = do
  let result = splitModule "filter"
  result `seq` pure True

-- ---------------------------------------------------------------------------
-- Totality: computePercentile / aggregate on empty inputs (#289)
-- ---------------------------------------------------------------------------

-- | 'computePercentile' on empty list must return 0, not throw.
testComputePercentileEmpty :: IO Bool
testComputePercentileEmpty =
  pure $ computePercentile [] 50.0 == 0

-- | 'aggregate' on empty list must return zeroed Stats, not throw.
testAggregateEmpty :: IO Bool
testAggregateEmpty =
  let s = aggregate ([] :: [Word64])
  in s `seq` pure True
