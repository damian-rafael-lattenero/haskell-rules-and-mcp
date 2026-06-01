-- | Unit tests for 'Data.PropertyStore' (#283) and qcMaxSuccess threshold.
--
-- Extracted from the Spec.hs monolith (#271) via the function-export shape.
module Spec.Store
  ( testStoreRoundtrip
  , testStoreIncrement
  , testStoreRecordsCases
  , testStoreSaveDefaultsCasesZero
  , testQcMaxSuccessRaised
  ) where

import HaskellFlows.Data.PropertyStore
  ( StoredProperty (..)
  , loadAll
  , openStore
  , save
  , saveCases
  )
import qualified HaskellFlows.Tool.QuickCheck as QcTool

import Spec.Helpers (withTempProject)

testStoreRoundtrip :: IO Bool
testStoreRoundtrip = withTempProject $ \pd -> do
  store <- openStore pd
  save store "\\(xs :: [Int]) -> reverse (reverse xs) == xs" (Just "src/Foo.hs")
  props <- loadAll store
  pure $ case props of
    [p] -> spExpression p == "\\(xs :: [Int]) -> reverse (reverse xs) == xs"
        && spModule p == Just "src/Foo.hs"
        && spPassed p == 1
    _   -> False

testStoreIncrement :: IO Bool
testStoreIncrement = withTempProject $ \pd -> do
  store <- openStore pd
  save store "prop_foo" Nothing
  save store "prop_foo" Nothing
  save store "prop_foo" Nothing
  props <- loadAll store
  pure $ case props of
    [p] -> spPassed p == 3
    _   -> False

-- | #283: saveCases persists the QuickCheck case count behind a law, and on
-- re-save keeps the HIGHEST confidence ever observed (a later weaker run must
-- not lower it).
testStoreRecordsCases :: IO Bool
testStoreRecordsCases = withTempProject $ \pd -> do
  store <- openStore pd
  saveCases store "prop_c" (Just "src/Foo.hs") 300
  saveCases store "prop_c" (Just "src/Foo.hs") 100  -- weaker — must not lower
  props <- loadAll store
  pure $ case props of
    [p] -> spCases p == 300 && spPassed p == 2
    _   -> False

-- | #283: the backward-compatible 3-arg save records cases=0 ("unknown"), so
-- pre-#283 call sites and old on-disk entries stay valid.
testStoreSaveDefaultsCasesZero :: IO Bool
testStoreSaveDefaultsCasesZero = withTempProject $ \pd -> do
  store <- openStore pd
  save store "prop_legacy" Nothing
  props <- loadAll store
  pure $ case props of
    [p] -> spCases p == 0
    _   -> False

-- | #283: the per-check QuickCheck case count was raised above the stdArgs
-- default of 100 so a single check is more likely to surface a false law.
testQcMaxSuccessRaised :: IO Bool
testQcMaxSuccessRaised = pure (QcTool.qcMaxSuccess > 100)
