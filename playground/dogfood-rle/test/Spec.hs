-- | Phase 11b dogfood test suite for 'DogfoodRle'.
--
-- Arbitrary instance hand-written — the MCP's 'ghci_arbitrary' was
-- found to reject records with strict fields under GHC 9.x (F-04,
-- fixed in tree at commit 42d830c but the running binary still
-- carries the bug until the user reinstalls). Writing the instance
-- by hand unblocks the rest of the dogfood without waiting on a
-- relaunch.
--
-- Properties were NOT taken from 'ghci_suggest' either: that tool
-- (pre-fix) emits two false laws for @encode :: [a] -> [Run a]@
-- because the list rules ignored element-type agreement (F-05, also
-- fixed in tree at 42d830c). Authored by hand below.
module Main where
import Data.List (group)
import DogfoodRle
import System.Exit (exitFailure, exitSuccess)
import Test.QuickCheck

-- | Arbitrary for 'Run'. Runs must have @runLen > 0@ — a zero-length
-- run cannot appear in the image of 'encode'. Using 'getPositive'
-- keeps the generator from producing malformed data when
-- 'DogfoodRle' is refactored and we want laws to stay meaningful.
instance Arbitrary a => Arbitrary (Run a) where
  arbitrary = do
    Positive n <- arbitrary
    Run n <$> arbitrary

-- | prop_roundtrip: decode reverses encode for every input list.
-- Core correctness invariant — if this fails, the RLE is broken.
prop_roundtrip :: [Int] -> Bool
prop_roundtrip xs = decode (encode xs) == xs

-- | prop_length_preserved: encoding + decoding conserves list length.
-- Weaker than roundtrip but catches off-by-one bugs that happen to
-- keep values right.
prop_length_preserved :: [Int] -> Bool
prop_length_preserved xs = length (decode (encode xs)) == length xs

-- | prop_runs_non_zero: every 'Run' in 'encode''s image carries a
-- positive length. Encoding a zero-length run would mean @group@
-- produced an empty list — which it never does.
prop_runs_non_zero :: [Int] -> Bool
prop_runs_non_zero xs = all ((> 0) . runLen) (encode xs)

-- | prop_runs_match_group: encode produces one 'Run' per equivalence
-- class produced by 'Data.List.group'. Locks in that our encode is
-- using the expected segmentation.
prop_runs_match_group :: [Int] -> Bool
prop_runs_match_group xs = length (encode xs) == length (group xs)

main :: IO ()
main = do
  results <-
    mapM runProp
      [ ("roundtrip"         , property prop_roundtrip)
      , ("length_preserved"  , property prop_length_preserved)
      , ("runs_non_zero"     , property prop_runs_non_zero)
      , ("runs_match_group"  , property prop_runs_match_group)
      ]
  if and results then exitSuccess else exitFailure

-- | Thin wrapper that reports pass/fail with the property name
-- visible in stdout. Keeps the runner output compatible with the
-- ci-local.sh `test-show-details=direct` expectation.
runProp :: (String, Property) -> IO Bool
runProp (name, prop) = do
  res <- quickCheckWithResult stdArgs { chatty = False, maxSuccess = 200 } prop
  let ok = case res of
        Success {} -> True
        _          -> False
  putStrLn ((if ok then "PASS  " else "FAIL  ") <> name)
  pure ok
