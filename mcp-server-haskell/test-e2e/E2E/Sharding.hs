-- | Round-robin shard split for the e2e suite.
--
-- The e2e harness ships with a built-in fast/slow split and a QSem
-- pool ('HASKELL_FLOWS_E2E_PARALLEL'), but the in-process pool only
-- buys ~10% on local 8-core machines: the bottleneck is filesystem
-- contention on @~/.cabal/store@ that several "slow" scenarios
-- collide on (cabal_test, ghc_coverage, ghc_check_project,
-- ghc_witness, …). The lock waits aren't CPU-bound, so adding
-- workers inside one process makes things WORSE past a small fan-out.
--
-- Sharding lifts that ceiling by spreading the work across
-- separate CI runners, each with its own clean cabal store. That
-- way the in-process serialisation cost only applies WITHIN a
-- shard, and three shards run truly in parallel — the wall-clock
-- becomes @max(shard1, shard2, shard3)@ rather than the sum.
--
-- The split is a deterministic round-robin on the original list
-- order: scenario at zero-based index @k@ goes to shard
-- @(k mod n) + 1@. This preserves the natural fast/slow
-- interleaving in the declaration list and balances each shard's
-- duration without needing to hand-tag each scenario with a
-- shard index — adding a new scenario is still a one-line append
-- to 'Main.scenarios', no shard bookkeeping.
module E2E.Sharding
  ( -- * Shard descriptor
    Shard (..)
    -- * Parsing
  , parseShard
    -- * Application
  , applyShard
    -- * Self-tests
  , selfTest
  ) where

import qualified Data.List as List
import Data.Maybe (isNothing)
import Text.Read (readMaybe)

-- | A shard descriptor parsed from @"i/n"@.
--
--   * 'shardIndex' is 1-based — matches GitHub Actions matrix
--     conventions (@matrix.shard: [1, 2, 3]@) so the workflow
--     value passes through verbatim with no off-by-one in the
--     glue layer.
--   * 'shardTotal' is the number of shards the suite is split
--     across.
--
-- Invariant maintained by 'parseShard': @1 <= shardIndex@,
-- @shardTotal >= 1@, and @shardIndex <= shardTotal@.
data Shard = Shard
  { shardIndex :: !Int
  , shardTotal :: !Int
  }
  deriving (Show, Eq)

-- | Parse @"i/n"@ into a 'Shard'. Returns 'Nothing' on any
-- malformed input — empty string, missing slash, non-integer
-- parts, zero or negative index, index out of range.
--
-- Lenient-on-malformed-input is the right policy here: an
-- accidental @HASKELL_FLOWS_E2E_SHARD=foo@ in a developer shell
-- should not crash the harness; we just fall back to "no
-- sharding" and run the full suite. The CI workflow always
-- supplies a well-formed value.
parseShard :: String -> Maybe Shard
parseShard s = case break (== '/') s of
  (a, '/':b)
    | Just i <- readMaybe a
    , Just n <- readMaybe b
    , n >= 1
    , i >= 1
    , i <= n
        -> Just (Shard i n)
  _   -> Nothing

-- | Round-robin shard split. Element at zero-based position @k@
-- is assigned to shard @(k mod n) + 1@. Preserves original
-- ordering within a shard.
--
-- Why round-robin (as opposed to chunked / hash-based / weighted):
--
--   * Round-robin requires no metadata (no scenario weights, no
--     shard index in the declaration list) — adding a new
--     scenario is still a one-line append, with the new entry
--     auto-routed to whichever shard balances it best.
--   * The original 'Main.scenarios' list interleaves fast and
--     slow entries (slow ones are scattered across the list, not
--     clustered). Round-robin therefore gives each shard a
--     similar fast/slow mix, balancing wall-time to within a
--     few seconds of @total / n@.
--   * Chunked split (first third / second third / last third)
--     would clump all the heavy 'FlowExprEvaluator',
--     'FlowCoverage', 'FlowDogfoodReplay' scenarios together
--     and starve the other shards — the opposite of what we
--     want.
applyShard :: Shard -> [a] -> [a]
applyShard (Shard i n) xs =
  [ x | (k, x) <- zip [0 :: Int ..] xs, k `mod` n == i - 1 ]

-- | Inline self-tests, called from 'Main.main' at startup.
--
-- Why inline rather than a hspec block in @test\/Spec.hs@:
--
--   * @test\/Spec.hs@ lives in the @haskell-flows-mcp-test@
--     stanza, which only depends on the library — it can't
--     import from @test-e2e\/@ without a stanza-shape change.
--     Inline tests called at e2e startup give the same
--     regression coverage with zero cabal surgery.
--   * The cost is microseconds; running them on every e2e
--     invocation is a free belt-and-suspenders. If the
--     partition logic ever drifts (@a `union` b `union` c@
--     no longer equals the original list), every CI cell
--     fails-fast at startup with a single, attributable error
--     before any scenario fires.
selfTest :: IO ()
selfTest = do
  -- parseShard happy paths
  expect "parseShard 1/3 = Shard 1 3"
    (parseShard "1/3" == Just (Shard 1 3))
  expect "parseShard 3/3 = Shard 3 3"
    (parseShard "3/3" == Just (Shard 3 3))
  expect "parseShard 1/1 = Shard 1 1 (single-shard identity)"
    (parseShard "1/1" == Just (Shard 1 1))
  expect "parseShard 7/10 = Shard 7 10 (multi-digit)"
    (parseShard "7/10" == Just (Shard 7 10))

  -- parseShard rejects
  expect "parseShard \"\" rejected (empty)"
    (isNothing (parseShard ""))
  expect "parseShard \"3\" rejected (no slash)"
    (isNothing (parseShard "3"))
  expect "parseShard 0/3 rejected (1-based, 0 invalid)"
    (isNothing (parseShard "0/3"))
  expect "parseShard 4/3 rejected (out of range)"
    (isNothing (parseShard "4/3"))
  expect "parseShard 1/0 rejected (zero shards)"
    (isNothing (parseShard "1/0"))
  expect "parseShard a/3 rejected (non-numeric)"
    (isNothing (parseShard "a/3"))
  expect "parseShard 1/-3 rejected (negative total)"
    (isNothing (parseShard "1/-3"))

  -- applyShard partition invariants — the load-bearing property
  let xs = [0 .. 69 :: Int]
      s1 = applyShard (Shard 1 3) xs
      s2 = applyShard (Shard 2 3) xs
      s3 = applyShard (Shard 3 3) xs
      sortedUnion = List.sort (s1 ++ s2 ++ s3)
  expect "shard 1+2+3 of 3 covers EXACTLY all 70 scenarios"
    (sortedUnion == xs)
  expect "shard 1/3 starts at index 0, stride 3"
    (s1 == [0, 3, 6, 9, 12, 15, 18, 21, 24, 27,
            30, 33, 36, 39, 42, 45, 48, 51, 54, 57,
            60, 63, 66, 69])
  expect "shard 2/3 starts at index 1, stride 3"
    (s2 == [1, 4, 7, 10, 13, 16, 19, 22, 25, 28,
            31, 34, 37, 40, 43, 46, 49, 52, 55, 58,
            61, 64, 67])
  expect "shard 3/3 starts at index 2, stride 3"
    (s3 == [2, 5, 8, 11, 14, 17, 20, 23, 26, 29,
            32, 35, 38, 41, 44, 47, 50, 53, 56, 59,
            62, 65, 68])
  expect "shard sizes balance to within 1 (24 / 23 / 23)"
    (length s1 == 24 && length s2 == 23 && length s3 == 23)

  -- shard 1/1 must be identity (no-op when sharding disabled)
  expect "shard 1/1 = identity"
    (applyShard (Shard 1 1) xs == xs)

  -- empty input handled cleanly
  expect "shard 2/3 of [] = []"
    (null (applyShard (Shard 2 3) ([] :: [Int])))
  where
    expect :: String -> Bool -> IO ()
    expect _    True  = pure ()
    expect name False =
      error ("E2E.Sharding self-test FAILED: " <> name)
