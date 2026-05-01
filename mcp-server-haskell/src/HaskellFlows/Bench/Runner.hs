-- | Timing harness helpers for the @haskell-flows-mcp@ benchmark
-- suite (#96 Phase A).
--
-- All functions here are pure — they operate on lists of millisecond
-- samples already collected by the caller. The caller is responsible
-- for timing each tool call (via 'Data.Time.Clock.POSIX.getPOSIXTime'
-- or similar) and passing the raw list here.
--
-- Phase A usage pattern:
--
-- @
--   samples <- replicateM (n + 1) (timedCall tool)
--   let result = computeStats samples   -- discards the cold-start sample
--   when (prP95 result > tbP95Ms budget) $
--     putStrLn ("BUDGET BREACH: " ++ toolName)
-- @
--
-- Phase C will wire 'computeStats' into the CI gate so breaches fail
-- the build.
module HaskellFlows.Bench.Runner
  ( -- * Result type
    PercentileResult (..)
    -- * Pure helpers
  , discardFirst
  , warmSamples
  , computePercentile
  , computeStats
  ) where

import Data.List (sort)

-- | Computed latency statistics over a set of warm samples.
-- All numeric fields are in milliseconds.
data PercentileResult = PercentileResult
  { prSamples :: ![Int]   -- ^ warm samples (cold-start already discarded)
  , prP50     :: !Int     -- ^ p50 (median), ms
  , prP95     :: !Int     -- ^ p95 upper-bound, ms
  , prMean    :: !Int     -- ^ arithmetic mean, ms
  , prStdDev  :: !Double  -- ^ standard deviation, ms
  }
  deriving stock (Show)

-- | Discard the first element of a list.
--
-- Used to strip the cold-start sample — the first call to any tool
-- that routes through 'loadForTarget' pays the cabal v2-repl
-- bootstrap cost (~5s). Subsequent warm calls are what the budget
-- contracts are measured against.
--
-- Returns an empty list unchanged (no error — just nothing to discard).
discardFirst :: [a] -> [a]
discardFirst []     = []
discardFirst (_:xs) = xs

-- | Alias for 'discardFirst' with a self-documenting name.
warmSamples :: [Int] -> [Int]
warmSamples = discardFirst

-- | Compute a percentile from a list of millisecond samples.
--
-- @pct@ is in [0, 100]; 50.0 → median, 95.0 → p95.
-- Returns 0 for an empty sample list.
-- Uses the ceiling method so a 1-element list returns that element for
-- any percentile.
computePercentile :: [Int] -> Double -> Int
computePercentile []  _   = 0
computePercentile xs  pct =
  let sorted  = sort xs
      n       = length sorted
      -- ceiling-based index; clamp to [0, n-1]
      rawIdx  = ceiling (pct / 100.0 * fromIntegral n :: Double) - 1
      idx     = max 0 (min (n - 1) rawIdx)
  in sorted !! idx

-- | Compute full latency statistics from a raw sample list.
--
-- The first sample is always discarded (cold-start tax). If the
-- resulting warm list is empty (i.e. only one raw sample was provided),
-- all fields are 0.
computeStats :: [Int] -> PercentileResult
computeStats []  = PercentileResult [] 0 0 0 0.0
computeStats raw =
  let warm = discardFirst raw
  in case warm of
       [] -> PercentileResult [] 0 0 0 0.0
       _  ->
         let p50    = computePercentile warm 50.0
             p95    = computePercentile warm 95.0
             mean   = sum warm `div` length warm
             stdDev = computeStdDev warm mean
         in PercentileResult warm p50 p95 mean stdDev

-- ---------------------------------------------------------------------------
-- Internal helpers
-- ---------------------------------------------------------------------------

computeStdDev :: [Int] -> Int -> Double
computeStdDev [] _     = 0.0
computeStdDev xs mean  =
  let n       = fromIntegral (length xs) :: Double
      meanD   = fromIntegral mean        :: Double
      sumSq   = sum (map (\x -> (fromIntegral x - meanD) ^ (2 :: Int)) xs)
  in sqrt (sumSq / n)
