-- | @haskell-flows-mcp-bench@ — Phase A entry point (#96).
--
-- Phase A: scaffold + budget table + timing harness.
-- The harness is wired up but the gate is NOT enforced yet (Phase C).
--
-- Phase B will add actual per-tool timing calls against the reference
-- project in @benchmarks\/Reference\/@; this file then becomes the
-- canonical measurement driver.
--
-- Run via:
--   @scripts\/bench-mcp.sh@              (local dev)
--   @cabal run haskell-flows-mcp-bench@  (manual)
module Main where

import qualified Data.Map.Strict as Map
import qualified Data.Text as T

import HaskellFlows.Bench.Budget (ToolBudget (..), allBudgets)
import HaskellFlows.Bench.Runner (PercentileResult (..), computeStats, discardFirst)
import HaskellFlows.Mcp.ToolName (ToolName, allToolNames, toolNameText)

main :: IO ()
main = do
  putStrLn "haskell-flows-mcp-bench — Phase A"
  putStrLn "Budget table loaded. Timing harness ready."
  putStrLn ""
  putStrLn "Per-tool budgets (p50 / p95 ms):"
  putStrLn (replicate 60 '-')
  mapM_ printBudgetRow allToolNames
  putStrLn (replicate 60 '-')
  putStrLn ""
  putStrLn "Phase A complete. Phase B will measure actual latencies."
  putStrLn "(Run scripts/bench-mcp.sh to invoke.)"

-- | Print one row of the budget table.
printBudgetRow :: ToolName -> IO ()
printBudgetRow t =
  case Map.lookup t allBudgets of
    Nothing ->
      putStrLn ("  " ++ show t ++ "  *** NO BUDGET ENTRY ***")
    Just b  ->
      putStrLn $ unwords
        [ "  " ++ padR 28 (T.unpack (toolNameText t))
        , "p50=" ++ padL 6 (show (tbP50Ms b) ++ "ms")
        , "p95=" ++ padL 7 (show (tbP95Ms b) ++ "ms")
        , maybe "" (\cs -> "cold=" ++ show cs ++ "ms") (tbColdStartMs b)
        ]
  where
    padR n s = s ++ replicate (max 0 (n - length s)) ' '
    padL n s = replicate (max 0 (n - length s)) ' ' ++ s

-- | Demonstrate the runner with a fake sample set (Phase A smoke-test).
_demoRunner :: IO ()
_demoRunner = do
  -- Simulate: cold-start=4800ms, then 5 warm samples
  let fakeSamples = [4800, 310, 290, 320, 280, 305] :: [Int]
  let result = computeStats fakeSamples
  putStrLn "Demo runner (fake samples, first discarded):"
  putStrLn $ "  warm=" ++ show (prSamples result)
  putStrLn $ "  p50=" ++ show (prP50 result) ++ "ms"
  putStrLn $ "  p95=" ++ show (prP95 result) ++ "ms"
  putStrLn $ "  mean=" ++ show (prMean result) ++ "ms"
  putStrLn $ "  stdDev=" ++ show (prStdDev result) ++ "ms"
  putStrLn $ "  discardFirst demo: " ++ show (discardFirst [1 :: Int, 2, 3])
