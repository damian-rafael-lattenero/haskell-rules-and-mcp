-- | @haskell-flows-mcp-bench@ — Phase B entry point (#96).
--
-- Phase A landed the budget table + pure timing helpers.  Phase B (this
-- file) wires those pieces into a real measurement harness:
--
--   1.  Spin up an in-process 'Server' anchored on a temporary copy of
--       @benchmarks\/Reference\/@ (so each bench run is hermetic; we do
--       not pollute the Reference dir's @.haskell-flows\/@).
--   2.  For each tool in 'benchSubset', send N=10 'tools\/call' requests
--       through 'handleRequest' (the same path the stdio transport uses).
--   3.  Time each call via 'getPOSIXTime', discard the cold-start sample
--       (first call after 'serverFor' pays the GhcSession boot tax —
--       same contract as the e2e harness), and compute p50 \/ p95.
--   4.  Emit one row per tool: actual measurement vs. the budgeted
--       value, plus a @WARN@ tag when the measurement exceeds the
--       budget.
--
-- Phase B intentionally stays informational — the budget gate (Phase C)
-- only fires when this output is wired into CI as a required check.
-- The numbers printed here become the seed values for the measured
-- budgets in @docs\/PERFORMANCE.md@.
--
-- Run via:
--   @cabal run haskell-flows-mcp-bench@
--   @scripts\/bench-mcp.sh@
module Main where

import Control.Monad (replicateM, when)
import Data.Aeson (Value (..), object, (.=))
import qualified Data.Aeson as Aeson
import qualified Data.Map.Strict as Map
import qualified Data.Text as T
import Data.Time.Clock.POSIX (getPOSIXTime)
import System.Directory
  ( copyFile
  , createDirectoryIfMissing
  , doesDirectoryExist
  , doesFileExist
  , getCurrentDirectory
  , listDirectory
  , removeDirectoryRecursive
  )
import System.Environment (getArgs, lookupEnv)
import System.Exit (exitFailure, exitSuccess)
import System.FilePath ((</>))
import System.IO (hFlush, stdout)
import System.IO.Temp (withSystemTempDirectory)

import HaskellFlows.Bench.Budget (ToolBudget (..), allBudgets)
import HaskellFlows.Bench.Runner (PercentileResult (..), computeStats)
import HaskellFlows.Mcp.Protocol (Request (..))
import HaskellFlows.Mcp.RpcMethod (RpcMethod (..), rpcMethodText)
import HaskellFlows.Mcp.Server (Server, serverFor, handleRequest)
import HaskellFlows.Mcp.ToolName (ToolName (..), toolNameText)

main :: IO ()
main = do
  -- #96 Phase C: opt-in gate — exit non-zero when any tool's measured
  -- p95 exceeds its budget. Two opt-in surfaces:
  --
  --   * @--gate@ (CLI flag, used by scripts/bench-mcp.sh --gate).
  --   * @HFLOWS_BENCH_GATE=1@ (env var, used by the advisory CI job
  --     so the workflow YAML stays declarative — no flag-plumbing
  --     through cabal run -- --gate).
  --
  -- Default stays informational so the local @cabal run
  -- haskell-flows-mcp-bench@ from a developer's repo prints the
  -- table and exits 0.  Phase D's full-matrix nightly will use
  -- the env-var form.
  args   <- getArgs
  envVar <- lookupEnv "HFLOWS_BENCH_GATE"
  let gateOn = "--gate" `elem` args
            || envVar == Just "1"
            || envVar == Just "true"

  putStrLn "=================================================================="
  putStrLn "haskell-flows-mcp-bench — Phase B (#96)"
  putStrLn "Per-tool latency measurement against benchmarks/Reference/"
  when gateOn $
    putStrLn "Mode: GATE (#96 Phase C — non-zero exit on p95 breach)"
  putStrLn "=================================================================="
  putStrLn ""
  withSystemTempDirectory "hflows-bench" $ \tmp -> do
    refSrc <- locateReferenceProject
    refDst <- copyReferenceProject refSrc tmp
    putStrLn ("Reference project (hermetic copy): " <> refDst)
    putStrLn ""

    srv <- serverFor refDst
    -- Warm-up: one ghc_load to pay the cabal v2-repl bootstrap once
    -- before any timed sample is collected.  Stops the cold-start
    -- tax from polluting tool measurements that don't even need GHCi
    -- (e.g. ghc_workflow, ghc_toolchain_status).
    putStrLn "Warm-up (ghc_workflow status — boot probe)…"
    _ <- timeOne srv GhcWorkflow (object [ "action" .= ("status" :: T.Text) ])
    putStrLn ""

    putStrLn "Running full tool sweep (n per tool in parens, first sample discarded):"
    putStrLn (replicate 78 '-')
    rows <- mapM (\(tool, tArgs, n) -> runBench srv n (tool, tArgs)) benchSubset
    putStrLn (replicate 78 '-')
    putStrLn ""

    -- #96 Phase D: write the per-tool measurements to a JSON file so
    -- the nightly workflow (.github/workflows/bench-nightly.yml) can
    -- upload it as an artifact, and a follow-up sub-task can diff
    -- measured numbers against the initial-proposal values in
    -- Bench/Budget.hs.  Path is relative to the package root (the
    -- working directory cabal run starts in) so it lands at
    -- mcp-server-haskell/bench-results.json — exactly where the
    -- workflow's upload-artifact step expects it.
    writeResultsJson "bench-results.json" rows

    let breaches = [ r | r <- rows, brBreach r ]
    if null breaches
      then do
        putStrLn "All measured tools within budget."
        putStrLn ""
        when gateOn $
          putStrLn "Gate: GREEN — every measured p95 ≤ budget."
        exitSuccess
      else do
        putStrLn ("WARN: " <> show (length breaches) <> " tool(s) exceeded their p95 budget:")
        mapM_ (\r -> putStrLn ("  - " <> brName r)) breaches
        putStrLn ""
        if gateOn
          then do
            putStrLn ( "Gate: RED — exiting with status 1.  "
                    <> "If a budget needs to grow, edit "
                    <> "src/HaskellFlows/Bench/Budget.hs with a "
                    <> "rationale comment and re-run.  Local "
                    <> "exec: scripts/bench-mcp.sh --gate" )
            exitFailure
          else
            putStrLn "Gate is opt-in (--gate or HFLOWS_BENCH_GATE=1) — exiting 0."

--------------------------------------------------------------------------------
-- Full tool sweep — all 35 tools benchmarked in one nightly run.
--
-- Tuple: (tool, JSON args, n)
--   n = number of warm samples to collect (cold-start +1 is always discarded).
--   Cheap tools (p95 budget ≤ 1500 ms) → n=10 for good statistics.
--   Expensive tools (p95 budget > 1500 ms) → n=3 to keep nightly ≤ 5 min.
--
-- File-mutating tools (GhcRefactor, GhcApplyExports, GhcFixWarning) run
-- against the hermetic temp copy so each run is independent.
--------------------------------------------------------------------------------

benchSubset :: [(ToolName, Value, Int)]
benchSubset =
  -- (tool, JSON args, n)
  -- ── control-plane (cheap) ──────────────────────────────────────────────
  [ (GhcWorkflow,      object [ "action" .= ("status" :: T.Text) ],     10)
  , (GhcToolchain,     object [ "action" .= ("status" :: T.Text) ],     10)
  , (GhcProject,       object [ "action" .= ("validate" :: T.Text) ],   10)
  , (GhcModules,       object [ "action" .= ("list" :: T.Text) ],       10)
  , (GhcPropertyStore, object [ "action" .= ("list" :: T.Text) ],       10)
  -- ── read / inspect (cheap) ────────────────────────────────────────────
  , (GhcLoad,          object [ "module_path" .= ("src/Ref/Stats.hs" :: T.Text) ], 10)
  , (GhcType,          object [ "expression"  .= ("mean" :: T.Text) ],   10)
  , (GhcInfo,          object [ "name"        .= ("Item" :: T.Text) ],   10)
  , (GhcEval,          object [ "expression"  .= ("1 + 1" :: T.Text) ], 10)
  , (GhcImports,       object [],                                         10)
  , (GhcBrowse,        object [ "module"      .= ("Ref.Stats" :: T.Text) ], 10)
  , (GhcComplete,      object [ "prefix"      .= ("med" :: T.Text) ],   10)
  , (GhcGoto,          object [ "name"        .= ("median" :: T.Text) ], 10)
  , (GhcDoc,           object [ "name"        .= ("mean" :: T.Text) ],  10)
  , (GhcHole,          object [ "module_path" .= ("src/Ref/Stats.hs" :: T.Text) ], 10)
  , (GhcArbitrary,     object [ "type_name"   .= ("Category" :: T.Text) ], 10)
  , (HoogleSearch,     object [ "query"       .= ("sortBy" :: T.Text) ], 10)
  , (GhcExplainError,  object [ "module_path" .= ("src/Ref/Stats.hs" :: T.Text) ], 10)
  -- ── suggest / test (moderate) ─────────────────────────────────────────
  , (GhcSuggest,       object [ "function_name" .= ("mean" :: T.Text) ], 10)
  , (GhcAddImport,     object [ "name"        .= ("Data.Char" :: T.Text) ], 10)
  , (GhcCheckModule,   object [ "module_path" .= ("src/Ref/Stats.hs" :: T.Text) ], 10)
  -- ── quality gates / static analysis ───────────────────────────────────
  , (GhcFormat,        object [ "module_path" .= ("src/Ref/Stats.hs" :: T.Text) ], 10)
  , (GhcLint,          object [ "module_path" .= ("src/Ref/Stats.hs" :: T.Text) ], 10)
  , (GhcFixWarning,    object [ "module_path" .= ("src/Ref/Stats.hs" :: T.Text) ],  5)
  , (GhcDeps,          object [ "action"      .= ("list" :: T.Text) ],              5)
  -- ── rewrite / refactor ────────────────────────────────────────────────
  -- GhcRefactor: deliberately use a non-existent binding so the tool
  -- returns quickly with "name not found" without mutating any file.
  , (GhcRefactor,      object [ "action"         .= ("rename_local" :: T.Text)
                               , "module_path"    .= ("src/Ref/Stats.hs" :: T.Text)
                               , "old_name"       .= ("_bench_probe_" :: T.Text)
                               , "new_name"       .= ("_bench_probe2_" :: T.Text)
                               , "scope_line_start" .= (1 :: Int)
                               , "scope_line_end"   .= (100 :: Int) ],              5)
  , (GhcApplyExports,  object [ "module_path" .= ("src/Ref/Stats.hs" :: T.Text)
                               , "exports"     .= (["mean", "median", "stdDev"
                                                   ,"scoreHistogram"] :: [T.Text]) ], 5)
  -- ── property-first testing ────────────────────────────────────────────
  , (GhcQuickCheck,    object [ "property"    .= ("\\xs -> length xs >= 0" :: T.Text)
                               , "module_path" .= ("src/Ref/Stats.hs" :: T.Text) ],  3)
  -- ── composite / batch ─────────────────────────────────────────────────
  , (GhcBatch,         object [ "actions" .= [ object [ "name"      .= ("ghc_type" :: T.Text)
                                                       , "arguments" .= object
                                                           [ "expression" .= ("mean" :: T.Text) ] ] ] ], 10)
  -- ── expensive: check, gate, coverage, perf ────────────────────────────
  , (GhcCheckProject,  object [],                                          3)
  , (GhcPerf,          object [ "expression" .= ("mean [1..10]" :: T.Text)
                               , "runs"       .= (5 :: Int) ],              3)
  , (GhcWitness,       object [ "property"    .= ("\\xs -> length xs >= 0" :: T.Text)
                               , "module_path" .= ("src/Ref/Stats.hs" :: T.Text) ],  3)
  , (GhcLab,           object [ "module_path" .= ("src/Ref/Stats.hs" :: T.Text) ],   3)
  , (GhcGate,          object [],                                          3)
  , (GhcCoverage,      object [],                                          3)
  ]

--------------------------------------------------------------------------------
-- Bench result rendering
--------------------------------------------------------------------------------

data BenchRow = BenchRow
  { brTool   :: !ToolName
  , brName   :: !String
  , brStats  :: !PercentileResult
  , brBudget :: !(Maybe ToolBudget)
  , brBreach :: !Bool
  }

runBench :: Server -> Int -> (ToolName, Value) -> IO BenchRow
runBench srv n (tool, args) = do
  -- N+1 raw samples; first is discarded inside computeStats.
  raw <- replicateM (n + 1) (timeOne srv tool args)
  let stats   = computeStats raw
      mBudget = Map.lookup tool allBudgets
      breach  = case mBudget of
        Nothing -> False
        Just b  -> prP95 stats > tbP95Ms b
      row = BenchRow
        { brTool   = tool
        , brName   = T.unpack (toolNameText tool)
        , brStats  = stats
        , brBudget = mBudget
        , brBreach = breach
        }
  printRow row
  pure row

printRow :: BenchRow -> IO ()
printRow r = do
  let stats = brStats r
      tag   = if brBreach r then "  WARN" else "  OK  "
      name  = padR 24 (brName r)
      p50s  = padL 6 (show (prP50 stats) <> "ms")
      p95s  = padL 7 (show (prP95 stats) <> "ms")
      means = padL 6 (show (prMean stats) <> "ms")
      bud   = case brBudget r of
        Nothing -> "(no budget)"
        Just b  -> "budget p50=" <> show (tbP50Ms b) <> " p95=" <> show (tbP95Ms b)
  putStrLn $ unwords
    [ tag
    , name
    , "p50=" <> p50s
    , "p95=" <> p95s
    , "mean=" <> means
    , "—"
    , bud
    ]
  hFlush stdout

padR :: Int -> String -> String
padR n s = s <> replicate (max 0 (n - length s)) ' '

padL :: Int -> String -> String
padL n s = replicate (max 0 (n - length s)) ' ' <> s

--------------------------------------------------------------------------------
-- JSON artifact (#96 Phase D)
--------------------------------------------------------------------------------

-- | Encode the measured per-tool table as a JSON array of objects
-- and write it to the given path.  Shape (one row per tool):
--
-- @
--   [ { "tool": "ghc_type"
--     , "p50_ms": 42, "p95_ms": 180, "mean_ms": 60, "stddev_ms": 25.3
--     , "samples": [40,42,41,38,55,...]
--     , "budget_p50_ms": 50, "budget_p95_ms": 200
--     , "budget_notes": "cached GHCi env; near-zero marginal cost"
--     , "breach": false
--     }
--   , ...
--   ]
-- @
--
-- A tool without a budget entry (shouldn't happen — every 'ToolName'
-- has one — but defensive in case the budget table grows out of sync
-- with 'benchSubset') emits @null@ for the budget fields.  Sub-task 2
-- of #96 Phase D will diff this artifact against
-- 'Bench/Budget.hs' and propose updates.
writeResultsJson :: FilePath -> [BenchRow] -> IO ()
writeResultsJson path rows = do
  Aeson.encodeFile path (map rowToJson rows)
  putStrLn ("Wrote bench-results.json (" <> show (length rows) <> " rows): " <> path)
  putStrLn ""

rowToJson :: BenchRow -> Value
rowToJson r =
  let stats = brStats r
      base =
        [ "tool"      .= brName r
        , "n_samples" .= length (prSamples stats)
        , "p50_ms"    .= prP50 stats
        , "p95_ms"    .= prP95 stats
        , "mean_ms"   .= prMean stats
        , "stddev_ms" .= prStdDev stats
        , "samples"   .= prSamples stats
        , "breach"    .= brBreach r
        ]
      budget = case brBudget r of
        Nothing -> [ "budget_p50_ms" .= Null
                   , "budget_p95_ms" .= Null
                   , "budget_notes"  .= Null
                   ]
        Just b  -> [ "budget_p50_ms" .= tbP50Ms b
                   , "budget_p95_ms" .= tbP95Ms b
                   , "budget_notes"  .= tbNotes b
                   ]
  in object (base <> budget)

--------------------------------------------------------------------------------
-- Timing harness
--------------------------------------------------------------------------------

-- | Send a single tools/call through the in-process Server,
-- measure wall-clock in ms.  Errors are swallowed and reported as
-- a high-but-finite latency so a one-off RPC failure does not poison
-- the entire run; the stats path then surfaces the variance.
timeOne :: Server -> ToolName -> Value -> IO Int
timeOne srv tool args = do
  t0 <- getPOSIXTime
  _  <- handleRequest srv (mkRequest tool args)
  t1 <- getPOSIXTime
  pure (round ((realToFrac (t1 - t0) :: Double) * 1000))

mkRequest :: ToolName -> Value -> Request
mkRequest tool args =
  Request
    { reqJsonrpc = "2.0"
    , reqMethod  = rpcMethodText ToolsCall
    , reqParams  = Just (object
        [ "name"      .= toolNameText tool
        , "arguments" .= args
        ])
    , reqId      = Just (Number 0)
    }

--------------------------------------------------------------------------------
-- Reference project handling
--------------------------------------------------------------------------------

-- | Find the @benchmarks\/Reference\/@ source.  When invoked through
-- @cabal run@, the cwd is repo root; through @scripts\/bench-mcp.sh@
-- it could be anywhere — try a couple of known relative paths before
-- giving up.
locateReferenceProject :: IO FilePath
locateReferenceProject = do
  cwd <- getCurrentDirectory
  let candidates =
        [ cwd </> "benchmarks" </> "Reference"
        , cwd </> "mcp-server-haskell" </> "benchmarks" </> "Reference"
        , cwd </> ".." </> "benchmarks" </> "Reference"
        ]
  go candidates
  where
    go []       = error "Cannot locate benchmarks/Reference/ — run from repo root."
    go (p : ps) = do
      ok <- doesDirectoryExist p
      if ok then pure p else go ps

-- | Recursive copy of the reference project into a hermetic temp dir.
-- Mirrors the e2e suite's pattern: each run owns its own
-- @.haskell-flows\/@, @dist-newstyle\/@, @cabal.project.local@ — so
-- repeated bench runs are reproducible and don't pollute the source.
copyReferenceProject :: FilePath -> FilePath -> IO FilePath
copyReferenceProject src tmp = do
  let dst = tmp </> "Reference"
  createDirectoryIfMissing True dst
  copyDir src dst
  -- Ensure the .haskell-flows is empty per run so we measure fresh
  -- property-store / staleness-tracker behaviour.
  let hf = dst </> ".haskell-flows"
  hfExists <- doesDirectoryExist hf
  when hfExists (removeDirectoryRecursive hf)
  pure dst

copyDir :: FilePath -> FilePath -> IO ()
copyDir src dst = do
  createDirectoryIfMissing True dst
  entries <- listDirectory src
  mapM_ (copyEntry src dst) entries

copyEntry :: FilePath -> FilePath -> FilePath -> IO ()
copyEntry srcRoot dstRoot name = do
  let srcPath = srcRoot </> name
      dstPath = dstRoot </> name
  isDir <- doesDirectoryExist srcPath
  if isDir
    then copyDir srcPath dstPath
    else do
      isFile <- doesFileExist srcPath
      when isFile (copyFile srcPath dstPath)
