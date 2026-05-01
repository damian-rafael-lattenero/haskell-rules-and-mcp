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
  putStrLn "=================================================================="
  putStrLn "haskell-flows-mcp-bench — Phase B (#96)"
  putStrLn "Per-tool latency measurement against benchmarks/Reference/"
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

    putStrLn "Running benchmark subset (N=10 per tool, first sample discarded):"
    putStrLn (replicate 78 '-')
    rows <- mapM (runBench srv 10) benchSubset
    putStrLn (replicate 78 '-')
    putStrLn ""

    let breaches = [ r | r <- rows, brBreach r ]
    if null breaches
      then putStrLn "All measured tools within budget."
      else do
        putStrLn ("WARN: " <> show (length breaches) <> " tool(s) exceeded their p95 budget:")
        mapM_ (\r -> putStrLn ("  - " <> brName r)) breaches
    putStrLn ""
    putStrLn "Phase B is informational; the gate (Phase C) is not yet wired into CI."

--------------------------------------------------------------------------------
-- Bench subset: representative tools across all four categories.
--   * ALL the cheap ones (read/inspect, control-plane) so cold-start
--     can be observed if mis-ascribed.
--   * Selected expensive tools (refactor, gates) to confirm warm budgets.
--   * Tools that need a project-bootstrap (ghc_load, ghc_check_*) that
--     hammer the GhcSession path.
-- The full 46-tool sweep is Phase D's nightly job.
--------------------------------------------------------------------------------

benchSubset :: [(ToolName, Value)]
benchSubset =
  -- (tool, JSON args)
  [ (GhcWorkflow,         object [ "action" .= ("status" :: T.Text) ])
  , (GhcToolchain,        object [ "action" .= ("status" :: T.Text) ])
  , (GhcValidateCabal,    object [])
  , (GhcLoad,             object [ "module_path" .= ("src/Ref/Stats.hs" :: T.Text) ])
  , (GhcType,             object [ "expression" .= ("mean" :: T.Text) ])
  , (GhcInfo,             object [ "name"       .= ("Item" :: T.Text) ])
  , (GhcEval,             object [ "expression" .= ("1 + 1" :: T.Text) ])
  , (GhcImports,          object [])
  , (GhcBrowse,           object [ "module"     .= ("Ref.Stats" :: T.Text) ])
  , (GhcComplete,         object [ "prefix"     .= ("med" :: T.Text) ])
  , (GhcGoto,             object [ "name"       .= ("median" :: T.Text) ])
  , (GhcSuggest,          object [ "function_name" .= ("mean" :: T.Text) ])
  , (GhcCheckModule,      object [ "module_path" .= ("src/Ref/Stats.hs" :: T.Text) ])
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
