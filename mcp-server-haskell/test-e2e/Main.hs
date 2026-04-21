-- | Entry point for the E2E test-suite.
--
-- Runs, in order:
--
--   1. Transport smoke (subprocess, 1 round-trip).
--   2. A list of scenarios — each scenario gets its own fresh
--      temp project dir + 'Server' so state can't leak between
--      them. Every scenario contributes a list of 'Check's; the
--      roll-up at the end is the union.
--
-- Adding a new scenario is a one-line append to 'scenarios' —
-- the framework handles tempdir, client lifecycle, section
-- banner, live progress streaming, and duration accounting.
module Main where

import Control.Exception (bracket, try, SomeException)
import qualified Data.Text as T
import Data.Time.Clock.POSIX (getPOSIXTime)
import System.Directory (createDirectoryIfMissing, getTemporaryDirectory, removePathForcibly)
import qualified System.Environment
import System.Exit (exitFailure, exitSuccess)
import System.FilePath ((</>))
import System.IO (BufferMode (..), hSetBuffering, stderr, stdout)

import qualified E2E.Assert as Assert
import qualified E2E.Client as Client
import qualified E2E.Smoke  as Smoke
import qualified Scenarios.ExprEvaluator        as Expr
import qualified Scenarios.FlowANSIEscape        as FlowANSI
import qualified Scenarios.FlowArbitrary        as FlowA
import qualified Scenarios.FlowBatch            as FlowB
import qualified Scenarios.FlowBootstrap        as FlowBoot
import qualified Scenarios.FlowCorpusTransport  as FlowCT
import qualified Scenarios.FlowConcurrentClients as FlowCC
import qualified Scenarios.FlowCoverage         as FlowCov
import qualified Scenarios.FlowCrossValidation  as FlowXV
import qualified Scenarios.FlowDependencyConflict as FlowDC
import qualified Scenarios.FlowDiskFull          as FlowDF
import qualified Scenarios.FlowExploratory      as FlowE
import qualified Scenarios.FlowExprEvaluatorDogfood as FlowEED
import qualified Scenarios.FlowFixWarning       as FlowFW
import qualified Scenarios.FlowGhciSigkill       as FlowSK
import qualified Scenarios.FlowGracefulMiss      as FlowGM
import qualified Scenarios.FlowInjectionGuard   as FlowIG
import qualified Scenarios.FlowNonUTF8           as FlowNU
import qualified Scenarios.FlowOversizedInput   as FlowOI
import qualified Scenarios.FlowSandboxEscape     as FlowSE
import qualified Scenarios.FlowTimeoutEnforcement as FlowTE
import qualified Scenarios.FlowMutation          as FlowMut
import qualified Scenarios.FlowPropertyLifecycle as FlowPL
import qualified Scenarios.FlowPropertyStoreRace as FlowPSR
import qualified Scenarios.FlowQualityGates     as FlowQG
import qualified Scenarios.FlowRefactor         as FlowR
import qualified Scenarios.FlowRefactorOutOfScope as FlowROS
import qualified Scenarios.FlowRegressionScopeFix as FlowRSF
import qualified Scenarios.FlowSessionRobustness as FlowSR
import qualified Scenarios.FlowTypeBreakage      as FlowTB
import qualified Scenarios.FlowScopeMgmt        as FlowS
import qualified Scenarios.FlowToolchain        as FlowTC
import qualified Scenarios.FlowTypedHoles       as FlowH
import qualified Scenarios.FlowValidateCabal    as FlowVC
import qualified Scenarios.FlowWorkflowHelp     as FlowWH

-- | Every scenario exposes the same shape:
--
--   @(label, isSlow, runFlow :: McpClient -> FilePath -> IO [Check])@
--
-- 'isSlow' = True marks scenarios that dominate wall-time — cabal
-- coverage runs, the 30 s inner-timeout assertion, real session
-- respawns, concurrent-clients fan-out, etc. CI runs everything by
-- default; the dev inner loop can skip them with
-- @HASKELL_FLOWS_E2E_SKIP_SLOW=1@ to drop ~3 min of wall-time.
--
-- Main iterates this list, each iteration = one fresh tempdir +
-- one fresh in-process Server pointed at it.
scenarios :: [(T.Text, Bool, Client.McpClient -> FilePath -> IO [Assert.Check])]
scenarios =
  [ ( "Scenario: Arithmetic Expression Evaluator (15 steps)"
    , True, Expr.runExprScenario )
  , ( "Flow: Exploratory (type / info / eval / complete / goto / doc)"
    , False, FlowE.runFlow )
  , ( "Flow: Typed holes (hole → patch → clean)"
    , False, FlowH.runFlow )
  , ( "Flow: Refactor (rename happy + rollback + keyword-reject)"
    , False, FlowR.runFlow )
  , ( "Flow: Arbitrary templates (flat / sized / polymorphic)"
    , False, FlowA.runFlow )
  , ( "Flow: Scope mgmt (browse / imports / apply_exports / add_import)"
    , False, FlowS.runFlow )
  , ( "Flow: Batch composition (happy + fail_fast)"
    , False, FlowB.runFlow )
  , ( "Flow: Toolchain probes (status + warmup)"
    , False, FlowTC.runFlow )
  , ( "Flow: Bootstrap host rules (preview + write + 3 hosts)"
    , False, FlowBoot.runFlow )
  , ( "Flow: Validate cabal (clean + duplicate deps)"
    , False, FlowVC.runFlow )
  , ( "Flow: Property lifecycle (store inspection)"
    , False, FlowPL.runFlow )
  , ( "Flow: Workflow help/next (phase + state hints)"
    , False, FlowWH.runFlow )
  , ( "Flow: Quality gates (lint / format / check_module / check_project)"
    , False, FlowQG.runFlow )
  , ( "Flow: Fix warning (unused-import patch preview)"
    , False, FlowFW.runFlow )
  , ( "Flow: Coverage (cabal test --enable-coverage + HPC)"
    , True, FlowCov.runFlow )
  , ( "Flow: Mutation testing (bug-finding oracle for regression)"
    , False, FlowMut.runFlow )
  , ( "Flow: Refactor out-of-scope (refuse silent no-op)"
    , False, FlowROS.runFlow )
  , ( "Flow: Type breakage (check_module must flag type mismatch)"
    , False, FlowTB.runFlow )
  , ( "Flow: Injection guard (newline / sentinel / path traversal)"
    , False, FlowIG.runFlow )
  , ( "Flow: Graceful miss (deps remove / hole-free / non-predicate QC)"
    , False, FlowGM.runFlow )
  , ( "Flow: Session robustness (user throws don't kill GHCi)"
    , False, FlowSR.runFlow )
  , ( "Flow: Timeout enforcement (inner 30 s budget must trip)"
    , True, FlowTE.runFlow )
  , ( "Flow: GHCi SIGKILL (child exitWith · recovery via evictSession)"
    , True, FlowSK.runFlow )
  , ( "Flow: Oversized input (256 KiB expression rejected at boundary)"
    , False, FlowOI.runFlow )
  , ( "Flow: Non-UTF-8 source file (graceful load error)"
    , False, FlowNU.runFlow )
  , ( "Flow: Dependency conflict (bogus dep · loud failure · clean remove)"
    , False, FlowDC.runFlow )
  , ( "Flow: Sandbox escape / RCE contract (documents ghci_eval capabilities)"
    , False, FlowSE.runFlow )
  , ( "Flow: Concurrent clients (two MCP clients, same project dir)"
    , True, FlowCC.runFlow )
  , ( "Flow: Disk full / permission denied on property store"
    , True, FlowDF.runFlow )
  , ( "Flow: ANSI escape in GHC error message (JSON safety)"
    , False, FlowANSI.runFlow )
  , ( "Flow: Property store race (two clients, concurrent save)"
    , True, FlowPSR.runFlow )
  , ( "Flow: Expr evaluator dogfood (full 4-module library build + 3 bug pins)"
    , True, FlowEED.runFlow )
  , ( "Flow: Corpus transport (hostile JSON-RPC lines · subprocess)"
    , False, FlowCT.runFlow )
  , ( "Flow: Cross-validation (MCP check_project vs cabal build)"
    , True, FlowXV.runFlow )
  , ( "Flow: Regression scope fix (module resolve + scope restore)"
    , False, FlowRSF.runFlow )
  ]

main :: IO ()
main = do
  hSetBuffering stdout LineBuffering
  hSetBuffering stderr LineBuffering
  binary <- Client.findMcpBinaryPath

  -- Read the opt-in slow-skip flag before we start banner-printing
  -- so the operator knows which mode they're about to watch.
  mSkipRaw <- System.Environment.lookupEnv "HASKELL_FLOWS_E2E_SKIP_SLOW"
  let skipSlow = mSkipRaw == Just "1"
      selected
        | skipSlow  = filter (\(_, slow, _) -> not slow) scenarios
        | otherwise = scenarios
      totalCount   = length scenarios
      selCount     = length selected
      skippedCount = totalCount - selCount

  putStrLn "==> haskell-flows-mcp e2e"
  putStrLn ("==> binary: " <> binary)
  if skipSlow
    then putStrLn ("==> HASKELL_FLOWS_E2E_SKIP_SLOW=1 — running "
                   <> show selCount <> " of " <> show totalCount
                   <> " scenarios (" <> show skippedCount <> " slow-tagged skipped)")
    else putStrLn ("==> running all " <> show totalCount <> " scenarios")

  -- Layer 1 — transport smoke.
  Assert.beginSection "Transport smoke (subprocess, 1 round-trip)"
  t0 <- Assert.stepHeader 0 "initialize + initialized + tools/list"
  smoke <- Smoke.runSmoke binary
  _ <- Assert.liveCheck (Assert.Check
         { Assert.cName   = "binary answers tools/list with ≥ 1 tool"
         , Assert.cOk     = Smoke.srPassed smoke
         , Assert.cDetail = T.pack ("smoke log: " <> Smoke.srLog smoke)
         })
  Assert.stepFooter 0 t0

  if not (Smoke.srPassed smoke)
    then do
      putStrLn ""
      putStrLn ("FAIL: transport smoke failed (log: " <> Smoke.srLog smoke <> ")")
      exitFailure
    else do
      wallStart <- getPOSIXTime
      -- Layer 2 — run every selected scenario in order.
      checks <- concat <$> mapM (runScenario binary) selected
      wallEnd <- getPOSIXTime
      let secs   = realToFrac (wallEnd - wallStart) :: Double
          passed = length (filter Assert.cOk checks)
          total  = length checks
          fails  = filter (not . Assert.cOk) checks
      putStrLn ""
      putStrLn "════════════════════════════════════════════════════════"
      putStrLn ("  " <> show passed <> " / " <> show total
                 <> " checks passed in "
                 <> formatSecs secs <> " s")
      mapM_ (\c -> putStrLn ("  · FAIL " <> takeLine (show (Assert.cName c))))
            fails
      putStrLn "════════════════════════════════════════════════════════"
      if Assert.allPassed checks then exitSuccess else exitFailure
  where
    formatSecs :: Double -> String
    formatSecs x =
      let r      = round (x * 100) :: Int
          whole  = r `div` 100
          frac   = r `mod` 100
          fracS  = if frac < 10 then '0' : show frac else show frac
      in show whole <> "." <> fracS
    takeLine = takeWhile (/= '\n')

-- | One scenario run. Fresh tempdir, fresh client. Framework
-- errors get converted to a single synthetic Failed check so
-- the aggregate report stays coherent.
runScenario
  :: FilePath
  -> (T.Text, Bool, Client.McpClient -> FilePath -> IO [Assert.Check])
  -> IO [Assert.Check]
runScenario binary (label, _slow, go) = do
  Assert.beginSection label
  withTempProjectDir $ \dir -> do
    res <- try $ bracket
             (Client.newClient binary [("HASKELL_PROJECT_DIR", dir)])
             Client.close
             (`go` dir)
    case res of
      Left (e :: SomeException) ->
        pure [ Assert.Check
                 { Assert.cName   = label <> " · framework error"
                 , Assert.cOk     = False
                 , Assert.cDetail = T.pack (show e)
                 }
             ]
      Right cs -> pure cs

-- | Fresh temp project dir per scenario.
withTempProjectDir :: (FilePath -> IO a) -> IO a
withTempProjectDir k = do
  base <- getTemporaryDirectory
  ts   <- getPOSIXTime
  let dir = base </> ("haskell-flows-e2e-" <> show (floor (ts * 1000000) :: Int))
  bracket
    (createDirectoryIfMissing True dir >> pure dir)
    removePathForcibly
    k
