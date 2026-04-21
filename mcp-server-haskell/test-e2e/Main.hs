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
import System.Exit (exitFailure, exitSuccess)
import System.FilePath ((</>))
import System.IO (BufferMode (..), hSetBuffering, stderr, stdout)

import qualified E2E.Assert as Assert
import qualified E2E.Client as Client
import qualified E2E.Smoke  as Smoke
import qualified Scenarios.ExprEvaluator        as Expr
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
--   @(label, runFlow :: McpClient -> FilePath -> IO [Check])@
--
-- Main iterates this list, each iteration = one fresh tempdir +
-- one fresh in-process Server pointed at it.
scenarios :: [(T.Text, Client.McpClient -> FilePath -> IO [Assert.Check])]
scenarios =
  [ ( "Scenario: Arithmetic Expression Evaluator (15 steps)"
    , Expr.runExprScenario )
  , ( "Flow: Exploratory (type / info / eval / complete / goto / doc)"
    , FlowE.runFlow )
  , ( "Flow: Typed holes (hole → patch → clean)"
    , FlowH.runFlow )
  , ( "Flow: Refactor (rename happy + rollback + keyword-reject)"
    , FlowR.runFlow )
  , ( "Flow: Arbitrary templates (flat / sized / polymorphic)"
    , FlowA.runFlow )
  , ( "Flow: Scope mgmt (browse / imports / apply_exports / add_import)"
    , FlowS.runFlow )
  , ( "Flow: Batch composition (happy + fail_fast)"
    , FlowB.runFlow )
  , ( "Flow: Toolchain probes (status + warmup)"
    , FlowTC.runFlow )
  , ( "Flow: Bootstrap host rules (preview + write + 3 hosts)"
    , FlowBoot.runFlow )
  , ( "Flow: Validate cabal (clean + duplicate deps)"
    , FlowVC.runFlow )
  , ( "Flow: Property lifecycle (store inspection)"
    , FlowPL.runFlow )
  , ( "Flow: Workflow help/next (phase + state hints)"
    , FlowWH.runFlow )
  , ( "Flow: Quality gates (lint / format / check_module / check_project)"
    , FlowQG.runFlow )
  , ( "Flow: Fix warning (unused-import patch preview)"
    , FlowFW.runFlow )
  , ( "Flow: Coverage (cabal test --enable-coverage + HPC)"
    , FlowCov.runFlow )
  , ( "Flow: Mutation testing (bug-finding oracle for regression)"
    , FlowMut.runFlow )
  , ( "Flow: Refactor out-of-scope (refuse silent no-op)"
    , FlowROS.runFlow )
  , ( "Flow: Type breakage (check_module must flag type mismatch)"
    , FlowTB.runFlow )
  , ( "Flow: Injection guard (newline / sentinel / path traversal)"
    , FlowIG.runFlow )
  , ( "Flow: Graceful miss (deps remove / hole-free / non-predicate QC)"
    , FlowGM.runFlow )
  , ( "Flow: Session robustness (user throws don't kill GHCi)"
    , FlowSR.runFlow )
  , ( "Flow: Timeout enforcement (inner 30 s budget must trip)"
    , FlowTE.runFlow )
  , ( "Flow: GHCi SIGKILL (child exitWith · recovery via evictSession)"
    , FlowSK.runFlow )
  , ( "Flow: Oversized input (256 KiB expression rejected at boundary)"
    , FlowOI.runFlow )
  , ( "Flow: Non-UTF-8 source file (graceful load error)"
    , FlowNU.runFlow )
  , ( "Flow: Dependency conflict (bogus dep · loud failure · clean remove)"
    , FlowDC.runFlow )
  , ( "Flow: Sandbox escape / RCE contract (documents ghci_eval capabilities)"
    , FlowSE.runFlow )
  , ( "Flow: Concurrent clients (two MCP clients, same project dir)"
    , FlowCC.runFlow )
  , ( "Flow: Disk full / permission denied on property store"
    , FlowDF.runFlow )
  , ( "Flow: Expr evaluator dogfood (full 4-module library build + 3 bug pins)"
    , FlowEED.runFlow )
  , ( "Flow: Corpus transport (hostile JSON-RPC lines · subprocess)"
    , FlowCT.runFlow )
  , ( "Flow: Cross-validation (MCP check_project vs cabal build)"
    , FlowXV.runFlow )
  , ( "Flow: Regression scope fix (module resolve + scope restore)"
    , FlowRSF.runFlow )
  ]

main :: IO ()
main = do
  hSetBuffering stdout LineBuffering
  hSetBuffering stderr LineBuffering
  binary <- Client.findMcpBinaryPath
  putStrLn "==> haskell-flows-mcp e2e"
  putStrLn ("==> binary: " <> binary)

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
      -- Layer 2 — run every scenario in order.
      checks <- concat <$> mapM (runScenario binary) scenarios
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
  -> (T.Text, Client.McpClient -> FilePath -> IO [Assert.Check])
  -> IO [Assert.Check]
runScenario binary (label, go) = do
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
