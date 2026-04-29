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

import qualified Control.Concurrent
import Control.Concurrent.Async (forConcurrently)
import Control.Concurrent.QSem (newQSem, signalQSem, waitQSem)
import Control.Exception (bracket, bracket_, try, SomeException)
import Control.Monad (when)
import qualified Data.List as List
import qualified Data.Text as T
import Data.IORef (IORef, atomicModifyIORef', newIORef)
import Data.Time.Clock.POSIX (getPOSIXTime)
import System.IO.Unsafe (unsafePerformIO)
import GHC.Conc (getNumCapabilities)
import System.Directory (createDirectoryIfMissing, getTemporaryDirectory, removePathForcibly)
import qualified System.Environment
import System.Exit (exitFailure, exitSuccess)
import System.FilePath ((</>))
import System.IO (BufferMode (..), hSetBuffering, stderr, stdout)
import Text.Read (readMaybe)

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
import qualified Scenarios.FlowDogfoodUxFixes   as FlowDUX
import qualified Scenarios.FlowFixWarning       as FlowFW
import qualified Scenarios.FlowGracefulMiss      as FlowGM
import qualified Scenarios.FlowInjectionGuard   as FlowIG
import qualified Scenarios.FlowNonUTF8           as FlowNU
import qualified Scenarios.FlowOversizedInput   as FlowOI
import qualified Scenarios.FlowSandboxEscape     as FlowSE
import qualified Scenarios.FlowTimeoutEnforcement as FlowTE
import qualified Scenarios.FlowModuleNameGuard   as FlowMNG
import qualified Scenarios.FlowMutation          as FlowMut
import qualified Scenarios.FlowPropertyLifecycle as FlowPL
import qualified Scenarios.FlowPropertyStoreRace as FlowPSR
import qualified Scenarios.FlowQuickCheckExportImports as FlowQcExp
import qualified Scenarios.FlowRegressionLoadFailure as FlowRegLF
import qualified Scenarios.FlowQualityGates     as FlowQG
import qualified Scenarios.FlowRefactor         as FlowR
import qualified Scenarios.FlowRefactorAdversarial as FlowRAdv
import qualified Scenarios.FlowRefactorOutOfScope as FlowROS
import qualified Scenarios.FlowRegressionScopeFix as FlowRSF
import qualified Scenarios.FlowSessionRobustness as FlowSR
import qualified Scenarios.FlowSwitchProject    as FlowSwP
import qualified Scenarios.FlowSwitchProjectStore as FlowSwPS
import qualified Scenarios.FlowCabalRecovery     as FlowCR
import qualified Scenarios.FlowRefactorPreExistingError as FlowRPE
import qualified Scenarios.FlowSuggestAssocOuter  as FlowSAO
import qualified Scenarios.FlowAddImportNoHoogle  as FlowAINH
import qualified Scenarios.FlowInfoConstructors   as FlowIC
import qualified Scenarios.FlowLoadHoleDiagnostics as FlowLHD
import qualified Scenarios.FlowGatesProperties    as FlowGP
import qualified Scenarios.FlowCreateProjectNameValidation as FlowCPV
import qualified Scenarios.FlowBootstrapDocs      as FlowBD
import qualified Scenarios.FlowFixWarningUnusedBinding as FlowFWUB
import qualified Scenarios.FlowRemoveModulesDownstream as FlowRMD
import qualified Scenarios.FlowMoveSymbol         as FlowMS
import qualified Scenarios.FlowDogfoodReplay    as FlowDFR
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
  , ( "Flow: Refactor adversarial (collision / extract / bad-scope / missing)"
    , False, FlowRAdv.runFlow )
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
  , ( "Flow: Module-name guard (ISSUE-47 · invalid module / export names refused)"
    , False, FlowMNG.runFlow )
  , ( "Flow: Graceful miss (deps remove / hole-free / non-predicate QC)"
    , False, FlowGM.runFlow )
  , ( "Flow: Session robustness (user throws don't kill GHCi)"
    , False, FlowSR.runFlow )
  , ( "Flow: Timeout enforcement (inner 30 s budget must trip)"
    , True, FlowTE.runFlow )
  , ( "Flow: Oversized input (256 KiB expression rejected at boundary)"
    , False, FlowOI.runFlow )
  , ( "Flow: Non-UTF-8 source file (graceful load error)"
    , False, FlowNU.runFlow )
  , ( "Flow: Dependency conflict (bogus dep · loud failure · clean remove)"
    , False, FlowDC.runFlow )
  , ( "Flow: QuickCheck export imports (#40 · self-import + lib widen)"
    , False, FlowQcExp.runFlow )
  , ( "Flow: Regression load_failed (#51 · scope failure ≠ regression)"
    , False, FlowRegLF.runFlow )
  , ( "Flow: Sandbox escape / RCE contract (documents ghc_eval capabilities)"
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
  , ( "Flow: Switch project (runtime projectDir swap + isolation)"
    , False, FlowSwP.runFlow )
  , ( "Flow: Switch project store reopen (#39 · property store isolation)"
    , False, FlowSwPS.runFlow )
  , ( "Flow: Cabal recovery (#49 · external edit picked up by non-load tool)"
    , False, FlowCR.runFlow )
  , ( "Flow: Refactor pre-existing-error (#50 · diagnostic-diff verify)"
    , False, FlowRPE.runFlow )
  , ( "Flow: Suggest Associative outer call (#52 · template type-checks)"
    , False, FlowSAO.runFlow )
  , ( "Flow: AddImport no-hoogle (#53 · honest error vs lying success)"
    , False, FlowAINH.runFlow )
  , ( "Flow: Info constructors (#54 · data + newtype expose ctors)"
    , False, FlowIC.runFlow )
  , ( "Flow: Load hole diagnostics (#57 · drop GHC-58427 artifact)"
    , False, FlowLHD.runFlow )
  , ( "Flow: Gates properties (#42 · status discriminator)"
    , False, FlowGP.runFlow )
  , ( "Flow: CreateProject name validation (#58 · Hackage rules)"
    , False, FlowCPV.runFlow )
  , ( "Flow: Bootstrap docs (#56 · in-process model, no retired vocab)"
    , False, FlowBD.runFlow )
  , ( "Flow: FixWarning unused binding (#55 · GHC-40910 patch + fixable)"
    , False, FlowFWUB.runFlow )
  , ( "Flow: RemoveModules downstream check (#41 · refuse without force)"
    , False, FlowRMD.runFlow )
  , ( "Flow: Move symbol cross-module (#62 · slice + rewrite + verify)"
    , True, FlowMS.runFlow )
  , ( "Flow: Dogfood replay (7 MCP fixes round-tripped through one session)"
    , True, FlowDFR.runFlow )
  , ( "Flow: Dogfood UX fixes (6 polish items from the expr-evaluator session)"
    , False, FlowDUX.runFlow )
  ]

main :: IO ()
main = do
  hSetBuffering stdout LineBuffering
  hSetBuffering stderr LineBuffering
  binary <- Client.findMcpBinaryPath

  -- Read the opt-in slow-skip flag before we start banner-printing
  -- so the operator knows which mode they're about to watch.
  mSkipRaw <- System.Environment.lookupEnv "HASKELL_FLOWS_E2E_SKIP_SLOW"
  mParRaw  <- System.Environment.lookupEnv "HASKELL_FLOWS_E2E_PARALLEL"
  mOnlyRaw <- System.Environment.lookupEnv "HASKELL_FLOWS_E2E_ONLY"
  numCaps  <- getNumCapabilities
  let skipSlow = mSkipRaw == Just "1"
      -- Parallel e2e is OPT-IN via HASKELL_FLOWS_E2E_PARALLEL=<N>.
      -- Default stays sequential: the Linux CI scheduler dispatches
      -- threads fast enough to surface races in scenarios that
      -- weren't authored with parallelism in mind (shared
      -- property-store locks, cabal build contention, test-suite
      -- tmp-dir collisions). Local `HASKELL_FLOWS_E2E_PARALLEL=4`
      -- still gets the ~15× speedup on macOS dev machines.
      parallelism = case mParRaw >>= readMaybe of
        Just k | k >= 1 -> min k numCaps
        _              -> 1
      -- Case-insensitive substring filter on scenario label.
      -- Empty string / unset env var = no filter. Lets the
      -- inner-loop pattern be: pick one red, fix, re-run only
      -- that one in ~20 s instead of ~200 s.
      mOnly = case mOnlyRaw of
        Just s | not (null s) -> Just (T.toLower (T.pack s))
        _                     -> Nothing
      matchOnly label = case mOnly of
        Nothing     -> True
        Just needle -> needle `T.isInfixOf` T.toLower label
      afterOnly
        | Just _ <- mOnly = filter (\(lbl, _, _) -> matchOnly lbl) scenarios
        | otherwise       = scenarios
      selected
        | skipSlow  = filter (\(_, slow, _) -> not slow) afterOnly
        | otherwise = afterOnly
      totalCount   = length scenarios
      selCount     = length selected
      skippedCount = length afterOnly - selCount

  putStrLn "==> haskell-flows-mcp e2e"
  putStrLn ("==> binary: " <> binary)
  case mOnly of
    Nothing -> pure ()
    Just n  -> putStrLn ("==> HASKELL_FLOWS_E2E_ONLY=" <> T.unpack n
                         <> " — running " <> show (length afterOnly)
                         <> " of " <> show totalCount <> " scenarios (substring match)")
  if skipSlow
    then putStrLn ("==> HASKELL_FLOWS_E2E_SKIP_SLOW=1 — running "
                   <> show selCount <> " of " <> show totalCount
                   <> " scenarios (" <> show skippedCount <> " slow-tagged skipped)")
    else putStrLn ("==> running "
                   <> show selCount <> " of " <> show totalCount <> " scenarios")
  when (parallelism > 1) $
    putStrLn ("==> HASKELL_FLOWS_E2E_PARALLEL=" <> show parallelism
              <> " (capabilities=" <> show numCaps <> ")")

  -- Layer 1 — transport smoke. One subprocess round-trip exercises
  -- every JSON-RPC method advertised in the 'RpcMethod' ADT, plus
  -- the unknown-method fallback. Each contract is a separate check
  -- so a regression in one path is attributable on its own.
  Assert.beginSection "Transport smoke (subprocess, 7 RPC methods)"
  t0 <- Assert.stepHeader 0 "initialize + tools + resources + notifications + unknown"
  smoke <- Smoke.runSmoke binary
  let logT = T.pack ("smoke log: " <> Smoke.srLog smoke)
  _ <- Assert.liveCheck (Assert.Check
         { Assert.cName   = "rpc · initialize handshake answered"
         , Assert.cOk     = Smoke.srInitializeOk smoke
         , Assert.cDetail = logT
         })
  _ <- Assert.liveCheck (Assert.Check
         { Assert.cName   = "rpc · initialized notification produced no response"
         , Assert.cOk     = Smoke.srInitializedNoResponse smoke
         , Assert.cDetail = logT
         })
  _ <- Assert.liveCheck (Assert.Check
         { Assert.cName   = "rpc · tools/list advertises ≥ 1 tool"
         , Assert.cOk     = Smoke.srToolsListOk smoke
         , Assert.cDetail = logT
         })
  _ <- Assert.liveCheck (Assert.Check
         { Assert.cName   = "rpc · resources/list advertises workflow-rules URI"
         , Assert.cOk     = Smoke.srResourcesListOk smoke
         , Assert.cDetail = logT
         })
  _ <- Assert.liveCheck (Assert.Check
         { Assert.cName   = "rpc · resources/read returns workflow-rules content"
         , Assert.cOk     = Smoke.srResourcesReadOk smoke
         , Assert.cDetail = logT
         })
  _ <- Assert.liveCheck (Assert.Check
         { Assert.cName   = "rpc · notifications/cancelled produced no response"
         , Assert.cOk     = Smoke.srCancelNoResponse smoke
         , Assert.cDetail = logT
         })
  _ <- Assert.liveCheck (Assert.Check
         { Assert.cName   = "rpc · unknown method returns -32601 error"
         , Assert.cOk     = Smoke.srUnknownMethodErr smoke
         , Assert.cDetail = logT
         })
  _ <- Assert.liveCheck (Assert.Check
         { Assert.cName   = "rpc · binary answers tools/list with ≥ 1 tool (legacy aggregate)"
         , Assert.cOk     = Smoke.srPassed smoke
         , Assert.cDetail = logT
         })
  Assert.stepFooter 0 t0

  if not (Smoke.srPassed smoke)
    then do
      putStrLn ""
      putStrLn ("FAIL: transport smoke failed (log: " <> Smoke.srLog smoke <> ")")
      exitFailure
    else do
      wallStart <- getPOSIXTime
      -- Layer 2 — scheduler:
      -- * Sequential (parallelism=1, default): 'mapM' in declared order.
      -- * Parallel (parallelism>=2): fast scenarios run through a QSem
      --   pool of the given width; slow scenarios (marked 'isSlow')
      --   stay sequential. Slow scenarios create fresh cabal projects
      --   that resolve deps through the shared ~/.cabal/store and
      --   contend heavily under parallel spawn — best to let them run
      --   one at a time even when parallelism is enabled.
      let (slowOnes, fastOnes) = List.partition (\(_, s, _) -> s) selected
      fastChecks <- if parallelism > 1
        then concat <$> runPool parallelism binary fastOnes
        else concat <$> mapM (runScenario binary) fastOnes
      slowChecks <- concat <$> mapM (runScenario binary) slowOnes
      let checks = fastChecks <> slowChecks
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
      mapM_ (\c -> do
                let detail = T.unpack (Assert.cDetail c)
                    name   = takeLine (show (Assert.cName c))
                putStrLn ("  · FAIL " <> name)
                when (not (null detail) && "framework error" `List.isInfixOf` name) $
                  putStrLn ("       → " <> takeLine detail))
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

-- | 'QSem'-bounded pool for parallel scenario execution. Each
-- scenario still owns its own tempdir + MCP server — the pool just
-- caps the concurrent spawn count so we don't overwhelm the cabal
-- store, the file system, or the GHC runtime.
runPool
  :: Int
  -> FilePath
  -> [(T.Text, Bool, Client.McpClient -> FilePath -> IO [Assert.Check])]
  -> IO [[Assert.Check]]
runPool bound binary xs = do
  sem <- newQSem bound
  forConcurrently xs $ \s ->
    bracket_ (waitQSem sem) (signalQSem sem) (runScenario binary s)

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
      Left (e :: SomeException) -> do
        -- Dump the exception inline so batched runs don't lose the
        -- root cause to the summary's name-only listing.
        putStrLn ("  [fx] framework error: " <> show e)
        pure [ Assert.Check
                 { Assert.cName   = label <> " · framework error"
                 , Assert.cOk     = False
                 , Assert.cDetail = T.pack (show e)
                 }
             ]
      Right cs -> pure cs

-- | Fresh temp project dir per scenario. Suffix mixes POSIX time,
-- 'ThreadId', AND a process-global atomic counter so parallel
-- scenarios that land on the same microsecond still get distinct
-- paths — without the counter, two parallel scenarios could
-- collide, stomp on each other's fixtures, and surface as
-- "file does not exist" mid-run under HASKELL_FLOWS_E2E_PARALLEL>=2.
withTempProjectDir :: (FilePath -> IO a) -> IO a
withTempProjectDir k = do
  base <- getTemporaryDirectory
  ts   <- getPOSIXTime
  tid  <- Control.Concurrent.myThreadId
  n    <- atomicTempCounter
  let tidTag = filter (\c -> c /= ' ' && c /= '(' && c /= ')')
                      (show tid)   -- e.g. "ThreadId42"
      dir = base </> ("haskell-flows-e2e-" <> show (floor (ts * 1000000) :: Int)
                       <> "-" <> tidTag <> "-" <> show n)
  bracket
    (createDirectoryIfMissing True dir >> pure dir)
    removePathForcibly
    k

-- | Process-global counter used to break tempdir-name ties. Each
-- 'withTempProjectDir' call bumps the counter atomically, so two
-- scenarios that land in the same microsecond AND somehow share a
-- 'ThreadId' (shouldn't happen but defensive) still get unique paths.
{-# NOINLINE tempCounter #-}
tempCounter :: IORef Int
tempCounter = unsafePerformIO (newIORef 0)

atomicTempCounter :: IO Int
atomicTempCounter = atomicModifyIORef' tempCounter (\n -> (n + 1, n))
