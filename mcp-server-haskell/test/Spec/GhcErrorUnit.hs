-- | Unit tests for ExplainError text-mode, Perf samples/regression,
-- DepsListAll stanzas, Move module-name-to-path, EvalIO unit helpers,
-- stdout capture, Load merge-diags, toolchain warmup gates, workflow
-- phase classification, and DepsExplain rejection splitting.
--
-- Extracted from the Spec.hs monolith (#271) via the function-export shape.
module Spec.GhcErrorUnit
  ( testExplainErrorNoModuleSource
  , testExplainErrorTextUsed
  , testPerfSamplesGated
  , testPerfRegressionCausePlain
  , testDepsListAllStanzas
  , testMoveModuleNameToPath
  , testEvalIoUnitResult
  , testEvalIoUnitHintNotCircular
  , testCaptureStdoutActuallyCaptures
  , testEvalIOUnitCaptureViaSess
  , testLoadMergeDiagsPreferDeferred
  , testWarmupIncludesGates
  , testClassifyPhaseNoLoad
  , testEnclosingRangePadding
  , testDepsExplainRejectionSplit
  ) where

import qualified Data.Aeson as A
import Data.Aeson (Value, object, (.=))
import qualified Data.Aeson.Key as AKey
import qualified Data.Aeson.KeyMap as AKM
import qualified Data.Text as T
import qualified Data.Text.IO as TIO
import Control.Exception (SomeException, try)
import System.Directory (getTemporaryDirectory)
import GHC
  ( InteractiveImport (IIDecl)
  , mkModuleName
  , setContext
  , simpleImportDecl
  )

import qualified HaskellFlows.Mcp.Envelope as Env
import qualified HaskellFlows.Mcp.WorkflowState as WS
import HaskellFlows.Mcp.ToolName (ToolName (..))
import HaskellFlows.Ghc.ApiSession
  ( captureStdout
  , evalIOUnitCapture
  , killGhcSession
  , startGhcSession
  , withGhcSession
  )
import HaskellFlows.Data.PropertyStore (StoredProperty (..))
import HaskellFlows.Parser.Error (GhcError (..), Severity (..))
import HaskellFlows.Types (mkProjectDir)
import qualified HaskellFlows.Tool.Deps as DepsTool
import qualified HaskellFlows.Tool.DepsExplain as DepsExplain
import qualified HaskellFlows.Tool.Eval as EvalTool
import qualified HaskellFlows.Tool.ExplainError as ExplainError
import qualified HaskellFlows.Tool.Load as LoadTool
import qualified HaskellFlows.Tool.Move as MoveTool
import qualified HaskellFlows.Tool.Perf as PerfTool
import qualified HaskellFlows.Tool.ToolchainWarmup as ToolchainWarmupTool
import qualified HaskellFlows.Tool.Workflow as WorkflowTool

import Spec.Extract (mkErr, mkWarn)
import Spec.Helpers (runToolEnvelope, withTempProject)

-- | F-25: 'ghc_explain_error' context must not include 'module_source'
-- alongside 'enclosing_slice' — they were byte-identical for small
-- files, doubling the payload size for no benefit. Verify the field
-- is absent from the rendered JSON.
testExplainErrorNoModuleSource :: IO Bool
testExplainErrorNoModuleSource = do
  let body = T.unlines ["module Foo where", "foo :: Int", "foo = _"]
      diag  = GhcError
        { geFile     = "src/Foo.hs"
        , geLine     = 3
        , geColumn   = 7
        , geSeverity = SevError
        , geCode     = Nothing
        , geMessage  = "Found hole: _ :: Int"
        }
      result = ExplainError.renderContext "src/Foo.hs" body diag [diag] Nothing
  pure $ case Env.reResult result of
    Just (A.Object r) ->
      case AKM.lookup (AKey.fromText "context") r of
        Just (A.Object ctx) -> not (AKM.member "module_source" ctx)
        _                   -> False
    _ -> False

-- | #153: when error_text is provided, renderContext must use that
-- text as the diagnostic message — not fall through to recompilation.
-- We test the pure 'syntheticError' + 'renderContext' path directly.
testExplainErrorTextUsed :: IO Bool
testExplainErrorTextUsed = do
  let body    = T.unlines ["module Foo where", "foo :: Int", "foo = 42"]
      errTxt  = "Couldn't match expected type 'Int' with actual type 'Bool'"
      -- Simulate the path that handle takes when error_text is supplied
      synDiag = ExplainError.syntheticError "src/Foo.hs" errTxt
      result  = ExplainError.renderContext "src/Foo.hs" body synDiag [synDiag] Nothing
  pure $ case Env.reResult result of
    Just (A.Object r) ->
      case AKM.lookup (AKey.fromText "diagnostic") r of
        Just (A.Object d) ->
          AKM.lookup (AKey.fromText "message") d == Just (A.String errTxt)
        _ -> False
    _ -> False

-- | F-26: when 'verbose=false' (default), the 'measurements' object
-- must not contain a 'samples' key — sending thousands of integers
-- for large 'runs' values is wasteful.
testPerfSamplesGated :: IO Bool
testPerfSamplesGated =
  let args = PerfTool.PerfArgs
               { PerfTool.paExpression      = "1 + 1"
               , PerfTool.paRuns            = 5
               , PerfTool.paSaveBaseline    = False
               , PerfTool.paCompareBaseline = False
               , PerfTool.paVerbose         = False
               , PerfTool.paThresholdPct    = 30.0
               }
      nss   = [100, 110, 90, 105, 95]
      stats = PerfTool.aggregate nss
      result = PerfTool.renderResult args nss stats [] Nothing 0
  in pure $ case Env.reResult result of
       Just (A.Object r) ->
         case AKM.lookup (AKey.fromText "measurements") r of
           Just (A.Object m) -> not (AKM.member "samples" m)
           _                 -> False
       _ -> False

-- | F-32: when a regression is detected, 'error.cause' must be a
-- plain human-readable string, not a stringified JSON blob (no
-- literal escaped braces or quotes in the cause field).
testPerfRegressionCausePlain :: IO Bool
testPerfRegressionCausePlain =
  let args = PerfTool.PerfArgs
               { PerfTool.paExpression      = "1 + 1"
               , PerfTool.paRuns            = 5
               , PerfTool.paSaveBaseline    = False
               , PerfTool.paCompareBaseline = True
               , PerfTool.paVerbose         = False
               , PerfTool.paThresholdPct    = 30.0
               }
      nss      = [1_000_000, 1_100_000, 900_000, 1_050_000, 950_000]
      stats    = PerfTool.aggregate nss
      baseline = Just (PerfTool.BaselineEntry { PerfTool.beMeanNs = 1000.0 })
      result   = PerfTool.renderResult args nss stats [] baseline 0
  in pure $ case Env.reError result of
       Just err ->
         case Env.eeCause err of
           Just cause ->
             not (T.isInfixOf "{" cause) && T.isInfixOf "baseline_mean_ns" cause
           _ -> False
       _ -> False

-- | F-08: 'ghc_deps list' without a stanza selector must return all
-- stanzas as a structured @{stanzas: {library: [...], ...}}@ map
-- rather than only the first 'build-depends' block.
testDepsListAllStanzas :: IO Bool
testDepsListAllStanzas =
  let cabal = T.unlines
        [ "cabal-version: 3.0"
        , "name:          mylib"
        , "library"
        , "    build-depends: base, text"
        , "test-suite mylib-test"
        , "    type:          exitcode-stdio-1.0"
        , "    build-depends: base, QuickCheck"
        ]
      stanzas = DepsTool.allStanzaDeps cabal
  in pure $  any (\(k, _) -> k == "library")        stanzas
          && any (\(k, _) -> "test-suite" `T.isPrefixOf` k) stanzas

-- | F-34: 'moduleNameToPath' must not mangle file paths that already
-- contain slashes or end with .hs.  Before the fix, passing
-- @"src/HaskellFlows/Util.hs"@ produced @"src/src/HaskellFlows/Util/hs.hs"@.
testMoveModuleNameToPath :: IO Bool
testMoveModuleNameToPath = pure $
     MoveTool.moduleNameToPath "Foo.Bar"           == "src/Foo/Bar.hs"
  && MoveTool.moduleNameToPath "src/Foo/Bar.hs"    == "src/Foo/Bar.hs"
  && MoveTool.moduleNameToPath "app/Main.hs"       == "app/Main.hs"
  && MoveTool.moduleNameToPath "Foo/Bar.hs"        == "src/Foo/Bar.hs"
  && MoveTool.moduleNameToPath "Foo/Bar"           == "src/Foo/Bar.hs"

-- | F-12: 'ioUnitResult' must carry @kind = "io_unit_no_output"@ so
-- agents know the expression was @IO ()@ and did execute (no silent
-- empty-string confusion from the old unsafeCoerce path).
testEvalIoUnitResult :: IO Bool
testEvalIoUnitResult =
  let result = EvalTool.ioUnitResult
  in pure $ case Env.reResult result of
       Just (A.Object r) ->
         AKM.lookup (AKey.fromText "kind") r == Just (A.String "io_unit_no_output")
       _ -> False

-- | #167: The old 'ioUnitResult' hint said "Use putStrLn / print for
-- visible output" — circular advice when the user already called
-- putStrLn and got @output: ""@. The new hint must NOT contain the
-- phrase "Use putStrLn" (or the equivalent "use putStrLn") and must
-- instead explain that the action ran but produced no stdout output.
testEvalIoUnitHintNotCircular :: IO Bool
testEvalIoUnitHintNotCircular =
  let result = EvalTool.ioUnitResult
  in pure $ case Env.reResult result of
       Just (A.Object r) ->
         case AKM.lookup (AKey.fromText "hint") r of
           Just (A.String hint) ->
             -- Must NOT contain circular "Use putStrLn for visible output"
             not (T.isInfixOf "Use putStrLn / print for visible output" hint)
             -- Must still describe the IO () execution
             && T.isInfixOf "IO ()" hint
           _ -> False
       _ -> False

-- | #182: 'captureStdout' must actually capture output written to
-- 'System.IO.stdout'. The previous 'hDuplicate'/'hDuplicateTo'
-- implementation silently returned empty string in the MCP runtime
-- (stdout is a pipe to the JSON-RPC transport) due to Handle-level FD
-- aliasing on the restore call. The new POSIX pipe implementation must
-- return the expected string.
testCaptureStdoutActuallyCaptures :: IO Bool
testCaptureStdoutActuallyCaptures = do
  -- Test captureStdout directly — no GHC session needed; it's a plain
  -- IO capture utility. putStrLn is already in scope.
  out <- captureStdout (putStrLn "hello")
  pure (out == "hello\n")

-- | #194: 'evalIOUnitCapture' must capture output even when the IO ()
-- action is compiled at runtime by the GHC API interpreter — not just
-- when a direct Haskell action is passed to 'captureStdout'. Exercises
-- the complete path used by 'ghc_eval("putStrLn \"hello\"")'.
testEvalIOUnitCaptureViaSess :: IO Bool
testEvalIOUnitCaptureViaSess = case mkProjectDir "/tmp" of
  Left _   -> pure False
  Right pd -> do
    sess   <- startGhcSession pd
    eOut   <- try (withGhcSession sess $ do
                -- Import both Prelude AND System.IO — the latter is needed
                -- because evalIOUnitCapture wraps the stmt with
                -- "System.IO.hFlush System.IO.stdout" (issue #194 fix).
                setContext
                  [ IIDecl (simpleImportDecl (mkModuleName "Prelude"))
                  , IIDecl (simpleImportDecl (mkModuleName "System.IO"))
                  ]
                evalIOUnitCapture "putStrLn \"hello-from-eval\"")
              :: IO (Either SomeException String)
    killGhcSession sess
    pure (case eOut of
            Right s -> s == "hello-from-eval\n"
            Left  _ -> False)

-- | F-23: 'mergeDiags' must prefer the deferred (warning-severity)
-- version when both passes report a diagnostic at the same position.
-- Before the fix it kept the strict error version, causing typed holes
-- to show up as plain errors rather than informative hole-fit warnings.
testLoadMergeDiagsPreferDeferred :: IO Bool
testLoadMergeDiagsPreferDeferred =
  let strictD   = [mkErr  "Foo.hs" 10 5 "error at hole"]
      deferredD = [mkWarn "Foo.hs" 10 5 "Found hole: _ :: Int"]
      merged    = LoadTool.mergeDiags strictD deferredD
  in pure $
       length merged == 1
       && T.isInfixOf "Found hole" (geMessage (head merged))

-- | F-04: 'ghc_toolchain(warmup)' must include a @gates@ field and
-- a top-level @gates_warm@ boolean so agents can distinguish gate
-- availability from optional-binary availability.
testWarmupIncludesGates :: IO Bool
testWarmupIncludesGates = do
  decoded <- runToolEnvelope ToolchainWarmupTool.handle (A.object [])
  pure $ case decoded of
    Right env ->
      case Env.reResult env of
        Just (A.Object payload) ->
          AKM.member (AKey.fromText "gates")      payload &&
          AKM.member (AKey.fromText "gates_warm") payload
        _ -> False
    Left _ -> False

-- | F-01: 'classifyPhase' must stay 'PhasePreScaffold' after many
-- tool calls with no load ever attempted.  Before the fix the
-- @wsToolCalls < 3@ guard caused it to fall through to
-- 'PhaseDeveloping' after the 3rd call.
testClassifyPhaseNoLoad :: IO Bool
testClassifyPhaseNoLoad = do
  ref <- WS.newWorkflowStateRef
  let anyPayload = A.object [ "success" .= True ]
  -- 5 read-only calls (workflow status, toolchain, etc.) — no load.
  mapM_ (\_ -> WS.trackTool ref GhcWorkflow True anyPayload) [1..5 :: Int]
  s <- WS.readState ref
  pure (WS.classifyPhase s == WS.PhasePreScaffold)

-- | F-24: with padding=15, 'enclosingLineRange' should not return
-- the whole file for a 28-line module (error at line 10).
testEnclosingRangePadding :: IO Bool
testEnclosingRangePadding =
  let (lo, hi) = ExplainError.enclosingLineRange 28 15 10
  in pure $ (hi - lo) < 28  -- must be smaller than the whole file

-- | F-09: 'parseRejections' must split comma-separated package
-- versions into separate 'Rejection' entries so 'rejection_count'
-- is accurate.
testDepsExplainRejectionSplit :: IO Bool
testDepsExplainRejectionSplit =
  let line = "[__1] rejecting: QuickCheck-2.14.2, QuickCheck-2.14.1 (conflict: text)"
      rs   = DepsExplain.parseRejections line
  in pure $
       length rs == 2
       && DepsExplain.rPackage (head rs) == "QuickCheck-2.14.2"
       && DepsExplain.rPackage (rs !! 1) == "QuickCheck-2.14.1"
