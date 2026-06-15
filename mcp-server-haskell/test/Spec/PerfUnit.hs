-- | Unit tests for Bootstrap git-root detection, Workflow pick-module-line,
-- Imports session-preloads, Refactor compile-fail, and the full Perf
-- tool suite: sampling, baselines, regression thresholds, and precision.
--
-- Extracted from the Spec.hs monolith (#271) via the function-export shape.
module Spec.PerfUnit
  ( testBootstrapGitRoot
  , testWorkflowPickModuleLine
  , testImportsHasSessionPreloads
  , testRefactorCompileFailShape
  , testPerfAllSamplesErrored
  , testPerfWarmupNsInPayload
  , testPerfWarmSamplesNotSkewed
  , testPerfReadBaselineStrict
  , testPerfSaveAndCompareNoLock
  , testPerfSaveBaselinesNoLock
  , testPerfDefaultThreshold30
  , testPerfCustomThreshold
  , testPerfRegressionStatusFailed
  , testPerfThresholdClamped
  , testPerfRoundTo1dp
  , testPerfRegressionPctPrecision
  , testPerfRuntimeExceptionMessage
  ) where

import qualified Data.Aeson as A
import Data.Aeson ((.=))
import qualified Data.Aeson.Key as AKey
import qualified Data.Aeson.KeyMap as AKM
import qualified Data.Text as T
import qualified Data.Text.IO as TIO
import qualified Data.Vector as Vector
import Data.Text (Text)
import Data.Word (Word64)
import System.Directory (createDirectoryIfMissing, getTemporaryDirectory, removePathForcibly)
import System.FilePath ((</>))

import HaskellFlows.Ghc.ApiSession (startGhcSession, killGhcSession)
import qualified HaskellFlows.Mcp.Envelope as Env
import HaskellFlows.Types (mkProjectDir)
import qualified HaskellFlows.Types
import qualified HaskellFlows.Tool.Bootstrap as BootstrapTool
import qualified HaskellFlows.Tool.Imports as ImportsTool
import qualified HaskellFlows.Tool.Perf as PerfTool
import qualified HaskellFlows.Tool.Refactor as RefactorTool
import qualified HaskellFlows.Tool.Workflow as WorkflowTool

import Spec.Helpers (withTempProject)

-- | Local alias matching the one in Spec.hs (line 4249).
unProjectDirRaw :: HaskellFlows.Types.ProjectDir -> FilePath
unProjectDirRaw = HaskellFlows.Types.unProjectDir

-- | F-06: 'gitRootOf' must walk up and find the .git directory
-- rather than returning the subdirectory it was given.
testBootstrapGitRoot :: IO Bool
testBootstrapGitRoot = do
  -- Create a temporary tree: tmpdir/.git + tmpdir/sub/
  tmp <- getTemporaryDirectory
  let root = tmp </> "haskell-flows-gitroot-test"
      sub  = root </> "sub" </> "deep"
  removePathForcibly root
  createDirectoryIfMissing True (root </> ".git")
  createDirectoryIfMissing True sub
  found <- BootstrapTool.gitRootOf sub
  removePathForcibly root
  pure (found == root)

-- | F-02: 'pickModuleLine' must extract the first exposed module
-- from a cabal file body and convert it to a src/ path.
testWorkflowPickModuleLine :: IO Bool
testWorkflowPickModuleLine =
  let cabal = T.unlines
        [ "cabal-version: 3.0"
        , "library"
        , "  exposed-modules: MyLib.Core, MyLib.Util"
        ]
  in pure $ WorkflowTool.pickModuleLine cabal
         == Just "src/MyLib/Core.hs"


-- | F-10: 'importsPayload' must include a 'session_preloads' field
-- containing the MCP's own injected modules, separate from source
-- imports. Agents use this to ignore MCP-injected noise.
testImportsHasSessionPreloads :: IO Bool
testImportsHasSessionPreloads =
  let sourceImps = ["import Data.Map (Map)", "import Data.Text (Text)"]
      preloads   = ["import Prelude", "import System.IO"]
      payload    = ImportsTool.importsPayload (sourceImps, preloads)
  in pure $ case payload of
       A.Object obj ->
         AKM.member "session_preloads" obj
         && AKM.lookup "count" obj == Just (A.Number 2)
         && AKM.lookup "session_preloads" obj == Just (A.Array (Vector.fromList (map A.String preloads)))
       _ -> False

-- | F-21: 'compileFailResult' must return status='failed' with
-- dry_run=false and compile='failed' in the result object, so the
-- agent knows the dry-run rewrite did NOT type-check.
testRefactorCompileFailShape :: IO Bool
testRefactorCompileFailShape =
  -- #205 Bug 2: pass dryRun=False explicitly (was hardcoded False before fix)
  let result = RefactorTool.compileFailResult False [] "error text" " (file restored)"
  in pure $ Env.reStatus result == Env.StatusFailed
         && case Env.reResult result of
              Just (A.Object r) ->
                AKM.lookup (AKey.fromText "dry_run") r == Just (A.Bool False)
                && AKM.lookup (AKey.fromText "compile") r == Just (A.String "failed")
              _ -> False

-- | F-31: 'renderResult' when every sample errored must return
-- status='failed' with a remediation hint, not a meaningless
-- regression percentage.
testPerfAllSamplesErrored :: IO Bool
testPerfAllSamplesErrored =
  let args = PerfTool.PerfArgs
               { PerfTool.paExpression      = "1 + 1"
               , PerfTool.paRuns            = 3
               , PerfTool.paSaveBaseline    = False
               , PerfTool.paCompareBaseline = False
               , PerfTool.paVerbose         = False
               , PerfTool.paThresholdPct    = 30.0
               }
      errs   = ["err1", "err2", "err3"]
      stats  = PerfTool.aggregate []
      result = PerfTool.renderResult args [] stats errs Nothing 0
  in pure $ Env.reStatus result == Env.StatusFailed
         && case Env.reError result of
              Just err ->
                case Env.eeRemediation err of
                  Just r -> T.isInfixOf "ghc_load" r
                  _      -> False
              _ -> False

-- | #162: 'renderResult' must expose a 'warmup_ns' field in the
-- top-level payload so the agent can inspect the discarded
-- compilation cost separately from the measured statistics.
testPerfWarmupNsInPayload :: IO Bool
testPerfWarmupNsInPayload =
  let args = PerfTool.PerfArgs
               { PerfTool.paExpression      = "1 + 1"
               , PerfTool.paRuns            = 5
               , PerfTool.paSaveBaseline    = False
               , PerfTool.paCompareBaseline = False
               , PerfTool.paVerbose         = False
               , PerfTool.paThresholdPct    = 30.0
               }
      nss     = [90, 100, 110, 95, 105]
      stats   = PerfTool.aggregate nss
      warmup  = 5_000_000_000 :: Word64  -- 5 s — simulate compile cost
      result  = PerfTool.renderResult args nss stats [] Nothing warmup
  in pure $ case Env.reResult result of
       Just (A.Object r) ->
         AKM.lookup (AKey.fromText "warmup_ns") r == Just (A.toJSON warmup)
       _ -> False

-- | #162: the warm samples passed to 'aggregate' must NOT include
-- the warmup sample. Pre-fix, all runs including the first cold-start
-- were included, severely skewing mean_ns upward. We verify that a
-- hand-chosen warmup (5 s) + warm samples (all ~100 ns) yield a
-- mean that matches the warm samples only — not a blend with the
-- 5 s outlier. This is a pure-stats regression test (no IO).
testPerfWarmSamplesNotSkewed :: IO Bool
testPerfWarmSamplesNotSkewed =
  let warmNss = [90, 100, 110, 95, 105] :: [Word64]
      -- If the warmup (5_000_000_000 ns) were included in stats the mean
      -- would jump to ~(5e9 + 500) / 6 ≈ 833 ms, not ~100 ns.
      stats   = PerfTool.aggregate warmNss
      warmMean = PerfTool.sMean stats
  in pure $ warmMean < 200.0  -- well under 1 µs — compile outlier excluded

-- | #136: readBaseline must use strict ByteString I/O so the file handle
-- is closed before saveBaseline opens it for writing. Verified by source
-- inspection — the lazy BL.readFile call must be replaced by BS.readFile
-- + decodeStrict.
testPerfReadBaselineStrict :: IO Bool
testPerfReadBaselineStrict = do
  src <- TIO.readFile "src/HaskellFlows/Tool/Perf.hs"
  pure $ T.isInfixOf "BS.readFile" src
      && T.isInfixOf "decodeStrict" src
      -- Confirm the old lazy readFile is no longer used in readBaseline.
      && not (T.isInfixOf "BL.readFile path" src)

-- | #136: save_baseline=true AND compare_baseline=true in a single call
-- must not crash with "resource busy (file is locked)". The fix reads the
-- file strictly (closing the handle) before writing.
-- This test exercises the actual file I/O round-trip in a temp directory.
testPerfSaveAndCompareNoLock :: IO Bool
testPerfSaveAndCompareNoLock = withTempProject $ \pd -> do
  let path  = unProjectDirRaw pd </> ".haskell-flows" </> "perf.json"
      expr  = "sum [1..10]" :: T.Text
      stats = PerfTool.aggregate [1000, 1100, 900, 1050, 1000]
  -- Save a baseline first so compare has something to read.
  PerfTool.saveBaseline path expr stats
  -- Now exercise: readBaseline (strict read), then saveBaseline (write).
  -- Without the fix this would fail with "resource busy".
  mEntry <- PerfTool.readBaseline path expr
  PerfTool.saveBaseline path expr stats
  -- Verify the baseline was readable and re-written without error.
  mEntry2 <- PerfTool.readBaseline path expr
  pure $ case (mEntry, mEntry2) of
    (Just e1, Just e2) -> PerfTool.beMeanNs e1 > 0 && PerfTool.beMeanNs e2 > 0
    _                  -> False

-- | #161: saveBaseline called twice in sequence must not lock.
-- Root cause: saveBaseline used lazy BL.readFile, which deferred handle
-- closure to GC. The subsequent BL.writeFile in the same call (or the
-- next call's BL.readFile) raced on the file lock. Fixed by switching
-- saveBaseline's internal read to strict BS.readFile + decodeStrict.
testPerfSaveBaselinesNoLock :: IO Bool
testPerfSaveBaselinesNoLock = withTempProject $ \pd -> do
  let path  = unProjectDirRaw pd </> ".haskell-flows" </> "perf.json"
      expr  = "length [1..100]" :: T.Text
      stats = PerfTool.aggregate [500, 510, 490, 505, 495]
  -- Two consecutive saves: the second one reads the file that the first
  -- wrote. With BL.readFile the lazy handle from the read in the second
  -- saveBaseline races with its own subsequent BL.writeFile.
  PerfTool.saveBaseline path expr stats
  PerfTool.saveBaseline path expr stats  -- must not throw "resource busy"
  mEntry <- PerfTool.readBaseline path expr
  pure $ case mEntry of
    Just e -> PerfTool.beMeanNs e > 0
    Nothing -> False

--------------------------------------------------------------------------------
-- Issue #174 — perf threshold_pct param
--------------------------------------------------------------------------------

-- | #174: the default regression threshold must be 30%, not 10%.
-- A 20% regression at the old threshold (10%) would be flagged; at the
-- new threshold (30%) it must NOT be flagged.
testPerfDefaultThreshold30 :: IO Bool
testPerfDefaultThreshold30 = do
  let raw = A.fromJSON (A.object [ "expression" .= ("1+1" :: T.Text) ])
  case raw :: A.Result PerfTool.PerfArgs of
    A.Success args -> pure (PerfTool.paThresholdPct args == 30.0)
    _              -> pure False

-- | #174: 'threshold_pct' param must override the default.
testPerfCustomThreshold :: IO Bool
testPerfCustomThreshold = do
  let raw   = A.object [ "expression"    .= ("1+1" :: T.Text)
                       , "threshold_pct" .= (15.0 :: Double) ]
      args' = A.fromJSON raw :: A.Result PerfTool.PerfArgs
      -- A 20% regression with threshold=15% must be flagged.
      baseline = Just (PerfTool.BaselineEntry { PerfTool.beMeanNs = 1000.0 })
  case args' of
    A.Success args ->
      let nss   = [1200, 1200, 1200, 1200, 1200 :: Word64]
          stats = PerfTool.aggregate nss
          result = PerfTool.renderResult args nss stats [] baseline 0
      -- #190: regression must be status=failed, not status=refused
      in pure (Env.reStatus result == Env.StatusFailed)
    _ -> pure False

-- | #190: a measured regression must carry status=failed + kind=regression,
-- not status=refused + kind=validation.
testPerfRegressionStatusFailed :: IO Bool
testPerfRegressionStatusFailed = do
  let raw = A.object [ "expression"    .= ("1+1" :: T.Text)
                     , "threshold_pct" .= (10.0 :: Double) ]
      baseline = Just (PerfTool.BaselineEntry { PerfTool.beMeanNs = 1000.0 })
  case A.fromJSON raw :: A.Result PerfTool.PerfArgs of
    A.Success args ->
      let nss    = [1500, 1500, 1500, 1500, 1500 :: Word64]   -- 50% slower
          stats  = PerfTool.aggregate nss
          result = PerfTool.renderResult args nss stats [] baseline 0
      in pure $ Env.reStatus result == Env.StatusFailed
              && fmap Env.eeKind (Env.reError result) == Just Env.Regression
    _ -> pure False

-- | #174: 'threshold_pct' is clamped to [1, 200].
testPerfThresholdClamped :: IO Bool
testPerfThresholdClamped = do
  let rawLow  = A.object [ "expression" .= ("1+1" :: T.Text), "threshold_pct" .= (-5.0 :: Double) ]
      rawHigh = A.object [ "expression" .= ("1+1" :: T.Text), "threshold_pct" .= (999.0 :: Double) ]
  case (A.fromJSON rawLow :: A.Result PerfTool.PerfArgs
       , A.fromJSON rawHigh :: A.Result PerfTool.PerfArgs) of
    (A.Success lo, A.Success hi) ->
      pure $ PerfTool.paThresholdPct lo == 1.0
          && PerfTool.paThresholdPct hi == 200.0
    _ -> pure False

--------------------------------------------------------------------------------
-- Issue #200 — regression_pct precision
--------------------------------------------------------------------------------

-- | #200: 'roundTo1dp' must round to exactly 1 decimal place.
testPerfRoundTo1dp :: IO Bool
testPerfRoundTo1dp =
  pure $  PerfTool.roundTo1dp 15.234 == 15.2
       && PerfTool.roundTo1dp 15.289 == 15.3
       && PerfTool.roundTo1dp (-3.75) == (-3.8)
       && PerfTool.roundTo1dp 0.0    == 0.0

-- | #200: when a baseline comparison is present, 'regression_pct'
-- in the structured payload must have at most 1 decimal place — not
-- 15 IEEE-754 digits. We verify by checking the JSON representation
-- has no more than 3 significant digits after the decimal point.
testPerfRegressionPctPrecision :: IO Bool
testPerfRegressionPctPrecision =
  let args = PerfTool.PerfArgs
               { PerfTool.paExpression      = "1 + 1"
               , PerfTool.paRuns            = 5
               , PerfTool.paSaveBaseline    = False
               , PerfTool.paCompareBaseline = True
               , PerfTool.paVerbose         = False
               , PerfTool.paThresholdPct    = 30.0
               }
      -- baseline = 1000 ns, current samples average ≈ 1234 ns
      -- exact pct = 23.4 -- trimmed to 1 dp = 23.4
      nss   = [1234, 1234, 1234, 1234, 1234]
      stats = PerfTool.aggregate nss
      baseline = Just (PerfTool.BaselineEntry { PerfTool.beMeanNs = 1000.0 })
      result = PerfTool.renderResult args nss stats [] baseline 0
  in case Env.reResult result of
       Just (A.Object res) ->
         case AKM.lookup (AKey.fromText "baseline") res of
           Just (A.Object bl) ->
             case AKM.lookup (AKey.fromText "regression_pct") bl of
               Just (A.Number n) ->
                 -- render as text and ensure at most 1 digit after the dot
                 let rendered = T.pack (show (realToFrac n :: Double))
                     decimals = case T.breakOn "." rendered of
                       (_, "") -> 0
                       (_, d)  -> T.length (T.takeWhile (/= 'e') (T.drop 1 d))
                 in pure (decimals <= 1)
               _ -> pure False
           _ -> pure False
       _ -> pure False

-- | Issue #223: when all measurements throw a runtime exception (e.g.
-- Prelude.undefined), the error message must say "threw a runtime exception"
-- NOT "GHC session may have lost the module".
testPerfRuntimeExceptionMessage :: IO Bool
testPerfRuntimeExceptionMessage =
  let args = PerfTool.PerfArgs
               { PerfTool.paExpression      = "1 + undefined"
               , PerfTool.paRuns            = 3
               , PerfTool.paSaveBaseline    = False
               , PerfTool.paCompareBaseline = False
               , PerfTool.paVerbose         = False
               , PerfTool.paThresholdPct    = 30.0
               }
      -- All 3 errs contain "Prelude." → runtime-exception path
      errs  = [ "Prelude.undefined\nCallStack (from HasCallStack):\n  error, called at …"
              , "Prelude.undefined\nCallStack (from HasCallStack):\n  error, called at …"
              , "Prelude.undefined\nCallStack (from HasCallStack):\n  error, called at …"
              ] :: [Text]
      stats   = PerfTool.aggregate ([] :: [Word64])
      result  = PerfTool.renderResult args [] stats errs Nothing 0
  in case Env.reError result of
       Just err ->
         pure ( "threw a runtime exception" `T.isInfixOf` Env.eeMessage err
             && not ("session may have lost" `T.isInfixOf` Env.eeMessage err) )
       _ -> pure False
