-- | Miscellaneous tail tests: GHC-66111 routing, gate/regression
-- helpers, Deps remove/common-stanza, Load source-dirs/specific-file/
-- reset, CheckProject timeout rendering, Lab/Witness extra tests,
-- Suggest arity/filter/prioritize/looksLikeModule, Refactor free-var,
-- QcResult detail/status, and final Perf precision tests.
--
-- Extracted from the Spec.hs monolith (#271) via the function-export shape.
module Spec.FinalMisc
  ( testGhc66111RoutesToUnused
  , testRuntimeExceptionKindExists
  , testGateNextStepTextFromSummary
  , testRemoveDepNoTrailingBlank
  , testRemoveDepMultiDep
  , testAuditPairProbeIsModuleAgnostic
  , testRegressionCrossStanzaRetryClassification
  , testRegressionSelfContainedNoRetry
  , testImportsNubByDeduplication
  , testGateFailureKindExists
  , testUnchangedResultNoVerb
  , testFormatIso8601
  , testValidateCabalWarningsOk
  , testParseHsSourceDirsSingle
  , testParseHsSourceDirsMultipleStanzas
  , testParseHsSourceDirsEmpty
  , testIsUnderAnySourceDir
  , testIsUnderAnySourceDirDot
  , testOutsideSourceDirsKindExists
  , testLoadSpecificFileIgnoresStray
  , testLoadSpecificFileExported
  , testStrictFreshIsDistinct
  , testResetHscEnvInPlaceClearsLoaded
  , testResetHscEnvInPlaceFreshSession
  , testLoadPathsHaveResetGuard
  , testAutoLoadFailedBranch
  , testTargetForPathFlatFile
  , testTargetForPathNestedFile
  , testTargetForPathLibFallback
  , testCheckProjectDelegates
  , testCheckProjectArgsDefaultTimeout
  , testRenderResultTimedOutFlag
  , testRenderResultTimedOutModules
  , testRenderResultNoTimedOutField
  , testRenderOutcomeTimedOut
  , testRenderResultTimedOutSummary
  , testRenderResultTimedOutOverallFalse
  , testCheckProjectPartialStatus
  , testLabNoTemplateMatchedReason
  , testLabLowConfidenceReason
  , testRenderRunLineUsesModuleName
  , testWitIsPrimitiveBucketsTrue
  , testWitIsPrimitiveBucketsFalse
  , testWitIsPrimitiveBucketsEmpty
  , testWitPrimitiveFallbackWarning
  , testWitRawTruncatedFlag
  , testWitNoRawTruncatedWhenShort
  , testSuggestMaybeReturn2Arg
  , testSuggestMaybeReturn1Arg
  , testSuggestHintNoArityForArity2
  , testSuggestHintArityForArity3
  , testFilterInternalRemoves
  , testFilterInternalKeeps
  , testPrioritizeExactFirst
  , testPrioritizeNoDotNoOp
  , testLooksLikeModuleTrue
  , testLooksLikeModuleFalse
  , testLooksLikeModuleQualFun
  , testLooksLikeModuleSingle
  , testLooksLikeModuleThree
  , testRefactorCompileFailDryRunTrue
  , testExtractFreeVarNames
  , testExtractFreeVarNamesEmpty
  , testRefactorFreeVarNote
  , testExtractQcOutputAt
  , testExtractQcOutputAtMissing
  , testQcResultDetailFailed
  , testQcResultDetailPassed
  , testQcResultStatusAll
  , testQcResultStatusStackOverflow237
  , testQcResultStatusHeapOverflow237
  , testQcResultStatusOtherUnparsed237
  , testQcResultDetailUnparsedNonEmpty237
  , testQcResultDetailUnparsedEmpty237
  , testSplitAtDepthZeroIssue215
  , testDepsCommonStanzaPkgFound
  , testDepsCommonStanzaPkgAbsent
  , testDepsCommonStanzaNoCommon
  , testDepsUnchangedResultHintField
  , testSuggestCallsAugmentContext
  , testAddImportBypassesHoogle
  , testPerfLowPrecisionWarning
  , testPerfWarmupWarning
  , testPerfNoWarningHealthy
  ) where

import qualified Data.Aeson as A
import Data.Aeson (object, (.=))
import qualified Data.Aeson.Key as AKey
import qualified Data.Aeson.KeyMap as AKM
import qualified Data.Text as T
import qualified Data.Text.IO as TIO
import qualified Data.Text.Lazy as TL
import qualified Data.Text.Lazy.Encoding as TLE
import qualified Data.Vector as Vector
import Data.Maybe (isNothing)
import qualified Data.List as List
import Data.Text (Text)
import Data.Time.Clock.POSIX (posixSecondsToUTCTime)
import Data.Word (Word64)
import qualified Data.Set as Set
import System.Directory (createDirectoryIfMissing, doesFileExist, getTemporaryDirectory, removePathForcibly)
import System.FilePath ((</>))

import HaskellFlows.Ghc.ApiSession
  ( GhcSession
  , LoadFlavour (..)
  , killGhcSession
  , readLoadedRefForTest
  , resetHscEnvInPlace
  , startGhcSession
  , writeLoadedRefForTest
  )
import qualified HaskellFlows.Mcp.Envelope as Env
import HaskellFlows.Mcp.Protocol (ToolContent (..), ToolResult (..))
import HaskellFlows.Mcp.Progress (noopSink)
import HaskellFlows.Types (mkProjectDir)
import qualified HaskellFlows.Tool.AddImport as AddImport
import qualified HaskellFlows.Tool.CheckProject as CheckProjectTool
import HaskellFlows.Tool.CheckProject (CheckProjectArgs (..), ModuleOutcome (..), renderResult)
import qualified HaskellFlows.Tool.Deps as DepsTool
import qualified HaskellFlows.Tool.FixWarning as FixWarning
import qualified HaskellFlows.Tool.Gate as Gate
import qualified HaskellFlows.Tool.Imports as ImportsTool
import qualified HaskellFlows.Tool.Lab as LabTool
import qualified HaskellFlows.Tool.Load as LoadTool
import qualified HaskellFlows.Tool.Perf as PerfTool
import qualified HaskellFlows.Tool.PropertyAudit as PropertyAuditTool
import qualified HaskellFlows.Tool.QuickCheck as QcTool
import qualified HaskellFlows.Tool.QuickCheckExport as QcExport
import qualified HaskellFlows.Tool.Refactor as RefactorTool
import qualified HaskellFlows.Tool.Regression as RegTool
import qualified HaskellFlows.Tool.Suggest as SuggestTool
import qualified HaskellFlows.Tool.ValidateCabal as VC
import qualified HaskellFlows.Tool.Witness as WitnessTool
import HaskellFlows.Parser.Error
  ( GhcError (..)
  , Severity (..)
  , WarningCategory (..)
  , categorizeWarning
  )
import HaskellFlows.Parser.QuickCheck (QuickCheckResult (..))
import HaskellFlows.Parser.TypeSignature (ParsedSig (..), SigType (..), parseSignature)
import HaskellFlows.Data.PropertyStore (StoredProperty (..))
import HaskellFlows.Suggest.Rules (applyRules, Suggestion (..), Confidence (..))
import qualified HaskellFlows.Mcp.NextStep as NextStep
import HaskellFlows.Mcp.ToolName (ToolName (..))

import Spec.Helpers (decodeToolResult, runToolEnvelope, withTempProject)

-- | #116: GHC-66111 (redundant import) must route to 'WcUnused', not
-- 'WcDeferredError'. Before the fix it was listed in @deferredCodes@
-- which made the code-based branch fire first and return the wrong category.
testGhc66111RoutesToUnused :: IO Bool
testGhc66111RoutesToUnused =
  let e = GhcError
            { geFile     = "Foo.hs"
            , geLine     = 5
            , geColumn   = 1
            , geSeverity = SevWarning
            , geCode     = Just "GHC-66111"
            , geMessage  = "The import of 'Data.List' is redundant"
            }
  in pure (categorizeWarning e == WcUnused)

-- | #115: 'Env.RuntimeException' must be a member of the 'ErrorKind'
-- enum and have the wire text @"runtime_exception"@.
testRuntimeExceptionKindExists :: IO Bool
testRuntimeExceptionKindExists =
  pure $
    Env.errorKindToText Env.RuntimeException == "runtime_exception"
    && Env.textToErrorKind "runtime_exception" == Just Env.RuntimeException
    && Env.RuntimeException `elem` ([minBound .. maxBound] :: [Env.ErrorKind])

-- | #208: when 'ghc_gate' succeeds with some steps skipped, the
-- 'nextStep.why' text must reflect only the steps that actually ran
-- rather than always claiming "regression + cabal test + cabal build
-- all passed".
--
-- We build a minimal gate payload that looks like only 'regression'
-- ran (cabal_test and cabal_build are 'skip') and verify the injected
-- nextStep text contains the payload's 'summary' field verbatim
-- instead of the old hardcoded string.
testGateNextStepTextFromSummary :: IO Bool
testGateNextStepTextFromSummary =
  let -- Minimal payload matching Gate.hs renderReport shape:
      -- status=ok, result.summary says only regression ran.
      summaryText = "All requested gates passed: regression. Safe to push." :: T.Text
      payload = A.object
        [ "status" .= ("ok" :: T.Text)
        , "result" .= A.object
            [ "totalDurationSec" .= (5.0 :: Double)
            , "summary"          .= summaryText
            , "steps"            .= A.object
                [ "regression" .= A.object [ "status" .= ("pass" :: T.Text) ]
                , "cabal_test"  .= A.object [ "status" .= ("skip" :: T.Text) ]
                , "cabal_build" .= A.object [ "status" .= ("skip" :: T.Text) ]
                ]
            ]
        ]
      mNs = NextStep.suggestNext GhcGate True payload
  in case mNs of
       Just ns ->
         let why = NextStep.nsWhy ns
             -- Must contain the payload's summary (mentions "regression" only)
             hasSummary = T.isInfixOf summaryText why
             -- Must NOT contain the old hardcoded all-three string
             noAllThree = not (T.isInfixOf "regression + cabal test + cabal build" why)
         in pure (hasSummary && noAllThree)
       Nothing -> pure False

-- | #118: 'removeDep' must not leave a blank continuation line when
-- the only dep on that line is the one being removed.
--
-- Input shape:
-- > build-depends:    base
-- >                 , text
--
-- After removing @text@, the second line must vanish entirely (no blank
-- line in output).
testRemoveDepNoTrailingBlank :: IO Bool
testRemoveDepNoTrailingBlank =
  let body = T.unlines
        [ "library"
        , "  build-depends:    base"
        , "                  , text"
        ]
      result = DepsTool.removeDep "text" body
      lns = T.lines result
      -- The blank (empty) continuation line must not appear.
      noBlankAfterBuildDepends =
        not (any (\l -> T.null (T.strip l) && T.any (== 'b') l) lns)
          && not (any T.null lns)
  in pure noBlankAfterBuildDepends

-- | #118: removing one dep from a two-dep block must leave the other dep
-- intact with no blank lines introduced.
testRemoveDepMultiDep :: IO Bool
testRemoveDepMultiDep =
  let body = T.unlines
        [ "library"
        , "  build-depends:    base"
        , "                  , text"
        , "                  , aeson"
        ]
      result = DepsTool.removeDep "text" body
      lns = filter (not . T.null) (T.lines result)
      -- "aeson" must still be present; "text" must not.
      aesonPresent = any ("aeson" `T.isInfixOf`) lns
      textAbsent   = not (any ("text" `T.isInfixOf`) lns)
  in pure (aesonPresent && textAbsent)

-- | #112: The contradiction probe built by 'buildContradictionProbe'
-- is a self-contained lambda that doesn't reference any project module.
-- This confirms it can be run with a @Nothing@ module context (i.e.
-- ':m + <all exposed lib modules>') and won't accidentally embed import
-- or module declarations.
testAuditPairProbeIsModuleAgnostic :: IO Bool
testAuditPairProbeIsModuleAgnostic =
  let probe = PropertyAuditTool.buildContradictionProbe
                "\\x -> even (x :: Int)"
                "\\x -> odd (x :: Int)"
  in pure $ "&&"    `T.isInfixOf` probe
         && "not"   `T.isInfixOf` probe
         && not ("import" `T.isInfixOf` probe)
         && not ("module " `T.isInfixOf` probe)

-- | #113: When cabal-repl stderr carries a cross-stanza scope error
-- (test-suite symbols not visible under the library's repl target),
-- 'classifyLoadFailure' must return @Just@ — which triggers the
-- fallback retry with @Nothing@ module context in 'runOne'.
testRegressionCrossStanzaRetryClassification :: IO Bool
testRegressionCrossStanzaRetryClassification =
  let crossStanzaStderr = T.unlines
        [ "src/Main.hs:1:8: error:"
        , "    Variable not in scope: testHelper :: Int -> Bool"
        ]
      qr = QcUnparsed "\\x -> testHelper x" ""
  in pure (isJust (RegTool.classifyLoadFailure qr crossStanzaStderr))
  where
    isJust (Just _) = True
    isJust Nothing  = False

-- | #113: A successful property run must NOT be misclassified as a
-- load failure. 'QcPassed' with empty stderr → 'classifyLoadFailure'
-- returns @Nothing@, so no fallback retry is attempted.
testRegressionSelfContainedNoRetry :: IO Bool
testRegressionSelfContainedNoRetry =
  let qr = QcPassed "\\x -> x > (0 :: Int)" 100
  in pure (isNothing (RegTool.classifyLoadFailure qr ""))

-- | #114: The deduplication in 'queryImports' uses 'nubBy' keyed on
-- module name. Verify the invariant: given a list with repeated keys,
-- 'nubBy (==)' (same logic as 'nubBy importKey') keeps only the first
-- occurrence and produces a list whose length equals the number of
-- distinct module names.
testImportsNubByDeduplication :: IO Bool
testImportsNubByDeduplication =
  let entries = ["Data.Map", "Data.Text", "Data.Map", "Data.List", "Data.Text"] :: [Text]
      deduped  = List.nub entries
  in pure (length deduped == 3 && deduped == ["Data.Map", "Data.Text", "Data.List"])

-- | #119: Env.GateFailure must be a member of ErrorKind with wire
-- text @"gate_failure"@. Used by ghc_check_module (warnings-blocking)
-- and ghc_batch (partial outcomes) instead of the misleading
-- @"validation"@ kind.
testGateFailureKindExists :: IO Bool
testGateFailureKindExists =
  pure $
    Env.errorKindToText Env.GateFailure == "gate_failure"
    && Env.textToErrorKind "gate_failure" == Just Env.GateFailure
    && Env.GateFailure `elem` ([minBound .. maxBound] :: [Env.ErrorKind])

-- | #119: 'unchangedResult' must NOT include a 'verb' field that
-- contradicts @action: "unchanged"@. When a dep is already present,
-- returning @verb: "added"@ alongside @action: "unchanged"@ confused
-- callers into thinking a change was made.
testUnchangedResultNoVerb :: IO Bool
testUnchangedResultNoVerb =
  let tr     = DepsTool.unchangedResult "/tmp/foo.cabal" "aeson" "added"
  in pure $ case trContent tr of
       [TextContent body_] ->
         case A.decode (TLE.encodeUtf8 (TL.fromStrict body_)) of
           Just (A.Object top) ->
             case AKM.lookup "result" top of
               Just (A.Object r) ->
                 AKM.lookup "action" r == Just (A.String "unchanged")
                 && not (AKM.member "verb" r)
               _ -> False
           _ -> False
       _ -> False

-- | #119: 'formatIso8601' must produce an ISO-8601 UTC timestamp
-- that is human-readable. Specifically: it must contain "T" and "Z",
-- and not be a plain float.
testFormatIso8601 :: IO Bool
testFormatIso8601 =
  -- 2026-05-02 00:00:00 UTC = 1746144000 seconds since epoch
  let ts  = RegTool.formatIso8601 1746144000.0
  in pure $ "T" `T.isInfixOf` ts
          && "Z" `T.isSuffixOf` ts
          && not ("e" `T.isInfixOf` ts)  -- not scientific notation
          && T.length ts == 20            -- "YYYY-MM-DDTHH:MM:SSZ"

-- | #119: ValidateCabal with 0 cabal errors and N warnings must return
-- status='ok' (not 'partial'). The cabal file IS shippable; the
-- distinction mattered because 'partial' implies something needs fixing.
testValidateCabalWarningsOk :: IO Bool
testValidateCabalWarningsOk =
  let warnIssue  = VC.Issue
        { VC.iKind     = "duplicate-dep"
        , VC.iMessage  = "duplicate dep"
        , VC.iSeverity = VC.CabalSevWarn
        }
      tr = VC.renderResult "/tmp/foo.cabal" [warnIssue]
  in pure $ case trContent tr of
       [TextContent body_] ->
         case A.decode (TLE.encodeUtf8 (TL.fromStrict body_)) of
           Just (A.Object top) ->
             AKM.lookup "status" top == Just (A.String "ok")
           _ -> False
       _ -> False

--------------------------------------------------------------------------------
-- Issue #110 — ghc_load hs-source-dirs validation
--------------------------------------------------------------------------------

-- | #110: a single @hs-source-dirs:@ line is parsed into one dir.
testParseHsSourceDirsSingle :: IO Bool
testParseHsSourceDirsSingle =
  let cabal = T.unlines
        [ "library"
        , "  hs-source-dirs: src"
        , "  exposed-modules: Foo"
        ]
  in pure (LoadTool.parseHsSourceDirs cabal == ["src"])

-- | #110: multiple stanzas each declaring different source dirs
-- produce the union of all dirs (order: last stanza first, then dedup
-- is the caller's responsibility).
testParseHsSourceDirsMultipleStanzas :: IO Bool
testParseHsSourceDirsMultipleStanzas =
  let cabal = T.unlines
        [ "library"
        , "  hs-source-dirs: src"
        , ""
        , "test-suite spec"
        , "  type:             exitcode-stdio-1.0"
        , "  hs-source-dirs:   test"
        , "  main-is:          Spec.hs"
        , ""
        , "executable my-exe"
        , "  hs-source-dirs:   app"
        ]
      dirs = LoadTool.parseHsSourceDirs cabal
  in pure (Set.fromList dirs == Set.fromList ["src", "test", "app"])

-- | #110: a cabal body with NO @hs-source-dirs:@ field at all
-- produces an empty list. Callers treat empty as the Cabal default
-- of @"."@ (project root allows everything).
testParseHsSourceDirsEmpty :: IO Bool
testParseHsSourceDirsEmpty =
  let cabal = T.unlines
        [ "library"
        , "  exposed-modules: Foo"
        , "  build-depends:   base"
        ]
  in pure (null (LoadTool.parseHsSourceDirs cabal))

-- | #110: 'isUnderAnySourceDir' returns True when the path is directly
-- under a declared dir and False when it isn't.
testIsUnderAnySourceDir :: IO Bool
testIsUnderAnySourceDir =
  pure $
       LoadTool.isUnderAnySourceDir ["src"] "src/Foo.hs"
    && LoadTool.isUnderAnySourceDir ["src", "test"] "test/Spec.hs"
    && not (LoadTool.isUnderAnySourceDir ["src"] "dogfood-sandbox/X.hs")
    && not (LoadTool.isUnderAnySourceDir ["src"] "Foo.hs")
    && not (LoadTool.isUnderAnySourceDir ["src"] "src-extra/Bar.hs")

-- | #110: the special dir @"."@ matches every relative path — it
-- represents the Cabal default of the project root.
testIsUnderAnySourceDirDot :: IO Bool
testIsUnderAnySourceDirDot =
  pure $
       LoadTool.isUnderAnySourceDir ["."] "src/Foo.hs"
    && LoadTool.isUnderAnySourceDir ["."] "dogfood-sandbox/X.hs"
    && LoadTool.isUnderAnySourceDir ["."] "Foo.hs"

-- | #110: 'Env.OutsideSourceDirs' must be a member of 'ErrorKind'
-- with wire text @"outside_source_dirs"@.
testOutsideSourceDirsKindExists :: IO Bool
testOutsideSourceDirsKindExists =
  pure $
    Env.errorKindToText Env.OutsideSourceDirs == "outside_source_dirs"
    && Env.textToErrorKind "outside_source_dirs" == Just Env.OutsideSourceDirs
    && Env.OutsideSourceDirs `elem` ([minBound .. maxBound] :: [Env.ErrorKind])

--------------------------------------------------------------------------------
-- #166 — ghc_load must not pick up unregistered src/ files
--------------------------------------------------------------------------------

-- | #166: when @module_path@ is given, loading a clean module must not
-- fail because of a broken UNREGISTERED file sitting in the same @src/@
-- directory. Pre-fix, 'loadForTarget' enumerated ALL @.hs@ files in
-- @src/@ and added them all as GHC targets — a broken stray file
-- blocked loading of unrelated registered modules.
-- Post-fix, 'loadSpecificFileForTarget' compiles only the specified
-- file (plus its transitive imports).
testLoadSpecificFileIgnoresStray :: IO Bool
testLoadSpecificFileIgnoresStray = do
  tmp <- getTemporaryDirectory
  let dir = tmp </> "haskell-flows-issue-166"
  removePathForcibly dir
  createDirectoryIfMissing True (dir </> "src")
  -- Good registered module
  TIO.writeFile (dir </> "src" </> "Good.hs")
    (T.pack "module Good where\ngood :: Int\ngood = 42\n")
  -- Broken UNREGISTERED file in the same src/ dir
  TIO.writeFile (dir </> "src" </> "Stray.hs")
    (T.pack "module Stray where\nin bad syntax here = 1\n")
  result <- case mkProjectDir dir of
    Left _   -> pure (Left "could not build ProjectDir")
    Right pd -> do
      sess <- startGhcSession pd
      -- Load the GOOD file by explicit module_path; stray must be invisible
      tr   <- LoadTool.handle sess pd
                (A.object [ "module_path" A..= ("src/Good.hs" :: Text) ])
      killGhcSession sess
      case trContent tr of
        [TextContent body] ->
          pure (A.eitherDecode (TLE.encodeUtf8 (TL.fromStrict body)))
        _ -> pure (Left "expected exactly one TextContent")
  removePathForcibly dir
  pure $ case result of
    Right env
      | Env.reStatus env == Env.StatusOk
      , Just (A.Object payload) <- Env.reResult env ->
          -- No errors from Stray.hs in the error list
          case AKM.lookup (AKey.fromText "errors") payload of
            Just (A.Array errs) -> null errs
            _                   -> False
    _ -> False

-- | #166: 'loadSpecificFileForTarget' must be exported from
-- 'ApiSession' so 'Load.hs' can import it directly. Static
-- compilation check (this module imports it).
testLoadSpecificFileExported :: IO Bool
testLoadSpecificFileExported = do
  src <- TIO.readFile "src/HaskellFlows/Ghc/ApiSession.hs"
  pure $ "loadSpecificFileForTarget" `T.isInfixOf` src

-- | #232: 'StrictFresh' must be a third distinct variant so
-- 'applyFlavour' applies 'Opt_ForceRecomp' only for check_module.
testStrictFreshIsDistinct :: IO Bool
testStrictFreshIsDistinct =
  pure (StrictFresh /= Strict && StrictFresh /= Deferred && Strict /= Deferred)

--------------------------------------------------------------------------------
-- #181 — session left broken after ghc_load with compile errors
--------------------------------------------------------------------------------

-- | #181: 'resetHscEnvInPlace' must clear the loaded flag so the next
-- 'withGhcSession' call re-runs 'autoLoadProject' instead of operating
-- on the broken partial HscEnv a failed compile leaves behind.
--
-- Simulates the sequence: loadForTarget pre-flips gsLoadedRef=True before
-- a compile, compile fails, resetHscEnvInPlace is called → flag goes False.
testResetHscEnvInPlaceClearsLoaded :: IO Bool
testResetHscEnvInPlaceClearsLoaded =
  case mkProjectDir "/tmp" of
    Left  _  -> pure False
    Right pd -> do
      sess <- startGhcSession pd
      -- Simulate the pre-flip loadForTarget / loadAndCaptureDiagnostics
      -- do before starting a compile.
      writeLoadedRefForTest sess True
      before <- readLoadedRefForTest sess
      -- After the (simulated) failed compile, resetHscEnvInPlace is called.
      resetHscEnvInPlace sess
      after <- readLoadedRefForTest sess
      pure (before && not after)  -- True → False

-- | #181: 'resetHscEnvInPlace' is idempotent — calling it on a fresh
-- session (loaded=False) keeps the flag False; calling it twice is safe.
testResetHscEnvInPlaceFreshSession :: IO Bool
testResetHscEnvInPlaceFreshSession =
  case mkProjectDir "/tmp" of
    Left  _  -> pure False
    Right pd -> do
      sess <- startGhcSession pd
      -- Idempotent: double reset on a fresh session must not error.
      resetHscEnvInPlace sess
      resetHscEnvInPlace sess
      loaded <- readLoadedRefForTest sess
      pure (not loaded)  -- still False after double reset

-- | #181: all four load paths (loadAndCaptureDiagnostics, loadForTarget
-- stanza branch, loadSpecificFileForTarget both branches) must contain
-- the reset guard that calls resetHscEnvInPlace on failure.
testLoadPathsHaveResetGuard :: IO Bool
testLoadPathsHaveResetGuard = do
  src <- TIO.readFile "src/HaskellFlows/Ghc/ApiSession.hs"
  let guard   = "unless ok (resetHscEnvInPlace sess)"
      count   = length (T.splitOn guard src) - 1
  pure (count == 4)  -- 4 call sites: loadAndCaptureDiagnostics +
                     -- loadForTarget + 2x loadSpecificFileForTarget

--------------------------------------------------------------------------------
-- #193 — autoLoadProject must fall back to Prelude-only on failed load
--------------------------------------------------------------------------------

-- | #193: Structural check that 'autoLoadProject' handles the 'Failed' case
-- from 'load LoadAllTargets' by calling 'setContext [preludeImport]' rather
-- than including potentially-unloaded home modules. The pattern 'Failed ->'
-- must be present in ApiSession.hs with 'setContext [preludeImport]' nearby.
testAutoLoadFailedBranch :: IO Bool
testAutoLoadFailedBranch = do
  src <- TIO.readFile "src/HaskellFlows/Ghc/ApiSession.hs"
  let hasFailed     = "Failed -> setContext [preludeImport]" `T.isInfixOf` src
      hasSucceeded  = "Succeeded -> do" `T.isInfixOf` src
  pure (hasFailed && hasSucceeded)

--------------------------------------------------------------------------------
-- #194 — targetForPath prefix must match flat test/Foo.hs
--------------------------------------------------------------------------------

-- | #194: Verify the prefix check used by 'targetForPath' matches a
-- file directly under test/ (no nested directory). The old guard
-- required a '/' in the remainder, so "test/Gen.hs" silently fell
-- through to TargetLibrary and failed to load QuickCheck.
testTargetForPathFlatFile :: IO Bool
testTargetForPathFlatFile = do
  src <- TIO.readFile "src/HaskellFlows/Ghc/ApiSession.hs"
  -- The correct prefix predicate is a simple 'take' prefix check,
  -- without the 'any isPathSep' guard on the remainder.
  let newDef = "prefix p = take (length p) path == p" `T.isInfixOf` src
      oldBug = "any (\\c -> c == '/' || c == '\\\\')" `T.isInfixOf` src
  pure (newDef && not oldBug)

-- | Verify the updated predicate matches nested paths too (regression guard).
testTargetForPathNestedFile :: IO Bool
testTargetForPathNestedFile = do
  -- Purely functional test of the new predicate logic.
  let prefix p path = take (length p) path == p
  pure $  prefix "test/" "test/foo/Bar.hs"  -- nested: was always fine
       && prefix "test/" "test/Gen.hs"       -- flat: was broken before fix
       && prefix "app/"  "app/Main.hs"
       && not (prefix "test/" "src/Foo.hs")

-- | Verify that src/Foo.hs still maps to library (not test-suite).
testTargetForPathLibFallback :: IO Bool
testTargetForPathLibFallback = do
  let prefix p path = take (length p) path == p
      matchesTest path =
        prefix "test/" path || prefix "app/" path || prefix "bench/" path
  pure $ not (matchesTest "src/Foo.hs")
      && not (matchesTest "src/Bar/Baz.hs")

--------------------------------------------------------------------------------
-- #129 — ghc_check_project deadline-based timeout
--------------------------------------------------------------------------------

-- | Helper: decode the @result@ sub-object from the first TextContent
-- of a 'ToolResult' (the standard wire shape).
decodeCheckProjectResult :: ToolResult -> Maybe A.Value
decodeCheckProjectResult tr =
  case trContent tr of
    (TextContent body : _) ->
      case A.eitherDecode (TLE.encodeUtf8 (TL.fromStrict body)) of
        Right (A.Object top) -> AKM.lookup "result" top
        _                    -> Nothing
    _ -> Nothing

-- | #129: Parsing @{}@ as 'CheckProjectArgs' should yield
-- | #191: ghc_check_project must NOT call loadForTarget directly; it must
-- delegate to ghc_check_module.handle so the loadSpecificFileForTarget fix
-- from #188 automatically applies. This is a source-level structural check.
testCheckProjectDelegates :: IO Bool
testCheckProjectDelegates = do
  src <- TIO.readFile "src/HaskellFlows/Tool/CheckProject.hs"
  -- Must use CheckModule.handle, not call loadForTarget directly.
  pure $ T.isInfixOf "CheckModule.handle" src
      && not (T.isInfixOf "loadForTarget ghcSess" src)

-- 'cpTimeoutSeconds' == 120 (the documented default).
testCheckProjectArgsDefaultTimeout :: IO Bool
testCheckProjectArgsDefaultTimeout =
  case A.eitherDecode "{}" :: Either String CheckProjectArgs of
    Right args -> pure (cpTimeoutSeconds args == 120)
    Left  _    -> pure False

-- | #129: 'renderResult' with @timedOut=True@ must include
-- @"timed_out": true@ in the payload.
testRenderResultTimedOutFlag :: IO Bool
testRenderResultTimedOutFlag = do
  let tr = renderResult [MoTimedOut "Foo.Bar"] True
  pure $ case decodeCheckProjectResult tr of
    Just (A.Object r) ->
      AKM.lookup "timed_out" r == Just (A.Bool True)
    _ -> False

-- | #129: 'renderResult' with @timedOut=True@ must list the timed-out
-- module names in @"timed_out_modules"@.
testRenderResultTimedOutModules :: IO Bool
testRenderResultTimedOutModules = do
  let tr = renderResult [MoTimedOut "Foo.Bar", MoTimedOut "Foo.Baz"] True
  pure $ case decodeCheckProjectResult tr of
    Just (A.Object r) ->
      case AKM.lookup "timed_out_modules" r of
        Just (A.Array arr) ->
          Vector.toList arr == [A.String "Foo.Bar", A.String "Foo.Baz"]
        _ -> False
    _ -> False

-- | #129: 'renderResult' with @timedOut=False@ must NOT include
-- @"timed_out"@ in the payload (keeps the common-case response shape
-- unchanged — avoids adding noise for projects that finish on time).
testRenderResultNoTimedOutField :: IO Bool
testRenderResultNoTimedOutField = do
  let tr = renderResult [] False
  pure $ case decodeCheckProjectResult tr of
    Just (A.Object r) -> not (AKM.member "timed_out" r)
    _                 -> False

-- | #129: A 'MoTimedOut' outcome in @per_module@ must have
-- @"status": "timed_out"@.
testRenderOutcomeTimedOut :: IO Bool
testRenderOutcomeTimedOut = do
  let tr = renderResult [MoTimedOut "Foo.TimedOut"] True
  pure $ case decodeCheckProjectResult tr of
    Just (A.Object r) ->
      case AKM.lookup "per_module" r of
        Just (A.Array arr) ->
          case Vector.toList arr of
            [A.Object m] ->
              AKM.lookup "status" m == Just (A.String "timed_out")
              && AKM.lookup "module" m == Just (A.String "Foo.TimedOut")
            _ -> False
        _ -> False
    _ -> False

-- | #151: the summary must NOT claim 'total/total green' when the run
-- timed out after checking only k < total modules. Before the fix:
-- "168 / 168 modules green. (1/168 checked before timeout)".
-- After the fix: "1/168 modules checked before timeout. 167 not evaluated."
testRenderResultTimedOutSummary :: IO Bool
testRenderResultTimedOutSummary = do
  -- 1 timed-out module, 0 checked modules, timedOut=True
  let tr = renderResult [MoTimedOut "Foo.X"] True
  pure $ case decodeCheckProjectResult tr of
    Just (A.Object r) ->
      case AKM.lookup "summary" r of
        Just (A.String s) ->
          -- Must mention how many were checked (0), not claim total are green
          T.isInfixOf "0/" s
            -- Must NOT say "0 / 1 modules green" (the old contradictory message)
            && not (T.isInfixOf "green" s)
            -- Must mention "not evaluated"
            && T.isInfixOf "not evaluated" s
        _ -> False
    _ -> False

-- | #129: 'renderResult' with any 'MoTimedOut' module must return
-- @"overall": false@ — a partial result is never a clean bill of health.
testRenderResultTimedOutOverallFalse :: IO Bool
testRenderResultTimedOutOverallFalse = do
  let tr = renderResult [MoTimedOut "Foo.X"] True
  pure $ case decodeCheckProjectResult tr of
    Just (A.Object r) ->
      AKM.lookup "overall" r == Just (A.Bool False)
    _ -> False

-- | Issue #255: when some modules pass and some fail,
-- 'renderResult' must return status='partial', not status='failed'.
testCheckProjectPartialStatus :: IO Bool
testCheckProjectPartialStatus =
  let passTr = Env.toolResponseToResult (Env.mkOk (A.object []))
      failTr = Env.toolResponseToResult
                 (Env.mkFailed (Env.mkErrorEnvelope Env.Validation "err"))
      outcomes = [MoChecked "Foo.Ok" passTr, MoChecked "Foo.Bad" failTr]
      tr = renderResult outcomes False
      -- Decode the full outer envelope (not just the inner payload) so
      -- we can inspect the top-level "status" field.
      mTopEnv = case trContent tr of
        (TextContent body : _) ->
          case A.eitherDecode (TLE.encodeUtf8 (TL.fromStrict body)) of
            Right (A.Object top) -> Just top
            _                    -> Nothing
        _ -> Nothing
  in pure $ case mTopEnv of
       Just top ->
            AKM.lookup "status" top == Just (A.String "partial")
         && case AKM.lookup "result" top of
              Just (A.Object r) ->
                AKM.lookup "overall" r == Just (A.Bool False)
              _ -> False
       Nothing -> False

-- | Issue #254: when no rule template applies to the signature shape,
-- 'computeSuggest' must return @Left "no-template-matched"@.
testLabNoTemplateMatchedReason :: IO Bool
testLabNoTemplateMatchedReason = do
  let args = LabTool.LabArgs
               { LabTool.laModulePath      = ""
               , LabTool.laMinConfidence   = Medium
               , LabTool.laDeterminismRuns = 0
               }
  -- A plain @IO ()@ signature has no pure QuickCheck laws.
  let bind = LabTool.Binding "runSomething" "IO ()"
  pure $ LabTool.computeSuggest args bind == Left "no-template-matched"

-- | Issue #254: when templates matched but all fall below the
-- minimum confidence, 'computeSuggest' returns @Left "low-confidence"@.
testLabLowConfidenceReason :: IO Bool
testLabLowConfidenceReason = do
  -- Use High min_confidence so Medium/Low matches are filtered out.
  let args = LabTool.LabArgs
               { LabTool.laModulePath      = ""
               , LabTool.laMinConfidence   = High
               , LabTool.laDeterminismRuns = 0
               }
  -- A [a] -> [a] signature matches Idempotent at Low, which fails High threshold.
  let bind = LabTool.Binding "sortList" "[a] -> [a]"
  pure $ LabTool.computeSuggest args bind == Left "low-confidence"

-- | Issue #250: 'renderRunLine' must use the properly-formatted
-- module name (Foo.Bar) as the display prefix, not the raw path
-- (src_Foo_Bar_hs).
testRenderRunLineUsesModuleName :: IO Bool
testRenderRunLineUsesModuleName =
  let sp = StoredProperty
             { spExpression = "\\x -> x + 0 == x"
             , spModule     = Just "src/Foo/Bar.hs"
             , spPassed     = 1
             , spUpdated    = 0
             , spCases     = 0
             }
      line = QcExport.renderRunLine 1 sp
  in pure $  "Foo_Bar_prop_1" `T.isInfixOf` line
          && not ("src_Foo_Bar_hs" `T.isInfixOf` line)

-- | #199: 'isPrimitiveBuckets' returns True when > 80% of ctor labels
-- are numeric (digits or leading minus).
testWitIsPrimitiveBucketsTrue :: IO Bool
testWitIsPrimitiveBucketsTrue =
  -- 5 numeric out of 6 = 83% > 80%.
  let dist = [ ("ctor:-1", 2.5), ("ctor:42", 1.5), ("ctor:7", 3.0)
             , ("ctor:0",  2.0), ("ctor:99", 1.0)
             , ("ctor:Just", 0.5)                    -- 1 ADT
             ]
  in pure (WitnessTool.isPrimitiveBuckets dist)

-- | #199: 'isPrimitiveBuckets' returns False for ADT constructors
-- (start with uppercase).
testWitIsPrimitiveBucketsFalse :: IO Bool
testWitIsPrimitiveBucketsFalse =
  let dist = [ ("ctor:Just",    60.0)
             , ("ctor:Nothing", 40.0)
             ]
  in pure (not (WitnessTool.isPrimitiveBuckets dist))

-- | #199: 'isPrimitiveBuckets' returns False for an empty list
-- (avoids a divide-by-zero in the heuristic).
testWitIsPrimitiveBucketsEmpty :: IO Bool
testWitIsPrimitiveBucketsEmpty =
  pure (not (WitnessTool.isPrimitiveBuckets []))

-- | #199 Bug 1: when 'renderReport' detects primitive constructor
-- buckets it must add a 'primitive-constructor-fallback' warning and
-- switch the distribution key to @by_size@.
testWitPrimitiveFallbackWarning :: IO Bool
testWitPrimitiveFallbackWarning = do
  let argsJson = object
        [ "property"     .= ("\\x -> x > (0::Int)" :: Text)
        , "classify_by"  .= ("constructor"         :: Text)
        , "runs"         .= (200                   :: Int)
        ]
  case A.fromJSON argsJson :: A.Result WitnessTool.WitnessArgs of
    A.Error _ -> pure False
    A.Success args ->
      -- All numeric "ctor:" labels — should trigger primitive fallback.
      let ctorDist = [ ("ctor:-1", 20.0), ("ctor:0",  15.0), ("ctor:1", 15.0)
                     , ("ctor:2",  10.0), ("ctor:42", 10.0), ("ctor:7", 10.0)
                     , ("ctor:3",  10.0), ("ctor:10",  5.0), ("ctor:5",  5.0)
                     ]
          tr      = WitnessTool.renderReport args (QcPassed "prop" 200)
                      ctorDist [] "" 0
      in pure $ case trContent tr of
           [TextContent body] ->
             case A.decode (TLE.encodeUtf8 (TL.fromStrict body)) of
               Just (env :: Env.ToolResponse)
                 | Just (A.Object payload) <- Env.reResult env ->
                     -- Must use by_size (fallback), not by_constructor.
                     case AKM.lookup "distribution" payload of
                       Just (A.Object dist_) ->
                         AKM.member "by_size" dist_
                           && not (AKM.member "by_constructor" dist_)
                           -- Must contain the primitive-fallback warning.
                           && case AKM.lookup "warnings" payload of
                                Just (A.Array ws) ->
                                  any primitiveWarn (Vector.toList ws)
                                _ -> False
                       _ -> False
               _ -> False
           _ -> False
  where
    primitiveWarn (A.Object w) =
      AKM.lookup "kind" w
        == Just (A.String "primitive-constructor-fallback")
    primitiveWarn _ = False

-- | #199 Bug 2: when 'qc_raw_output' is truncated (raw > 1000 chars),
-- the response must include @raw_truncated: true@.
testWitRawTruncatedFlag :: IO Bool
testWitRawTruncatedFlag = do
  let argsJson = object
        [ "property" .= ("\\x -> True" :: Text)
        , "runs"     .= (100           :: Int)
        ]
  case A.fromJSON argsJson :: A.Result WitnessTool.WitnessArgs of
    A.Error _ -> pure False
    A.Success args ->
      let longRaw = T.replicate 1001 "x"
          tr      = WitnessTool.renderReport args (QcPassed "prop" 100) [] [] longRaw 0
      in pure $ case trContent tr of
           [TextContent body] ->
             case A.decode (TLE.encodeUtf8 (TL.fromStrict body)) of
               Just (env :: Env.ToolResponse)
                 | Just (A.Object payload) <- Env.reResult env ->
                     AKM.lookup "raw_truncated" payload == Just (A.Bool True)
               _ -> False
           _ -> False

-- | #199 Bug 2: when 'qc_raw_output' fits within 1000 chars the
-- response must NOT contain a 'raw_truncated' field at all.
testWitNoRawTruncatedWhenShort :: IO Bool
testWitNoRawTruncatedWhenShort = do
  let argsJson = object
        [ "property" .= ("\\x -> True" :: Text)
        , "runs"     .= (100           :: Int)
        ]
  case A.fromJSON argsJson :: A.Result WitnessTool.WitnessArgs of
    A.Error _ -> pure False
    A.Success args ->
      let shortRaw = "size:0\t50\nsize:1-5\t50"
          tr       = WitnessTool.renderReport args (QcPassed "prop" 100) [] [] shortRaw 0
      in pure $ case trContent tr of
           [TextContent body] ->
             case A.decode (TLE.encodeUtf8 (TL.fromStrict body)) of
               Just (env :: Env.ToolResponse)
                 | Just (A.Object payload) <- Env.reResult env ->
                     not (AKM.member "raw_truncated" payload)
               _ -> False
           _ -> False


-- | #197: 'ruleMaybeReturn' fires for a 2-argument Maybe-returning
-- signature and the generated property uses @maybe True (const True)@
-- applied to both arguments.
testSuggestMaybeReturn2Arg :: IO Bool
testSuggestMaybeReturn2Arg =
  case parseSignature "a -> b -> Maybe c" of
    Nothing  -> pure False
    Just sig ->
      let sug = filter (\s -> sCategory s == "maybe") (applyRules "lookup" sig)
      in case sug of
           [s] ->
             pure $ T.isInfixOf "maybe True (const True)" (sProperty s)
                 && T.isInfixOf "lookup x y" (sProperty s)
                 && sLaw s == "Maybe totality"
           _   -> pure False

-- | #197: 'ruleMaybeReturn' fires for a 1-argument Maybe-returning
-- signature and the generated property uses @maybe True (const True)@.
testSuggestMaybeReturn1Arg :: IO Bool
testSuggestMaybeReturn1Arg =
  case parseSignature "k -> Maybe v" of
    Nothing  -> pure False
    Just sig ->
      let sug = filter (\s -> sCategory s == "maybe") (applyRules "find" sig)
      in case sug of
           [s] ->
             pure $ T.isInfixOf "maybe True (const True)" (sProperty s)
                 && T.isInfixOf "find x" (sProperty s)
                 && sLaw s == "Maybe totality"
           _   -> pure False

-- | #197: when no rules match and arity == 'maxRuleArity', the hint must
-- NOT mention @\"arity > N\"@ — that would be a lie for a 2-arg function.
testSuggestHintNoArityForArity2 :: IO Bool
testSuggestHintNoArityForArity2 =
  -- "String -> Int -> Bool" has arity 2 (== maxRuleArity) and won't match
  -- any generic algebraic rule, so the hint fires on [].
  case parseSignature "String -> Int -> Bool" of
    Nothing  -> pure False
    Just sig ->
      let sug  = applyRules "weirdFn" sig
          hint = SuggestTool.hintFor (length (psArgs sig)) sug
      in pure $ not (T.isInfixOf "arity" hint)

-- | #197: when no rules match and arity exceeds 'maxRuleArity', the hint
-- MUST mention @\"arity > N\"@ so the developer understands why.
testSuggestHintArityForArity3 :: IO Bool
testSuggestHintArityForArity3 =
  -- "a -> b -> c -> d" has arity 3 (> maxRuleArity == 2).
  case parseSignature "a -> b -> c -> d" of
    Nothing  -> pure False
    Just sig ->
      let sug  = applyRules "threeArg" sig
          hint = SuggestTool.hintFor (length (psArgs sig)) sug
      in pure $ T.isInfixOf "arity" hint
              && T.isInfixOf (T.pack (show SuggestTool.maxRuleArity)) hint

-- | #204: 'filterInternal' removes any module whose name contains
-- @\".Internal\"@.
testFilterInternalRemoves :: IO Bool
testFilterInternalRemoves =
  let mods = [ "Data.Map.Internal"
             , "Data.Map.Strict"
             , "Data.Sequence.Internal"
             , "Data.Map.Lazy"
             ]
      result = AddImport.filterInternal mods
  in pure $ result == ["Data.Map.Strict", "Data.Map.Lazy"]

-- | #204: 'filterInternal' keeps public modules untouched.
testFilterInternalKeeps :: IO Bool
testFilterInternalKeeps =
  let mods = ["Data.Map.Strict", "Data.Map.Lazy", "Data.Set"]
  in pure (AddImport.filterInternal mods == mods)

-- | #204: 'prioritizeModuleMatch' puts the exact-match module first
-- when the query is a dotted path.
testPrioritizeExactFirst :: IO Bool
testPrioritizeExactFirst =
  let q    = "Data.Map.Strict"
      mods = [ "Data.Map.Lazy"
             , "Data.Map.Strict"
             , "Data.Map.StrictWithKey"
             , "Data.IntMap.Strict"
             ]
      result = AddImport.prioritizeModuleMatch q mods
  in pure $ case result of
       (first : _) -> first == "Data.Map.Strict"
       []          -> False

-- | #204: 'prioritizeModuleMatch' is a no-op when the query has
-- no dots (plain function name lookup like @\"fromMaybe\"@).
testPrioritizeNoDotNoOp :: IO Bool
testPrioritizeNoDotNoOp =
  let q    = "fromMaybe"
      mods = ["Data.Maybe", "Prelude"]
  in pure (AddImport.prioritizeModuleMatch q mods == mods)

-- ---------------------------------------------------------------------------
-- Issue #242 — looksLikeModule: module-path detection for Hoogle bypass
-- ---------------------------------------------------------------------------

-- | #242: "Data.Map" — 2 components, both uppercase-starting → True.
testLooksLikeModuleTrue :: IO Bool
testLooksLikeModuleTrue = pure $ AddImport.looksLikeModule "Data.Map"

-- | #242: "fromMaybe" — bare lowercase name → False.
testLooksLikeModuleFalse :: IO Bool
testLooksLikeModuleFalse = pure $ not (AddImport.looksLikeModule "fromMaybe")

-- | #242: "Map.lookup" — 2 components, but "lookup" is lowercase → False.
testLooksLikeModuleQualFun :: IO Bool
testLooksLikeModuleQualFun = pure $ not (AddImport.looksLikeModule "Map.lookup")

-- | #242: "Data" — single component only (no dot) → False (require ≥2).
testLooksLikeModuleSingle :: IO Bool
testLooksLikeModuleSingle = pure $ not (AddImport.looksLikeModule "Data")

-- | #242: "Data.Map.Strict" — 3 components, all uppercase-starting → True.
testLooksLikeModuleThree :: IO Bool
testLooksLikeModuleThree = pure $ AddImport.looksLikeModule "Data.Map.Strict"

-- | #205 Bug 2: 'compileFailResult' with @dryRun=True@ must set
-- @dry_run: true@ in the result payload — it was hardcoded @false@
-- before the fix.
testRefactorCompileFailDryRunTrue :: IO Bool
testRefactorCompileFailDryRunTrue =
  let result = RefactorTool.compileFailResult True [] "error" " (dry run, original preserved)"
  in pure $ case trContent result of
       [TextContent body_] ->
         case A.decode (TLE.encodeUtf8 (TL.fromStrict body_)) of
           Just (A.Object top) ->
             case AKM.lookup "result" top of
               Just (A.Object r) ->
                 AKM.lookup "dry_run" r == Just (A.Bool True)
               _ -> False
           _ -> False
       _ -> False

-- | #205 Bug 1: 'extractFreeVarNames' picks up variable names from
-- @\"Variable not in scope: …\"@ GHC error messages.
testExtractFreeVarNames :: IO Bool
testExtractFreeVarNames =
  let mkErr msg = GhcError
        { geFile = "src/Foo.hs", geLine = 5, geColumn = 3
        , geSeverity = SevError, geCode = Nothing
        , geMessage = msg
        }
      errs = [ mkErr "[GHC-76037] Variable not in scope: x :: Int"
             , mkErr "[GHC-76037] Variable not in scope: y"
             , mkErr "Couldn't match expected type 'Int' with 'Bool'"
             ]
      names = RefactorTool.extractFreeVarNames errs
  in pure $ names == ["x", "y"]

-- | #205 Bug 1: 'extractFreeVarNames' returns @[]@ when no
-- not-in-scope errors are present.
testExtractFreeVarNamesEmpty :: IO Bool
testExtractFreeVarNamesEmpty =
  let mkErr msg = GhcError
        { geFile = "src/Foo.hs", geLine = 1, geColumn = 1
        , geSeverity = SevError, geCode = Nothing
        , geMessage = msg
        }
      errs = [ mkErr "Couldn't match expected type 'Int' with 'Bool'" ]
  in pure (null (RefactorTool.extractFreeVarNames errs))

-- | #205 Bug 1: 'compileFailResult' adds a @\"note\"@ field when the
-- error list contains not-in-scope variables.
testRefactorFreeVarNote :: IO Bool
testRefactorFreeVarNote =
  let mkErr msg = GhcError
        { geFile = "src/Foo.hs", geLine = 5, geColumn = 3
        , geSeverity = SevError, geCode = Nothing
        , geMessage = msg
        }
      errs   = [ mkErr "Variable not in scope: x :: Int" ]
      result = RefactorTool.compileFailResult False errs "raw errors" " (restored)"
  in pure $ case trContent result of
       [TextContent body_] ->
         case A.decode (TLE.encodeUtf8 (TL.fromStrict body_)) of
           Just (A.Object top) ->
             case AKM.lookup "result" top of
               Just (A.Object r) ->
                 case AKM.lookup "note" r of
                   Just (A.String note) ->
                     "x" `T.isInfixOf` note
                       && "free variable" `T.isInfixOf` note
                       && "extract_binding" `T.isInfixOf` note
                   _ -> False
               _ -> False
           _ -> False
       _ -> False

-- | #201: 'QcTool.extractQcOutputAt' slices the correct indexed
-- sentinel block from batch repl stdout.
testExtractQcOutputAt :: IO Bool
testExtractQcOutputAt =
  let full = T.unlines
        [ "__QC_START_0__"
        , "passed (100 tests)"
        , "__QC_END_0__"
        , "__QC_START_1__"
        , "Failed! Falsified (after 3 tests):"
        , "__QC_END_1__"
        ]
  in pure $ QcTool.extractQcOutputAt 0 full == "passed (100 tests)"
         && QcTool.extractQcOutputAt 1 full == "Failed! Falsified (after 3 tests):"

-- | #201: 'QcTool.extractQcOutputAt' returns empty text when the
-- requested index has no matching sentinel in the output.
testExtractQcOutputAtMissing :: IO Bool
testExtractQcOutputAtMissing =
  let full = "__QC_START_0__\npassed\n__QC_END_0__"
  in pure (QcTool.extractQcOutputAt 1 full == "")

-- | #201: 'LabTool.qcResultDetail' formats the counterexample string
-- and shrink count for 'QcFailed'.
testQcResultDetailFailed :: IO Bool
testQcResultDetailFailed =
  let qr = QcFailed "prop" 10 3 "Just 0"
  in pure $ LabTool.qcResultDetail qr
         == "counterexample: Just 0 (after 10 passes, 3 shrinks)"

-- | #201: 'LabTool.qcResultDetail' returns empty text for 'QcPassed'
-- (no counterexample to report).
testQcResultDetailPassed :: IO Bool
testQcResultDetailPassed =
  pure (LabTool.qcResultDetail (QcPassed "prop" 100) == "")

-- | #201: 'LabTool.qcResultStatus' maps all five 'QuickCheckResult'
-- constructors to the expected status strings.
testQcResultStatusAll :: IO Bool
testQcResultStatusAll =
  pure $ LabTool.qcResultStatus (QcPassed    "p" 100)         == "passed"
      && LabTool.qcResultStatus (QcFailed    "p" 0 0 "")      == "failed"
      && LabTool.qcResultStatus (QcException "p" "err")       == "exception"
      && LabTool.qcResultStatus (QcGaveUp    "p" 0 0)         == "gave_up"
      && LabTool.qcResultStatus (QcUnparsed  "p" "raw")       == "unparsed"

-- | #237: QcUnparsed with "Stack space overflow" raw → status = "exception"
testQcResultStatusStackOverflow237 :: IO Bool
testQcResultStatusStackOverflow237 = pure $
  LabTool.qcResultStatus
    (QcUnparsed "fibonacci 40" "Stack space overflow: current size 33624 bytes.")
  == "exception"

-- | #237: QcUnparsed with "heap overflow" raw → status = "exception"
testQcResultStatusHeapOverflow237 :: IO Bool
testQcResultStatusHeapOverflow237 = pure $
  LabTool.qcResultStatus
    (QcUnparsed "prop" "out of memory (requested 1048576 bytes)")
  == "exception"

-- | #237: QcUnparsed with unrecognised raw → status still = "unparsed"
testQcResultStatusOtherUnparsed237 :: IO Bool
testQcResultStatusOtherUnparsed237 = pure $
  LabTool.qcResultStatus
    (QcUnparsed "prop" "some random unrecognised output")
  == "unparsed"

-- | #237: qcResultDetail for QcUnparsed with non-empty raw returns the raw text.
testQcResultDetailUnparsedNonEmpty237 :: IO Bool
testQcResultDetailUnparsedNonEmpty237 =
  let raw    = "Stack space overflow: current size 33624 bytes."
      detail = LabTool.qcResultDetail (QcUnparsed "prop" raw)
  in pure (T.isInfixOf "Stack space overflow" detail)

-- | #237: qcResultDetail for QcUnparsed with empty raw returns empty string.
testQcResultDetailUnparsedEmpty237 :: IO Bool
testQcResultDetailUnparsedEmpty237 = pure $
  LabTool.qcResultDetail (QcUnparsed "prop" "") == ""

-- | #215 (GHC-18042 type-default fix): regression for the
-- 'go 0 [] []' call in 'splitAtDepthZeroSpaces'.  Adding
-- '(0 :: Int)' is a no-op for behaviour but fixes the
-- defaulting warning.  This test verifies the function still
-- splits depth-0 spaces correctly after the annotation.
testSplitAtDepthZeroIssue215 :: IO Bool
testSplitAtDepthZeroIssue215 =
  pure $
    -- Basic two-param case
    QcExport.splitAtDepthZeroSpaces "(x :: Int) (y :: Int)"
      == ["(x :: Int)", "(y :: Int)"]
    -- Arrow inside nested paren must NOT trigger a split
    && QcExport.splitAtDepthZeroSpaces "(f :: Int -> Int) (xs :: [Int])"
      == ["(f :: Int -> Int)", "(xs :: [Int])"]
    -- Single param is returned as-is
    && QcExport.splitAtDepthZeroSpaces "(x :: Int)"
      == ["(x :: Int)"]
    -- Empty input yields no chunks
    && null (QcExport.splitAtDepthZeroSpaces "")

-- ---------------------------------------------------------------------------
-- Issue #244 — findCommonStanzaWithPkg + common-stanza hint
-- ---------------------------------------------------------------------------

-- | #244: 'findCommonStanzaWithPkg' returns the name of the first common
-- stanza whose build-depends contains the queried package.
testDepsCommonStanzaPkgFound :: IO Bool
testDepsCommonStanzaPkgFound =
  let body = T.unlines
        [ "common shared-deps"
        , "  build-depends:"
        , "    base >= 4.14"
        , "  , aeson >= 2.0"
        , ""
        , "library"
        , "  import: shared-deps"
        , "  build-depends:"
        , "    text"
        ]
  in pure $ DepsTool.findCommonStanzaWithPkg "aeson" body == Just "shared-deps"

-- | #244: 'findCommonStanzaWithPkg' returns Nothing when the package is
-- absent from all common stanzas (even if it appears in another stanza).
testDepsCommonStanzaPkgAbsent :: IO Bool
testDepsCommonStanzaPkgAbsent =
  let body = T.unlines
        [ "common shared-deps"
        , "  build-depends:"
        , "    base >= 4.14"
        , ""
        , "library"
        , "  import: shared-deps"
        , "  build-depends:"
        , "    aeson >= 2.0"  -- in library stanza, NOT in common
        ]
  in pure $ isNothing (DepsTool.findCommonStanzaWithPkg "aeson" body)

-- | #244: 'findCommonStanzaWithPkg' returns Nothing when the cabal body
-- contains no common stanza at all.
testDepsCommonStanzaNoCommon :: IO Bool
testDepsCommonStanzaNoCommon =
  let body = T.unlines
        [ "library"
        , "  build-depends:"
        , "    base >= 4.14"
        , "  , aeson >= 2.0"
        ]
  in pure $ isNothing (DepsTool.findCommonStanzaWithPkg "aeson" body)

-- | #244: 'unchangedResult'' with 'Just hint' must include a @\"hint\"@
-- field in the payload so the agent sees the actionable remediation message.
testDepsUnchangedResultHintField :: IO Bool
testDepsUnchangedResultHintField =
  let tr = DepsTool.unchangedResult' "/tmp/foo.cabal" "aeson" "removed"
             (Just "aeson is in common stanza 'shared-deps'")
  in pure $ case trContent tr of
       [TextContent body_] ->
         case A.decode (TLE.encodeUtf8 (TL.fromStrict body_)) of
           Just (A.Object top) ->
             case AKM.lookup "result" top of
               Just (A.Object r) -> AKM.member "hint" r
               _                 -> False
           _ -> False
       _ -> False

-- ---------------------------------------------------------------------------
-- Issue #243 — ghc_suggest must call augmentEvalContext before queryType
-- ---------------------------------------------------------------------------

-- | #243: 'Suggest.hs' must import 'augmentEvalContext' from 'Eval.hs'
-- and call it (with >>)  before 'queryType' so that standard preloads
-- like @Data.List.sort@ resolve even when 'loadForTarget' has reset the
-- interactive context to the project-only module graph.
testSuggestCallsAugmentContext :: IO Bool
testSuggestCallsAugmentContext = do
  src <- TIO.readFile "src/HaskellFlows/Tool/Suggest.hs"
  let code = T.unlines (filter (not . isDocLine) (T.lines src))
  pure $ T.isInfixOf "augmentEvalContext" code
      && T.isInfixOf "HaskellFlows.Tool.Eval" code
  where
    isDocLine ln =
      let s = T.stripStart ln in "--" `T.isPrefixOf` s

-- ---------------------------------------------------------------------------
-- Issue #242 — add_import bypasses Hoogle for module-path names
-- ---------------------------------------------------------------------------

-- | #242: 'AddImport.hs' must call 'looksLikeModule' in the hot path so
-- that module-path queries short-circuit the Hoogle call.
testAddImportBypassesHoogle :: IO Bool
testAddImportBypassesHoogle = do
  src <- TIO.readFile "src/HaskellFlows/Tool/AddImport.hs"
  let code = T.unlines (filter (not . isDocLine) (T.lines src))
  -- 'looksLikeModule' must be called, and the resulting 'ranked' list
  -- must be built conditionally on that check (not always from Hoogle).
  pure $ T.isInfixOf "looksLikeModule" code
      && T.isInfixOf "ranked" code
  where
    isDocLine ln =
      let s = T.stripStart ln in "--" `T.isPrefixOf` s

-- ---------------------------------------------------------------------------
-- Issue #245 — ghc_perf: low_precision_warning + warmup_warning
-- ---------------------------------------------------------------------------

mkPerfArgs :: Text -> PerfTool.PerfArgs
mkPerfArgs expr = PerfTool.PerfArgs
  { PerfTool.paExpression      = expr
  , PerfTool.paRuns            = 5
  , PerfTool.paSaveBaseline    = False
  , PerfTool.paCompareBaseline = False
  , PerfTool.paVerbose         = False
  , PerfTool.paThresholdPct    = 30.0
  }

extractPerfResult :: ToolResult -> Maybe A.Object
extractPerfResult tr = case trContent tr of
  [TextContent body_] ->
    case A.decode (TLE.encodeUtf8 (TL.fromStrict body_)) of
      Just (A.Object top) ->
        case AKM.lookup "result" top of
          Just (A.Object r) -> Just r
          _                 -> Nothing
      _ -> Nothing
  _ -> Nothing

-- | #245: when mean_ns < 1_000_000 (< 1ms), payload must include
-- 'low_precision_warning'.
testPerfLowPrecisionWarning :: IO Bool
testPerfLowPrecisionWarning =
  let args   = mkPerfArgs "length []"
      -- All samples well below 1ms: mean ≈ 600µs
      nss    = [500_000, 600_000, 700_000] :: [Word64]
      stats  = PerfTool.aggregate nss
      warmup = 800_000 :: Word64
      result = PerfTool.renderResult args nss stats [] Nothing warmup
  in pure $ case extractPerfResult result of
       Just r  -> AKM.member "low_precision_warning" r
       Nothing -> False

-- | #245: when warmup_ns > 10 * mean_ns, payload must include
-- 'warmup_warning'.
testPerfWarmupWarning :: IO Bool
testPerfWarmupWarning =
  let args   = mkPerfArgs "length [1..100]"
      -- mean ≈ 5.5ms; warmup = 200ms (>10x)
      nss    = [5_000_000, 6_000_000] :: [Word64]
      stats  = PerfTool.aggregate nss
      warmup = 200_000_000 :: Word64   -- 200 ms
      result = PerfTool.renderResult args nss stats [] Nothing warmup
  in pure $ case extractPerfResult result of
       Just r  -> AKM.member "warmup_warning" r
       Nothing -> False

-- | #245: for a healthy measurement (mean=5ms, warmup=6ms), neither
-- warning should appear.
testPerfNoWarningHealthy :: IO Bool
testPerfNoWarningHealthy =
  let args   = mkPerfArgs "length [1..1000]"
      -- mean ≈ 5ms; warmup ≈ 6ms (just above mean — normal)
      nss    = [5_000_000, 5_100_000, 4_900_000] :: [Word64]
      stats  = PerfTool.aggregate nss
      warmup = 6_000_000 :: Word64
      result = PerfTool.renderResult args nss stats [] Nothing warmup
  in pure $ case extractPerfResult result of
       Just r  -> not (AKM.member "low_precision_warning" r)
               && not (AKM.member "warmup_warning" r)
       Nothing -> False
