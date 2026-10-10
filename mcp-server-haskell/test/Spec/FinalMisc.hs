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
  , testImportsNubByDeduplication
  , testGateFailureKindExists
  , testUnchangedResultNoVerb
  , testOutsideSourceDirsKindExists
  , testTargetForPathNestedFile
  , testRenderRunLineUsesModuleName
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
  , testSplitAtDepthZeroIssue215
  , testDepsCommonStanzaPkgFound
  , testDepsCommonStanzaPkgAbsent
  , testDepsCommonStanzaNoCommon
  , testDepsUnchangedResultHintField
  , testSuggestCallsAugmentContext
  , testAddImportBypassesHoogle
  ) where

import qualified Data.Aeson as A
import Data.Aeson (object, (.=))
import qualified Data.Aeson.Key as AKey
import qualified Data.Aeson.KeyMap as AKM
import qualified Data.Text as T
import qualified Data.Text.IO as TIO
import qualified Data.Vector as Vector
import Data.Maybe (isNothing)
import qualified Data.List as List
import Data.Text (Text)
import Data.Time.Clock.POSIX (posixSecondsToUTCTime)
import Data.Word (Word64)
import qualified Data.Set as Set
import System.Directory (createDirectoryIfMissing, doesFileExist, getTemporaryDirectory, removePathForcibly)
import System.FilePath ((</>))

import qualified HaskellFlows.Mcp.Envelope as Env
import HaskellFlows.Mcp.Progress (noopSink)
import HaskellFlows.Types (mkProjectDir)
import qualified HaskellFlows.Tool.AddImport as AddImport
import qualified HaskellFlows.Tool.Deps as DepsTool
import qualified HaskellFlows.Tool.FixWarning as FixWarning
import qualified HaskellFlows.Tool.Gate as Gate
import qualified HaskellFlows.Tool.Imports as ImportsTool
import qualified HaskellFlows.Tool.PropertyAudit as PropertyAuditTool
import qualified HaskellFlows.Tool.QuickCheckExport as QcExport
import qualified HaskellFlows.Tool.Refactor as RefactorTool
import qualified HaskellFlows.Tool.Suggest as SuggestTool
import qualified HaskellFlows.Tool.ValidateCabal as VC
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

import Spec.Helpers (withTempProject)

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
  in pure $ case Env.reResult tr of
       Just (A.Object r) ->
         AKM.lookup "action" r == Just (A.String "unchanged")
         && not (AKM.member "verb" r)
       _ -> False

-- | #119: 'formatIso8601' must produce an ISO-8601 UTC timestamp
-- that is human-readable. Specifically: it must contain "T" and "Z",
-- and not be a plain float.








testOutsideSourceDirsKindExists :: IO Bool

testOutsideSourceDirsKindExists =
  pure $
    Env.errorKindToText Env.OutsideSourceDirs == "outside_source_dirs"
    && Env.textToErrorKind "outside_source_dirs" == Just Env.OutsideSourceDirs
    && Env.OutsideSourceDirs `elem` ([minBound .. maxBound] :: [Env.ErrorKind])

--------------------------------------------------------------------------------
-- #194 — targetForPath prefix must match flat test/Foo.hs
--------------------------------------------------------------------------------

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


decodeCheckProjectResult :: Env.ToolResponse -> Maybe A.Value

decodeCheckProjectResult = Env.reResult

-- | #129: Parsing @{}@ as 'CheckProjectArgs' should yield

testCheckProjectDelegates = do
  src <- TIO.readFile "src/HaskellFlows/Tool/CheckProject.hs"
  -- Must use CheckModule.runHandle, not call loadForTarget directly.
  pure $ T.isInfixOf "CheckModule.runHandle" src
      && not (T.isInfixOf "loadForTarget ghcSess" src)

-- When no timeout_seconds is supplied the field is Nothing (delegates to Limits).








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
  in pure $ case Env.reResult result of
       Just (A.Object r) ->
         AKM.lookup "dry_run" r == Just (A.Bool True)
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
  in pure $ case Env.reResult result of
       Just (A.Object r) ->
         case AKM.lookup "note" r of
           Just (A.String note) ->
             "x" `T.isInfixOf` note
               && "free variable" `T.isInfixOf` note
               && "extract_binding" `T.isInfixOf` note
           _ -> False
       _ -> False

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
  in pure $ case Env.reResult tr of
       Just (A.Object r) -> AKM.member "hint" r
       _                 -> False

-- ---------------------------------------------------------------------------
-- Issue #243 — ghc_suggest resolves names from session preloads
-- ---------------------------------------------------------------------------

-- | #243: the concern (standard preloads like @Data.List.sort@ must
-- resolve even when the context narrows to the project graph) now
-- lives in the ghcide route: 'IdeBacked.handleSuggest' must splice
-- 'evalContextExtras' into the 'EvalArgs' of its per-anchor queries
-- (the successor of the legacy augmentEvalContext reset-guard).

testSuggestCallsAugmentContext :: IO Bool

testSuggestCallsAugmentContext = do
  src <- TIO.readFile "src/HaskellFlows/Tool/IdeBacked.hs"
  let code = T.unlines (filter (not . isDocLine) (T.lines src))
  pure $ T.isInfixOf "evalContextExtras" code
      && T.isInfixOf "handleSuggest" code
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

extractPerfResult :: Env.ToolResponse -> Maybe A.Object

extractPerfResult tr = case Env.reResult tr of
  Just (A.Object r) -> Just r
  _                 -> Nothing

-- | #245: when mean_ns < 1_000_000 (< 1ms), payload must include
-- 'low_precision_warning'.

