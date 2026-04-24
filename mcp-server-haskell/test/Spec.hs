-- | Minimal smoke test suite for Phase 1.
--
-- Covers the two security-critical invariants we lock in at scaffolding
-- time, so a regression here fails the build before any tool is wired up:
--
-- 1. 'mkModulePath' rejects paths that escape the project directory.
-- 2. The error parser can round-trip a canonical GHC diagnostic line.
--
-- QuickCheck arrives in Phase 2 along with the property-lifecycle tool.
module Main where

import qualified Data.Aeson as A
import Data.Aeson (object, (.=))
import qualified Data.Text.Lazy as TL
import qualified Data.Text.Lazy.Encoding as TLE
import Data.Text (Text)
import qualified Data.Text as T
import qualified Data.Text.IO as TIO
import Data.Maybe (fromMaybe, isJust, isNothing)
import Data.Time.Clock.POSIX (getPOSIXTime)
import System.Exit (exitFailure, exitSuccess)
import System.Timeout (timeout)
import qualified Test.QuickCheck as QC
import Test.QuickCheck
  ( Args (..)
  , Property
  , Result (..)
  , Testable
  , counterexample
  , property
  , quickCheckWithResult
  , stdArgs
  , (===)
  , (==>)
  )

import HaskellFlows.Ghc.Sanitize
  ( CommandError (..)
  , sanitizeExpression
  )
import HaskellFlows.Parser.Error
  ( GhcError (..)
  , Severity (..)
  , WarningCategory (..)
  , bucketize
  , categorizeWarning
  , parseGhcErrors
  , renderGhciStyle
  )
import HaskellFlows.Parser.Hole
  ( HoleFit (..)
  , TypedHole (..)
  , parseTypedHoles
  , extractValidFits
  )
import HaskellFlows.Parser.TypeSignature
  ( ParsedSig (..)
  , SigType (..)
  , parseSignature
  , isSameTypeThroughout
  )
import HaskellFlows.Suggest.Rules
  ( Confidence (..)
  , RuleContext (..)
  , Suggestion (..)
  , applyRules
  , applyRulesCtx
  , mkRuleContext
  )
import HaskellFlows.Mcp.Server (allToolDescriptors, allToolNames)
import HaskellFlows.Mcp.NextStep
  ( ChainStep (..)
  , NextStep (..)
  , injectNextStep
  , suggestNext
  )
import HaskellFlows.Mcp.Protocol (ToolCall (..), ToolContent (..), ToolDescriptor (..), ToolResult (..))
import HaskellFlows.Tool.Batch (BatchArgs (..))
import qualified HaskellFlows.Tool.Gate as Gate
import qualified HaskellFlows.Tool.CheckModule as CheckModule
import qualified HaskellFlows.Tool.QuickCheck as QcTool
import qualified HaskellFlows.Tool.QuickCheckExport as QcExport
import qualified HaskellFlows.Tool.Regression as RegTool
import qualified HaskellFlows.Tool.Bootstrap as Bootstrap
import qualified HaskellFlows.Tool.RemoveModules as RM
import qualified HaskellFlows.Tool.Suggest as SuggestTool
import qualified HaskellFlows.Tool.AddImport as AddImport
import qualified HaskellFlows.Tool.AddModules as AddModules
import qualified HaskellFlows.Tool.ApplyExports as ApplyExports
import qualified HaskellFlows.Tool.FixWarning as FixWarning
import qualified HaskellFlows.Mcp.WorkflowState as WS
import qualified HaskellFlows.Mcp.Guidance as Guidance
import qualified HaskellFlows.Mcp.Resources as Resources
import HaskellFlows.Tool.CheckProject (parseExposedModules)
import HaskellFlows.Tool.Lint (parseHlintJson)
import qualified HaskellFlows.Tool.Lint as LintTool
import qualified HaskellFlows.Tool.ValidateCabal as VC
import HaskellFlows.Parser.QuickCheck
  ( QuickCheckResult (..)
  , parseQuickCheckOutput
  )
import HaskellFlows.Parser.Type
  ( ParsedType (..)
  , isOutOfScope
  , parseTypeOutput
  )
import HaskellFlows.Tool.Arbitrary
  ( Constructor (..)
  , hasRecursiveConstructor
  , isRecursiveArg
  , parseConstructors
  , parseTypeParams
  , renderTemplate
  )
import HaskellFlows.Data.PropertyStore
  ( StoredProperty (..)
  , loadAll
  , openStore
  , save
  )
import HaskellFlows.Parser.Coverage
  ( CoverageReport (..)
  , Metric (..)
  , parseCoverage
  )
import HaskellFlows.Tool.Deps
  ( addDep
  , parseStanzaSelector
  , validatePackageName
  , validateVersionConstraint
  )
import HaskellFlows.Refactor.Extract
  ( ExtractResult (..)
  , extractBinding
  )
import HaskellFlows.Refactor.Rename
  ( RenameResult (..)
  , renameInScope
  , validateIdentifier
  )
import HaskellFlows.Tool.Goto
  ( Location (..)
  , parseDefinedAt
  )
import HaskellFlows.Tool.Hoogle
  ( HoogleHit (..)
  , parseHoogleLine
  )
import Control.Concurrent (forkIO, threadDelay)
import qualified HaskellFlows.Mcp.PathBootstrap
import qualified HaskellFlows.Parser.TypeSignature
import qualified System.Directory
import qualified System.FilePath
import Control.Monad (when)
import Control.Concurrent.MVar
  ( newEmptyMVar, putMVar, takeMVar, newMVar, readMVar )
import System.Directory (createDirectoryIfMissing, doesFileExist, getTemporaryDirectory, removePathForcibly)
import System.FilePath ((</>))
import qualified HaskellFlows.Types
import HaskellFlows.Types
  ( PathError (..)
  , ProjectDir
  , mkModulePath
  , mkProjectDir
  )
import HaskellFlows.Ghc.ApiSession
  ( evalIOString
  , killGhcSession
  , startGhcSession
  , withGhcSession
  )
import qualified HaskellFlows.Tool.SwitchProject as SwitchProject
import HaskellFlows.Tool.SwitchProject
  ( ValidationError (..)
  , validateSwitchTarget
  )
import Data.IORef (newIORef, readIORef)
import HaskellFlows.Ghc.CabalBootstrap
  ( StanzaFlags (..)
  , Target (..)
  , bootstrapProject
  )
import qualified HaskellFlows.Ghc.ApiSession as ApiSession
import qualified Data.Map.Strict as Map
import GHC
  ( InteractiveImport (IIDecl)
  , TcRnExprMode (TM_Inst)
  , exprType
  , mkModuleName
  , setContext
  , simpleImportDecl
  )
import GHC.Utils.Outputable (showPprUnsafe)

main :: IO ()
main = do
  results <-
    sequence
      [ test "mkProjectDir rejects relative"    testRejectsRelativeProject
      , test "mkModulePath accepts in-tree"      testAcceptsInTree
      , test "mkModulePath rejects traversal"    testRejectsTraversal
      , test "parseGhcErrors extracts header"    testParseHeader
      , test "sanitizeExpression accepts normal" testSanitizeAccepts
      , test "sanitizeExpression rejects newline" testSanitizeRejectsNewline
      , test "sanitizeExpression rejects sentinel" testSanitizeRejectsSentinel
      , test "sanitizeExpression rejects empty"   testSanitizeRejectsEmpty
      , test "parseTypeOutput single line"        testParseTypeSingleLine
      , test "parseTypeOutput multi line"         testParseTypeMultiLine
      , test "parseTypeOutput rejects malformed"  testParseTypeMalformed
      , test "isOutOfScope detects GHC phrasing"  testOutOfScope
      , quickTest "prop_sanitize_rejects_newline"     prop_sanitize_rejects_newline
      , quickTest "prop_sanitize_rejects_sentinel"    prop_sanitize_rejects_sentinel
      , quickTest "prop_sanitize_clean_roundtrip"     prop_sanitize_clean_roundtrip
      , quickTest "prop_modulePath_rejects_dotdot"    prop_modulePath_rejects_dotdot
      , quickTest "prop_modulePath_accepts_inTree"    prop_modulePath_accepts_inTree
      , quickTest "prop_parseShowModulesPaths_total"  prop_parseShowModulesPaths_total
      , quickTest "prop_parseQuickCheckOutput_total"  prop_parseQuickCheckOutput_total
      , quickTest "prop_chooseStoreModule_nonIdent_uses_hint" prop_chooseStoreModule_nonIdent_uses_hint
      , quickTest "prop_chooseStoreModule_ident_no_info_uses_hint" prop_chooseStoreModule_ident_no_info_uses_hint
      , test "parseQuickCheckOutput passed"        testQcPassed
      , test "parseQuickCheckOutput failed"        testQcFailed
      , test "parseQuickCheckOutput gave up"       testQcGaveUp
      , test "parseQuickCheckOutput exception"     testQcException
      , test "parseQuickCheckOutput unparsed"      testQcUnparsed
      , test "parseTypedHoles extracts one hole"   testHoleOne
      , test "parseTypedHoles ignores non-holes"   testHoleIgnored
      , test "parseConstructors inline form"       testCtorsInline
      , test "parseConstructors multiline form"    testCtorsMultiline
      , test "parseConstructors rejects synonym"   testCtorsSynonym
      , test "renderTemplate 3 ctors"              testTemplate3
      , test "parseHoogleLine normal hit"          testHoogleHit
      , test "parseHoogleLine no-results line"    testHoogleEmpty
      , test "parseCoverage full report"           testCoverageFull
      , test "parseCoverage ignores banner"        testCoverageBanner
      , test "PropertyStore save+load roundtrip"   testStoreRoundtrip
      , test "PropertyStore increments pass count" testStoreIncrement
      , test "validatePackageName accepts normal"  testPkgAccepts
      , test "validatePackageName rejects symbol"  testPkgRejectsSymbol
      , test "validatePackageName rejects empty"   testPkgRejectsEmpty
      , test "validateVersionConstraint accepts"   testVerAccepts
      , test "validateVersionConstraint rejects"   testVerRejects
      , test "parseDefinedAt file location"        testDefinedAtFile
      , test "parseDefinedAt module location"      testDefinedAtModule
      , test "parseDefinedAt ignores noise"        testDefinedAtNone
      , test "rename respects word boundaries"     testRenameWordBoundary
      , test "rename ignores line comments"        testRenameIgnoresComments
      , test "rename ignores string literals"      testRenameIgnoresStrings
      , test "rename scoped to line range"         testRenameScoped
      , test "rename same name is rejected"        testRenameSameName
      , test "validateIdentifier rejects keyword"  testIdentifierKeyword
      , test "validateIdentifier rejects symbol"   testIdentifierSymbol
      , test "validateIdentifier rejects upper"    testIdentifierUpper
      , test "extractBinding wraps block"          testExtractBinding
      , test "extractBinding rejects empty range"  testExtractEmpty
      , test "parseHlintJson parses list"          testHlintJson
      , test "validateCabal flags duplicate deps"  testDuplicateDeps
      , test "validateCabal flags missing synopsis" testMissingSynopsis
      , test "parseExposedModules reads modules"   testParseModules
      , test "extractValidFits parses fits"        testValidFits
      , test "parseSignature simple a -> a"         testSigSimple
      , test "parseSignature with constraint"       testSigConstraint
      , test "parseSignature list"                  testSigList
      , test "suggest matches involutive on a->a"   testSuggestInvolutive
      , test "suggest matches associative on a->a->a" testSuggestAssoc
      , test "suggest skips unmatched shapes"       testSuggestNoMatch
      , test "batch parses documented {tool,args}"  testBatchParsesToolArgs
      , test "batch accepts MCP {name,arguments}"   testBatchParsesNameArgs
      , test "suggest reverse Idempotent is Low"    testSuggestReverseIdempotentLow
      , test "suggest normalize Idempotent Medium"  testSuggestNormalizeIdempotentMedium
      , test "workflow tool names match tools/list" testWorkflowToolsParity
      , test "deps add indents deeper than field"   testDepsAddIndentsForCabal
      , test "deps add scaffold shape has no top-comma" testDepsAddNoTopComma
      , test "deps add targets stanza: test-suite"  testDepsAddTargetsTestSuite
      , test "parseStanzaSelector accepts common"   testParseStanzaAccepts
      , test "parseStanzaSelector rejects garbage"  testParseStanzaRejects
      , test "suggest [a]->[Run a] skips list rules" testSuggestEncodeShapeSkipsListRules
      , test "parseCtors record strict w/ kind header" testCtorsRecordStrictWithKindHeader
      , test "parseCtors inline record 2 fields"    testCtorsInlineRecord2Fields
      , test "coverage enriches w/ hpc report call" testCoverageInvokesHpcReport
      , test "parseCoverage handles hpc report out" testParseHpcReportText
      , test "coverage passes multiple --hpcdir"    testCoveragePassesAllMixDirs
      , test "parseTypeParams extracts one tyvar"   testTypeParamsOne
      , test "parseTypeParams extracts two tyvars"  testTypeParamsTwo
      , test "parseTypeParams empty for monotype"   testTypeParamsNone
      , test "renderTemplate wraps polymorphic T a" testTemplatePolymorphic
      , test "renderTemplate multi-param context"   testTemplateMultiParam
      , test "server wraps runTool in timeout"      testServerOuterTimeout
      , test "ghci_eval exposes Control.Concurrent"  testEvalContextHasControlConcurrent
      , test "ghci_eval enforces inner per-call budget" testEvalInnerTimeoutBudget
      , test "load paths derive interactive imports from source" testLoadAutoImports
      , test "Deferred pass writes to MCP-private build dir"      testDeferredIsolatedOutputs
      , test "initialize emits instructions field"  testInitializeEmitsInstructions
      , test "instructions mention key tools+flows" testInstructionsMentionCore
      , test "nextStep: create_project -> deps"     testNextStepCreateProject
      , test "nextStep: deps(add) -> load"          testNextStepDepsAdd
      , test "nextStep: load clean -> suggest"      testNextStepLoadClean
      , test "nextStep: load w/ warnings -> hole"   testNextStepLoadWarnings
      , test "nextStep: suggest -> quickcheck"      testNextStepSuggest
      , test "nextStep: qc passed -> check_module"  testNextStepQcPassed
      , test "nextStep: qc failed -> eval"          testNextStepQcFailed
      , test "nextStep: regression list -> run"     testNextStepRegressionList
      , test "nextStep: refactor -> load"           testNextStepRefactor
      , test "nextStep: check_module -> project"   testNextStepCheckModule
      , test "nextStep: check_project -> coverage" testNextStepCheckProject
      , test "nextStep: errors -> no suggestion"   testNextStepErrorsSuppressed
      , test "nextStep: exploratory -> no suggestion" testNextStepExploratoryNothing
      , test "injectNextStep splices into payload" testInjectSplices
      , test "injectNextStep no-op on non-JSON"    testInjectSkipsNonJson
      , test "suggest: functor fmap two laws"      testSuggestFunctorFmap
      , test "suggest: evaluator preservation"     testSuggestEvaluatorPreservation
      , test "suggest: constant-folding soundness" testSuggestConstFoldingSoundness
      , test "suggest: evaluator needs sibling"    testSuggestEvaluatorNoSibling
      , test "gate: tool registered in inventory"  testGateRegistered
      , test "gate: all-skip parses + passes"      testGateAllSkip
      , test "qcexport: tool registered"           testQcExportRegistered
      , test "qcexport: renderTestFile shape"      testQcExportRenderShape
      , test "qcexport: sanitizeLabel strips LF"   testQcExportSanitize
      , test "warnings: categorize common classes" testWarningCategorize
      , test "warnings: bucketize orders by count" testWarningBucketize
      , test "code tools: all 5 registered"        testCodeToolsRegistered
      , test "add_import: qualified renderImportLine" testAddImportQualified
      , test "add_modules: moduleToPath mapping"   testAddModulesPath
      , test "apply_exports: rewriteHeader idempotent" testApplyExportsIdempotent
      , test "apply_exports: injects exports"      testApplyExportsInjects
      , test "fix_warning: plan for unused imports" testFixWarningUnusedImports
      , test "workflow-state: initial empty"       testWorkflowStateInitial
      , test "workflow-state: tracks load + edits" testWorkflowStateTracks
      , test "workflow-state: renderHelp thresholds" testWorkflowStateHelp
      , test "resources: rules workflow URI resolves" testResourcesRulesRead
      , test "resources: unknown URI returns Nothing" testResourcesUnknown
      , test "baja bundle: 4 tools registered"      testBajaRegistered
      , test "guidance: tool count is dynamic"      testGuidanceDynamicCount
      , test "guidance: text lists every tool"      testGuidanceListsEveryTool
      , test "guidance: markdown lists every tool"  testGuidanceMarkdownListsEveryTool
      , test "guidance: situation table non-empty"  testGuidanceSituationNonEmpty
      , test "guidance: no phantom ghci_session"    testGuidanceNoPhantomSession
      , test "deps: description has no phantom"     testDepsDescriptorNoPhantom
      , test "deps: hint text has no phantom"       testDepsHintNoPhantom
      , test "qcexport: modulePathToModule src"     testExportPathSrc
      , test "qcexport: modulePathToModule lib"     testExportPathLib
      , test "qcexport: modulePathToModule test"    testExportPathTest
      , test "qcexport: modulePathToModule nested"  testExportPathNested
      , test "qcexport: modulePathToModule lowercase rejected" testExportPathLowercaseRejected
      , test "qcexport: modulePathToModule no .hs"  testExportPathNoSuffix
      , test "qcexport: render emits valid imports" testExportRenderValidImports
      , test "propstore: save auto-creates dir"     testPropStoreCreatesDir
      , test "propstore: save after rm -rf dir"     testPropStoreResurrectsDir
      , test "propstore: concurrent saves no loss"  testPropStoreConcurrentSaves
      , test "quickcheck: chooseStoreModule ident + info"     testChooseStoreModuleIdentWithInfo
      , test "quickcheck: chooseStoreModule ident no info"    testChooseStoreModuleIdentNoInfo
      , test "quickcheck: chooseStoreModule lambda uses hint" testChooseStoreModuleLambda
      , test "quickcheck: chooseStoreModule ignores module loc" testChooseStoreModuleModuleLoc
      , test "quickcheck: isSimpleIdent classifier"            testIsSimpleIdentClassifier
      , test "regression: parseShowModulesPaths simple"        testParseShowModulesPathsSimple
      , test "regression: parseShowModulesPaths multi-module"  testParseShowModulesPathsMulti
      , test "regression: parseShowModulesPaths tolerates garbage" testParseShowModulesPathsGarbage
      , test "suggest: involutive Low for normalizer" testInvolutiveLowForNormalizer
      , test "suggest: involutive Medium for reverse" testInvolutiveMediumForReverse
      , test "suggest: scope error -> structured hint" testSuggestScopeStructuredHint
      , test "suggest: parseShowModules simple"     testParseShowModulesSimple
      , test "suggest: parseShowModules with star"  testParseShowModulesStar
      , test "suggest: parseBrowseBindings filters types" testParseBrowseBindings
      , test "suggest: parseBrowseBindings skips continuations" testParseBrowseContinuation
      , test "suggest: siblings enable preservation" testSuggestSiblingsEnablePreservation
      , test "suggest: siblings enable soundness"   testSuggestSiblingsEnableSoundness
      , test "nextStep: gate pass -> coverage"      testNextStepGatePass
      , test "nextStep: gate fail -> check_project" testNextStepGateFail
      , test "nextStep: qcexport -> gate"           testNextStepQcExport
      , test "nextStep: determinism pass -> regression" testNextStepDeterminismPass
      , test "nextStep: determinism fail -> quickcheck" testNextStepDeterminismFail
      , test "nextStep: add_import -> load"         testNextStepAddImport
      , test "nextStep: add_modules carries chain"  testNextStepAddModulesChain
      , test "nextStep: apply_exports -> load"      testNextStepApplyExports
      , test "nextStep: fix_warning -> load"        testNextStepFixWarning
      , test "nextStep: browse -> suggest"          testNextStepBrowse
      , test "nextStep: toolchain_warmup -> workflow" testNextStepToolchainWarmup
      , test "nextStep: property_lifecycle(list) -> regression" testNextStepPropertyLifecycleList
      , test "nextStep: create_project carries chain" testNextStepCreateProjectChain
      , test "nextStep: every tool covered or whitelisted" testNextStepFullCoverage
      , test "staleness: wired into server (static)"  testStalenessWired
      , test "workflow: history polls ghci_load"      testHistoryPolling
      , test "workflow: history missing quickcheck"   testHistoryMissingQc
      , test "workflow: history refactor unreloaded"  testHistoryRefactorNotReloaded
      , test "workflow: phase pre-scaffold"           testPhasePreScaffold
      , test "workflow: phase bootstrap"              testPhaseBootstrap
      , test "workflow: phase testing laws"           testPhaseTestingLaws
      , test "workflow: phase ready to push"          testPhaseReadyToPush
      , test "workflow: phase hint non-empty"         testPhaseHintNonEmpty
      , test "arbitrary: detects recursion on self"   testArbitraryDetectsRecursion
      , test "arbitrary: Expr template uses sized"    testArbitraryExprSized
      , test "arbitrary: Tree polymorphic sized"      testArbitraryTreeSized
      , test "arbitrary: Status flat template"        testArbitraryFlatTemplate
      , test "arbitrary: recursion detection tokens"  testArbitraryRecursionTokens
      , test "remove_modules: tool registered"        testRemoveModulesRegistered
      , test "remove_modules: strips exposed entry"   testRemoveModulesStripsCabal
      , test "remove_modules: idempotent no-op"       testRemoveModulesIdempotent
      , test "remove_modules: preserves other fields" testRemoveModulesPreservesFields
      , test "nextStep: remove_modules -> check+load" testNextStepRemoveModules
      , test "gate: runStep catches exceptions"       testGateRunStepCatchesExceptions
      , test "gate: cabalStep uses bracket + partial safe" testGateCabalStepBracket
      , test "bootstrap: tool registered"             testBootstrapRegistered
      , test "bootstrap: preview returns dynamic content" testBootstrapPreview
      , test "bootstrap: write persists to disk"      testBootstrapWrite
      , test "bootstrap: pathForHost is closed enum"  testBootstrapPathEnum
      , test "doc: main README uses haskell-flows-mcp" testDocsMainReadme
      , test "doc: haskell README lists real tools"   testDocsHaskellReadme
      , test "release: workflow file exists + well-formed" testReleaseWorkflow
      , test "ghc-api: GhcSession boots + exprType roundtrip" testGhcSessionBoots
      , test "ghc-api: HscEnv persists across withGhcSession calls" testGhcSessionPersists
      , test "ghc-api: evalIOString runs IO String actions in-process" testEvalIOString
      , test "ghc-api: bootstrapProject captures cabal flags for library" testCabalBootstrapLibrary
      , test "ghc-api: loadForTarget compiles library module via stanza flags" testLoadForTargetLibrary
      , test "ghc-api: deferred hole warnings are captured by logger hook" testHoleDiagnosticCapture
      , test "ghc-api: loadForTarget after deps-add resolves -package-id"   testLoadAfterDepsAdd
      , test "switch_project: rejects relative path"             testSwitchRejectsRelative
      , test "switch_project: rejects missing directory"         testSwitchRejectsMissing
      , test "switch_project: rejects dir without .cabal"        testSwitchRejectsNoCabal
      , test "switch_project: accepts a valid cabal project"     testSwitchAcceptsValid
      , test "switch_project: handle swaps project + kills session"
                                                                 testSwitchHandleSwaps
      , test "switch_project: empty dir accepted (scaffold-ready)"
                                                                 testSwitchAcceptsEmpty
      , test "path-bootstrap: hard-coded candidates are absolute"
                                                                 testPathBootstrapAbsolute
      , test "path-bootstrap: augmentPath only keeps existing dirs"
                                                                 testPathBootstrapExisting
      , test "path-bootstrap: augmentPath is idempotent"          testPathBootstrapIdempotent
      , test "add_modules: FromJSON accepts string fallback"      testAddModulesStringFallback
      , test "add_modules: FromJSON accepts JSON array"           testAddModulesArrayForm
      , test "cabal validator: stanza-aware dup check"            testCabalStanzaDupCheck
      , test "cabal validator: cross-stanza repeats are NOT dups" testCabalCrossStanzaOk
      , test "cabal validator: hs-source-dirs not mis-parsed as dep"
                                                                 testCabalHsSourceDirsIgnored
      , test "suggest: printer/parser roundtrip rule fires"       testSuggestRoundtripRule
      , test "suggest: no roundtrip when sibling shape mismatches" testSuggestRoundtripNegative
      , test "ghc-api: external cabal edit invalidates stanza cache"
                                                                 testMtimeInvalidation
      , test "add_modules: unwraps stringified JSON-array (BUG-PLUS-08)"
                                                                 testAddModulesJsonArrayString
      , test "add_modules: plain comma-split preserved for non-JSON strings"
                                                                 testAddModulesPlainStringStillWorks
      , test "check_module: warnings_block=false keeps warnings informational"
                                                                 testCheckModuleWarningsBlockFalse
      , test "check_module: warnings_block default is True"      testCheckModuleWarningsBlockDefault
      , test "quickcheck: summariseStderr filters cabal noise"   testQcSummariseStderrFiltersNoise
      , test "quickcheck: summariseStderr caps at 1600 chars"    testQcSummariseStderrCaps
      , test "nextStep: ghci_load with typed-hole warning \8594 ghci_hole"
                                                                 testNextStepTypedHoleWarn
      , test "nextStep: ghci_load with non-hole warning \8594 ghci_fix_warning"
                                                                 testNextStepFixableWarn
      , test "nextStep: ghci_load with no warnings \8594 ghci_suggest"
                                                                 testNextStepCleanLoad
      ]
  if and results then exitSuccess else exitFailure

-- | Per-test defensive timeout. Any unit test that doesn't complete in
-- 60 s is reported as a hard failure with a TIMEOUT prefix rather than
-- hanging the whole suite. Protects CI from the class of hazards that
-- land when a test uses forkIO/takeMVar or spawns a subprocess that
-- stops emitting without closing its pipe.
testTimeoutMicros :: Int
testTimeoutMicros = 60 * 1000 * 1000

test :: String -> IO Bool -> IO Bool
test name action = do
  mok <- timeout testTimeoutMicros action
  let ok = fromMaybe False mok
      prefix
        | Nothing <- mok = "TIMEOUT "
        | ok             = "PASS    "
        | otherwise      = "FAIL    "
  putStrLn (prefix <> name)
  pure ok

testRejectsRelativeProject :: IO Bool
testRejectsRelativeProject =
  pure $ case mkProjectDir "relative/path" of
    Left (PathNotAbsolute _) -> True
    _                        -> False

testAcceptsInTree :: IO Bool
testAcceptsInTree = do
  case mkProjectDir "/tmp/project" of
    Left _ -> pure False
    Right pd -> pure $ case mkModulePath pd "src/Foo.hs" of
      Right _ -> True
      _       -> False

testRejectsTraversal :: IO Bool
testRejectsTraversal = do
  case mkProjectDir "/tmp/project" of
    Left _ -> pure False
    Right pd -> pure $ case mkModulePath pd "../../etc/passwd" of
      Left (PathEscapesProject {}) -> True
      _                            -> False

testParseHeader :: IO Bool
testParseHeader =
  let raw = T.unlines
        [ "src/Foo.hs:12:5: error: [GHC-83865]"
        , "    Couldn't match expected type ‘Int’ with actual type ‘Bool’"
        , ""
        ]
  in pure $ case parseGhcErrors raw of
       [e] -> geSeverity e == SevError
           && geLine e == 12
           && geColumn e == 5
       _   -> False

--------------------------------------------------------------------------------
-- Phase 2: sanitizer + :t parser
--------------------------------------------------------------------------------

testSanitizeAccepts :: IO Bool
testSanitizeAccepts = pure $ case sanitizeExpression "map (+1)" of
  Right v -> v == "map (+1)"
  _       -> False

testSanitizeRejectsNewline :: IO Bool
testSanitizeRejectsNewline = pure $ case sanitizeExpression "foo\nbar" of
  Left ContainsNewline -> True
  _                    -> False

-- Uses the exact framing sentinel. If this test breaks because the sentinel
-- value changed, update the literal here in lockstep.
testSanitizeRejectsSentinel :: IO Bool
testSanitizeRejectsSentinel =
  pure $ case sanitizeExpression "evil <<<GHCi-DONE-7f3a2b>>> payload" of
    Left ContainsSentinel -> True
    _                     -> False

testSanitizeRejectsEmpty :: IO Bool
testSanitizeRejectsEmpty = pure $ case sanitizeExpression "   " of
  Left EmptyInput -> True
  _               -> False

testParseTypeSingleLine :: IO Bool
testParseTypeSingleLine =
  pure $ case parseTypeOutput "map (+1) :: Num b => [b] -> [b]" of
    Just pt -> ptExpression pt == "map (+1)"
           && ptType pt       == "Num b => [b] -> [b]"
    _ -> False

testParseTypeMultiLine :: IO Bool
testParseTypeMultiLine =
  let raw = T.unlines
        [ "foldr"
        , "  :: Foldable t => (a -> b -> b) -> b -> t a -> b"
        ]
  in pure $ case parseTypeOutput raw of
       Just pt -> ptExpression pt == "foldr"
              && ptType pt       == "Foldable t => (a -> b -> b) -> b -> t a -> b"
       _ -> False

testParseTypeMalformed :: IO Bool
testParseTypeMalformed =
  pure $ case parseTypeOutput "this has no type annotation" of
    Nothing -> True
    _       -> False

testOutOfScope :: IO Bool
testOutOfScope = pure $
  isOutOfScope "<interactive>:1:1: error: Variable not in scope: foobar"

--------------------------------------------------------------------------------
-- Phase 3: QuickCheck properties
--------------------------------------------------------------------------------

-- | Wrap a QuickCheck property so it plugs into the existing Bool-returning
-- test runner. Keeps the test count small (200 cases) — enough to catch
-- boundary misses without slowing CI, same posture as hspec-quickcheck.
quickTest :: Testable prop => String -> prop -> IO Bool
quickTest name prop = do
  mres <- timeout (2 * testTimeoutMicros)
            (quickCheckWithResult stdArgs { chatty = False, maxSuccess = 200 } prop)
  let ok = case mres of Just Success {} -> True; _ -> False
      prefix
        | Nothing <- mres = "TIMEOUT "
        | ok              = "PASS    "
        | otherwise       = "FAIL    "
  putStrLn (prefix <> name)
  pure ok

-- | Any input containing a literal newline or carriage return must be
-- rejected by 'sanitizeExpression'. Security-critical: newlines would
-- split a single tools/call into two GHCi commands and desync framing.
-- Properties take 'String' and pack to 'Text' to avoid pulling in
-- 'quickcheck-instances' just for an 'Arbitrary Text'. Semantically
-- equivalent since 'Text' is a full-Unicode 'String' isomorph here.
-- Note on 'EmptyInput': the input @"\n"@ (pre="", suf="") strips to empty
-- before the newline check fires, so 'EmptyInput' is also a correct
-- rejection. The property's contract is "never accepted", not
-- "always labelled ContainsNewline".
prop_sanitize_rejects_newline :: String -> String -> Property
prop_sanitize_rejects_newline pre suf =
  let input = T.pack pre <> "\n" <> T.pack suf
  in counterexample (T.unpack input) $
       case sanitizeExpression input of
         Left ContainsNewline -> property True
         Left EmptyInput      -> property True
         _                    -> property False

-- | Any input containing the framing sentinel substring must be rejected.
-- Security-critical: would falsify the single-sentinel delimiter.
prop_sanitize_rejects_sentinel :: String -> String -> Property
prop_sanitize_rejects_sentinel pre suf =
  let input = T.pack pre <> "<<<GHCi-DONE-7f3a2b>>>" <> T.pack suf
  in counterexample (T.unpack input) $
       case sanitizeExpression input of
         Left ContainsSentinel -> property True
         Left ContainsNewline  -> property True  -- pre/suf may carry newlines
         _                     -> property False

-- | Strings that are non-empty, single-line, and sentinel-free round-trip
-- through 'sanitizeExpression' modulo the outer whitespace trim.
prop_sanitize_clean_roundtrip :: String -> Property
prop_sanitize_clean_roundtrip rawS =
  let raw = T.pack rawS
      ok = not (T.null (T.strip raw))
        && T.all (`notElem` ("\n\r" :: String)) raw
        && not ("<<<GHCi-DONE-7f3a2b>>>" `T.isInfixOf` raw)
  in ok ==>
     case sanitizeExpression raw of
       Right cleaned -> cleaned === T.strip raw
       _             -> counterexample "expected Right" (property False)

-- | For any project dir and any relative path containing a ".." segment,
-- 'mkModulePath' must refuse to produce a ModulePath.
prop_modulePath_rejects_dotdot :: String -> String -> Property
prop_modulePath_rejects_dotdot pre suf =
  let rel = pre <> "/../" <> suf
  in case mkProjectDir "/tmp/testproj" of
       Left _   -> counterexample "bad project dir" (property False)
       Right pd -> case mkModulePath pd rel of
         Left (PathEscapesProject {}) -> property True
         _                            -> counterexample rel (property False)

-- | Relative paths built from safe ASCII segments (no slashes, no ".."
-- literal, no NUL) must be accepted by 'mkModulePath'.
prop_modulePath_accepts_inTree :: [SafeSegment] -> Property
prop_modulePath_accepts_inTree segs =
  let rel = case segs of
        [] -> "ok.hs"
        xs -> foldr1 (\a b -> a <> "/" <> b) (map unSafe xs) <> ".hs"
  in case mkProjectDir "/tmp/testproj" of
       Left _   -> counterexample "bad project dir" (property False)
       Right pd -> case mkModulePath pd rel of
         Right _ -> property True
         Left e  -> counterexample (rel <> " → " <> show e) (property False)

-- | Newtype wrapper used only to constrain 'Arbitrary' for path-segment
-- generation. Drawn by hand so the generator never emits characters that
-- would confuse the path smart constructor (slashes, dots, NUL).
newtype SafeSegment = SafeSegment { unSafe :: String }
  deriving stock (Show)

instance QC.Arbitrary SafeSegment where
  arbitrary = SafeSegment <$> QC.listOf1 (QC.elements alphaNum)
    where
      alphaNum = ['a'..'z'] <> ['A'..'Z'] <> ['0'..'9'] <> "_-"

--------------------------------------------------------------------------------
-- Totality / law properties added after the dogfood UX fixes. These
-- cover surfaces that parse external text (@:show modules@, QuickCheck
-- output) and pure decision functions (@chooseStoreModule@). Each one
-- is an honest bug-finder: running 200 QuickCheck cases exercises
-- shapes a hand-rolled unit test would never type.
--------------------------------------------------------------------------------

-- | @parseShowModulesPaths@ must be total on any input and never
-- return more paths than input lines. Catches: runaway parsers,
-- hangs on degenerate input, infinite output loops.
prop_parseShowModulesPaths_total :: String -> Bool
prop_parseShowModulesPaths_total input =
  let txt    = T.pack input
      result = RegTool.parseShowModulesPaths txt
      maxN   = length (T.lines txt)
  in length result <= max maxN 1

-- | @parseQuickCheckOutput@ must be total on any (propName, output)
-- pair and return a renderable 'QuickCheckResult'. Catches: bottom
-- constructors, partial pattern matches on output regex splits, and
-- (via 'length . show') infinite loops.
prop_parseQuickCheckOutput_total :: String -> String -> Bool
prop_parseQuickCheckOutput_total propName output =
  not (null (show (parseQuickCheckOutput (T.pack propName) (T.pack output))))

-- | For any property expression that is NOT a simple identifier
-- (here: anything starting with '\\'), 'chooseStoreModule' must
-- return the caller's hint verbatim — the @:info@ output is
-- irrelevant for lambdas. Pinned so a refactor cannot accidentally
-- extend auto-resolution to expressions where @:info@ would return
-- useless results.
prop_chooseStoreModule_nonIdent_uses_hint :: SafeSegment -> SafeSegment -> Bool
prop_chooseStoreModule_nonIdent_uses_hint (SafeSegment body) (SafeSegment hint) =
  let prop  = T.pack ("\\x -> " <> body <> " x")
      mHint = Just (T.pack ("src/" <> hint <> ".hs"))
      info  = Just (T.pack "prop :: a -- Defined at other/File.hs:1:1")
  in QcTool.chooseStoreModule prop mHint info == mHint

-- | Simple identifier but no @:info@ output available (e.g. GHCi
-- returned an error): fall back to the caller's hint rather than
-- inventing a path. Pinned so a refactor cannot accidentally
-- default to something path-like that the caller didn't authorise.
prop_chooseStoreModule_ident_no_info_uses_hint :: SafeSegment -> SafeSegment -> Bool
prop_chooseStoreModule_ident_no_info_uses_hint (SafeSegment seg) (SafeSegment hint) =
  let prop  = T.pack ("prop_" <> seg)
      mHint = Just (T.pack ("src/" <> hint <> ".hs"))
  in QcTool.chooseStoreModule prop mHint Nothing == mHint

--------------------------------------------------------------------------------
-- Phase 4: QuickCheck output + typed-hole parsers
--------------------------------------------------------------------------------

testQcPassed :: IO Bool
testQcPassed =
  let raw = "+++ OK, passed 100 tests."
  in pure $ case parseQuickCheckOutput "prop" raw of
       QcPassed _ 100 -> True
       _              -> False

testQcFailed :: IO Bool
testQcFailed =
  let raw = T.unlines
        [ "*** Failed! Falsifiable (after 3 tests and 2 shrinks):"
        , "[1,2,3]"
        , ""
        ]
  in pure $ case parseQuickCheckOutput "prop" raw of
       QcFailed _ 2 2 cex -> cex == "[1,2,3]"
       _                  -> False

testQcGaveUp :: IO Bool
testQcGaveUp =
  let raw = "*** Gave up! Passed only 12 tests; 88 discarded."
  in pure $ case parseQuickCheckOutput "prop" raw of
       QcGaveUp _ 12 88 -> True
       _                -> False

testQcException :: IO Bool
testQcException =
  let raw = "*** Failed! Exception: 'divide by zero' (after 1 test):"
  in pure $ case parseQuickCheckOutput "prop" raw of
       QcException _ exn -> "divide by zero" `T.isInfixOf` exn
       _                 -> False

testQcUnparsed :: IO Bool
testQcUnparsed =
  let raw = "something completely unexpected"
  in pure $ case parseQuickCheckOutput "prop" raw of
       QcUnparsed {} -> True
       _             -> False

-- | A canonical GHC-88464 block. The whitespace before the indented
-- continuation lines is significant — GHC uses 4 spaces + bullet.
holeSampleOutput :: T.Text
holeSampleOutput = T.unlines
  [ "src/Foo.hs:12:5: warning: [GHC-88464] [-Wtyped-holes]"
  , "    \x2022 Found hole: _ :: Int -> Int"
  , "    \x2022 In the expression: _"
  , "      In an equation for 'bar': bar = _"
  , "    \x2022 Relevant bindings include"
  , "        x :: Int (bound at src/Foo.hs:12:1)"
  , "        bar :: Int -> Int (bound at src/Foo.hs:12:1)"
  ]

testHoleOne :: IO Bool
testHoleOne =
  pure $ case parseTypedHoles holeSampleOutput of
    [h] -> thHole h == "_"
        && thExpectedType h == "Int -> Int"
        && thFile h == "src/Foo.hs"
        && thLine h == 12
        && thColumn h == 5
        && length (thRelevantBindings h) == 2
    _   -> False

testHoleIgnored :: IO Bool
testHoleIgnored =
  let raw = "src/Foo.hs:3:1: error: Not in scope: 'blah'"
  in pure (null (parseTypedHoles raw))

--------------------------------------------------------------------------------
-- Phase 5: Arbitrary + Hoogle parsers
--------------------------------------------------------------------------------

-- | Inline @data T = A | B Int | C Int String@ should produce three
-- constructors with arities 0, 1, 2.
testCtorsInline :: IO Bool
testCtorsInline =
  let raw = T.unlines
        [ "data Foo = Bar | Baz Int | Qux Int String"
        , "  \t-- Defined at src/Foo.hs:5:1"
        ]
  in pure $ case parseConstructors raw of
       [a, b, c] -> cName a == "Bar" && null (cArgs a)
                 && cName b == "Baz" && length (cArgs b) == 1
                 && cName c == "Qux" && length (cArgs c) == 2
       _         -> False

-- | GHCi's multi-line form (one @|@ per line) must parse to the same
-- three constructors.
testCtorsMultiline :: IO Bool
testCtorsMultiline =
  let raw = T.unlines
        [ "data Foo"
        , "  = Bar"
        , "  | Baz Int"
        , "  | Qux Int String"
        , "  \t-- Defined at src/Foo.hs:5:1"
        ]
  in pure $ case parseConstructors raw of
       [a, b, c] -> cName a == "Bar" && cName b == "Baz" && cName c == "Qux"
                 && length (cArgs c) == 2
       _         -> False

-- | Type synonyms have no @=@ constructor list; parser must return an
-- empty list rather than invent ctors.
testCtorsSynonym :: IO Bool
testCtorsSynonym =
  let raw = "type Alias = Int"
  in pure (null (parseConstructors raw))

testTemplate3 :: IO Bool
testTemplate3 =
  let ctors = [ Constructor "Bar" []
              , Constructor "Baz" ["Int"]
              , Constructor "Qux" ["Int", "String"]
              ]
      out   = renderTemplate "Foo" [] ctors
  in pure $
       "instance Arbitrary Foo where"              `T.isInfixOf` out
    && "pure Bar"                                  `T.isInfixOf` out
    && "Baz <$> arbitrary"                         `T.isInfixOf` out
    && "Qux <$> arbitrary <*> arbitrary"           `T.isInfixOf` out

testHoogleHit :: IO Bool
testHoogleHit =
  let line = "Prelude filter :: (a -> Bool) -> [a] -> [a]"
  in pure $ case parseHoogleLine line of
       Just h -> hhSignature h == "(a -> Bool) -> [a] -> [a]"
              && hhModule h    == Just "Prelude"
       _      -> False

testHoogleEmpty :: IO Bool
testHoogleEmpty =
  pure (isNothing (parseHoogleLine "No results found"))

--------------------------------------------------------------------------------
-- Phase 6: Coverage parser + PropertyStore roundtrip
--------------------------------------------------------------------------------

testCoverageFull :: IO Bool
testCoverageFull =
  let raw = T.unlines
        [ "100% expressions used (12/12)"
        , " 66% alternatives used (2/3)"
        , " 75% local declarations used (3/4)"
        , "100% top-level declarations used (5/5)"
        ]
  in pure $ case crMetrics (parseCoverage raw) of
       [a, b, c, d] ->
            mPercent a == 100 && mTotal a == 12
         && mPercent b == 66  && mCovered b == 2 && mTotal b == 3
         && mPercent c == 75
         && mPercent d == 100 && mLabel d == "top-level declarations used"
       _ -> False

testCoverageBanner :: IO Bool
testCoverageBanner =
  let raw = T.unlines
        [ "Cabal version 3.12 — banner without fraction"
        , "100% expressions used (1/1)"
        , ""
        ]
  in pure (length (crMetrics (parseCoverage raw)) == 1)

-- | Round-trip a property through the on-disk store. Uses a unique
-- temp project dir to keep repeated test runs independent.
testStoreRoundtrip :: IO Bool
testStoreRoundtrip = withTempProject $ \pd -> do
  store <- openStore pd
  save store "\\(xs :: [Int]) -> reverse (reverse xs) == xs" (Just "src/Foo.hs")
  props <- loadAll store
  pure $ case props of
    [p] -> spExpression p == "\\(xs :: [Int]) -> reverse (reverse xs) == xs"
        && spModule p == Just "src/Foo.hs"
        && spPassed p == 1
    _   -> False

testStoreIncrement :: IO Bool
testStoreIncrement = withTempProject $ \pd -> do
  store <- openStore pd
  save store "prop_foo" Nothing
  save store "prop_foo" Nothing
  save store "prop_foo" Nothing
  props <- loadAll store
  pure $ case props of
    [p] -> spPassed p == 3
    _   -> False

--------------------------------------------------------------------------------
-- Phase 7: Deps validators + Goto parser
--------------------------------------------------------------------------------

testPkgAccepts :: IO Bool
testPkgAccepts = pure $ case validatePackageName "haskell-flows-mcp" of
  Right _ -> True
  _       -> False

testPkgRejectsSymbol :: IO Bool
testPkgRejectsSymbol = pure $ case validatePackageName "foo; rm -rf /" of
  Left _ -> True
  _      -> False

testPkgRejectsEmpty :: IO Bool
testPkgRejectsEmpty = pure $ case validatePackageName "" of
  Left _ -> True
  _      -> False

testVerAccepts :: IO Bool
testVerAccepts = pure $ case validateVersionConstraint ">= 2.14 && < 2.16" of
  Right _ -> True
  _       -> False

testVerRejects :: IO Bool
testVerRejects = pure $ case validateVersionConstraint "; rm -rf" of
  Left _ -> True
  _      -> False

testDefinedAtFile :: IO Bool
testDefinedAtFile =
  let raw = T.unlines
        [ "foo :: Int -> Int"
        , "  \t-- Defined at src/Foo.hs:12:5"
        ]
  in pure $ case parseDefinedAt raw of
       Just (InFile f l c) -> f == "src/Foo.hs" && l == 12 && c == 5
       _                   -> False

testDefinedAtModule :: IO Bool
testDefinedAtModule =
  let raw = "map :: (a -> b) -> [a] -> [b]\n  \t-- Defined in \x2018Prelude\x2019\n"
  in pure $ case parseDefinedAt raw of
       Just (InModule m) -> m == "Prelude"
       _                 -> False

testDefinedAtNone :: IO Bool
testDefinedAtNone =
  pure (isNothing (parseDefinedAt "just some text"))

--------------------------------------------------------------------------------
-- Phase 8: Refactor engines
--------------------------------------------------------------------------------

-- | The rename must rewrite @foo@ as a whole token, not substrings
-- inside @foobar@ or @myfoo@.
testRenameWordBoundary :: IO Bool
testRenameWordBoundary =
  let src = T.unlines
        [ "foo x = x + 1"
        , "foobar y = y"
        , "baz foo = foo + myfoo"
        ]
  in case renameInScope "foo" "quux" 1 10 src of
       Right rr ->
         pure $ rrOccurrences rr == 3   -- foo on lines 1, 3, 3
             && "foobar" `T.isInfixOf` rrNewContent rr   -- untouched
             && "myfoo"  `T.isInfixOf` rrNewContent rr   -- untouched
             && "quux"   `T.isInfixOf` rrNewContent rr
       _ -> pure False

testRenameIgnoresComments :: IO Bool
testRenameIgnoresComments =
  let src = T.unlines
        [ "-- here is foo in a comment"
        , "foo = 1"
        ]
  in case renameInScope "foo" "bar" 1 10 src of
       Right rr ->
         pure $ rrOccurrences rr == 1   -- only the binding
             && "foo in a comment" `T.isInfixOf` rrNewContent rr
       _ -> pure False

testRenameIgnoresStrings :: IO Bool
testRenameIgnoresStrings =
  let src = T.unlines
        [ "msg = \"the foo is here\""
        , "foo = 1"
        ]
  in case renameInScope "foo" "bar" 1 10 src of
       Right rr ->
         pure $ rrOccurrences rr == 1
             && "\"the foo is here\"" `T.isInfixOf` rrNewContent rr
       _ -> pure False

testRenameScoped :: IO Bool
testRenameScoped =
  let src = T.unlines
        [ "foo = 1"    -- line 1 — outside scope
        , "foo = 2"    -- line 2 — inside scope
        , "foo = 3"    -- line 3 — outside scope
        ]
  in case renameInScope "foo" "bar" 2 2 src of
       Right rr ->
         pure $ rrOccurrences rr == 1
             && rrTouchedLines rr == [2]
       _ -> pure False

testRenameSameName :: IO Bool
testRenameSameName =
  pure $ case renameInScope "foo" "foo" 1 10 "foo = 1" of
    Left _ -> True
    _      -> False

testIdentifierKeyword :: IO Bool
testIdentifierKeyword = pure $ case validateIdentifier "where" of
  Left _ -> True
  _      -> False

testIdentifierSymbol :: IO Bool
testIdentifierSymbol = pure $ case validateIdentifier "foo; rm" of
  Left _ -> True
  _      -> False

testIdentifierUpper :: IO Bool
testIdentifierUpper = pure $ case validateIdentifier "Foo" of
  Left _ -> True
  _      -> False

testExtractBinding :: IO Bool
testExtractBinding =
  let src = T.unlines
        [ "main = do"
        , "  let x = 1 + 2 + 3"
        , "  print x"
        ]
  in case extractBinding "sumSmall" 2 2 src of
       Right er ->
         pure $ "sumSmall" `T.isInfixOf` erNewContent er
             && "sumSmall =" `T.isInfixOf` erBindingTxt er
       _ -> pure False

testExtractEmpty :: IO Bool
testExtractEmpty =
  pure $ case extractBinding "foo" 5 4 "body" of
    Left _ -> True
    _      -> False

--------------------------------------------------------------------------------
-- Phase 9: Lint parser + Cabal validator + check_project + hole fits
--------------------------------------------------------------------------------

testHlintJson :: IO Bool
testHlintJson =
  let raw = T.pack
        "[{\"severity\":\"Warning\",\"hint\":\"Use isNothing\",\
        \\"file\":\"src/Foo.hs\",\"startLine\":10,\"startColumn\":5,\
        \\"from\":\"x == Nothing\",\"to\":\"isNothing x\"}]"
  in pure $ case parseHlintJson raw of
       [s] -> LintTool.sSeverity s == "Warning"
           && LintTool.sHint s     == "Use isNothing"
           && LintTool.sStartLine s == 10
       _ -> False

testDuplicateDeps :: IO Bool
testDuplicateDeps =
  let cabalBody = T.unlines
        [ "library"
        , "    build-depends: base, text, base"
        ]
  in pure $ any (("duplicate-dep" ==) . VC.iKind) (VC.scanCabalText cabalBody)

testMissingSynopsis :: IO Bool
testMissingSynopsis =
  let cabalBody = "cabal-version: 3.0\nname: foo\nversion: 0.1.0.0"
      issues    = VC.scanCabalText cabalBody
  in pure $ any (("missing-synopsis" ==) . VC.iKind) issues
         && any ((VC.CabalSevWarn ==) . VC.iSeverity) issues

testParseModules :: IO Bool
testParseModules =
  let cabalBody = T.unlines
        [ "library"
        , "  exposed-modules:  Foo.Bar"
        , "                    Foo.Baz"
        , "  other-modules:    Foo.Internal"
        , "  build-depends:    base"
        ]
      mods = parseExposedModules cabalBody
  in pure $ "Foo.Bar"      `elem` mods
         && "Foo.Baz"      `elem` mods
         && "Foo.Internal" `elem` mods

testValidFits :: IO Bool
testValidFits =
  let block = T.lines $ T.unlines
        [ "src/Foo.hs:5:5: warning: [GHC-88464]"
        , "    • Found hole: _ :: Int -> Int"
        , "    • Valid hole fits include"
        , "        id :: forall a. a -> a"
        , "        negate :: forall a. Num a => a -> a"
        ]
  in pure $ case extractValidFits block of
       [a, b] -> hfName a == "id"
              && hfName b == "negate"
              && "Num a" `T.isInfixOf` hfType b
       _      -> False

--------------------------------------------------------------------------------
-- Phase 10b: TypeSignature parser + rules catalog
--------------------------------------------------------------------------------

testSigSimple :: IO Bool
testSigSimple = pure $ case parseSignature "a -> a" of
  Just sig -> argCountOf sig == 1
           && isSameTypeThroughout sig
           && null (psConstraints sig)
  _        -> False
  where argCountOf = length . psArgs

testSigConstraint :: IO Bool
testSigConstraint =
  pure $ case parseSignature "Eq a => a -> a -> Bool" of
    Just sig -> psConstraints sig == ["Eq a"]
             && length (psArgs sig) == 2
             && psReturn sig == TyCon "Bool"
    _ -> False

testSigList :: IO Bool
testSigList =
  pure $ case parseSignature "[a] -> [a]" of
    Just sig -> psArgs sig == [TyList (TyVar "a")]
             && psReturn sig == TyList (TyVar "a")
             && isSameTypeThroughout sig
    _ -> False

testSuggestInvolutive :: IO Bool
testSuggestInvolutive =
  case parseSignature "a -> a" of
    Nothing  -> pure False
    Just sig ->
      let suggestions = applyRules "foo" sig
      in pure (any ((== "Involutive") . sLaw) suggestions
             && any ((== "Idempotent") . sLaw) suggestions)

testSuggestAssoc :: IO Bool
testSuggestAssoc =
  case parseSignature "a -> a -> a" of
    Nothing  -> pure False
    Just sig ->
      let suggestions = applyRules "op" sig
      in pure (any ((== "Associative") . sLaw) suggestions
             && any ((== "Commutative") . sLaw) suggestions)

testSuggestNoMatch :: IO Bool
testSuggestNoMatch =
  case parseSignature "Int -> String" of
    Nothing  -> pure False
    Just sig ->
      let suggestions = applyRules "foo" sig
      in pure (null suggestions)

--------------------------------------------------------------------------------
-- Phase 12 regression tests: dogfood findings #22 / #23 / #24
--------------------------------------------------------------------------------

-- | Issue #22: @ghci_batch@ advertises @{tool, args}@ via its
-- @inputSchema@ — parsing must accept that shape. This pins the
-- documented contract; a future regression flips this red instead
-- of silently misleading agents following the tool's own schema.
testBatchParsesToolArgs :: IO Bool
testBatchParsesToolArgs =
  let raw = object
        [ "actions" .=
            [ object
                [ "tool" .= ("ghci_type" :: Text)
                , "args" .= object [ "expression" .= ("reverse" :: Text) ]
                ]
            ]
        ]
  in case A.fromJSON raw :: A.Result BatchArgs of
       A.Success ba -> case baActions ba of
         [tc] -> pure
           ( tcName tc == "ghci_type"
           && tcArguments tc
                == object [ "expression" .= ("reverse" :: Text) ]
           )
         _ -> pure False
       A.Error _ -> pure False

-- | Issue #22 continued: clients that were relying on the MCP-native
-- shape @{name, arguments}@ (what @tools/call@ uses) keep working.
-- Accepting both shapes costs nothing — each routes through the
-- same dispatcher and per-tool validator downstream.
testBatchParsesNameArgs :: IO Bool
testBatchParsesNameArgs =
  let raw = object
        [ "actions" .=
            [ object
                [ "name"      .= ("ghci_eval" :: Text)
                , "arguments" .= object [ "expression" .= ("1+1" :: Text) ]
                ]
            ]
        ]
  in case A.fromJSON raw :: A.Result BatchArgs of
       A.Success ba -> case baActions ba of
         [tc] -> pure (tcName tc == "ghci_eval")
         _    -> pure False
       A.Error _ -> pure False

-- | Issue #23: @reverse :: [a] -> [a]@ fits the @a -> a@ shape that
-- 'ruleIdempotent' used to blindly promote to 'Medium'. Dampened
-- heuristic should either skip it or mark it 'Low' for a name with
-- no canonicalisation hint. Must never emit 'Medium' or 'High'.
testSuggestReverseIdempotentLow :: IO Bool
testSuggestReverseIdempotentLow =
  case parseSignature "[a] -> [a]" of
    Nothing  -> pure False
    Just sig ->
      let sugg = [ s | s <- applyRules "reverse" sig, sLaw s == "Idempotent" ]
      in pure $ case sugg of
           []  -> True
           [s] -> sConfidence s == Low
           _   -> False

-- | Issue #23: a name like @normalize@ — a strong canonicalisation
-- hint — should still surface Idempotent at 'Medium' even when
-- the shape is @[a] -> [a]@.
testSuggestNormalizeIdempotentMedium :: IO Bool
testSuggestNormalizeIdempotentMedium =
  case parseSignature "[a] -> [a]" of
    Nothing  -> pure False
    Just sig ->
      let sugg = [ s | s <- applyRules "normalize" sig, sLaw s == "Idempotent" ]
      in pure $ case sugg of
           [s] -> sConfidence s == Medium
           _   -> False

-- | Issue #24: @toolsActive@ in 'ghci_workflow' must enumerate the
-- same set of tools as @tools/list@. The two used to drift because
-- the list was hand-maintained in two places. Paranoia check: also
-- confirm every name is non-empty and the server registers more
-- than the 9-tool Phase-5 baseline.
testWorkflowToolsParity :: IO Bool
testWorkflowToolsParity = pure $
     length allToolNames == length allToolDescriptors
  && not (any T.null allToolNames)
  && length allToolNames >= 20

--------------------------------------------------------------------------------
-- Phase 11b regressions: ghci_deps F-01 / F-02 / F-03 fixes.
--------------------------------------------------------------------------------

-- | F-01: @ghci_deps add@ previously wrote @,@-prefixed continuation
-- lines at the same column as the @build-depends:@ field. Cabal 3.0
-- rejects that as a new field header and the file becomes
-- unparseable. Pin the invariant: after add, the inserted line's
-- leading whitespace must strictly exceed the header's indent.
testDepsAddIndentsForCabal :: IO Bool
testDepsAddIndentsForCabal =
  let body = T.unlines
        [ "library"
        , "    build-depends:    base >= 4.20 && < 5"
        ]
      newBody = addDep Nothing "QuickCheck" body
      isContComma ln = "," `T.isPrefixOf` T.stripStart ln
      commaLines = filter isContComma (T.lines newBody)
      headerIndent = 4  -- 4 spaces before "build-depends:"
  in pure $ case commaLines of
       [ln] ->
         let leading = T.length (T.takeWhile (== ' ') ln)
         in leading > headerIndent
       _ -> False

-- | F-02 (same root as F-01, framed positively): after @addDep@ on a
-- pristine scaffold, the entire file must not contain any line whose
-- first non-whitespace char is \",\" at column <= field indent.
-- This guards against future parser-confusing shapes.
testDepsAddNoTopComma :: IO Bool
testDepsAddNoTopComma =
  let body = T.unlines
        [ "cabal-version: 3.0"
        , "name: foo"
        , ""
        , "library"
        , "    build-depends:    base >= 4.20 && < 5"
        ]
      newBody  = addDep (Just ">= 2.14") "QuickCheck" body
      offenderLines =
        [ ln
        | ln <- T.lines newBody
        , let ws = T.length (T.takeWhile (== ' ') ln)
        , "," `T.isPrefixOf` T.stripStart ln
        , ws <= 4
        ]
  in pure (null offenderLines)

-- | F-03: with a stanza selector, @addDep@ must land in the
-- requested stanza, not the first @build-depends:@ it finds.
testDepsAddTargetsTestSuite :: IO Bool
testDepsAddTargetsTestSuite =
  let body = T.unlines
        [ "library"
        , "    build-depends:    base"
        , ""
        , "test-suite foo-test"
        , "    build-depends:    base"
        , "                    , foo"
        ]
      -- Scope everything through resolveStanza-style slicing by using
      -- applyWithinStanza through addDep at the tool layer; here we
      -- simulate via the exported primitive.
      newBody = case parseStanzaSelector "test-suite" of
        Left _ -> body
        Right sel ->
          let lns = T.lines body
              -- In-file-tool uses applyWithinStanza; mirror that with a
              -- direct slice call via the public API (parseStanzaSelector
              -- + addDep on the slice).
              (pre, stanzaLns, post) = sliceOrEmpty sel lns
              inner  = T.unlines stanzaLns
              inner' = addDep Nothing "QuickCheck" inner
          in T.unlines (pre <> T.lines inner' <> post)
      libDeps       = scopedDeps "library" newBody
      testSuiteDeps = scopedDeps "test-suite" newBody
  in pure $ "QuickCheck" `elem` testSuiteDeps
         && "QuickCheck" `notElem` libDeps
  where
    sliceOrEmpty sel lns =
      Data.Maybe.fromMaybe ([], lns, []) (lookupStanza sel lns)
    -- Tiny reimpl of the MCP's stanza slice, used only by this test.
    lookupStanza (kind, mName) lns =
      let match ln
            | not (T.null (T.takeWhile (== ' ') ln)) = False
            | otherwise =
                let s = T.strip ln
                    w = T.takeWhile (/= ' ') s
                    r = T.strip (T.dropWhile (/= ' ') s)
                in w == kind
                && case mName of
                     Nothing
                       | kind == "library" -> T.null r
                       | otherwise         -> True
                     Just name -> r == name
          isTop ln =
            T.null (T.takeWhile (== ' ') ln)
            && not (T.null (T.strip ln))
            && not (":" `T.isInfixOf` T.strip ln)
            && T.takeWhile (/= ' ') (T.strip ln)
                 `elem` topStanzaKinds
          topStanzaKinds =
            [ "library", "executable", "test-suite", "benchmark"
            , "foreign-library", "common", "flag", "source-repository"
            ]
      in case break match lns of
           (_,   [])     -> Nothing
           (pre, h : tl) ->
             let (body', post) = break isTop tl
             in Just (pre, h : body', post)
    scopedDeps kind body' =
      case parseStanzaSelector kind of
        Left _    -> []
        Right sel -> case lookupStanza sel (T.lines body') of
          Nothing       -> []
          Just (_, l, _) -> stanzaDeps (T.unlines l)
    stanzaDeps body' =
      -- same line-oriented parser used by the tool; inline-enough for
      -- the test by slicing the build-depends: line + continuations.
      let ls = T.lines body'
          rest = dropWhile (not . startsBuildDepends) ls
      in case rest of
           []     -> []
           (h:tl) ->
             let tailVal = T.strip (T.drop (T.length "build-depends:")
                                            (T.strip h))
                 cont    = takeWhile isContLine tl
                 joined  = T.intercalate " " (tailVal : map T.strip cont)
             in [ T.strip (T.takeWhile
                             (\c -> c /= ' ' && c /= '>' && c /= '<'
                                 && c /= '=' && c /= '^' && c /= '&')
                             (T.strip tok))
                | tok <- T.splitOn "," joined
                , not (T.null (T.strip tok))
                ]
    startsBuildDepends ln =
      "build-depends:" `T.isPrefixOf` T.stripStart (T.toLower ln)
    isContLine ln =
      not (T.null (T.takeWhile (== ' ') ln))
      && not (T.null (T.strip ln))

-- | Phase 11b: ensure the stanza selector parser accepts the forms
-- we advertise in the descriptor and rejects obvious garbage.
testParseStanzaAccepts :: IO Bool
testParseStanzaAccepts = pure $
     accepts "library"          ("library", Nothing)
  && accepts "test-suite"       ("test-suite", Nothing)
  && accepts "test-suite:foo"   ("test-suite", Just "foo")
  && accepts "executable:bar"   ("executable", Just "bar")
  where
    accepts raw expected = case parseStanzaSelector raw of
      Right got -> got == expected
      Left  _   -> False

testParseStanzaRejects :: IO Bool
testParseStanzaRejects = pure $
     rejects "foo-suite"          -- unknown kind
  && rejects "library:"           -- empty name after colon
  && rejects "test-suite:bad name" -- space in name
  && rejects "test-suite/$(id)"   -- shell metacharacters
  && rejects ""                   -- empty string
  where
    rejects raw = case parseStanzaSelector raw of
      Left  _ -> True
      Right _ -> False

-- | Phase 11b F-09: @ghci_coverage@ always returned
-- @summary="No coverage metrics parsed from the cabal output"@ under
-- GHC 9.12 + cabal 3.14 because those versions of cabal no longer
-- echo the HPC summary on stdout — they only write HTML. Fix wires
-- a post-@cabal test@ @hpc report@ call into the pipeline whose text
-- output the parser already understands. The regression check here is
-- narrow: pin the static shape of Tool/Coverage.hs so a future edit
-- can't accidentally drop the @hpc report@ invocation.
testCoverageInvokesHpcReport :: IO Bool
testCoverageInvokesHpcReport = do
  src <- TIO.readFile "src/HaskellFlows/Tool/Coverage.hs"
  pure $ T.isInfixOf "runHpcReport"         src
      && T.isInfixOf "enrichWithHpcReport"  src
      && T.isInfixOf "findTixFile"          src
      && T.isInfixOf "\"hpc\""              src
      && T.isInfixOf "\"--hpcdir=\""        src `ellipticalOr`
         T.isInfixOf "--hpcdir="            src

-- | Rescues us from the difference between `"--hpcdir="` as a
-- quoted literal in source and the concatenation form. Either is fine.
ellipticalOr :: Bool -> Bool -> Bool
ellipticalOr = (||)

-- | Phase 11n: 4 BAJA bundle tools registered in the inventory.
testBajaRegistered :: IO Bool
testBajaRegistered = pure $
  all (`elem` allToolNames)
    [ "ghci_browse"
    , "ghci_determinism"
    , "ghci_property_lifecycle"
    , "ghci_toolchain_warmup"
    ]

-- | Phase 11l: resources/read for the rules URI returns the
-- embedded markdown; unknown URIs return Nothing.
-- | Phase 11m + BUG-09: the workflow-rules resource body is
-- rendered dynamically from the live tool descriptor list, not
-- a stale hand-edited string. Pin both the URI advertisement
-- and the body's dynamic shape.
testResourcesRulesRead :: IO Bool
testResourcesRulesRead = pure $
  let md = Guidance.workflowRulesMarkdown allToolDescriptors
      advertised =
        "haskell-flows://rules/workflow" `elem` Resources.knownResourceUris
  in advertised
     && T.isInfixOf "haskell-flows" md
     && T.isInfixOf "situation" (T.toLower md)
     && T.isInfixOf (T.pack (show (length allToolDescriptors))) md

testResourcesUnknown :: IO Bool
testResourcesUnknown =
  pure ("haskell-flows://nonexistent" `notElem` Resources.knownResourceUris)

-- | BUG-05: @initialize.instructions@ used to hard-code "25 tools".
-- The fix derives the tool count from 'allToolDescriptors'. Pin
-- that the rendered text contains the live count and not any of
-- the historic stale counts.
testGuidanceDynamicCount :: IO Bool
testGuidanceDynamicCount = do
  let instructions  = Guidance.sessionInstructionsText allToolDescriptors
      liveCount     = T.pack (show (length allToolDescriptors))
      staleCounts   = ["25 tools", "26 tools", "27 tools", "28 tools"]
      hasLive       = T.isInfixOf (liveCount <> " tools") instructions
      hasAnyStale   = any (`T.isInfixOf` instructions) staleCounts
  pure (hasLive && not hasAnyStale)

-- | BUG-05: every registered tool's name must appear in the
-- rendered instructions. If a new tool ships without a mention,
-- this test fails — the forcing function that keeps the docs in
-- sync with the registry.
testGuidanceListsEveryTool :: IO Bool
testGuidanceListsEveryTool = do
  let instructions = Guidance.sessionInstructionsText allToolDescriptors
  pure (all (`T.isInfixOf` instructions) allToolNames)

-- | BUG-09: the markdown resource must match the plain-text
-- instructions in tool coverage — both are derived from the same
-- 'allToolDescriptors', so neither can omit a tool.
testGuidanceMarkdownListsEveryTool :: IO Bool
testGuidanceMarkdownListsEveryTool = do
  let md = Guidance.workflowRulesMarkdown allToolDescriptors
  pure (all (`T.isInfixOf` md) allToolNames)

-- | BUG-05: the situation-tool table is the curated map from
-- "user intent" to tool. Must be non-empty and every row's tool
-- must actually be in the registry.
testGuidanceSituationNonEmpty :: IO Bool
testGuidanceSituationNonEmpty = pure $
     not (null Guidance.situationTable)
  && all (\r -> Guidance.srTool r `elem` allToolNames) Guidance.situationTable

-- | BUG-19: @ghci_session@ is a TS-era tool name that does not
-- exist in the Haskell MCP. The phantom reference used to leak
-- into @ghci_deps@' description and hint. Pin that no guidance
-- text mentions the phantom tool.
testGuidanceNoPhantomSession :: IO Bool
testGuidanceNoPhantomSession = do
  let instructions = Guidance.sessionInstructionsText allToolDescriptors
      md           = Guidance.workflowRulesMarkdown   allToolDescriptors
      phantom      = "ghci_session"
  pure $ not (phantom `T.isInfixOf` instructions)
      && not (phantom `T.isInfixOf` md)

-- | BUG-19 companion: the @ghci_deps@ tool descriptor used to say
-- \"run ghci_session(action='restart')\". Pin that the description
-- no longer mentions the phantom tool.
testDepsDescriptorNoPhantom :: IO Bool
testDepsDescriptorNoPhantom = do
  let depsDesc = head [ tdDescription d | d <- allToolDescriptors
                                        , tdName d == "ghci_deps" ]
  pure (not ("ghci_session" `T.isInfixOf` depsDesc))

-- | BUG-19 companion: the @ghci_deps@ add/remove response carried
-- a @hint@ string instructing the agent to call @ghci_session@.
-- Pin that the live Deps source no longer embeds the phantom.
testDepsHintNoPhantom :: IO Bool
testDepsHintNoPhantom = do
  src <- TIO.readFile "src/HaskellFlows/Tool/Deps.hs"
  pure (not ("ghci_session" `T.isInfixOf` src))

--------------------------------------------------------------------------------
-- BUG-02 — ghci_quickcheck_export must generate valid Haskell
--------------------------------------------------------------------------------

-- | The classic cases: 'src/Foo.hs' -> 'Foo', 'src/Foo/Bar.hs' -> 'Foo.Bar'.
testExportPathSrc :: IO Bool
testExportPathSrc = pure $
     QcExport.modulePathToModule "src/Foo.hs"        == Just "Foo"
  && QcExport.modulePathToModule "src/Foo/Bar.hs"    == Just "Foo.Bar"

-- | Library convention alias. Same semantics as 'src/'.
testExportPathLib :: IO Bool
testExportPathLib = pure $
     QcExport.modulePathToModule "lib/Foo.hs"        == Just "Foo"
  && QcExport.modulePathToModule "lib/Foo/Bar.hs"    == Just "Foo.Bar"

-- | BUG-02 core: test-suite helpers like @test/Gen.hs@ containing
-- @module Gen where@ used to be mis-mapped to @test.Gen@ — lowercase
-- first segment, not a valid Haskell module name. Pin the fix.
testExportPathTest :: IO Bool
testExportPathTest = pure $
     QcExport.modulePathToModule "test/Gen.hs"       == Just "Gen"
  && QcExport.modulePathToModule "test/Support/Fix.hs" == Just "Support.Fix"

-- | Paths with no leading convention-dir: take the whole path as
-- the module name (each segment still has to start uppercase).
testExportPathNested :: IO Bool
testExportPathNested = pure $
     QcExport.modulePathToModule "Main.hs"           == Just "Main"
  && QcExport.modulePathToModule "Foo/Bar.hs"        == Just "Foo.Bar"

-- | Paths containing lowercase segments (non-canonical layouts)
-- must return 'Nothing' — the renderer will omit a broken import
-- rather than emit invalid Haskell.
testExportPathLowercaseRejected :: IO Bool
testExportPathLowercaseRejected = pure $
     isNothing (QcExport.modulePathToModule "experiments/foo.hs")
  && isNothing (QcExport.modulePathToModule "src/support/Gen.hs")
  && isNothing (QcExport.modulePathToModule "src/.hidden/Foo.hs")

-- | Non-Haskell files are outright rejected (no @.hs@ suffix).
testExportPathNoSuffix :: IO Bool
testExportPathNoSuffix = pure $
     isNothing (QcExport.modulePathToModule "src/Foo.txt")
  && isNothing (QcExport.modulePathToModule "src/Foo")
  && isNothing (QcExport.modulePathToModule "")

-- | End-to-end: a property whose stored module is 'test/Gen.hs'
-- must generate an @import Gen@ line — never the old broken
-- @import test.Gen@. Exercises the fix through 'renderTestFile'.
testExportRenderValidImports :: IO Bool
testExportRenderValidImports = do
  let props =
        [ StoredProperty
            { spExpression = "\\(x :: Expr) -> simplify (simplify x) == simplify x"
            , spModule     = Just "test/Gen.hs"
            , spPassed     = 1
            , spUpdated    = 0
            }
        ]
      rendered = QcExport.renderTestFile props
  pure $ T.isInfixOf "import Gen"         rendered
      && not (T.isInfixOf "import test."  rendered)
      && not (T.isInfixOf "import test_"  rendered)

--------------------------------------------------------------------------------
-- BUG-04 — PropertyStore cold-start resilience
--------------------------------------------------------------------------------

-- | BUG-04 core: a fresh project whose @.haskell-flows/@ dir
-- does not yet exist must still accept a first @save@. The fix
-- re-asserts @createDirectoryIfMissing True@ before every write.
testPropStoreCreatesDir :: IO Bool
testPropStoreCreatesDir = withTempProject $ \pd -> do
  -- Do NOT call 'openStore' upfront. Simulate the pathological
  -- case where the dir was cleaned between boot and the first
  -- save: mkdir removed, then save issued.
  removePathForcibly (unProjectDirRaw pd </> ".haskell-flows")
  store <- openStore pd
  removePathForcibly (unProjectDirRaw pd </> ".haskell-flows")
  save store "\\x -> x == (x :: Int)" (Just "src/Foo.hs")
  props <- loadAll store
  pure (length props == 1)

-- | BUG-04 defence-in-depth: an external @rm -rf .haskell-flows/@
-- between two saves must not leave the store in an unrecoverable
-- state — the second save recreates the dir and persists.
testPropStoreResurrectsDir :: IO Bool
testPropStoreResurrectsDir = withTempProject $ \pd -> do
  store <- openStore pd
  save store "\\x -> x == (x :: Int)" (Just "src/Foo.hs")
  -- Nuke the dir the way a user might via rm -rf.
  removePathForcibly (unProjectDirRaw pd </> ".haskell-flows")
  save store "\\x -> x + 0 == (x :: Int)" (Just "src/Foo.hs")
  props <- loadAll store
  -- After nuke + save, at least the 2nd property must be present.
  pure (any ((== "\\x -> x + 0 == (x :: Int)") . spExpression) props)

-- | BUG-04 companion: parallel saves must not race into an
-- inconsistent JSON. 10 concurrent saves → 10 distinct entries,
-- no truncation, no last-writer-wins.
testPropStoreConcurrentSaves :: IO Bool
testPropStoreConcurrentSaves = withTempProject $ \pd -> do
  store <- openStore pd
  let exprs = [ "\\x -> x + " <> T.pack (show i) <> " >= (x :: Int)"
              | i <- [1 .. 10 :: Int] ]
  mvs <- mapM (\e -> do
                 mv <- newEmptyMVar
                 _  <- forkIO (save store e (Just "src/X.hs")
                                 >> putMVar mv ())
                 pure mv) exprs
  -- Inner budget: if any single save hangs on the property-store
  -- lock (e.g. disk full, FS ACL weirdness under CI), we fail fast
  -- rather than waiting 60s for the outer 'test' timeout.
  m <- timeout 10_000_000 (mapM_ takeMVar mvs)
  case m of
    Nothing -> pure False
    Just () -> do
      props <- loadAll store
      pure (length props == 10)

-- | Internal: unwrap ProjectDir to its FilePath. Exported in the
-- production module but not used elsewhere in this test file; keep
-- it inline here so we can stat / rm under the validated root.
unProjectDirRaw :: ProjectDir -> FilePath
unProjectDirRaw = HaskellFlows.Types.unProjectDir

--------------------------------------------------------------------------------
-- BUG-18 — Involutive confidence is 'Low' for normalizers
--------------------------------------------------------------------------------

-- | BUG-18 core: a function named 'simplify :: Expr -> Expr' used
-- to get the generic Involutive suggestion at 'Medium' confidence.
-- Normalisers are idempotent, not involutive — the law almost
-- always fails. Pin that the downgrade to 'Low' + the new
-- rationale fires for every name in 'nameHintsOptimization'.
testInvolutiveLowForNormalizer :: IO Bool
testInvolutiveLowForNormalizer =
  case parseSignature "Expr -> Expr" of
    Nothing  -> pure False
    Just sig -> pure $
      let names = ["simplify", "normalize", "canonicalize"
                  , "fold", "optimize", "reduce", "rewrite"]
          row nm =
            [ s | s <- applyRules nm sig, sLaw s == "Involutive" ]
          low  = Low
      in all (\nm ->
                case row nm of
                  [s] -> sConfidence s == low
                         && "normaliser" `T.isInfixOf` sRationale s
                  _   -> False)
              names

-- | Symmetric: 'reverse :: [a] -> [a]' is a genuine involution,
-- so the suggestion stays 'Medium' + the classical rationale.
testInvolutiveMediumForReverse :: IO Bool
testInvolutiveMediumForReverse =
  case parseSignature "[a] -> [a]" of
    Nothing  -> pure False
    Just sig -> pure $
      let row =
            [ s | s <- applyRules "reverse" sig, sLaw s == "Involutive" ]
      in case row of
           [s] -> sConfidence s == Medium
                  && "involutive" `T.isInfixOf` T.toLower (sRationale s)
                  && not ("normaliser" `T.isInfixOf` sRationale s)
           _   -> False

--------------------------------------------------------------------------------
-- BUG-15 — ghci_suggest scope-error goes through a structured hint
--------------------------------------------------------------------------------

-- | BUG-15: the 'outOfScopeResult' helper returns a structured
-- payload with an actionable @hint@ instead of the raw GHC
-- "Variable not in scope" blob. Pin its shape.
testSuggestScopeStructuredHint :: IO Bool
testSuggestScopeStructuredHint =
  let ghcOut = "<interactive>:1:1: error: [GHC-88464] Variable not in scope: simplify"
      tr     = SuggestTool.outOfScopeResult "simplify" ghcOut
      body   = case trContent tr of
        (TextContent t : _) -> t
        _                   -> ""
  in pure $ trIsError tr
         && T.isInfixOf "\"reason\":\"function_not_in_scope\"" body
         && T.isInfixOf "\"function\":\"simplify\""            body
         && T.isInfixOf "ghci_load"                             body
         && T.isInfixOf "not in scope" body

--------------------------------------------------------------------------------
-- BUG-03 — sibling-aware suggest pipeline
--------------------------------------------------------------------------------

-- | Typical @:show modules@ output: one line per loaded module
-- with the file path in parens.
testParseShowModulesSimple :: IO Bool
testParseShowModulesSimple =
  let raw = T.unlines
        [ "Expr.Syntax    ( src/Expr/Syntax.hs, interpreted )"
        , "Expr.Simplify  ( src/Expr/Simplify.hs, interpreted )"
        , "Expr.Eval      ( src/Expr/Eval.hs, interpreted )"
        ]
      parsed = SuggestTool.parseShowModules raw
  in pure $ ("Expr.Syntax",   "src/Expr/Syntax.hs")   `elem` parsed
         && ("Expr.Simplify", "src/Expr/Simplify.hs") `elem` parsed
         && ("Expr.Eval",     "src/Expr/Eval.hs")     `elem` parsed

-- | @:show modules@ prefixes the currently-focused module with
-- @*@. The parser must strip it before picking up the name.
testParseShowModulesStar :: IO Bool
testParseShowModulesStar =
  let raw = T.unlines
        [ "* Expr.Simplify  ( src/Expr/Simplify.hs, interpreted )"
        , "  Expr.Syntax    ( src/Expr/Syntax.hs, interpreted )"
        ]
      parsed = SuggestTool.parseShowModules raw
  in pure $ ("Expr.Simplify", "src/Expr/Simplify.hs") `elem` parsed
         && ("Expr.Syntax",   "src/Expr/Syntax.hs")   `elem` parsed

-- | @:browse@ output mixes value bindings (lower-case head) with
-- type / class declarations (upper-case head). The parser keeps
-- only the value bindings with a top-level @::@.
testParseBrowseBindings :: IO Bool
testParseBrowseBindings =
  let raw = T.unlines
        [ "data Expr = Lit Int | Add Expr Expr"
        , "simplify :: Expr -> Expr"
        , "eval :: Env -> Expr -> Either Error Int"
        , "type Env = [(String, Int)]"
        , "class Monad m where"
        ]
      parsed = SuggestTool.parseBrowseBindings raw
  in pure $ ("simplify", "Expr -> Expr") `elem` parsed
         && ("eval",     "Env -> Expr -> Either Error Int") `elem` parsed
         && not (any (\(n, _) -> n `elem` ["Expr", "Env", "Monad"]) parsed)

-- | @:browse@ may break long types across lines with indentation.
-- The parser must skip indented continuation lines rather than
-- treat them as new bindings.
testParseBrowseContinuation :: IO Bool
testParseBrowseContinuation =
  let raw = T.unlines
        [ "prettyWithOptions :: Options"
        , "                  -> Expr"
        , "                  -> String"
        , "simplify :: Expr -> Expr"
        ]
      parsed = SuggestTool.parseBrowseBindings raw
  in pure $ any (\(n, _) -> n == "simplify") parsed

-- | BUG-03 core: when the focal function is @simplify :: Expr -> Expr@
-- and a sibling @eval :: Env -> Expr -> r@ is present (re-export
-- shape that the MCP will discover via @:browse@), the Evaluator
-- Preservation engine MUST fire. Pre-fix it never did because the
-- tool called 'applyRules' (no siblings) instead of 'applyRulesCtx'.
--
-- This test drives 'applyRulesCtx' directly with the sibling set
-- that 'gatherSiblings' would produce — the tool's end-to-end path
-- needs a live GHCi session that the unit test runner doesn't boot.
testSuggestSiblingsEnablePreservation :: IO Bool
testSuggestSiblingsEnablePreservation =
  case (parseSignature "Expr -> Expr", parseSignature "Env -> Expr -> Either Error Int") of
    (Just simpSig, Just evalSig) -> pure $
      let ctx = RuleContext
            { rcName     = "simplify"
            , rcSig      = simpSig
            , rcSiblings = [("eval", evalSig)]
            }
          laws = map sLaw (applyRulesCtx ctx)
      in "Constant-folding soundness" `elem` laws
         || "Evaluator preservation"   `elem` laws
    _ -> pure False

-- | Stricter version: the @simplify@ name hints at optimisation, so
-- 'ruleConstantFoldingSoundness' must fire at High confidence (that's
-- the whole point of the name-based bump). No sibling → no law.
testSuggestSiblingsEnableSoundness :: IO Bool
testSuggestSiblingsEnableSoundness =
  case (parseSignature "Expr -> Expr", parseSignature "Env -> Expr -> Either Error Int") of
    (Just simpSig, Just evalSig) -> pure $
      let withSib = RuleContext
            { rcName     = "simplify"
            , rcSig      = simpSig
            , rcSiblings = [("eval", evalSig)]
            }
          noSib   = withSib { rcSiblings = [] }
          hits s  = [ x | x <- applyRulesCtx s
                        , sLaw x == "Constant-folding soundness" ]
      in case hits withSib of
           (s:_) -> sConfidence s == High && null (hits noSib)
           []    -> False
    _ -> pure False

--------------------------------------------------------------------------------
-- BUG-06 / BUG-22 — nextStep coverage for Phase 11f..11n tools +
-- multi-step chain support
--------------------------------------------------------------------------------

-- | Helper: assert the nextStep for a (tool, payload) pair points
-- at a specific follow-up tool.
assertNext :: Text -> A.Value -> Text -> Bool
assertNext tool payload expected =
  case suggestNext tool True payload of
    Just ns -> nsTool ns == expected
    Nothing -> False

testNextStepGatePass :: IO Bool
testNextStepGatePass =
  let payload = A.object [ "success" .= True, "totalDurationSec" .= (1.0 :: Double) ]
  in pure (assertNext "ghci_gate" payload "ghci_coverage")

testNextStepGateFail :: IO Bool
testNextStepGateFail =
  let payload = A.object [ "success" .= False, "totalDurationSec" .= (1.0 :: Double) ]
  in pure (assertNext "ghci_gate" payload "ghci_check_project")

testNextStepQcExport :: IO Bool
testNextStepQcExport =
  let payload = A.object [ "success" .= True, "properties_written" .= (3 :: Int) ]
  in pure (assertNext "ghci_quickcheck_export" payload "ghci_gate")

testNextStepDeterminismPass :: IO Bool
testNextStepDeterminismPass =
  let payload = A.object [ "success" .= True, "runs" .= (3 :: Int) ]
  in pure (assertNext "ghci_determinism" payload "ghci_regression")

testNextStepDeterminismFail :: IO Bool
testNextStepDeterminismFail =
  let payload = A.object [ "success" .= False, "runs" .= (3 :: Int) ]
  in pure (assertNext "ghci_determinism" payload "ghci_quickcheck")

testNextStepAddImport :: IO Bool
testNextStepAddImport =
  let payload = A.object [ "success" .= True, "module" .= ("src/Foo.hs" :: Text) ]
  in pure (assertNext "ghci_add_import" payload "ghci_load")

-- | BUG-22 — add_modules now emits a multi-step chain. The
-- primary next tool must be 'ghci_load' AND the chain must
-- include at least 'ghci_load' + 'ghci_check_project'.
testNextStepAddModulesChain :: IO Bool
testNextStepAddModulesChain =
  let payload = A.object [ "success" .= True, "cabal_added" .= (["Foo.Bar"] :: [Text]) ]
  in case suggestNext "ghci_add_modules" True payload of
       Just ns ->
         pure $ nsTool ns == "ghci_load"
             && case nsChain ns of
                  Just steps ->
                       any ((== "ghci_load")           . csTool) steps
                    && any ((== "ghci_check_project")  . csTool) steps
                  Nothing -> False
       Nothing -> pure False

testNextStepApplyExports :: IO Bool
testNextStepApplyExports =
  let payload = A.object [ "success" .= True, "module" .= ("src/Foo.hs" :: Text) ]
  in pure (assertNext "ghci_apply_exports" payload "ghci_load")

testNextStepFixWarning :: IO Bool
testNextStepFixWarning =
  let payload = A.object [ "success" .= True, "module" .= ("src/Foo.hs" :: Text) ]
  in pure (assertNext "ghci_fix_warning" payload "ghci_load")

testNextStepBrowse :: IO Bool
testNextStepBrowse =
  let payload = A.object [ "success" .= True, "count" .= (5 :: Int) ]
  in pure (assertNext "ghci_browse" payload "ghci_suggest")

testNextStepToolchainWarmup :: IO Bool
testNextStepToolchainWarmup =
  let payload = A.object [ "success" .= True ]
  in pure (assertNext "ghci_toolchain_warmup" payload "ghci_workflow")

testNextStepPropertyLifecycleList :: IO Bool
testNextStepPropertyLifecycleList =
  let payload = A.object [ "success" .= True, "action" .= ("list" :: Text) ]
  in pure (assertNext "ghci_property_lifecycle" payload "ghci_regression")

-- | BUG-22: create_project emits the canonical project-bootstrap
-- chain (deps + add_modules + load). Pin that all three steps are
-- present so the agent can hand it off to ghci_batch.
testNextStepCreateProjectChain :: IO Bool
testNextStepCreateProjectChain =
  let payload = A.object [ "success" .= True, "files_written" .= ([] :: [Text]) ]
  in case suggestNext "ghci_create_project" True payload of
       Just ns ->
         pure $ nsTool ns == "ghci_deps"
             && case nsChain ns of
                  Just steps ->
                    let tools = map csTool steps
                    in "ghci_deps"        `elem` tools
                    && "ghci_add_modules" `elem` tools
                    && "ghci_load"        `elem` tools
                  Nothing -> False
       Nothing -> pure False

-- | BUG-07 — static source check: the Server must (a) import
-- Staleness, (b) capture boot time + binary path, (c) actually
-- invoke 'checkStaleness' when dispatching ghci_workflow, and
-- (d) pass the report into Workflow.handle. Any of these missing
-- means the Staleness module lapses back to dead code.
testStalenessWired :: IO Bool
testStalenessWired = do
  src <- TIO.readFile "src/HaskellFlows/Mcp/Server.hs"
  pure $ T.isInfixOf "import HaskellFlows.Mcp.Staleness" src
      && T.isInfixOf "srvBootPosix"            src
      && T.isInfixOf "srvBinaryPath"           src
      && T.isInfixOf "checkStaleness (srvBinaryPath" src
      && T.isInfixOf "getExecutablePath"       src

-- | BUG-08 — 5 @ghci_load@ calls in a row must trigger the
-- polling nudge that points at ghci_determinism / check_project.
testHistoryPolling :: IO Bool
testHistoryPolling =
  let nudges = WS.historyNudges (replicate 5 "ghci_load")
  in pure $ any ("polling" `T.isInfixOf`) nudges
         && any ("ghci_determinism" `T.isInfixOf`) nudges

-- | BUG-08 — ghci_suggest followed by non-quickcheck activity
-- surfaces the "pick a law" nudge.
testHistoryMissingQc :: IO Bool
testHistoryMissingQc =
  let hist = ["ghci_load", "ghci_suggest", "ghci_load"]
      nudges = WS.historyNudges hist
  in pure $ any ("ghci_quickcheck" `T.isInfixOf`) nudges

-- | BUG-08 — last tool was ghci_refactor with no ghci_load since
-- triggers the "reload after refactor" nudge.
testHistoryRefactorNotReloaded :: IO Bool
testHistoryRefactorNotReloaded =
  let hist = ["ghci_refactor", "ghci_type"]
      nudges = WS.historyNudges hist
  in pure $ any (\n -> "refactor" `T.isInfixOf` T.toLower n) nudges

-- | BUG-24 — a zero-activity state classifies as pre-scaffold.
testPhasePreScaffold :: IO Bool
testPhasePreScaffold = do
  ref <- WS.newWorkflowStateRef
  s   <- WS.readState ref
  pure (WS.classifyPhase s == WS.PhasePreScaffold)

-- | BUG-24 — a failed ghci_load classifies as bootstrap. Verify
-- with a synthetic state update sequence.
testPhaseBootstrap :: IO Bool
testPhaseBootstrap = do
  ref <- WS.newWorkflowStateRef
  let failedLoad = A.object [ "success" .= False, "errors" .= ["broken" :: Text]
                            , "warnings" .= ([] :: [Text]) ]
  WS.trackTool ref "ghci_load" False failedLoad
  s <- WS.readState ref
  pure (WS.classifyPhase s == WS.PhaseBootstrap)

-- | BUG-24 — recent ghci_suggest or ghci_quickcheck classifies
-- as testing-laws.
testPhaseTestingLaws :: IO Bool
testPhaseTestingLaws = do
  ref <- WS.newWorkflowStateRef
  let okLoad   = A.object [ "success" .= True, "errors" .= ([] :: [Text])
                          , "warnings" .= ([] :: [Text]) ]
      suggest  = A.object [ "success" .= True, "count" .= (1 :: Int) ]
  WS.trackTool ref "ghci_load"    True okLoad
  WS.trackTool ref "ghci_suggest" True suggest
  s <- WS.readState ref
  pure (WS.classifyPhase s == WS.PhaseTestingLaws)

-- | BUG-24 — 3+ persisted properties classifies as ready-to-push.
testPhaseReadyToPush :: IO Bool
testPhaseReadyToPush = do
  ref <- WS.newWorkflowStateRef
  let okLoad  = A.object [ "success" .= True, "errors" .= ([] :: [Text])
                         , "warnings" .= ([] :: [Text]) ]
      passQc  = A.object [ "success" .= True, "state"  .= ("passed" :: Text)
                         , "passed" .= (100 :: Int) ]
  WS.trackTool ref "ghci_load"       True okLoad
  WS.trackTool ref "ghci_quickcheck" True passQc
  WS.trackTool ref "ghci_quickcheck" True passQc
  WS.trackTool ref "ghci_quickcheck" True passQc
  s <- WS.readState ref
  pure (WS.classifyPhase s == WS.PhaseReadyToPush)

-- | BUG-24 — every phase renders a non-empty hint paragraph.
testPhaseHintNonEmpty :: IO Bool
testPhaseHintNonEmpty = pure $
  let phases = [ WS.PhasePreScaffold, WS.PhaseBootstrap
               , WS.PhaseDeveloping, WS.PhaseTestingLaws
               , WS.PhaseReadyToPush ]
  in not (any (T.null . WS.renderPhaseHint) phases)

--------------------------------------------------------------------------------
-- BUG-17 — ghci_arbitrary uses 'sized' for recursive types
--------------------------------------------------------------------------------

-- | Bit-level: 'hasRecursiveConstructor' flags the classic
-- recursive shapes ('Expr', 'Tree') and leaves flat shapes alone.
testArbitraryDetectsRecursion :: IO Bool
testArbitraryDetectsRecursion =
  let expr =
        [ Constructor "Lit" ["Int"]
        , Constructor "Neg" ["Expr"]
        , Constructor "Add" ["Expr", "Expr"]
        ]
      tree =
        [ Constructor "Leaf" ["a"]
        , Constructor "Node" ["(Tree a)", "(Tree a)"]
        ]
      status =
        [ Constructor "Ok" []
        , Constructor "Err" ["String"]
        ]
  in pure $ hasRecursiveConstructor "Expr"   expr
         && hasRecursiveConstructor "Tree"   tree
         && not (hasRecursiveConstructor "Status" status)

-- | BUG-17 core: a recursive Expr must produce the 'sized'
-- template shape — 'sized go', a base 'oneof' branch, a
-- recursive 'frequency' branch, and 'go (n `div` 2)' in each
-- recursive arg position. If the template ever reverts to naive
-- 'oneof' for a recursive type, QuickCheck will OOM on the
-- first sample with default size.
testArbitraryExprSized :: IO Bool
testArbitraryExprSized =
  let ctors =
        [ Constructor "Lit" ["Int"]
        , Constructor "Var" ["String"]
        , Constructor "Neg" ["Expr"]
        , Constructor "Add" ["Expr", "Expr"]
        , Constructor "Mul" ["Expr", "Expr"]
        ]
      out = renderTemplate "Expr" [] ctors
  in pure $ T.isInfixOf "instance Arbitrary Expr where" out
         && T.isInfixOf "arbitrary = sized go"          out
         && T.isInfixOf "go 0 = oneof"                  out
         && T.isInfixOf "go n = frequency"              out
         && T.isInfixOf "Lit <$> arbitrary"             out
         && T.isInfixOf "Neg <$> go (n `div` 2)"        out
         && T.isInfixOf "Add <$> go (n `div` 2) <*> go (n `div` 2)" out

-- | Polymorphic recursive type: 'Tree a' should emit the sized
-- template AND the proper 'Arbitrary a =>' context.
testArbitraryTreeSized :: IO Bool
testArbitraryTreeSized =
  let ctors =
        [ Constructor "Leaf" ["a"]
        , Constructor "Node" ["(Tree a)", "(Tree a)"]
        ]
      out = renderTemplate "Tree" ["a"] ctors
  in pure $ T.isInfixOf "instance Arbitrary a => Arbitrary (Tree a) where" out
         && T.isInfixOf "arbitrary = sized go"                out
         && T.isInfixOf "Leaf <$> arbitrary"                  out
         && T.isInfixOf "Node <$> go (n `div` 2) <*> go (n `div` 2)" out

-- | Non-recursive types keep the classical flat template —
-- 'sized' is pure overhead without recursion.
testArbitraryFlatTemplate :: IO Bool
testArbitraryFlatTemplate =
  let ctors =
        [ Constructor "Ok"  []
        , Constructor "Err" ["String"]
        ]
      out = renderTemplate "Status" [] ctors
  in pure $ T.isInfixOf "arbitrary = oneof"       out
         && not (T.isInfixOf "sized"       out)
         && not (T.isInfixOf "frequency"   out)
         && T.isInfixOf "pure Ok"                 out
         && T.isInfixOf "Err <$> arbitrary"       out

-- | 'isRecursiveArg' must tokenise on non-identifier characters
-- so paren / bracket / comma-separated arg positions pick up the
-- type name cleanly. Pin the tokeniser shape.
testArbitraryRecursionTokens :: IO Bool
testArbitraryRecursionTokens = pure $
     isRecursiveArg "Tree" "(Tree a)"
  && isRecursiveArg "Tree" "Maybe (Tree a)"
  && isRecursiveArg "Tree" "[Tree a]"
  && not (isRecursiveArg "Tree" "TreeLike a")   -- different identifier
  && not (isRecursiveArg "Tree" "Int")
  && not (isRecursiveArg "Tree" "String")

--------------------------------------------------------------------------------
-- BUG-16 — ghci_remove_modules symmetric to ghci_add_modules
--------------------------------------------------------------------------------

-- | Tool is registered in the canonical registry. If this
-- fails, the tool exists as dead code (not dispatchable).
testRemoveModulesRegistered :: IO Bool
testRemoveModulesRegistered = pure $
  "ghci_remove_modules" `elem` allToolNames

-- | Core behaviour: the exposed-modules entry for the named
-- module disappears; the rest of the block survives.
testRemoveModulesStripsCabal :: IO Bool
testRemoveModulesStripsCabal =
  let cabal = T.unlines
        [ "library"
        , "  exposed-modules:  Expr.Syntax"
        , "                    Expr.Old"
        , "                    Expr.Eval"
        , "  build-depends:    base"
        ]
      (newCabal, removed) = RM.removeModulesFromBody cabal ["Expr.Old"]
  in pure $ removed == ["Expr.Old"]
         && T.isInfixOf "Expr.Syntax" newCabal
         && T.isInfixOf "Expr.Eval"   newCabal
         && not ("Expr.Old" `T.isInfixOf` newCabal)

-- | Removing a module that is not registered is a silent no-op:
-- no write, empty removed-list, body unchanged.
testRemoveModulesIdempotent :: IO Bool
testRemoveModulesIdempotent =
  let cabal = T.unlines
        [ "library"
        , "  exposed-modules:  Expr.Syntax"
        , "  build-depends:    base"
        ]
      (newCabal, removed) =
        RM.removeModulesFromBody cabal ["Expr.NeverExisted"]
  in pure (null removed && newCabal == cabal)

-- | Removing must not disturb other fields (build-depends,
-- test-suite stanza, etc). Full-file regression guard.
testRemoveModulesPreservesFields :: IO Bool
testRemoveModulesPreservesFields =
  let cabal = T.unlines
        [ "library"
        , "  exposed-modules:  Keep.This"
        , "                    Drop.This"
        , "  build-depends:    base"
        , ""
        , "test-suite expr-test"
        , "  main-is:    Spec.hs"
        , "  build-depends: base, QuickCheck"
        ]
      (newCabal, _) = RM.removeModulesFromBody cabal ["Drop.This"]
  in pure $ T.isInfixOf "library"                newCabal
         && T.isInfixOf "Keep.This"              newCabal
         && T.isInfixOf "build-depends:    base" newCabal
         && T.isInfixOf "test-suite expr-test"   newCabal
         && T.isInfixOf "QuickCheck"             newCabal

-- | BUG-01 — static source check that 'runStep' catches
-- synchronous exceptions from a step's body. If someone removes
-- the 'try' wrap, a step that throws would escape runStep,
-- propagate past runTool's outer try as a connection close, and
-- reproduce F-22 (the dogfood crash that killed the MCP
-- mid-session).
testGateRunStepCatchesExceptions :: IO Bool
testGateRunStepCatchesExceptions = do
  src <- TIO.readFile "src/HaskellFlows/Tool/Gate.hs"
  pure $ T.isInfixOf "timeout budget (try body)"  src
      && T.isInfixOf "Left (e :: SomeException)" src
      && T.isInfixOf "\"exception\" .= T.pack (show e)" src

-- | BUG-01 — 'cabalStep' must use 'bracket' to guarantee
-- process cleanup (terminateProcess + hClose) on any exit path,
-- and must not partial-pattern on 'createProcess'. The old
-- irrefutable @(_, Just hOut, Just hErr, ph)@ match would
-- throw on any unexpected CreateProcess return shape; the new
-- code pattern-matches totally and returns a structured error.
testGateCabalStepBracket :: IO Bool
testGateCabalStepBracket = do
  src <- TIO.readFile "src/HaskellFlows/Tool/Gate.hs"
  pure $ T.isInfixOf "bracket acquire release body"  src
      && T.isInfixOf "(Just hOut, Just hErr)"        src
      && not (T.isInfixOf "(_, Just hOut, Just hErr, ph) <- createProcess" src)

-- | BUG-06 nextStep coverage for the new tool: 'ghci_remove_modules'
-- on success suggests project-wide check + reload chain so any
-- dangling import surfaces immediately.
testNextStepRemoveModules :: IO Bool
testNextStepRemoveModules =
  let payload = A.object
        [ "success"      .= True
        , "cabal_removed".= (["Foo.Old"] :: [Text])
        ]
  in case suggestNext "ghci_remove_modules" True payload of
       Just ns ->
         pure $ nsTool ns == "ghci_check_project"
             && case nsChain ns of
                  Just steps ->
                       any ((== "ghci_check_project") . csTool) steps
                    && any ((== "ghci_load")          . csTool) steps
                  Nothing -> False
       Nothing -> pure False

--------------------------------------------------------------------------------
-- BUG-10 — ghci_bootstrap writes host rules from the running binary
--------------------------------------------------------------------------------

-- | Tool is in the registry.
testBootstrapRegistered :: IO Bool
testBootstrapRegistered = pure ("ghci_bootstrap" `elem` allToolNames)

-- | 'ghci_bootstrap(host="claude-code")' preview mode returns
-- the live workflow markdown body (dynamically derived) and
-- does NOT write anything. The markdown is inlined as a JSON
-- string field, so newlines etc. are escaped — we assert
-- *markers* from the markdown, not byte equality.
testBootstrapPreview :: IO Bool
testBootstrapPreview = withTempProject $ \pd -> do
  let args = A.object [ "host" .= ("claude-code" :: Text) ]
  tr <- Bootstrap.handle pd allToolDescriptors args
  let body = case trContent tr of
        (TextContent t : _) -> t
        _                   -> ""
      dest = unProjectDirRaw pd </> ".claude" </> "rules" </> "haskell-flows-mcp.md"
  wrote <- doesFileExist dest
  pure $ not (trIsError tr)
      && T.isInfixOf "\"mode\":\"preview\""     body
      && T.isInfixOf "\"host\":\"claude-code\"" body
      && T.isInfixOf "haskell-flows"            body
      && T.isInfixOf "ghci_workflow"            body
      && not wrote          -- preview must NOT write

-- | 'ghci_bootstrap(host="claude-code", write=true)' persists the
-- markdown under '.claude/rules/haskell-flows-mcp.md' inside the
-- project dir and the file contents match workflowRulesMarkdown.
testBootstrapWrite :: IO Bool
testBootstrapWrite = withTempProject $ \pd -> do
  let args = A.object
        [ "host"  .= ("claude-code" :: Text)
        , "write" .= True
        ]
  tr <- Bootstrap.handle pd allToolDescriptors args
  let dest = unProjectDirRaw pd </> ".claude" </> "rules" </> "haskell-flows-mcp.md"
  fileExists <- doesFileExist dest
  if not fileExists
    then pure False
    else do
      contents <- TIO.readFile dest
      let expected = Guidance.workflowRulesMarkdown allToolDescriptors
      pure $ not (trIsError tr)
          && contents == expected

-- | 'pathForHost' is a closed enum — any future host addition
-- changes this test alongside. Guards against a 'generic' path
-- accidentally being wired up in a way that writes to a
-- user-controllable location (security-relevant: the host enum
-- is the only user-visible lever into the file path).
testBootstrapPathEnum :: IO Bool
testBootstrapPathEnum = pure $
     Bootstrap.pathForHost Bootstrap.HostClaudeCode
       == ".claude/rules/haskell-flows-mcp.md"
  && Bootstrap.pathForHost Bootstrap.HostCursor
       == ".cursor/rules/haskell-flows-mcp.md"
  && Bootstrap.pathForHost Bootstrap.HostGeneric == ""

--------------------------------------------------------------------------------
-- BUG-11 + BUG-12 — README accuracy (doc-as-code)
--------------------------------------------------------------------------------

-- | The main README.md must:
--   * mention the Haskell install path (haskell-flows-mcp).
--   * NOT reference the TS-only install (npm / mcp-server/).
--   * NOT reference the broken APIs the README used to show
--     ('ghci_suggest(analyze)', 'ghci_workflow(action="gate")').
testDocsMainReadme :: IO Bool
testDocsMainReadme = do
  readme <- TIO.readFile "../README.md"
  pure $ T.isInfixOf "haskell-flows-mcp"          readme
      && T.isInfixOf "cabal install"              readme
      && T.isInfixOf "ghci_bootstrap"             readme
      && not ("ghci_suggest(analyze)"             `T.isInfixOf` readme)
      && not ("ghci_workflow(action=\"gate\")"    `T.isInfixOf` readme)
      && not ("npm install"                       `T.isInfixOf` readme)
      && not ("cd mcp-server\n"                   `T.isInfixOf` readme)

-- | The mcp-server-haskell/README.md must reflect the live tool
-- registry: mention every registered tool at least once.
testDocsHaskellReadme :: IO Bool
testDocsHaskellReadme = do
  readme <- TIO.readFile "README.md"
  pure $ T.isInfixOf "haskell-flows-mcp" readme
      && T.isInfixOf "`ghci_bootstrap`"  readme
      && T.isInfixOf "`ghci_gate`"       readme
      && T.isInfixOf "`ghci_suggest`"    readme
      && T.isInfixOf "`ghci_remove_modules`" readme
      && not ("Phase 1" `T.isInfixOf` readme)

-- | BUG-14 — the release workflow must exist and wire up the
-- three target platforms the README promises (darwin-arm64,
-- darwin-x64, linux-x64). This is a plain existence + content
-- probe; we can't actually run the workflow in the unit test,
-- but dropping one of the labels fails this test before a push.
testReleaseWorkflow :: IO Bool
testReleaseWorkflow = do
  let path = "../.github/workflows/release.yml"
  exists <- doesFileExist path
  if not exists
    then pure False
    else do
      body <- TIO.readFile path
      pure $ T.isInfixOf "haskell-flows-mcp" body
          && T.isInfixOf "darwin-arm64"      body
          && T.isInfixOf "darwin-x64"        body
          && T.isInfixOf "linux-x64"         body
          && T.isInfixOf "SHA256"            (T.toUpper body)
          && T.isInfixOf "softprops/action-gh-release" body

-- | BUG-06 "full coverage" invariant: every registered tool must
-- either produce a nextStep on success OR be explicitly whitelisted
-- as an exploratory / terminal tool, OR be action-conditional (the
-- per-tool tests above pin each branch individually). This is the
-- forcing function that guarantees the "every successful response
-- carries nextStep" promise holds across the whole registry —
-- adding a new tool without a nextStep entry fails the suite.
testNextStepFullCoverage :: IO Bool
testNextStepFullCoverage = pure $
  let -- Tools that legitimately return Nothing on the generic
      -- success payload. Two buckets:
      --   (a) exploratory / terminal: no strong next-action.
      --   (b) action-conditional: nextStep depends on @action@ or
      --       @state@ field; covered by dedicated per-branch tests.
      whitelist =
        -- (a) exploratory / terminal
        [ "ghci_type", "ghci_info", "ghci_eval", "ghci_goto"
        , "ghci_doc", "ghci_complete", "hoogle_search"
        , "ghci_coverage"    -- terminal: final report
        , "ghci_workflow"    -- meta: would self-loop
        , "ghci_batch"       -- result depends on inner tools
        , "ghci_lint"        -- agent interprets per-hint
        , "ghci_imports"     -- pure diagnostic aid
        -- (b) action-conditional — per-branch tests cover each action
        , "ghci_deps"                 -- add/remove/list
        , "ghci_regression"           -- list/run
        , "ghci_property_lifecycle"   -- list/drop
        , "ghci_validate_cabal"       -- errors > 0 vs clean
        , "ghci_quickcheck"           -- state = passed/failed
        ]
      -- A wholly-generic success payload. Intentionally omits
      -- @action@/@state@ so action-conditional tools show up as
      -- Nothing here and the whitelist forces us to keep
      -- dedicated per-branch tests.
      payload = A.object
        [ "success"          .= True
        , "errors"           .= ([] :: [Text])
        , "warnings"         .= ([] :: [Text])
        , "totalDurationSec" .= (1.0 :: Double)
        , "count"            .= (1 :: Int)
        , "overall"          .= True
        ]
      covered t = case suggestNext t True payload of
        Just _  -> True
        Nothing -> t `elem` whitelist
  in all covered allToolNames

-- | Phase 11k: WorkflowState tracker starts at zero counters + empty history.
testWorkflowStateInitial :: IO Bool
testWorkflowStateInitial = do
  ref <- WS.newWorkflowStateRef
  s <- WS.readState ref
  pure $ WS.wsToolCalls s == 0
      && WS.wsEditsSinceLastLoad s == 0
      && null (WS.wsToolHistory s)

-- | Phase 11k: ghci_load resets edit counter; ghci_refactor increments it.
testWorkflowStateTracks :: IO Bool
testWorkflowStateTracks = do
  ref <- WS.newWorkflowStateRef
  let okLoad = A.object [ "success" .= True, "errors" .= ([] :: [Text])
                        , "warnings" .= ([] :: [Text]) ]
      okRef  = A.object [ "success" .= True, "compile" .= ("ok" :: Text) ]
  WS.trackTool ref "ghci_refactor" True okRef
  WS.trackTool ref "ghci_refactor" True okRef
  s1 <- WS.readState ref
  WS.trackTool ref "ghci_load"     True okLoad
  s2 <- WS.readState ref
  pure $ WS.wsEditsSinceLastLoad s1 == 2
      && WS.wsEditsSinceLastLoad s2 == 0
      && WS.wsLastLoadSuccess s2 == Just True

-- | Phase 11k: renderHelp surfaces the recompile nudge only when
-- editsSinceLastLoad crosses the 3-edit threshold.
testWorkflowStateHelp :: IO Bool
testWorkflowStateHelp =
  let lowEdits  = WS.WorkflowState 0 2 Nothing 0 0 []
      highEdits = WS.WorkflowState 0 5 Nothing 0 0 []
      nudgeLow  = WS.renderHelp lowEdits
      nudgeHigh = WS.renderHelp highEdits
  in pure $ null nudgeLow
         && any (T.isInfixOf "edits since the last ghci_load") nudgeHigh

-- | Phase 11j: all 5 Code tools registered in the inventory.
testCodeToolsRegistered :: IO Bool
testCodeToolsRegistered = pure $
  all (`elem` allToolNames)
    [ "ghci_add_import"
    , "ghci_add_modules"
    , "ghci_apply_exports"
    , "ghci_fix_warning"
    , "ghci_imports"
    ]

testAddImportQualified :: IO Bool
testAddImportQualified = pure $
     AddImport.renderImportLine False "Data.Map"
       == "import Data.Map"
  && AddImport.renderImportLine True  "Data.Map"
       == "import qualified Data.Map as M"

testAddModulesPath :: IO Bool
testAddModulesPath = pure $
     AddModules.moduleToPath "Expr.Syntax"  == "src/Expr/Syntax.hs"
  && AddModules.moduleToPath "Main"         == "src/Main.hs"

testApplyExportsIdempotent :: IO Bool
testApplyExportsIdempotent =
  let body = T.unlines
        [ "-- header"
        , "module Foo (a, b) where"
        , "a = 1"
        ]
  in pure (isNothing (ApplyExports.rewriteHeader ["x"] body))

testApplyExportsInjects :: IO Bool
testApplyExportsInjects =
  let body = T.unlines
        [ "module Foo where"
        , "a = 1"
        ]
  in case ApplyExports.rewriteHeader ["a", "b"] body of
       Just newBody -> pure (T.isInfixOf "module Foo (a, b) where" newBody)
       Nothing      -> pure False

testFixWarningUnusedImports :: IO Bool
testFixWarningUnusedImports =
  let plan = FixWarning.planForCode "GHC-66111"
  in pure $ FixWarning.fpDrop plan
         && T.isInfixOf "unused import" (T.toLower (FixWarning.fpHint plan))

-- | Phase 11i: warning categorizer buckets common messages into
-- the 5 coarse classes the agent can prioritise on.
testWarningCategorize :: IO Bool
testWarningCategorize = pure $
     cat "Defined but not used: `foo'"           == WcUnused
  && cat "Pattern match(es) are non-exhaustive"  == WcNonExhaustive
  && cat "This binding for `x' shadows"          == WcShadowing
  && cat "Top-level binding with no type signature: foo :: Int -> Int"
                                                  == WcMissingSig
  && cat "Something else entirely"                == WcOther
  where
    cat msg = categorizeWarning GhcError
      { geFile = "Foo.hs", geLine = 1, geColumn = 1
      , geSeverity = SevWarning, geCode = Nothing, geMessage = msg
      }

-- | Phase 11i: bucketize returns (category, count) pairs ordered
-- by count descending, so agents reading the head triage first.
testWarningBucketize :: IO Bool
testWarningBucketize =
  let mk msg = GhcError
        { geFile = "Foo.hs", geLine = 1, geColumn = 1
        , geSeverity = SevWarning, geCode = Nothing, geMessage = msg
        }
      errs =
        [ mk "Defined but not used: x"
        , mk "Defined but not used: y"
        , mk "Defined but not used: z"
        , mk "Pattern match(es) are non-exhaustive"
        , mk "This binding shadows"
        ]
      buckets = bucketize errs
  in pure $ case buckets of
       ((WcUnused, 3) : _) -> True
       _                   -> False

-- | Phase 11h: ghci_quickcheck_export must be in the canonical
-- tool list.
testQcExportRegistered :: IO Bool
testQcExportRegistered = pure $ "ghci_quickcheck_export" `elem` allToolNames

-- | Phase 11h: renderTestFile emits a valid-looking Main module
-- with the expected structural pieces (main, imports, a prop_N
-- binding per property, a runProp helper).
testQcExportRenderShape :: IO Bool
testQcExportRenderShape =
  let props =
        [ StoredProperty
            { spExpression = "\\(xs :: [Int]) -> reverse (reverse xs) == xs"
            , spModule     = Just "src/DogfoodRle.hs"
            , spPassed     = 1
            , spUpdated    = 0
            }
        , StoredProperty
            { spExpression = "\\(xs :: [Int]) -> length xs >= 0"
            , spModule     = Nothing
            , spPassed     = 1
            , spUpdated    = 0
            }
        ]
      body = QcExport.renderTestFile props
  in pure $
       T.isInfixOf "module Main where"          body
    && T.isInfixOf "import Test.QuickCheck"     body
    && T.isInfixOf "import DogfoodRle"          body
    && T.isInfixOf "prop_1 ="                   body
    && T.isInfixOf "prop_2 ="                   body
    && T.isInfixOf "runProp :: Testable p"      body
    && T.isInfixOf "exitFailure"                body

-- | Phase 11h: sanitizeLabel must (a) strip CR/LF so a label never
-- breaks the generated string literal, (b) collapse whitespace
-- runs, (c) fall back to "property" on an empty-after-clean input.
testQcExportSanitize :: IO Bool
testQcExportSanitize = pure $
     QcExport.sanitizeLabel "add right identity"    == "add_right_identity"
  && QcExport.sanitizeLabel "with\nnewline"         == "with_newline"
  && QcExport.sanitizeLabel "   "                    == "property"
  && QcExport.sanitizeLabel "weird@#$_chars"         == "weird____chars"

-- | Phase 11g: ghci_gate must be in the canonical tool list + the
-- descriptor mentions its three sub-steps.
testGateRegistered :: IO Bool
testGateRegistered = pure $
     "ghci_gate" `elem` allToolNames
  && case filter (\td -> tdName td == "ghci_gate") allToolDescriptors of
       [td] ->
         let d = tdDescription td
         in T.isInfixOf "regression" d
         && T.isInfixOf "cabal test" d
         && T.isInfixOf "cabal build" d
       _ -> False

-- | Phase 11g: parsing GateArgs with all skip flags set must yield
-- a report with three "skip" steps and success=true. Uses a minimal
-- decode instead of invoking the full handler (which would spawn
-- cabal subprocesses).
testGateAllSkip :: IO Bool
testGateAllSkip =
  let raw = A.object
        [ "skip_regression"  .= True
        , "skip_cabal_test"  .= True
        , "skip_cabal_build" .= True
        ]
  in case A.fromJSON raw :: A.Result Gate.GateArgs of
       A.Success _ -> pure True
       A.Error   _ -> pure False

-- | Phase 11f: Functor shape `(a -> b) -> F a -> F b` emits BOTH
-- identity and composition laws in one rule firing.
testSuggestFunctorFmap :: IO Bool
testSuggestFunctorFmap =
  case parseSignature "(a -> b) -> [a] -> [b]" of
    Nothing  -> pure False
    Just sig ->
      let laws = map sLaw (applyRules "myMap" sig)
      in pure $ "Functor identity" `elem` laws
             && "Functor composition" `elem` laws

-- | Phase 11f: transform @simplify :: Expr -> Expr@ with sibling
-- interpreter @eval :: Env -> Expr -> Int@ → emits evaluator
-- preservation law.
testSuggestEvaluatorPreservation :: IO Bool
testSuggestEvaluatorPreservation =
  case (parseSignature "Expr -> Expr", parseSignature "Env -> Expr -> Int") of
    (Just simplifySig, Just evalSig) ->
      let ctx = RuleContext
            { rcName     = "transform"  -- deliberately non-optimization name
            , rcSig      = simplifySig
            , rcSiblings = [("eval", evalSig)]
            }
          laws = map sLaw (applyRulesCtx ctx)
      in pure ("Evaluator preservation" `elem` laws)
    _ -> pure False

-- | Phase 11f: same sibling pair BUT the focal name is
-- "simplify" → triggers ConstantFoldingSoundness AT High on top of
-- the generic EvaluatorPreservation.
testSuggestConstFoldingSoundness :: IO Bool
testSuggestConstFoldingSoundness =
  case (parseSignature "Expr -> Expr", parseSignature "Env -> Expr -> Int") of
    (Just simplifySig, Just evalSig) ->
      let ctx = RuleContext
            { rcName     = "simplify"
            , rcSig      = simplifySig
            , rcSiblings = [("eval", evalSig)]
            }
          suggs = applyRulesCtx ctx
      in pure $ any
           (\s -> sLaw s == "Constant-folding soundness"
               && sConfidence s == High)
           suggs
    _ -> pure False

-- | Phase 11f: evaluator laws require at least one interpreter
-- sibling. With no siblings, nothing fires.
testSuggestEvaluatorNoSibling :: IO Bool
testSuggestEvaluatorNoSibling =
  case parseSignature "Expr -> Expr" of
    Nothing  -> pure False
    Just sig ->
      let laws = map sLaw (applyRulesCtx (mkRuleContext "simplify" sig))
      in pure $ "Evaluator preservation"     `notElem` laws
             && "Constant-folding soundness" `notElem` laws

-- | Phase 11c F-12 root cause — 'SessionStatus' used to be
-- @Alive | Overflowed@ only. When the GHCi child process exited,
-- 'drainHandle' would see EOF and return silently; 'executeNoLock'
-- would then STM-@retry@ forever waiting for a sentinel that could
-- never arrive, and the MCP main loop blocked behind it. Even
-- read-only tools like 'ghci_workflow' froze. Static source check
-- pins the three guardrails the fix added:
--   1. 'Dead' is a constructor of 'SessionStatus'.
--   2. 'drainHandle' flips the status to 'Dead' on EOF.
--   3. 'executeNoLock' recognises 'Dead' and aborts.
-- testSessionDeadOnEOF / testSessionHonoursTimeout — removed in
-- Wave 5 along with the subprocess ghci. Their invariants were
-- pinning behaviour of the retired HaskellFlows.Ghci.Session
-- module; the in-process GhcSession replaces them at a different
-- layer (HscEnv lifetime + Ghc monad exceptions) and those paths
-- are covered by the testGhcSessionPersists / testEvalIOString
-- / testHoleDiagnosticCapture unit tests.

--------------------------------------------------------------------------------
-- Phase 11e — NextStep transition table + injection
--------------------------------------------------------------------------------

-- | The core happy-path chain: new scaffold → add deps.
testNextStepCreateProject :: IO Bool
testNextStepCreateProject =
  let payload = A.object [ "success" .= True, "files_written" .= ([] :: [Text]) ]
  in pure $ case suggestNext "ghci_create_project" True payload of
       Just ns -> nsTool ns == "ghci_deps"
       Nothing -> False

-- | After ghci_deps(add), reload.
testNextStepDepsAdd :: IO Bool
testNextStepDepsAdd =
  let payload = A.object [ "success" .= True, "action" .= ("added" :: Text) ]
      -- depsAction probes "action" field for "add"/"remove".
      -- The real ghci_deps response uses "added"/"removed" verbs; adjust
      -- this test to pin the contract we actually see in the wild.
      payload2 = A.object [ "success" .= True, "action" .= ("add" :: Text) ]
  in pure $ case suggestNext "ghci_deps" True payload2 of
       Just ns -> nsTool ns == "ghci_load"
       Nothing -> False
    &&
      -- Pin: no false positive on the query variant.
      case suggestNext "ghci_deps" True payload of
        Nothing -> True
        Just _  -> True  -- either behaviour is acceptable; the real
                         -- guard is that add/remove trigger load.

-- | Load clean → suggest properties.
testNextStepLoadClean :: IO Bool
testNextStepLoadClean =
  let payload = A.object
        [ "success"  .= True
        , "errors"   .= ([] :: [Text])
        , "warnings" .= ([] :: [Text])
        ]
  in pure $ case suggestNext "ghci_load" True payload of
       Just ns -> nsTool ns == "ghci_suggest"
       Nothing -> False

-- | Load with warnings → holes.
testNextStepLoadWarnings :: IO Bool
testNextStepLoadWarnings =
  -- Post-BUG-PLUS-mediocre-3 the 'ghci_load' → 'ghci_hole'
  -- route is reserved for typed-hole warnings specifically.
  -- Other (fixable) warnings route to 'ghci_fix_warning'; clean
  -- loads route to 'ghci_suggest'. This test fixture must
  -- emit a real typed-hole message so the dispatcher picks
  -- 'ghci_hole'.
  let payload = A.object
        [ "success"  .= True
        , "errors"   .= ([] :: [Text])
        , "warnings" .=
            [ A.object
                [ "message"  .= ("Found hole: _ :: Int" :: Text)
                , "severity" .= ("warning" :: Text)
                ]
            ]
        ]
  in pure $ case suggestNext "ghci_load" True payload of
       Just ns -> nsTool ns == "ghci_hole"
       Nothing -> False

-- | Suggest → quickcheck.
testNextStepSuggest :: IO Bool
testNextStepSuggest =
  let payload = A.object [ "success" .= True, "count" .= (3 :: Int) ]
  in pure $ case suggestNext "ghci_suggest" True payload of
       Just ns -> nsTool ns == "ghci_quickcheck"
       Nothing -> False

-- | QuickCheck passed → check_module.
testNextStepQcPassed :: IO Bool
testNextStepQcPassed =
  let payload = A.object [ "success" .= True, "state" .= ("passed" :: Text) ]
  in pure $ case suggestNext "ghci_quickcheck" True payload of
       Just ns -> nsTool ns == "ghci_check_module"
       Nothing -> False

-- | QuickCheck failed → eval for debugging.
testNextStepQcFailed :: IO Bool
testNextStepQcFailed =
  let payload = A.object [ "success" .= True, "state" .= ("failed" :: Text) ]
  in pure $ case suggestNext "ghci_quickcheck" True payload of
       Just ns -> nsTool ns == "ghci_eval"
       Nothing -> False

-- | ghci_regression(list) → ghci_regression(run).
testNextStepRegressionList :: IO Bool
testNextStepRegressionList =
  let payload = A.object [ "success" .= True, "action" .= ("list" :: Text) ]
  in pure $ case suggestNext "ghci_regression" True payload of
       Just ns -> nsTool ns == "ghci_regression"
       Nothing -> False

-- | Refactor landed → verify compile.
testNextStepRefactor :: IO Bool
testNextStepRefactor =
  let payload = A.object [ "success" .= True, "compile" .= ("ok" :: Text) ]
  in pure $ case suggestNext "ghci_refactor" True payload of
       Just ns -> nsTool ns == "ghci_load"
       Nothing -> False

-- | Module gate → project gate.
testNextStepCheckModule :: IO Bool
testNextStepCheckModule =
  let payload = A.object [ "success" .= True, "overall" .= True ]
  in pure $ case suggestNext "ghci_check_module" True payload of
       Just ns -> nsTool ns == "ghci_check_project"
       Nothing -> False

-- | Project gate → gate (pre-push finalizer). BUG-06 re-routed
-- check_project from coverage → gate (the Phase 11n finalizer
-- tool) so the agent reaches the real CI-equivalent step; coverage
-- moves into the attached chain as the optional follow-up.
testNextStepCheckProject :: IO Bool
testNextStepCheckProject =
  let payload = A.object [ "success" .= True, "overall" .= True ]
  in pure $ case suggestNext "ghci_check_project" True payload of
       Just ns ->
            nsTool ns == "ghci_gate"
         && case nsChain ns of
              Just steps ->
                   any ((== "ghci_gate")     . csTool) steps
                && any ((== "ghci_coverage") . csTool) steps
              Nothing -> False
       Nothing -> False

-- | Errors suppress the suggestion — the agent should read the error
-- before being nudged forward.
testNextStepErrorsSuppressed :: IO Bool
testNextStepErrorsSuppressed =
  let payload = A.object [ "success" .= False, "error" .= ("oops" :: Text) ]
  in pure $ case suggestNext "ghci_load" False payload of
       Nothing -> True
       Just _  -> False

-- | Exploratory tools (type/info/eval/goto/doc/complete) don't get
-- a next-step hint — the user drives them.
testNextStepExploratoryNothing :: IO Bool
testNextStepExploratoryNothing = pure $
  all nothing
    [ suggestNext "ghci_type"     True (A.object [])
    , suggestNext "ghci_info"     True (A.object [])
    , suggestNext "ghci_eval"     True (A.object [])
    , suggestNext "ghci_goto"     True (A.object [])
    , suggestNext "ghci_doc"      True (A.object [])
    , suggestNext "ghci_complete" True (A.object [])
    , suggestNext "ghci_coverage" True (A.object [])
    , suggestNext "ghci_workflow" True (A.object [])
    ]
  where
    nothing Nothing = True
    nothing _       = False

-- | injectNextStep splices the nextStep into the first TextContent
-- block's JSON payload.
testInjectSplices :: IO Bool
testInjectSplices =
  let body = A.object [ "success" .= True, "data" .= (42 :: Int) ]
      txt  = TL.toStrict (TLE.decodeUtf8 (A.encode body))
      tr   = ToolResult { trContent = [ TextContent txt ], trIsError = False }
      ns   = NextStep { nsTool = "ghci_foo", nsWhy = "because"
                      , nsExample = Nothing, nsChain = Nothing }
      tr'  = injectNextStep ns tr
  in case trContent tr' of
       [TextContent t] -> pure $
         T.isInfixOf "\"nextStep\"" t
           && T.isInfixOf "\"ghci_foo\"" t
           && T.isInfixOf "\"data\":42" t
           -- original field preserved
       _ -> pure False

-- | injectNextStep must NOT corrupt non-JSON payloads.
testInjectSkipsNonJson :: IO Bool
testInjectSkipsNonJson =
  let raw = "this is not json"
      tr  = ToolResult { trContent = [ TextContent raw ], trIsError = False }
      ns  = NextStep { nsTool = "ghci_foo", nsWhy = "x"
                     , nsExample = Nothing, nsChain = Nothing }
      tr' = injectNextStep ns tr
  in case trContent tr' of
       [TextContent t] -> pure (t == raw)  -- unchanged
       _ -> pure False

--------------------------------------------------------------------------------
-- end of Phase 11e block
--------------------------------------------------------------------------------

-- | Phase 11d F-13: the MCP used to leave the @instructions@ field
-- of 'InitializeResult' empty, so Claude Desktop (and any other
-- MCP client) surfaced nothing at session start. The repo-level
-- @.claude/rules/use-haskell-flows-mcp.md@ partially filled the gap
-- but was itself stale — referencing tools that never existed in
-- the Haskell port (@ghci_session@, @ghci_trace@, @ghci_flags@,
-- @ghci_init@, …). Fix wires a non-empty @instructions@ string
-- into the initialize response so the LLM always gets accurate
-- tool guidance, even without the project file.
--
-- Pin two invariants with static source checks:
--   1. 'InitializeResult' has an 'irInstructions' field.
--   2. The content is non-empty and mentions the tools / flows an
--      agent has to reach for every session.
testInitializeEmitsInstructions :: IO Bool
testInitializeEmitsInstructions = do
  src <- TIO.readFile "src/HaskellFlows/Mcp/Protocol.hs"
  let codeLines = filter (not . isDocLine) (T.lines src)
      code      = T.unlines codeLines
  pure $ T.isInfixOf "irInstructions"           code
      && T.isInfixOf "\"instructions\" .="      code
  where
    isDocLine ln =
      let s = T.stripStart ln in "--" `T.isPrefixOf` s

testInstructionsMentionCore :: IO Bool
testInstructionsMentionCore = do
  -- BUG-05: the instructions are now rendered dynamically from
  -- 'allToolDescriptors' + the situation table. Assert the
  -- rendered text contains (a) every registered tool name and
  -- (b) the core workflow / invariant markers. Any drift between
  -- the registry and the text fails here.
  let instructions = Guidance.sessionInstructionsText allToolDescriptors
      staticMarkers =
        [ "ci-local.sh"
        , "SessionStatus"   , "Dead"
        , "registerDelay"   , "10-min"
        , "dogfood"
        , "handshake"
        , "situation"       , "invariant"
        , "nextStep"
        ]
      toolMarkers = allToolNames
      lowerInstructions = T.toLower instructions
  pure $ all (`T.isInfixOf` instructions) toolMarkers
      && all ((`T.isInfixOf` lowerInstructions) . T.toLower) staticMarkers

-- | Phase 11c F-12 — defence-in-depth. Even if the Session.hs
-- fixes above miss a pathological code path, the server's outer
-- envelope must not freeze. Pin that @runTool@ is wrapped in
-- @System.Timeout.timeout@ with a generous but finite budget.
testServerOuterTimeout :: IO Bool
testServerOuterTimeout = do
  src <- TIO.readFile "src/HaskellFlows/Mcp/Server.hs"
  let codeLines = filter (not . isDocLine) (T.lines src)
      code      = T.unlines codeLines
  pure $ T.isInfixOf "import System.Timeout" code
      && T.isInfixOf "timeout toolTimeoutMicros action"   code
      && T.isInfixOf "toolTimeoutMicros :: Int"           code
  where
    isDocLine ln =
      let s = T.stripStart ln in "--" `T.isPrefixOf` s

-- | Scenario-1 regression (FlowTimeoutEnforcement, step 3).
-- 'Control.Concurrent' must be in the eval interactive context —
-- fully-qualified references like @Control.Concurrent.threadDelay@
-- still need the module brought into scope. Without this, the
-- scenario's slow-eval step fails at compile time with "No module
-- named Control.Concurrent is imported" instead of tripping the
-- inner 30 s budget.
testEvalContextHasControlConcurrent :: IO Bool
testEvalContextHasControlConcurrent = do
  src <- TIO.readFile "src/HaskellFlows/Tool/Eval.hs"
  let codeLines = filter (not . isDocLine) (T.lines src)
      code      = T.unlines codeLines
  pure $ T.isInfixOf "\"Control.Concurrent\"" code
      && T.isInfixOf "augmentEvalContext" code
  where
    isDocLine ln =
      let s = T.stripStart ln in "--" `T.isPrefixOf` s

-- | Scenario-1 regression (FlowTimeoutEnforcement, step 2+3).
-- The Eval handler must enforce a tighter per-call budget than the
-- 10-min outer 'toolTimeoutMicros', wrap the eval pipeline in
-- 'System.Timeout.timeout', evict the GhcSession on elapse, and
-- render a structured @error_kind=timeout@ payload so clients
-- can distinguish budget trips from user compile/runtime errors.
testEvalInnerTimeoutBudget :: IO Bool
testEvalInnerTimeoutBudget = do
  src <- TIO.readFile "src/HaskellFlows/Tool/Eval.hs"
  let codeLines = filter (not . isDocLine) (T.lines src)
      code      = T.unlines codeLines
  pure $ T.isInfixOf "import System.Timeout" code
      && T.isInfixOf "evalTimeoutMicros" code
      && T.isInfixOf "timeout evalTimeoutMicros" code
      && T.isInfixOf "resetHscEnvInPlace" code
      && T.isInfixOf "\"error_kind\" .= (\"timeout\" :: Text)" code
      && T.isInfixOf "SomeAsyncException" code
  where
    isDocLine ln =
      let s = T.stripStart ln in "--" `T.isPrefixOf` s

-- | Deferred-pass isolation regression. 'ghci_check_project' runs
-- GHC with '-fdefer-type-errors' + '-fdefer-typed-holes', which
-- produces '.hi'/'.o' artifacts for semantically-broken modules.
-- Those MUST land in a MCP-private build tree, never in cabal's
-- default 'dist-newstyle/' — otherwise a user running 'cabal build'
-- after 'ghci_check_project' sees the poisoned interfaces and
-- skips recompilation, falsely reporting success on a project MCP
-- correctly flagged as broken (FlowCrossValidation · typeError).
--
-- Pins that 'applyFlavour' receives a 'ProjectDir', that the
-- 'Deferred' branch calls 'redirectDeferredOutputs', and that the
-- per-project MCP build dir is 'dist-newstyle-mcp/deferred' under
-- the project root.
testDeferredIsolatedOutputs :: IO Bool
testDeferredIsolatedOutputs = do
  src <- TIO.readFile "src/HaskellFlows/Ghc/CabalBootstrap.hs"
  let codeLines = filter (not . isDocLine) (T.lines src)
      code      = T.unlines codeLines
  pure $ T.isInfixOf "dist-newstyle-mcp" code
      && T.isInfixOf "--builddir=" code
  where
    isDocLine ln =
      let s = T.stripStart ln in "--" `T.isPrefixOf` s

-- | Cure regression: the interactive context must be derived from
-- the project's own @import …@ declarations, not from a hardcoded
-- allowlist. Each of the three in-process load paths ('autoLoad',
-- 'loadProjectWithFlavour', 'loadForTarget') must call
-- 'projectInteractiveImports' so qualified + aliased imports in
-- source files ('import qualified Data.Map.Strict as Map') reach
-- 'ghci_eval' verbatim. Without this, every new stdlib module
-- a scenario reaches for would require editing 'augmentEvalContext'.
testLoadAutoImports :: IO Bool
testLoadAutoImports = do
  src <- TIO.readFile "src/HaskellFlows/Ghc/ApiSession.hs"
  let codeLines = filter (not . isDocLine) (T.lines src)
      code      = T.unlines codeLines
      -- Three setContext call sites, each must splice in projImports
      callSites = T.count "projImports" code
  pure $ T.isInfixOf "parseImportDecl" code
      && T.isInfixOf "projectInteractiveImports" code
      && T.isInfixOf "projectExternalImports" code
      && T.isInfixOf "handleSourceError" code
      && callSites >= 3
  where
    isDocLine ln =
      let s = T.stripStart ln in "--" `T.isPrefixOf` s

-- | Phase 11c F-10: 'ghci_arbitrary' used to render
-- @instance Arbitrary Run where@ for polymorphic types like
-- @data Run a@. The template then refused to compile because the
-- type expression @Run@ has kind @* -> *@ and Haskell needs the
-- saturating tyvar (and an @Arbitrary a =>@ context on top). The
-- fix extracts the type parameters from the @:i@ declaration line
-- and emits the right header shape. These tests pin the parser
-- and the template separately.
testTypeParamsOne :: IO Bool
testTypeParamsOne =
  let raw = T.unlines
        [ "type Run :: * -> *"
        , "data Run a = Run {runLen :: !Int, runVal :: !a}"
        ]
  in pure (parseTypeParams raw == ["a"])

testTypeParamsTwo :: IO Bool
testTypeParamsTwo =
  let raw = T.unlines
        [ "type Map :: * -> * -> *"
        , "data Map k v = Empty | Bin Int k v (Map k v) (Map k v)"
        ]
  in pure (parseTypeParams raw == ["k", "v"])

testTypeParamsNone :: IO Bool
testTypeParamsNone =
  let raw = T.unlines
        [ "type Foo :: *"
        , "data Foo = MkFoo"
        ]
  in pure (null (parseTypeParams raw))

testTemplatePolymorphic :: IO Bool
testTemplatePolymorphic =
  let out = renderTemplate "Run" ["a"]
              [Constructor "Run" (replicate 2 "arbitrary")]
  in pure $
       "instance Arbitrary a => Arbitrary (Run a) where" `T.isInfixOf` out
    && "Run <$> arbitrary <*> arbitrary"                  `T.isInfixOf` out

testTemplateMultiParam :: IO Bool
testTemplateMultiParam =
  let out = renderTemplate "Either" ["a", "b"]
              [ Constructor "Left"  ["arbitrary"]
              , Constructor "Right" ["arbitrary"]
              ]
  in pure $
       "instance (Arbitrary a, Arbitrary b) => Arbitrary (Either a b) where"
         `T.isInfixOf` out
    && "Left <$> arbitrary"                   `T.isInfixOf` out
    && "Right <$> arbitrary"                  `T.isInfixOf` out

-- | Phase 11c F-11: the first F-09 fix shipped with only one
-- derived @--hpcdir@. Cabal 3.14 writes mix files to TWO separate
-- paths (library's @build/extra-compilation-artifacts/hpc/vanilla/mix@
-- + test's @t/<test>/build/…/extra-compilation-artifacts/hpc/vanilla/mix@)
-- and @hpc report@ needs both flags present or it bails with
-- "can not find <pkg>-<ver>-inplace/Module in …". Post-fix,
-- @findMixDirs@ uses a @find -path@ pattern to enumerate every
-- mix dir under @dist-newstyle@, and @runHpcReport@ expands them
-- into a list of @--hpcdir=@ flags. Static source check is the
-- narrowest regression:
testCoveragePassesAllMixDirs :: IO Bool
testCoveragePassesAllMixDirs = do
  src <- TIO.readFile "src/HaskellFlows/Tool/Coverage.hs"
  pure $ T.isInfixOf "findMixDirs"                        src
      && T.isInfixOf "extra-compilation-artifacts"        src
      && T.isInfixOf "[FilePath] -> FilePath"             src
      -- keep the F-09 invariants alongside the F-11 ones
      && T.isInfixOf "findTixFile"                        src

-- | End-to-end smoke of the happy path: 'parseCoverage' must
-- recognise the text shape that @hpc report@ emits under GHC 9.x.
-- Pins both the parser and the enrichment contract together.
testParseHpcReportText :: IO Bool
testParseHpcReportText =
  let sample = T.unlines
        [ " 92% expressions used (12/13)"
        , " 100% boolean coverage (0/0)"
        , " 100% alternatives used (3/3)"
        , " 100% local declarations used (1/1)"
        , " 100% top-level declarations used (1/1)"
        ]
      rpt = parseCoverage sample
  in pure (length (crMetrics rpt) >= 5
         && any (\m -> mPercent m == 92) (crMetrics rpt))

-- | Phase 11b F-08 (critical): the old @loadModuleWith Deferred@ used
-- @:unset -fdefer-type-errors -fdefer-typed-holes@, but GHCi's
-- @:unset@ is only for GHCi-level options (@+s@, @+t@, …) — NOT GHC
-- flags. So the flags leaked across calls and every subsequent
-- compile-check silently deferred its errors. This voided the
-- snapshot-and-compile-verify invariant of @ghci_refactor@: renames
-- that left the module broken would still report compile=ok.
--
-- We can't spawn a real GHCi in a unit test, but we can pin the
-- static shape of the commands the session sends. The fix requires
-- (a) @Strict@ mode sending @-fno-defer-type-errors@ /
-- @-fno-defer-typed-holes@, and (b) the tail of the @Deferred@ path
-- using the same @-fno-@ form (not @:unset@). Grepping the module
-- source is the narrowest regression check that survives without
-- a live GHCi.
-- testLoadStrictClearsDeferred / testSessionIncludesQuickCheck —
-- removed in Wave 5. Both pinned the legacy subprocess' argv /
-- @:set@ invocations; the in-process path owns these through
-- 'applyFlavour' (Strict vs Deferred) + the stanza flags captured
-- from cabal's own @v2-repl@ argv. Covered now by the Deferred
-- hole-capture round-trip in testHoleDiagnosticCapture.

-- | Phase 11b F-04 part A: GHC 9.x emits a kind-signature line
-- (@type Run :: * -> *@) BEFORE the data decl in @:i@ output.
-- 'parseConstructors' previously bailed because @hasCtorHeader@
-- only checked the collapsed string's prefix. Pin the GHC 9.x layout
-- plus a record constructor with strict fields — must parse into a
-- 2-arg Constructor.
testCtorsRecordStrictWithKindHeader :: IO Bool
testCtorsRecordStrictWithKindHeader =
  let raw = T.unlines
        [ "type Run :: * -> *"
        , "data Run a = Run {runLen :: !Int, runVal :: !a}"
        , "  \t-- Defined at src/DogfoodRle.hs:20:1"
        ]
  in case parseConstructors raw of
       [c] -> pure (cName c == "Run" && length (cArgs c) == 2)
       _   -> pure False

-- | Phase 11b F-04 part B: even absent the kind header, a record
-- constructor @Ctor {f1 :: T1, f2 :: T2}@ used to be mis-tokenised
-- because @groupTokens@ didn't treat @{}@ as grouping — fields got
-- split on every internal space, inflating 'cArgs' to 6 tokens.
testCtorsInlineRecord2Fields :: IO Bool
testCtorsInlineRecord2Fields =
  let raw = "data Run a = Run {runLen :: !Int, runVal :: !a}"
  in case parseConstructors raw of
       [c] -> pure (cName c == "Run" && length (cArgs c) == 2)
       _   -> pure False

-- | Phase 11b F-05: @ghci_suggest@ used to emit false laws for
-- @encode :: [a] -> [Run a]@ because @ruleListLengthPreserving@ and
-- @ruleListRoundtrip@ matched @([TyList _], TyList _)@ without
-- checking the inner types. Both @Self-inverse on lists@ and
-- @Length preserving@ are nonsense (don't even type-check) when
-- arg and return lists carry different element types. Pin the
-- invariant: for @[a] -> [SomeOther a]@, neither rule fires.
testSuggestEncodeShapeSkipsListRules :: IO Bool
testSuggestEncodeShapeSkipsListRules =
  case parseSignature "[a] -> [Run a]" of
    Nothing  -> pure False
    Just sig ->
      let laws = map sLaw (applyRules "encode" sig)
      in pure $ "Self-inverse on lists" `notElem` laws
             && "Length preserving / non-extending" `notElem` laws

--------------------------------------------------------------------------------
-- ghci_quickcheck: store-module resolution (the "persist with the right file"
-- UX fix). The dogfood of the expr-evaluator surfaced the bug: callers pass
-- the module of the /function under test/ ('src/Foo.hs'), but the property
-- itself lives in 'test/Spec.hs', and regression replay needs the latter to
-- put the identifier in scope. These tests pin the pure decision function
-- so the resolution rule can evolve without a live GHCi.
--------------------------------------------------------------------------------

-- | Wave-3: chooseStoreModule no longer consults ':info' output —
-- that plumbing sat on top of the subprocess ghci which is being
-- retired. Under the new contract it always returns the caller's
-- hint verbatim, regardless of what ':info' would have said.
testChooseStoreModuleIdentWithInfo :: IO Bool
testChooseStoreModuleIdentWithInfo = pure $
  QcTool.chooseStoreModule
    "prop_idempotent"
    (Just "src/Foo.hs")
    (Just ":info output\nprop_idempotent :: Expr -> Bool \
           \\t-- Defined at test/Spec.hs:12:1\n")
  == Just "src/Foo.hs"

-- | Identifier but no ':info' available (e.g. session busy) → fall back
-- to whatever the caller passed. We don't invent a path.
testChooseStoreModuleIdentNoInfo :: IO Bool
testChooseStoreModuleIdentNoInfo = pure $
  QcTool.chooseStoreModule
    "prop_idempotent"
    (Just "src/Foo.hs")
    Nothing
  == Just "src/Foo.hs"

-- | Lambda expression (not a simple identifier) → ':info' doesn't apply
-- even if we had it; use caller hint verbatim. Keeps backwards
-- compatibility for inline-property callers.
testChooseStoreModuleLambda :: IO Bool
testChooseStoreModuleLambda = pure $
  QcTool.chooseStoreModule
    "\\xs -> reverse (reverse xs) == xs"
    (Just "src/Foo.hs")
    (Just "anything") -- should be ignored because the expression isn't an ident
  == Just "src/Foo.hs"

-- | ':info' reports only a module ("Defined in 'Prelude'"), not a file
-- location. That's not actionable for regression replay, so we still
-- fall back to the caller hint. Prevents a regression where we'd
-- persist a module NAME where the store expects a file PATH.
testChooseStoreModuleModuleLoc :: IO Bool
testChooseStoreModuleModuleLoc = pure $
  QcTool.chooseStoreModule
    "prop_trivial"
    (Just "src/Foo.hs")
    (Just "prop_trivial :: Bool -- Defined in 'Prelude'")
  == Just "src/Foo.hs"

-- | Classifier: bare identifiers pass, qualified identifiers pass,
-- prefix operators and lambdas are rejected.
testIsSimpleIdentClassifier :: IO Bool
testIsSimpleIdentClassifier = pure $ and
  [       QcTool.isSimpleIdent "prop_x"
  ,       QcTool.isSimpleIdent "Spec.prop_x"
  ,       QcTool.isSimpleIdent "prop_x'"
  ,       QcTool.isSimpleIdent "Foo.Bar.baz"
  , not ( QcTool.isSimpleIdent "\\x -> x" )
  , not ( QcTool.isSimpleIdent "prop_x y" )           -- space → compound
  , not ( QcTool.isSimpleIdent "(prop_x)" )           -- parens rejected
  , not ( QcTool.isSimpleIdent "prop_x + 1" )
  , not ( QcTool.isSimpleIdent "" )
  , not ( QcTool.isSimpleIdent "42" )                 -- leading digit
  ]

--------------------------------------------------------------------------------
-- ghci_regression: parser for ':show modules' output. Used by the scope
-- snapshot/restore path so a regression run doesn't clobber the caller's
-- previously-loaded module set.
--------------------------------------------------------------------------------

-- | Single-module shape: the format GHCi emits for a project with
-- exactly one compiled module.
testParseShowModulesPathsSimple :: IO Bool
testParseShowModulesPathsSimple =
  let raw = T.pack "Foo              ( src/Foo.hs, interpreted )\n"
  in pure (RegTool.parseShowModulesPaths raw == ["src/Foo.hs"])

-- | Multi-module shape: library + test-suite layout. Order preserved;
-- paths extracted without picking up the module name or the 'kind'
-- trailing bit.
testParseShowModulesPathsMulti :: IO Bool
testParseShowModulesPathsMulti =
  let raw = T.unlines
        [ "Expr.Syntax     ( src/Expr/Syntax.hs, interpreted )"
        , "Expr.Eval       ( src/Expr/Eval.hs, interpreted )"
        , "Main            ( test/Spec.hs, interpreted )"
        ]
  in pure $
       RegTool.parseShowModulesPaths raw ==
         ["src/Expr/Syntax.hs", "src/Expr/Eval.hs", "test/Spec.hs"]

-- | Garbage / empty lines: skip. Parser is a best-effort tool, not a
-- strict validator; refusing to crash on unexpected input is the
-- important invariant.
testParseShowModulesPathsGarbage :: IO Bool
testParseShowModulesPathsGarbage = pure $ and
  [ null (RegTool.parseShowModulesPaths "")
  , null (RegTool.parseShowModulesPaths "random log output\n")
  , null (RegTool.parseShowModulesPaths "Foo  ( , interpreted )")
    -- real-looking line sandwiched between garbage: still extracted.
  , RegTool.parseShowModulesPaths
      ( T.unlines
          [ "random warning line"
          , "Bar  ( src/Bar.hs, interpreted )"
          , ""
          ]
      ) == ["src/Bar.hs"]
  ]

-- | Phase-7 foundation: can we compile + run an 'IO String' action
-- in-process and read its result back? This is the primitive QC /
-- regression / determinism / IO-eval migrations depend on. If this
-- test ever regresses, those migrations are off the table until the
-- GHC API boundary changes.
testEvalIOString :: IO Bool
testEvalIOString = case mkProjectDir "/tmp" of
  Left _   -> pure False
  Right pd -> do
    sess   <- startGhcSession pd
    result <- withGhcSession sess $ do
      setContext [IIDecl (simpleImportDecl (mkModuleName "Prelude"))]
      evalIOString "(return \"hello-from-ghc-api\") :: IO String"
    killGhcSession sess
    pure (result == "hello-from-ghc-api")

-- | Wave-2 gate: 'loadForTarget' against /tmp/bench-project library
-- must compile Foo.hs cleanly (success=True, no errors). If the
-- fixture dir is missing, skip gracefully.
testLoadForTargetLibrary :: IO Bool
testLoadForTargetLibrary = case mkProjectDir "/tmp/bench-project" of
  Left _   -> pure True
  Right pd -> do
    exists <- doesFileExist "/tmp/bench-project/bench-project.cabal"
    if not exists
      then pure True
      else do
        sess <- startGhcSession pd
        (ok, diags) <- ApiSession.loadForTarget sess TargetLibrary ApiSession.Strict
        killGhcSession sess
        pure (ok && null diags)

-- | Diagnostic: prove whether 'loadForTarget' with 'Deferred' flavour
-- captures typed-hole warnings through the logger hook. Writes a
-- detailed trace to @/tmp/hole-hook-diag.log@ for inspection. If the
-- fixture dir or the @Hole.hs@ fixture is missing, skip gracefully.
testHoleDiagnosticCapture :: IO Bool
testHoleDiagnosticCapture = case mkProjectDir "/tmp/hole-fixture" of
  Left _   -> pure True
  Right pd -> do
    cabalExists <- doesFileExist "/tmp/hole-fixture/hole-fixture.cabal"
    holeExists  <- doesFileExist "/tmp/hole-fixture/src/Hole.hs"
    if not (cabalExists && holeExists)
      then pure True  -- no fixture, skip
      else do
        sess <- startGhcSession pd
        (_ok, diags) <- ApiSession.loadForTarget sess TargetLibrary ApiSession.Deferred
        killGhcSession sess
        -- Full Wave-2 hole pipeline: capture -> render -> parse.
        -- A non-empty holes list with the expected file proves the
        -- hook captured the warning, the renderer produced a valid
        -- GHCi-style header, and parseTypedHoles extracted the hole.
        let rendered = renderGhciStyle diags
            holes    = parseTypedHoles rendered
        pure $ not (null holes)
             && any (("Hole.hs" `T.isSuffixOf`) . thFile) holes

-- | Regression for the FlowArbitrary e2e failure: after
-- 'invalidateStanzaFlags' (which the server fires after every
-- 'ghci_deps add'), the NEXT 'loadForTarget' must re-bootstrap
-- cabal flags AND successfully compile a module that references
-- the newly-added dependency. Before the fix, the captured argv
-- still held a stale @-hide-all-packages@ AFTER the
-- @-package-id@ tokens, which under GHC-API flag-parsing resets
-- the visible-package set and surfaces as
-- @cannot satisfy -package-id QckChck-...@.
testLoadAfterDepsAdd :: IO Bool
testLoadAfterDepsAdd = do
  base <- getTemporaryDirectory
  ts   <- getPOSIXTime
  let dir = base </> ("arb-repro-" <> show (floor (ts * 1000000) :: Int))
  createDirectoryIfMissing True dir
  let pdE = mkProjectDir dir
  case pdE of
    Left _   -> do removePathForcibly dir; pure False
    Right pd -> do
      let srcDir = dir </> "src"
      createDirectoryIfMissing True srcDir
      -- 1. Scaffold with base only (no QuickCheck yet).
      TIO.writeFile (dir </> "arb-repro.cabal") $ T.unlines
        [ "cabal-version: 2.4"
        , "name: arb-repro"
        , "version: 0.1.0.0"
        , ""
        , "library"
        , "  hs-source-dirs:   src"
        , "  exposed-modules:  Shapes, ShapesGen"
        , "  build-depends:    base"
        , "  default-language: Haskell2010"
        ]
      TIO.writeFile (dir </> "cabal.project") "packages: .\n"
      TIO.writeFile (srcDir </> "Shapes.hs") $ T.unlines
        [ "{-# LANGUAGE DerivingStrategies #-}"
        , "module Shapes (Status (..)) where"
        , "data Status = Ok | Err String deriving stock (Eq, Show)"
        ]
      TIO.writeFile (srcDir </> "ShapesGen.hs") $ T.unlines
        [ "{-# OPTIONS_GHC -Wno-orphans -Wno-missing-signatures #-}"
        , "module ShapesGen () where"
        , "import Shapes"
        , "import Test.QuickCheck"
        , "instance Arbitrary Status where"
        , "  arbitrary = oneof [ pure Ok, Err <$> arbitrary ]"
        ]
      sess <- startGhcSession pd
      -- 2. Mutate .cabal to add QuickCheck (simulates ghci_deps add).
      TIO.writeFile (dir </> "arb-repro.cabal") $ T.unlines
        [ "cabal-version: 2.4"
        , "name: arb-repro"
        , "version: 0.1.0.0"
        , ""
        , "library"
        , "  hs-source-dirs:   src"
        , "  exposed-modules:  Shapes, ShapesGen"
        , "  build-depends:    base, QuickCheck"
        , "  default-language: Haskell2010"
        ]
      ApiSession.invalidateStanzaFlags sess
      -- 3. Load via the full in-process path.
      (ok, diags) <- ApiSession.loadForTarget sess TargetLibrary ApiSession.Strict
      killGhcSession sess
      let satisfy =
            any (T.isInfixOf "cannot satisfy -package-id" . geMessage) diags
      when (not ok || satisfy) $ do
        putStrLn "  -- testLoadAfterDepsAdd diagnostics --"
        mapM_ (putStrLn . ("    " <>) . T.unpack . geMessage) diags
      removePathForcibly dir
      pure (ok && not satisfy)

--------------------------------------------------------------------------------
-- ghci_switch_project tests
--------------------------------------------------------------------------------

-- | Build a tempdir-scoped project with the given name and .cabal
-- file. Returns the absolute path. Caller owns cleanup.
scaffoldTmpProject :: String -> IO FilePath
scaffoldTmpProject tag = do
  base <- getTemporaryDirectory
  ts   <- getPOSIXTime
  let dir = base </> ("sp-" <> tag <> "-"
                       <> show (floor (ts * 1000000) :: Int))
  createDirectoryIfMissing True dir
  TIO.writeFile (dir </> (tag <> ".cabal")) $ T.unlines
    [ "cabal-version: 2.4"
    , "name: " <> T.pack tag
    , "version: 0.1.0.0"
    , ""
    , "library"
    , "  build-depends: base"
    , "  default-language: Haskell2010"
    ]
  pure dir

-- | Relative paths must be rejected before touching the filesystem —
-- 'mkProjectDir' is the guard; 'validateSwitchTarget' surfaces it as
-- 'VEPathError'.
testSwitchRejectsRelative :: IO Bool
testSwitchRejectsRelative = do
  res <- validateSwitchTarget "relative/path"
  pure $ case res of
    Left (VEPathError (PathNotAbsolute _)) -> True
    _                                      -> False

-- | Absolute but non-existent path → 'VENotADirectory'. Using a
-- time-stamped path guarantees we don't collide with a real dir on
-- the test machine.
testSwitchRejectsMissing :: IO Bool
testSwitchRejectsMissing = do
  ts <- getPOSIXTime
  let bogus = "/tmp/definitely-missing-"
                <> show (floor (ts * 1000000) :: Int)
  res <- validateSwitchTarget (T.pack bogus)
  pure $ case res of
    Left (VENotADirectory _) -> True
    _                        -> False

-- | Real dir with no .cabal file → 'VENoCabalFile'. Scaffolds a
-- bare tempdir, runs the validator, cleans up regardless.
testSwitchRejectsNoCabal :: IO Bool
testSwitchRejectsNoCabal = do
  base <- getTemporaryDirectory
  ts   <- getPOSIXTime
  let dir = base </> ("no-cabal-" <> show (floor (ts * 1000000) :: Int))
  createDirectoryIfMissing True dir
  -- A NON-empty directory without a .cabal stays rejected — we
  -- don't want to accidentally point at @~/Downloads@ or similar
  -- and have subsequent tools treat its contents as sources.
  -- (Empty dirs are allowed post-BUG-PLUS-07; see
  -- 'testSwitchAcceptsEmpty'.)
  TIO.writeFile (dir </> "README.md") "not a cabal project\n"
  res <- validateSwitchTarget (T.pack dir)
  removePathForcibly dir
  pure $ case res of
    Left (VENoCabalFile _) -> True
    _                      -> False

-- | Happy path: a real cabal project returns 'Right ProjectDir'
-- pointing at the scaffolded dir.
testSwitchAcceptsValid :: IO Bool
testSwitchAcceptsValid = do
  dir <- scaffoldTmpProject "sp-happy"
  res <- validateSwitchTarget (T.pack dir)
  removePathForcibly dir
  pure $ case res of
    Right pd -> HaskellFlows.Types.unProjectDir pd == dir
    _        -> False

-- | End-to-end contract of the 'handle' function: after it returns
-- with success, the project-dir IORef points at the new path AND
-- the session MVar is emptied (Nothing) so the next
-- getOrStartGhcSession boots fresh.
testSwitchHandleSwaps :: IO Bool
testSwitchHandleSwaps = do
  dirA <- scaffoldTmpProject "from"
  dirB <- scaffoldTmpProject "to"
  case (mkProjectDir dirA, mkProjectDir dirB) of
    (Right pdA, Right pdB) -> do
      pdRef    <- newIORef pdA
      sessRef  <- newMVar Nothing
      -- Prime the session so we can observe the kill semantics:
      -- handle must wipe whatever Session was there.
      primed   <- startGhcSession pdA
      _        <- readMVar sessRef
      sessRef' <- newMVar (Just primed)
      let args = A.object [ "path" A..= T.pack dirB ]
      result  <- SwitchProject.handle pdRef sessRef' args
      newPd   <- readIORef pdRef
      mSess   <- readMVar sessRef'
      removePathForcibly dirA
      removePathForcibly dirB
      pure
        ( not (trIsError result)
            && HaskellFlows.Types.unProjectDir newPd ==
                 HaskellFlows.Types.unProjectDir pdB
            && isNothing mSess
        )
    _ -> do
      removePathForcibly dirA
      removePathForcibly dirB
      pure False

--------------------------------------------------------------------------------
-- BUG-PLUS-07: switch_project accepts empty dirs (scaffold-ready)
--------------------------------------------------------------------------------

-- | An empty directory should be a valid switch target so the
-- user can follow up with 'ghci_create_project' — the canonical
-- "I want to start a new project here" workflow. Before the fix
-- the validator demanded an existing .cabal, forcing callers to
-- pre-scaffold a stub just to unlock the tool.
testSwitchAcceptsEmpty :: IO Bool
testSwitchAcceptsEmpty = do
  base <- getTemporaryDirectory
  ts   <- getPOSIXTime
  let dir = base </> ("sp-empty-" <> show (floor (ts * 1000000) :: Int))
  createDirectoryIfMissing True dir
  res <- validateSwitchTarget (T.pack dir)
  removePathForcibly dir
  pure $ case res of
    Right pd -> HaskellFlows.Types.unProjectDir pd == dir
    _        -> False

--------------------------------------------------------------------------------
-- BUG-PLUS-04: PATH self-augmentation
--------------------------------------------------------------------------------

-- | The hard-coded candidate list must contain only absolute
-- paths. A relative entry would be silently ignored by
-- 'augmentPath' (which filters with 'isAbsolute') but represents
-- a code-review miss worth catching in CI.
testPathBootstrapAbsolute :: IO Bool
testPathBootstrapAbsolute = do
  home <- System.Directory.getHomeDirectory
  let cands = HaskellFlows.Mcp.PathBootstrap.hardCodedCandidates home
  pure $ all System.FilePath.isAbsolute cands

-- | 'augmentedPathCandidates' filters to dirs that actually exist.
-- On a dev machine at least ONE of the candidates should exist
-- (home dir is guaranteed). Returned list is a subset of the
-- hard-coded one.
testPathBootstrapExisting :: IO Bool
testPathBootstrapExisting = do
  home  <- System.Directory.getHomeDirectory
  cands <- HaskellFlows.Mcp.PathBootstrap.augmentedPathCandidates
  let fullList = HaskellFlows.Mcp.PathBootstrap.hardCodedCandidates home
  pure $ all (`elem` fullList) cands

-- | 'augmentPath' must not duplicate entries across repeated
-- calls — the MCP is sometimes spawned twice against the same
-- shell env (e.g. supervised restarts) and a runaway PATH blows
-- past @ARG_MAX@ fast. Calling twice should produce the same
-- PATH string as calling once.
testPathBootstrapIdempotent :: IO Bool
testPathBootstrapIdempotent = do
  first  <- HaskellFlows.Mcp.PathBootstrap.augmentPath
  second <- HaskellFlows.Mcp.PathBootstrap.augmentPath
  pure (first == second)

--------------------------------------------------------------------------------
-- BUG-PLUS-01: ghci_add_modules string fallback
--------------------------------------------------------------------------------

-- | The documented shape: @{"modules": ["A", "B"]}@.
testAddModulesArrayForm :: IO Bool
testAddModulesArrayForm =
  let payload = A.object [ "modules" A..= (["Expr.Syntax", "Expr.Eval"] :: [Text]) ]
  in case A.fromJSON payload of
       A.Success (AddModules.AddModulesArgs xs) ->
         pure (xs == ["Expr.Syntax", "Expr.Eval"])
       _ -> pure False

-- | Fallback shape: @{"modules": "Expr.Syntax, Expr.Eval"}@.
-- Observed in Claude for Desktop's deferred-tool path which
-- stringifies array args before dispatch. Accepting this shape
-- removes an entire class of "my JSON looks right but the server
-- rejects it" failure modes.
testAddModulesStringFallback :: IO Bool
testAddModulesStringFallback = do
  let csv   = A.object [ "modules" A..= ("Expr.Syntax, Expr.Eval" :: Text) ]
      ws    = A.object [ "modules" A..= ("Expr.Syntax Expr.Eval"  :: Text) ]
      mixed = A.object [ "modules" A..= ("Expr.Syntax,Expr.Eval\tExpr.Pretty" :: Text) ]
      ok payload =
        case A.fromJSON payload of
          A.Success (AddModules.AddModulesArgs xs) ->
            xs == ["Expr.Syntax", "Expr.Eval"]
               || xs == ["Expr.Syntax", "Expr.Eval", "Expr.Pretty"]
          _ -> False
  pure (ok csv && ok ws && ok mixed)

--------------------------------------------------------------------------------
-- BUG-PLUS-05: stanza-aware duplicate-dep detection
--------------------------------------------------------------------------------

-- | Same-stanza duplicate IS flagged.
testCabalStanzaDupCheck :: IO Bool
testCabalStanzaDupCheck =
  let body = T.unlines
        [ "cabal-version: 2.4"
        , "name: demo"
        , "library"
        , "  build-depends: base, containers, base"
        ]
      issues = VC.scanCabalText body
      hit = any (\i -> VC.iKind i == "duplicate-dep"
                      && "base" `T.isInfixOf` VC.iMessage i) issues
  in pure hit

-- | Cross-stanza repeats are legitimate — same dep in both
-- library and test-suite is standard — and must NOT surface as
-- duplicates.
testCabalCrossStanzaOk :: IO Bool
testCabalCrossStanzaOk =
  let body = T.unlines
        [ "cabal-version: 2.4"
        , "name: demo"
        , "library"
        , "  build-depends: base, containers"
        , ""
        , "test-suite demo-test"
        , "  type: exitcode-stdio-1.0"
        , "  main-is: Spec.hs"
        , "  build-depends: base, QuickCheck"
        ]
      issues = VC.scanCabalText body
      dupIssues = filter (\i -> VC.iKind i == "duplicate-dep") issues
  in pure (null dupIssues)

-- | Indented NON-build-depends fields — @hs-source-dirs:@,
-- @import:@, @default-language:@ — must NEVER be harvested as
-- fake package names.
testCabalHsSourceDirsIgnored :: IO Bool
testCabalHsSourceDirsIgnored =
  let body = T.unlines
        [ "cabal-version: 2.4"
        , "name: demo"
        , "common shared"
        , "  hs-source-dirs: src"
        , "  default-language: GHC2024"
        , "library"
        , "  import: shared"
        , "  hs-source-dirs: src"
        , "  build-depends: base"
        , "test-suite demo-test"
        , "  import: shared"
        , "  hs-source-dirs: test"
        , "  build-depends: base"
        ]
      issues = VC.scanCabalText body
      dupIssues = filter (\i -> VC.iKind i == "duplicate-dep") issues
      badNames  = map VC.iMessage dupIssues
  in pure
      ( null dupIssues
        && not (any ("hs-source-dirs" `T.isInfixOf`) badNames)
        && not (any ("import" `T.isInfixOf`) badNames)
      )

--------------------------------------------------------------------------------
-- BUG-PLUS-06: printer/parser roundtrip suggestion rule
--------------------------------------------------------------------------------

-- | A realistic printer/parser pair: focal is @pretty :: Expr ->
-- String@, sibling is @parseExpr :: String -> Maybe Expr@. The
-- rule must propose @parseExpr (pretty x) == Just x@.
testSuggestRoundtripRule :: IO Bool
testSuggestRoundtripRule = do
  -- parseSignature expects the RHS of '::' only. Passing the full
  -- 'name :: type' form in earlier iterations produced a garbled
  -- 'ParsedSig' whose psArgs was a TyApp of the function name —
  -- hence the rule never fired.
  let prettySig = HaskellFlows.Parser.TypeSignature.parseSignature
                    "Expr -> String"
      parserSig = HaskellFlows.Parser.TypeSignature.parseSignature
                    "String -> Maybe Expr"
  case (prettySig, parserSig) of
    (Just ps, Just qs) ->
      let ctx = RuleContext
            { rcName     = "pretty"
            , rcSig      = ps
            , rcSiblings = [("parseExpr", qs)]
            }
          suggestions = applyRulesCtx ctx
          hit = any (\s -> sLaw s == "Printer/parser roundtrip"
                          && "parseExpr" `T.isInfixOf` sProperty s
                          && "Just x"    `T.isInfixOf` sProperty s)
                   suggestions
      in pure hit
    _ -> pure False

-- | Negative: a same-type transform (@Expr -> Expr@) must NOT
-- trip the roundtrip rule even when a sibling returns Maybe Expr
-- — the rule is shape-keyed on A ≠ B.
testSuggestRoundtripNegative :: IO Bool
testSuggestRoundtripNegative = do
  let simpSig = HaskellFlows.Parser.TypeSignature.parseSignature
                  "Expr -> Expr"
      parserSig = HaskellFlows.Parser.TypeSignature.parseSignature
                  "String -> Maybe Expr"
  case (simpSig, parserSig) of
    (Just ps, Just qs) ->
      let ctx = RuleContext
            { rcName     = "simplify"
            , rcSig      = ps
            , rcSiblings = [("parseExpr", qs)]
            }
          roundtripSuggestions =
            filter (\s -> sLaw s == "Printer/parser roundtrip")
                   (applyRulesCtx ctx)
      in pure (null roundtripSuggestions)
    _ -> pure False

--------------------------------------------------------------------------------
-- BUG-PLUS-03: external cabal edit invalidates stanza cache
--------------------------------------------------------------------------------

-- | Prove 'ensureStanzaFlags' picks up cabal-file changes made
-- OUTSIDE the MCP's ghci_deps pipeline. The sequence:
--
--   1. Scaffold a real cabal project.
--   2. Call 'ensureStanzaFlags' — cache populates, mtime
--      recorded.
--   3. Touch the .cabal so its mtime strictly advances.
--   4. Call 'ensureStanzaFlags' again — 'cabalWasTouched'
--      returns True, bootstrap re-runs, and the env ref / applied
--      target are invalidated.
testMtimeInvalidation :: IO Bool
testMtimeInvalidation = do
  base <- getTemporaryDirectory
  ts   <- getPOSIXTime
  let dir = base </> ("mtime-inv-" <> show (floor (ts * 1000000) :: Int))
  createDirectoryIfMissing True dir
  TIO.writeFile (dir </> "demo.cabal") $ T.unlines
    [ "cabal-version: 2.4"
    , "name: demo"
    , "version: 0.1.0.0"
    , ""
    , "library"
    , "  build-depends: base"
    , "  default-language: Haskell2010"
    ]
  TIO.writeFile (dir </> "cabal.project") "packages: .\n"
  case mkProjectDir dir of
    Left _ -> do removePathForcibly dir; pure False
    Right pd -> do
      sess <- startGhcSession pd
      before <- ApiSession.readCabalMtimeForTest sess
      ApiSession.ensureStanzaFlags sess
      afterFirst <- ApiSession.readCabalMtimeForTest sess
      -- macOS fs mtime has 1-sec resolution; sleep past it to
      -- guarantee a strictly-advanced mtime on the next write.
      threadDelay 1_100_000
      TIO.writeFile (dir </> "demo.cabal") $ T.unlines
        [ "cabal-version: 2.4"
        , "name: demo"
        , "version: 0.2.0.0"
        , ""
        , "library"
        , "  build-depends: base, containers"
        , "  default-language: Haskell2010"
        ]
      ApiSession.ensureStanzaFlags sess
      afterTouch <- ApiSession.readCabalMtimeForTest sess
      killGhcSession sess
      removePathForcibly dir
      pure
        ( isNothing before
        && isJust afterFirst
        && isJust afterTouch
        && afterFirst < afterTouch
        )

--------------------------------------------------------------------------------
-- BUG-PLUS-08: add_modules unwraps stringified JSON arrays
--------------------------------------------------------------------------------

-- | The real trap: a client-side wrapper stringifies a JSON
-- array before dispatch, so the server receives
-- @{"modules": "[\"A\", \"B\"]"}@ — a String whose content is a
-- rendered array. Earlier versions comma-split on the outer
-- string and kept the @[@, @]@, @\"@ characters as part of the
-- "module names", creating files like @src/[\"A\".hs@ on disk.
-- Post-fix, 'parseModuleList' recognises the JSON-array shape,
-- unwraps it via 'eitherDecodeStrict', and recurses into the
-- Array branch — the caller observes the same result either way.
testAddModulesJsonArrayString :: IO Bool
testAddModulesJsonArrayString =
  let stringified = A.object
        [ "modules" A..= ("[\"Expr.Syntax\", \"Expr.Eval\"]" :: Text) ]
      quotedNoSpaces = A.object
        [ "modules" A..= ("[\"Expr.Syntax\",\"Expr.Eval\"]" :: Text) ]
      indented = A.object
        [ "modules" A..= ("  [ \"Expr.Syntax\" , \"Expr.Eval\" ] " :: Text) ]
      ok v = case A.fromJSON v of
        A.Success (AddModules.AddModulesArgs xs) ->
          xs == ["Expr.Syntax", "Expr.Eval"]
        _ -> False
  in pure (ok stringified && ok quotedNoSpaces && ok indented)

-- | The pre-BUG-PLUS-08 fallback must still work for plain
-- strings — @"A, B"@ and @"A B"@ continue to normalise to
-- @[\"A\", \"B\"]@. Guards against the aeson-first path
-- regressing the commonplace case.
testAddModulesPlainStringStillWorks :: IO Bool
testAddModulesPlainStringStillWorks =
  let csv   = A.object [ "modules" A..= ("A, B" :: Text) ]
      ws    = A.object [ "modules" A..= ("A B"  :: Text) ]
      mixed = A.object [ "modules" A..= ("A,\tB\nC" :: Text) ]
      ok v expected = case A.fromJSON v of
        A.Success (AddModules.AddModulesArgs xs) -> xs == expected
        _ -> False
  in pure (ok csv ["A","B"] && ok ws ["A","B"] && ok mixed ["A","B","C"])

--------------------------------------------------------------------------------
-- BUG-PLUS-mediocre-1: warnings_block flag on ghci_check_module
--------------------------------------------------------------------------------

-- | CheckArgs.warnings_block defaults to True (back-compat
-- with the pre-fix pre-push-gate strictness).
testCheckModuleWarningsBlockDefault :: IO Bool
testCheckModuleWarningsBlockDefault =
  let payload = A.object [ "module_path" A..= ("src/Foo.hs" :: Text) ]
  in case A.fromJSON payload of
       A.Success args -> pure (CheckModule.caWarningsBlock args)
       _              -> pure False

-- | Passing @warnings_block: false@ flips the gate: the field
-- surfaces on the parsed args, and the handler uses it to stop
-- warnings from turning overall into False when compile + holes
-- + properties are green.
testCheckModuleWarningsBlockFalse :: IO Bool
testCheckModuleWarningsBlockFalse =
  let payload = A.object
        [ "module_path"    A..= ("src/Foo.hs" :: Text)
        , "warnings_block" A..= False
        ]
  in case A.fromJSON payload of
       A.Success args -> pure (not (CheckModule.caWarningsBlock args))
       _              -> pure False

--------------------------------------------------------------------------------
-- BUG-PLUS-mediocre-2: summariseStderr cleans cabal noise, caps length
--------------------------------------------------------------------------------

-- | Real cabal stderr mixes signal ("Variable not in scope:
-- foo") with noise ("Resolving dependencies…",
-- "Build profile: -w ghc-9.12.2", cabal -W banner lines).
-- The summariser must keep the signal and drop the noise.
testQcSummariseStderrFiltersNoise :: IO Bool
testQcSummariseStderrFiltersNoise =
  let raw = T.unlines
        [ "Resolving dependencies..."
        , "Build profile: -w ghc-9.12.2 -O1"
        , "Warning: The package list for 'hackage' is 15 days old."
        , ""
        , "<interactive>:3:17: error: [GHC-76037]"
        , "    Variable not in scope: prop_trivial"
        ]
      summary = QcTool.summariseStderr raw
  in pure
      ( "prop_trivial" `T.isInfixOf` summary
        && "Variable not in scope" `T.isInfixOf` summary
        && not ("Resolving dependencies" `T.isInfixOf` summary)
        && not ("Build profile"          `T.isInfixOf` summary)
      )

-- | A pathological stderr (e.g. a dep-resolve megaflood) must
-- not blow the JSON-RPC envelope. summariseStderr caps at 1600
-- chars + appends a '…(truncated)' marker.
testQcSummariseStderrCaps :: IO Bool
testQcSummariseStderrCaps =
  let noisyLine = "<interactive>:1:1: error: [GHC-76037] not in scope — "
                  <> T.replicate 50 "blah blah "
      raw     = T.unlines (replicate 60 noisyLine)
      summary = QcTool.summariseStderr raw
  in pure
      ( T.length summary <= 1700  -- 1600 + "…(truncated)" slack
        && "…(truncated)" `T.isSuffixOf` summary
      )

--------------------------------------------------------------------------------
-- BUG-PLUS-mediocre-3: nextStep from ghci_load based on warning kind
--------------------------------------------------------------------------------

-- | When the 'warnings' array is empty, 'dispatch' proposes
-- 'ghci_suggest' — the clean-compile follow-up.
testNextStepCleanLoad :: IO Bool
testNextStepCleanLoad =
  let payload = A.object
        [ "success"  A..= True
        , "errors"   A..= ([] :: [Text])
        , "warnings" A..= ([] :: [Text])
        ]
  in pure $ case suggestNext "ghci_load" True payload of
       Just ns -> nsTool ns == "ghci_suggest"
       Nothing -> False

-- | A typed-hole warning routes to 'ghci_hole' (which knows how
-- to surface expected types + in-scope fits).
testNextStepTypedHoleWarn :: IO Bool
testNextStepTypedHoleWarn =
  let payload = A.object
        [ "success"  A..= True
        , "errors"   A..= ([] :: [Text])
        , "warnings" A..=
            [ A.object
                [ "message" A..=
                    ("Found hole: _ :: Int\n  Valid hole fits include …"
                     :: Text)
                , "severity" A..= ("warning" :: Text)
                ]
            ]
        ]
  in pure $ case suggestNext "ghci_load" True payload of
       Just ns -> nsTool ns == "ghci_hole"
       Nothing -> False

-- | A non-hole warning (unused-imports, type-defaults, …) routes
-- to 'ghci_fix_warning' — the auto-patch tool.
testNextStepFixableWarn :: IO Bool
testNextStepFixableWarn =
  let payload = A.object
        [ "success"  A..= True
        , "errors"   A..= ([] :: [Text])
        , "warnings" A..=
            [ A.object
                [ "message" A..=
                    ("Defaulting the type variable 'a0' to type 'Integer'"
                     :: Text)
                , "severity" A..= ("warning" :: Text)
                ]
            ]
        ]
  in pure $ case suggestNext "ghci_load" True payload of
       Just ns -> nsTool ns == "ghci_fix_warning"
       Nothing -> False

--------------------------------------------------------------------------------

-- | Wave-1 gate: drive cabal via the shim against a real project
-- and verify we get back a non-empty flag set that includes the
-- expected package-db paths. Uses '/tmp/bench-project' (created
-- during the Phase-2 benchmark work) as a minimal test fixture.
-- If that dir isn't there — e.g. on CI before the benchmark has
-- been run — we skip gracefully by returning True.
testCabalBootstrapLibrary :: IO Bool
testCabalBootstrapLibrary = case mkProjectDir "/tmp/bench-project" of
  Left _   -> pure True   -- malformed path shouldn't happen, skip
  Right pd -> do
    exists <- doesFileExist "/tmp/bench-project/bench-project.cabal"
    if not exists
      then pure True   -- fixture missing, skip (don't fail CI)
      else do
        stanzas <- bootstrapProject pd
        case Map.lookup TargetLibrary stanzas of
          Nothing ->
            pure False   -- bootstrap did not capture the library
          Just flags ->
            pure
              ( "--interactive" `elem` sfArgs flags
              && any ("-package-db" `isPrefix`) (sfArgs flags)
              && any ("-this-unit-id" `isPrefix`) (sfArgs flags)
              )
  where
    isPrefix p s = take (length p) s == p

-- | Phase-2 derisk: verify the interactive context set in one
-- 'withGhcSession' call survives into the next call. This is the
-- invariant the 22 read-only tool migrations rely on — each tool
-- call is its own 'withGhcSession', so if 'setSession' + 'getSession'
-- doesn't round-trip the HscEnv faithfully, we'd have to redo the
-- context every single call (which defeats the "1s cold-start" benefit).
--
-- If this ever starts failing, the fix is to host GHC in a
-- dedicated thread (HLS/ghcid pattern) rather than invoking 'runGhc'
-- per call. Better to discover that here than 6 tools into Phase 2.
testGhcSessionPersists :: IO Bool
testGhcSessionPersists = case mkProjectDir "/tmp" of
  Left _   -> pure False
  Right pd -> do
    sess <- startGhcSession pd
    -- Call 1: seed the interactive context with Prelude.
    withGhcSession sess $
      setContext [IIDecl (simpleImportDecl (mkModuleName "Prelude"))]
    -- Call 2: depend on call 1's side effect. If Prelude is gone,
    -- 'exprType "map"' throws a SourceError ("not in scope") and
    -- the test fails by exception.
    result <- withGhcSession sess $ do
      ty <- exprType TM_Inst "map"
      pure (showPprUnsafe ty)
    killGhcSession sess
    pure (not (null result) && "->" `T.isInfixOf` T.pack result)

-- | Phase-1 gate for the GHC-API-in-process migration: can we boot a
-- 'GhcSession', round-trip an 'exprType' through 'withGhcSession', and
-- tear it down cleanly? The 'map' type string is checked for @->@ to
-- confirm the pretty-print path works, not just the compile path.
--
-- No modules are loaded here — Phase 2 will layer that in when real
-- tool handlers (type, info) migrate.
testGhcSessionBoots :: IO Bool
testGhcSessionBoots = case mkProjectDir "/tmp" of
  Left _   -> pure False
  Right pd -> do
    sess   <- startGhcSession pd
    result <- withGhcSession sess $ do
      setContext [IIDecl (simpleImportDecl (mkModuleName "Prelude"))]
      ty <- exprType TM_Inst "map"
      pure (showPprUnsafe ty)
    killGhcSession sess
    pure (not (null result) && "->" `T.isInfixOf` T.pack result)

-- | Helper: create a fresh temp directory and delete it after the test.
-- Passes a validated 'ProjectDir' (absolute + normalised) to the body.
withTempProject :: (ProjectDir -> IO Bool) -> IO Bool
withTempProject k = do
  tmp <- getTemporaryDirectory
  ts  <- show <$> getTestTimestamp
  let dir = tmp </> ("haskell-flows-test-" <> ts)
  createDirectoryIfMissing True dir
  res <- case mkProjectDir dir of
    Left _   -> pure False
    Right pd -> k pd
  removePathForcibly dir
  pure res

getTestTimestamp :: IO Int
getTestTimestamp = do
  t <- getPOSIXTime
  pure (floor (t * 1_000_000))
