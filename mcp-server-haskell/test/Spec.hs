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
import Data.Aeson (Value, object, (.=))
import qualified Data.Aeson.Key as AKey
import qualified Data.Aeson.KeyMap as AKM
import qualified Data.ByteString.Lazy as BL
import qualified Data.Set as Set
import qualified Data.Text.Lazy as TL
import qualified Data.Text.Lazy.Encoding as TLE
import qualified Data.Vector as Vector
import Data.Char (isAsciiLower, isDigit)
import qualified Data.List as List
import Data.Text (Text)
import qualified Data.Text as T
import qualified Data.Text.IO as TIO
import Data.Maybe (catMaybes, fromMaybe, isJust, isNothing)
import Data.Time.Clock.POSIX (getPOSIXTime, posixSecondsToUTCTime)
import Data.Word (Word64)
import System.Exit (exitFailure, exitSuccess)
import System.Environment (lookupEnv, setEnv, unsetEnv)
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
  , sanitizeDeclarations
  , sanitizeExpression
  , sentinel
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
  , isContinuationFitLine
  , parseFitLine
  , splitFitTypeSource
  , repairConstraintInSource
  )
import HaskellFlows.Parser.TypeSignature
  ( ParsedSig (..)
  , SigType (..)
  , parseSignature
  , isSameTypeThroughout
  , stripForall
  , stripLineComments
  )
import HaskellFlows.Suggest.Rules
  ( Confidence (..)
  , RuleContext (..)
  , Suggestion (..)
  , applyRules
  , applyRulesCtx
  , mkRuleContext
    -- #147: name-semantic helpers
  , nameHintsInterpreter
  , nameHintsPrinter
  , nameHintsParser
  , namesFormPrinterParserPair
  )
import HaskellFlows.Mcp.Server (allToolDescriptors, allToolNameTexts)
import HaskellFlows.Mcp.NextStep
import qualified HaskellFlows.Mcp.SelfProject as SelfProject
import qualified HaskellFlows.Mcp.NextStep as NextStep
import HaskellFlows.Mcp.Protocol (ToolCall (..), ToolContent (..), ToolDescriptor (..), ToolResult (..))
import qualified HaskellFlows.Bench.Budget as Budget
import qualified HaskellFlows.Bench.Runner as Runner
import HaskellFlows.Mcp.ToolName
  ( ToolCategory (..)
  , ToolName (..)
  , allToolNames
  , parseToolName
  , toolCategory
  , toolCategoryText
  , toolVersion
  , toolNameText
  )
import HaskellFlows.Mcp.ErrorKind
  ( ErrorKind (..)
  , parseErrorKind
  , renderErrorKind
  )
import HaskellFlows.Mcp.RpcMethod
  ( RpcMethod (..)
  , allRpcMethods
  , allRpcMethodTexts
  , isNotification
  , parseRpcMethod
  , rpcMethodText
  )
import HaskellFlows.Mcp.ParseError
  ( InterpretedParseError (..)
  , interpretParseError
  )
import qualified HaskellFlows.Mcp.Schema as Schema
import qualified PathTraversal
import HaskellFlows.Mcp.PermissiveJSON
  ( BoolField (..)
  , IntField (..)
  )
import qualified HaskellFlows.Tool.Batch as Batch
import HaskellFlows.Tool.Batch (BatchArgs (..), unwrapResult)
import qualified HaskellFlows.Tool.Coverage as CoverageTool
import qualified HaskellFlows.Tool.Gate as Gate
import qualified HaskellFlows.Tool.CheckModule as CheckModule
import qualified HaskellFlows.Tool.CreateProject as CreateProject
import qualified HaskellFlows.Tool.Move as MoveTool
import qualified HaskellFlows.Tool.DepsExplain as DepsExplain
import qualified HaskellFlows.Tool.Lab as LabTool
import qualified HaskellFlows.Tool.ExplainError as ExplainError
import qualified HaskellFlows.Tool.Perf as PerfTool
import qualified HaskellFlows.Tool.PropertyAudit as PropertyAuditTool
import qualified HaskellFlows.Tool.Witness as WitnessTool
import qualified HaskellFlows.Tool.Determinism as DeterminismTool
import qualified HaskellFlows.Tool.QuickCheck as QcTool
import qualified HaskellFlows.Tool.QuickCheckExport as QcExport
import qualified HaskellFlows.Tool.Regression as RegTool
import qualified HaskellFlows.Tool.Bootstrap as Bootstrap
import qualified HaskellFlows.Tool.RemoveModules as RM
import qualified HaskellFlows.Tool.Modules as Modules
import qualified HaskellFlows.Tool.Suggest as SuggestTool
import qualified HaskellFlows.Tool.AddImport as AddImport
import qualified HaskellFlows.Tool.AddModules as AddModules
import qualified HaskellFlows.Tool.ApplyExports as ApplyExports
import qualified HaskellFlows.Tool.FixWarning as FixWarning
import qualified HaskellFlows.Mcp.WorkflowState as WS
import qualified HaskellFlows.Mcp.Logging as Logging
import qualified HaskellFlows.Mcp.Guidance as Guidance
import HaskellFlows.Mcp.ResourceUri
  ( ResourceUri (..)
  , allResourceUris
  , allResourceUriTexts
  , parseResourceUri
  , resourceUriText
  )
import qualified HaskellFlows.Mcp.ResourceUri as ResourceUri
import qualified HaskellFlows.Mcp.Resources as Resources
import HaskellFlows.Tool.CheckProject
  ( parseExposedModules
  , CheckProjectArgs (..)
  , ModuleOutcome (..)
  , renderResult
  )
import HaskellFlows.Tool.Lint (parseHlintJson)
import qualified HaskellFlows.Tool.Lint as LintTool
import HaskellFlows.Tool.Load (checkPathExists)
import qualified HaskellFlows.Tool.Load as LoadTool
import qualified HaskellFlows.Tool.ValidateCabal as VC
import HaskellFlows.Parser.QuickCheck
  ( QuickCheckResult (..)
  , parseQuickCheckOutput
  )
import HaskellFlows.Parser.Type
  ( InfoKind (..)
  , ParsedInfo (..)
  , ParsedType (..)
  , isOutOfScope
  , parseTypeOutput
  )
import HaskellFlows.Tool.Arbitrary
  ( Constructor (..)
  , compileFailedErr
  , pathToModule
  , renderArbitraryModule
  , hasRecursiveConstructor
  , hasUnboxedConstructor
  , isRecursiveArg
  , parseConstructors
  , parseTypeParams
  , renderTemplate
  )
import HaskellFlows.Data.PropertyStore
  ( Store
  , StoredProperty (..)
  , loadAll
  , openStore
  , save
  , saveCases
  )
import qualified HaskellFlows.Data.Scratchpad as SP
import qualified HaskellFlows.Tool.Scratch as ScratchTool
import HaskellFlows.Parser.Coverage
  ( CoverageReport (..)
  , Metric (..)
  , parseCoverage
  )
import HaskellFlows.Parser.ModuleName
  ( ModuleNameError (..)
  , isReservedKeyword
  , renderModuleNameError
  , reservedKeywords
  , validateModuleName
  , validateModuleNames
  )
import HaskellFlows.Tool.Deps
  ( addDep
  , extractErrorSummary
  , importsMatchingPackage
  , parseStanzaSelector
  , validatePackageName
  , validateVersionConstraint
  )
import qualified HaskellFlows.Tool.Deps as DepsTool
import HaskellFlows.Refactor.Extract
  ( ExtractResult (..)
  , extractBinding
  , isGuardBranch
  )
import HaskellFlows.Refactor.Rename
  ( RenameResult (..)
  , renameInScope
  , validateIdentifier
  )
import qualified HaskellFlows.Tool.Refactor as RefactorTool
import qualified HaskellFlows.Tool.Info as InfoTool
import HaskellFlows.Tool.Goto
  ( Location (..)
  , parseDefinedAt
  , locationPayload
  , qualifiedPreloadPayload
  )
import HaskellFlows.Tool.Hoogle
  ( HoogleHit (..)
  , parseHoogleLine
  )
import Control.Concurrent (forkIO, threadDelay)
import Control.Exception (SomeException, bracket_, try)
import qualified HaskellFlows.Mcp.PathBootstrap
import qualified System.Directory
import qualified System.FilePath
import Control.Monad (unless, when)
import Control.Concurrent.MVar
  ( newEmptyMVar, putMVar, takeMVar, newMVar, readMVar )
import System.Directory (createDirectoryIfMissing, doesFileExist, getTemporaryDirectory, listDirectory, removePathForcibly)
import System.FilePath ((</>))
import qualified HaskellFlows.Types
import HaskellFlows.Types
  ( PathError (..)
  , ProjectDir
  , mkModulePath
  , mkProjectDir
  )
import HaskellFlows.Ghc.ApiSession
  ( GhcSession
  , LoadFlavour (..)
  , captureStdout
  , evalIOString
  , evalIOUnitCapture
  , killGhcSession
  , readLoadedRefForTest
  , resetHscEnvInPlace
  , startGhcSession
  , withGhcSession
  , writeLoadedRefForTest
  )
import qualified HaskellFlows.Mcp.Envelope as Env
import qualified HaskellFlows.Tool.Bootstrap as BootstrapTool
import qualified HaskellFlows.Tool.Browse as BrowseTool
import qualified HaskellFlows.Tool.Complete as CompleteTool
import qualified HaskellFlows.Tool.Doc as DocTool
import qualified HaskellFlows.Tool.Eval as EvalTool
import qualified HaskellFlows.Tool.AddImport as AddImportTool
import qualified HaskellFlows.Tool.Hole as HoleTool
import qualified HaskellFlows.Tool.Hoogle as HoogleTool
import qualified HaskellFlows.Tool.Goto as GotoTool
import qualified HaskellFlows.Tool.Imports as ImportsTool
import qualified HaskellFlows.Tool.ToolchainWarmup as ToolchainWarmupTool
import qualified HaskellFlows.Tool.ValidateCabal as ValidateCabalTool
import qualified HaskellFlows.Tool.Workflow as WorkflowTool
import HaskellFlows.Mcp.Staleness (StalenessReport (..), binaryIdentityStale)
import HaskellFlows.Mcp.Progress
  ( ProgressEvent (..)
  , ProgressSink (..)
  , mkProgressSink
  , noopSink
  , progressNotification
  , progressTokenFrom
  )
import qualified HaskellFlows.Tool.Type as TypeTool
import qualified HaskellFlows.Tool.SwitchProject as SwitchProject
import HaskellFlows.Tool.SwitchProject
  ( ValidationError (..)
  , validateSwitchTarget
  )
import Data.IORef (modifyIORef', newIORef, readIORef, writeIORef)
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

import Spec.Harness (test, testTimeoutMicros, quickTest)
import Spec.Scratch (scratchTests)
import Spec.AddImportUnit
import Spec.AddModulesHandle
import Spec.ApplyExports
import Spec.Arbitrary
import Spec.ArbitraryUnit
import Spec.Batch
import Spec.Bootstrap
import Spec.Browse
import Spec.BudgetGate
import Spec.Config
import Spec.Complete
import Spec.Coverage
import Spec.CreateProject
import Spec.Deps
import Spec.DepsExplainLab
import Spec.DepsFormat
import Spec.DepsUnit
import Spec.Descriptors
import Spec.Discover hiding (testCodeToolsRegistered)
import Spec.Doc
import Spec.DogfoodHint
import Spec.Envelope
import Spec.Extract
import Spec.FilterArtifacts
import Spec.FinalMisc
import Spec.FixWarningUnit
import Spec.Format
import Spec.GateUnit
import Spec.GhcErrorUnit
import Spec.Goto
import Spec.Guidance
import Spec.HaddockUnit
import Spec.HandleApplyRemove
import Spec.HoleParse
import Spec.HoleTool
import Spec.InfoAdvanced
import Spec.InfoHoogle
import Spec.InfoParse
import Spec.Lint
import Spec.Load
import Spec.LoadUnit
import Spec.ModuleNameUnit
import Spec.MoveUnit
import Spec.NextStepCoverage
import Spec.NextStepFull
import Spec.NextStepLoad
import Spec.NextStepUnit
import Spec.ParseError
import Spec.ParseHeaderUnit
import Spec.ParseQc
import Spec.Perf
import Spec.PerfUnit
import Spec.PermissiveJSON
import Spec.PlanUnit
import Spec.Progress
import Spec.PropertyAuditUnit
import Spec.Protocol
import Spec.QcExport
import Spec.RefactorTool
import Spec.RegressionUnit
import Spec.RemoveModulesUnit
import Spec.Rename
import Spec.Sanitize
import Spec.Schema
import Spec.ServerUnit
import Spec.Store
import Spec.SuggestAdvanced
import Spec.SuggestLaws
import Spec.SuggestSig
import Spec.SummariseUnit
import Spec.SwitchProjectUnit
import Spec.TaxonomyUnit
import Spec.Toolchain
  ( testToolchainStatusBackcompatFields
  , testToolchainStatusEnvelopeShape
  , testToolchainWarmupEnvelopeShape
  , testToolchainWarmupPartialWarnings
  )
import Spec.PartialFunctions
import Spec.TraversalGuards
import Spec.TypeEval
import Spec.ValidateCabal
import Spec.Witness
  ( testWitnessCompileErrorResult
  , testWitnessEvalExprStructure
  , testWitnessInProcessFallback
  , testWitnessUsesInProcessPath
  )
import Spec.WorkflowState hiding
  ( testArbitraryPathToModule
  , testArbitraryModuleRender
  )
import Spec.WorkflowTool
import Spec.DispatchUnit
  ( testHandlerForExhaustive
  , testHandlerForGhcQuickCheckIsQcTool
  , testMkToolEnvFields
  )
import Spec.RegistryUnit
  ( testRegistryTotalOverToolName
  , testRegistryNoDuplicateNames
  , testRegistryBudgetKeysAgree
  , testToolCategoryTotalOverToolName
  , testToolCategoryAgreesWithToolName
  )
import Spec.ProcessUnit
  ( testRunArgvCompletes
  , testRunArgvTimeout
  , testRunArgvNonZeroExit
  )

main :: IO ()
main = do
  results <-
    sequence $
      [ test "mkProjectDir rejects relative"    testRejectsRelativeProject
      , test "mkModulePath accepts in-tree"      testAcceptsInTree
      , test "mkModulePath rejects traversal"    testRejectsTraversal
      , test "ghc_load #79: checkPathExists Right" testCheckPathExistsAccepts
      , test "ghc_load #79: checkPathExists Left"  testCheckPathExistsRejects
      , test "ghc_load #84: empty project → no_match" testGhcLoadEmptyProjectNoMatch
      -- Issue #214 — no-args reload uses library stanza, not test-suite stanza
      , test "#214: ghc_load no-args uses firstLibraryOrTestSuite (not firstTestSuiteOrLibrary)"
             testGhcLoadNoArgsUsesLibraryTarget
      , test "parseGhcErrors extracts header"    testParseHeader
      , test "sanitizeExpression accepts normal" testSanitizeAccepts
      , test "sanitizeExpression rejects newline" testSanitizeRejectsNewline
      , test "sanitizeExpression rejects sentinel" testSanitizeRejectsSentinel
      , test "sanitizeExpression rejects empty"   testSanitizeRejectsEmpty
      , test "sanitizeExpression rejects large literal (#127)" testSanitizeRejectsLargeLiteral
      , test "sanitizeExpression rejects big exponent (#127)"  testSanitizeRejectsBigExponent
      , test "sanitizeExpression accepts 19-digit literal (#127)" testSanitizeAccepts19Digits
      , test "sanitizeExpression accepts small exponent (#127)"   testSanitizeAcceptsSmallExp
      , test "sanitizeRejection OversizedIntegerLiteral -> oversized_input (#127)"
             testSanitizeRejectionOversizedInteger
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
      , test "#211: QcGaveUp renderResult kind=validation" testQcGaveUpValidationKind
      , test "parseQuickCheckOutput exception"     testQcException
      , test "parseQuickCheckOutput unparsed"      testQcUnparsed
      , test "parseTypedHoles extracts one hole"   testHoleOne
      , test "parseTypedHoles ignores non-holes"   testHoleIgnored
      , test "parseConstructors inline form"       testCtorsInline
      , test "parseConstructors multiline form"    testCtorsMultiline
      , test "parseConstructors rejects synonym"   testCtorsSynonym
      , test "parseConstructors: no unique-suffix in args (#170)" testCtorsNoUniqueSuffix
      , test "renderTemplate 3 ctors"              testTemplate3
      , test "parseHoogleLine normal hit"          testHoogleHit
      , test "parseHoogleLine no-results line"    testHoogleEmpty
      , test "parseCoverage full report"           testCoverageFull
      , test "parseCoverage ignores banner"        testCoverageBanner
      , test "parseCoverage flags 0/0 not_applicable (#89)"
                                                   testCoverageMetricNotApplicable
      , test "summarise skips not_applicable (#89)"
                                                   testCoverageAverageSkipsNotApplicable
      , test "summarise covers all-applicable case (#89)"
                                                   testCoverageAllMetricsApplicable
      , test "summarise handles all-non-applicable (#89)"
                                                   testCoverageAllNotApplicable
      , test "#176: summarise excludes boolean-coverage parent from average"
                                                   testCoverageSummariseExcludesBooleanParent
      , test "#177: parseCoverage captures alwaysTrue annotation"
                                                   testCoverageAlwaysTrueParsed
      , test "#177: parseCoverage captures alwaysFalse annotation"
                                                   testCoverageAlwaysFalseParsed
      , test "#177: parseCoverage captures combined always annotation"
                                                   testCoverageAlwaysBothParsed
      , test "#178: renderResult omits raw by default"
                                                   testCoverageRawOmittedByDefault
      , test "#178: renderResult includes raw when verbose=true"
                                                   testCoverageRawIncludedWhenVerbose
      , test "eval ctx · empty adds all 5 extras (#86)"
                                                   testEvalContextEmptyAddsAll
      , test "eval ctx · existing Prelude suppresses dup (#86)"
                                                   testEvalContextSkipsExistingPrelude
      , test "eval ctx · second call is no-op (#86)"
                                                   testEvalContextSecondCallNoop
      , test "eval ctx · subset existing only fills gaps (#86)"
                                                   testEvalContextSubsetExisting
      , test "eval ctx · idempotent on its own output (#86)"
                                                   testEvalContextIdempotent
      -- Issue #88: PermissiveJSON IntField + BoolField
      , test "IntField · canonical JSON number (#88)"
                                                   testIntFieldNumber
      , test "IntField · numeric string \"42\" (#88)"
                                                   testIntFieldNumericString
      , test "IntField · signed string -17 / +17 (#88)"
                                                   testIntFieldSignedString
      , test "IntField · whitespace stripped (#88)"
                                                   testIntFieldStrippedString
      , test "IntField · rejects non-numeric (#88)"
                                                   testIntFieldRejectsNonNumeric
      , test "IntField · rejects trailing garbage (#88)"
                                                   testIntFieldRejectsTrailingGarbage
      , test "IntField · rejects fractional (#88)"
                                                   testIntFieldRejectsFractional
      , test "BoolField · canonical JSON bool (#88)"
                                                   testBoolFieldNative
      , test "BoolField · accepts \"true\"/\"1\"/\"FALSE\"/etc (#88)"
                                                   testBoolFieldStringForms
      , test "BoolField · rejects truthy strings (#88)"
                                                   testBoolFieldRejectsTruthy
      -- Integration: each migrated tool's *Args parses both wires.
      , test "Refactor · scope_line_start/_end accept strings (#88)"
                                                   testRefactorPermissiveLineRange
      , test "RemoveModules · delete_files/force accept strings (#88)"
                                                   testRemoveModulesPermissiveBool
      , test "FixWarning · line/apply accept strings (#88)"
                                                   testFixWarningPermissiveLine
      , test "Complete · limit accepts string + default (#88)"
                                                   testCompletePermissiveLimit
      -- Issue #85: friendly parse-error formatting
      , test "ParseError · missing key extracted + flagged (#85)"
                                                   testParseErrorMissingKey
      , test "ParseError · dotted-path type mismatch (#85)"
                                                   testParseErrorTypeMismatchDotted
      , test "ParseError · bracket-quoted field (#85)"
                                                   testParseErrorTypeMismatchBracketed
      , test "ParseError · type mismatch w/o field (#85)"
                                                   testParseErrorTypeMismatchNoField
      , test "ParseError · unrecognised falls through to Validation (#85)"
                                                   testParseErrorUnrecognisedFalls
      , test "ParseError · raw text always preserved on ipRaw (#85)"
                                                   testParseErrorRawAlwaysPreserved
      -- Issue #100 · property-based path-traversal fuzz (Phase A)
      , quickTest "path guard · canonical invariant (#100)"
                                                   PathTraversal.prop_pathGuard_canonical_invariant
      , quickTest "path guard · mkModulePath ↔ resolveTarget agree (#100)"
                                                   PathTraversal.prop_pathGuard_lint_resolveTarget_consistent
      , quickTest "path guard · any '..' segment always rejected (#100)"
                                                   PathTraversal.prop_pathGuard_dotdot_always_rejected
      , test "path guard · symlink escape detected (#100 Phase B)"
                                                   PathTraversal.testSymlinkEscapeAcceptedByPureGuard
      , test "path guard · canonicalCheck catches symlink (#100 Phase D)"
                                                   PathTraversal.testCanonicalCheckCatchesSymlink
      -- Issue #100 Phase C · cross-tool traversal harness
      , test "#100C: ghc_apply_exports rejects traversal path"
                                                   testApplyExportsRejectsTraversal
      , test "#100C: ghc_fix_warning rejects traversal path"
                                                   testFixWarningRejectsTraversal
      , test "#100C: ghc_format rejects traversal path"
                                                   testFormatRejectsTraversal
      , test "#246: ghc_format missing file returns clean error" testFormatMissingFile
      , test "#100C: ghc_check_module rejects traversal path"
                                                   testCheckModuleRejectsTraversal
      , test "#150: ghc_check_module non-existent file → module_path_does_not_exist"
                                                   testCheckModuleNonExistentFile
      , test "#100C: ghc_explain_error rejects traversal path"
                                                   testExplainErrorRejectsTraversal
      , test "#100C: ghc_lab rejects traversal path"
                                                   testLabRejectsTraversal
      , test "#160: ghc_lab non-existent file → module_path_does_not_exist"
                                                   testLabNonExistentFile
      , test "#100C: ghc_load rejects traversal path"
                                                   testLoadRejectsTraversal
      , test "#100C: ghc_refactor rejects traversal path"
                                                   testRefactorRejectsTraversal
      -- Issue #92 Phase A · discriminated schema helpers
      , test "Schema · flat top-level shape (no oneOf/allOf/anyOf) — Claude API"
                                                   testSchemaTopLevelOneOf
      , test "Schema · discriminant published as enum field"
                                                   testSchemaDiscriminantInEveryBranch
      , test "Schema · discriminant enum lists every branch value"
                                                   testSchemaDiscriminantConstMatchesValue
      , test "Schema · top-level required = [discriminant]"
                                                   testSchemaRequiredSetsAreCorrect
      , test "Schema · additionalProperties:false at top level"
                                                   testSchemaAdditionalPropertiesFalse
      , test "Schema · flatObjectSchema for non-discriminated tools (#92)"
                                                   testSchemaFlatObject
      , test "Schema · field builders surface correct type+description (#92)"
                                                   testSchemaFieldBuilders
      -- Issue #92 Phase B: ghc_refactor migration
      , test "Refactor · rename_local complete payload parses (#92B)"
                                                   testRefactorRenameLocalCompleteParses
      , test "Refactor · rename_local missing old_name fails parse (#92B)"
                                                   testRefactorRenameLocalMissingOldName
      , test "Refactor · rename_local missing scope_line_start fails (#92B)"
                                                   testRefactorRenameLocalMissingScopeStart
      , test "Refactor · extract_binding doesn't need old_name (#92B)"
                                                   testRefactorExtractBindingNoOldName
      , test "Refactor · extract_binding still needs both scope lines (#92B)"
                                                   testRefactorExtractBindingMissingScope
      , test "Refactor · published schema uses discriminatedSchema (#92B)"
                                                   testRefactorSchemaIsDiscriminated
      , test "#154: list_actions returns available actions without module_path"
                                                   testRefactorListActions
      , test "#154: list_actions response has required field catalogue"
                                                   testRefactorListActionsHasRequired
      -- Issue #92 Phase B: ghc_deps migration
      , test "Deps · 'list' bare {action:list} parses (#92B)"
                                                   testDepsListBareParses
      , test "Deps · 'add' missing package fails parse (#92B)"
                                                   testDepsAddMissingPackage
      , test "Deps · 'remove' missing package fails parse (#92B)"
                                                   testDepsRemoveMissingPackage
      , test "Deps · 'add' with package + version parses (#92B)"
                                                   testDepsAddCompleteParses
      , test "Deps · published schema uses discriminatedSchema (#92B)"
                                                   testDepsSchemaIsDiscriminated
      , test "Schema · every registered tool publishes valid JSON Schema (#92D)"
                                                   testEveryToolPublishesValidSchema
      , test "nextStep · every recommended tool is in the registry (#95)"
                                                   testNextStepReferencesRegisteredToolsOnly
      , test "Tool descriptors · every tool has a non-empty description"
                                                   testEveryToolHasNonEmptyDescription
      , test "Tool descriptors · every tdName is in the canonical ADT"
                                                   testEveryToolNameIsCanonical
      , test "Tool descriptors · tdName ≤ 50 chars"
                                                   testEveryToolNameIsShort
      , test "Tool descriptors · tdDescription ≥ 20 chars"
                                                   testEveryToolDescriptionIsSubstantive
      , test "nextStep · nsExample is JSON Object when present (#95)"
                                                   testNextStepExampleIsObjectWhenPresent
      , test "nextStep · every chain-step carries Object args (#95)"
                                                   testNextStepChainStepsCarryObjectArgs
      , test "PropertyStore save+load roundtrip"   testStoreRoundtrip
      , test "PropertyStore increments pass count" testStoreIncrement
      , test "#283: saveCases records + keeps max cases" testStoreRecordsCases
      , test "#283: 3-arg save defaults cases to 0"      testStoreSaveDefaultsCasesZero
      , test "#283: qcMaxSuccess raised above 100"       testQcMaxSuccessRaised
      , test "validatePackageName accepts normal"  testPkgAccepts
      , test "validatePackageName rejects symbol"  testPkgRejectsSymbol
      , test "validatePackageName rejects empty"   testPkgRejectsEmpty
      , test "#48 extractErrorSummary picks pkg line"  testExtractErrorSummaryFindsPackage
      , test "#48 extractErrorSummary falls back"      testExtractErrorSummaryFallsBackOnNoMatch
      , test "#48 extractErrorSummary case-insensitive" testExtractErrorSummaryCaseInsensitive
      , test "validateVersionConstraint accepts"   testVerAccepts
      , test "validateVersionConstraint rejects"   testVerRejects
      , test "#292 explain action parses via ADT"   testExplainActionParses
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
      , test "extractBinding wraps block"           testExtractBinding
      , test "refactor: errorKey identifies same diag (#50)"      testRefactorErrorKeySame
      , test "refactor: errorKey distinguishes msgs (#50)"        testRefactorErrorKeyDistinct
      , test "refactor: signatures filter only errors (#50)"      testRefactorSignaturesErrorsOnly
      , test "refactor: post ⊆ pre means no new errors (#50)"     testRefactorPostSubsetPre
      , test "refactor: new error not in pre is detected (#50)"   testRefactorNewErrorDetected
      , test "extractBinding rejects empty range"   testExtractEmpty
      , test "extractBinding refuses top-level eq"  testExtractRefusesTopLevelEquation
      , test "extractBinding refuses type sig"      testExtractRefusesTypeSignature
      , test "extractBinding refuses import line"   testExtractRefusesImport
      , test "extractBinding allows indented body"  testExtractAllowsIndentedBody
      , test "extractBinding refuses module decl"   testExtractRefusesModuleDecl
      , test "extractBinding refuses data decl"     testExtractRefusesDataDecl
      , test "extractBinding refuses newtype decl"  testExtractRefusesNewtypeDecl
      , test "extractBinding refuses class decl"    testExtractRefusesClassDecl
      , test "extractBinding refuses instance decl" testExtractRefusesInstanceDecl
      , test "extractBinding refuses pragma"        testExtractRefusesPragma
      , test "#227: extractBinding refuses guard branch"       testExtractRefusesGuardBranch
      , test "#227: isGuardBranch detects | pattern"          testIsGuardBranch
      , test "#227: isGuardBranch ignores non-guard"          testIsGuardBranchNeg
      , test "extractBinding refuses operator def"  testExtractRefusesOperatorDef
      , test "extractBinding refuses multiline eq"  testExtractRefusesMultilineEquation
      , test "extractBinding refuses mixed range"   testExtractRefusesMixedRange
      , test "extractBinding refuses leading blanks"
          testExtractRefusesLeadingBlanksWithCol0
      , test "extractBinding refusal message shape"
          testExtractRefusalMessageShape
      , test "extractBinding allows let body"       testExtractAllowsLetBody
      , test "extractBinding allows do body"        testExtractAllowsDoBody
      , test "extractBinding allows where body"     testExtractAllowsWhereBody
      , test "extractBinding allows multiline body" testExtractAllowsMultilineBody
      , test "extractBinding survives EOL whitespace"
          testExtractSurvivesEolWhitespace
      , test "extractBinding produces single ="     testExtractProducesSingleEquals
      , test "extractBinding empty-ish range refused"
          testExtractAllBlankRangeRefused
      , test "ToolName: render-parse round-trip"    testToolNameRoundTrip
      , test "ToolName: parse rejects unknown"      testToolNameParseUnknown
      , test "ToolName: wire forms unique"          testToolNameWireUnique
      , test "ToolName: wire forms snake_case"      testToolNameSnakeCase
      , test "ToolName: allToolNames is exhaustive" testToolNameExhaustive
      , test "ErrorKind: render-parse round-trip"   testErrorKindRoundTrip
      , test "ErrorKind: parse rejects unknown"     testErrorKindParseUnknown
      , test "ErrorKind: wire forms unique"         testErrorKindWireUnique
      , test "ErrorKind: covers timeout/exhausted/exception"
          testErrorKindCoversThree
      , test "RpcMethod: render-parse round-trip"   testRpcMethodRoundTrip
      , test "RpcMethod: parse rejects unknown"     testRpcMethodParseUnknown
      , test "RpcMethod: wire forms unique"         testRpcMethodWireUnique
      , test "RpcMethod: required JSON-RPC methods" testRpcMethodCoversAllMcp
      , test "RpcMethod: isNotification correct"    testRpcMethodIsNotification
      , test "ResourceUri: render-parse round-trip" testResourceUriRoundTrip
      , test "ResourceUri: parse rejects unknown"   testResourceUriParseUnknown
      , test "ResourceUri: wire forms canonical"    testResourceUriWireCanonical
      , test "Envelope #90: ToolStatus round-trips JSON wire form"
                                                   testEnvelopeStatusRoundTrip
      , test "Envelope #90: ErrorKind round-trips JSON wire form"
                                                   testEnvelopeErrorKindRoundTrip
      , test "Envelope #90: WarningKind round-trips JSON wire form"
                                                   testEnvelopeWarningKindRoundTrip
      , test "Envelope #90: mkOk produces status=ok with result"
                                                   testEnvelopeMkOk
      , test "Envelope #90: mkRefused produces status=refused with error"
                                                   testEnvelopeMkRefused
      , test "Envelope #90: FromJSON rejects status=ok without result"
                                                   testEnvelopeFromJSONRequiresResult
      , test "Envelope #90: FromJSON rejects status=failed without error"
                                                   testEnvelopeFromJSONRequiresError
      , test "Envelope #90: ToolResponse JSON encode/decode round-trip"
                                                   testEnvelopeRoundTrip
      , test "Envelope #90: ErrorEnvelope optional fields default to Nothing"
                                                   testEnvelopeErrorOptionalFields
      , test "Envelope #90: warnings field omitted when empty"
                                                   testEnvelopeWarningsOmittedEmpty
      , quickTest "prop_envelope_status_total"     prop_envelopeStatusTotal
      , quickTest "prop_envelope_errorkind_total"  prop_envelopeErrorKindTotal
      , quickTest "prop_envelope_warningkind_total" prop_envelopeWarningKindTotal
      , test "Envelope #90 Phase B: ghc_toolchain_status emits envelope shape"
                                                   testToolchainStatusEnvelopeShape
      , test "Envelope #90 Phase B: ghc_toolchain_status preserves tools/blocking_gates"
                                                   testToolchainStatusBackcompatFields
      , test "Envelope #90 Phase B: ghc_toolchain_warmup emits envelope shape"
                                                   testToolchainWarmupEnvelopeShape
      , test "Envelope #90 Phase B: ghc_toolchain_warmup partial → warnings populated"
                                                   testToolchainWarmupPartialWarnings
      , test "Envelope #90 Phase B: ghc_validate_cabal clean → status=ok"
                                                   testValidateCabalClean
      , test "Envelope #90 Phase B: ghc_validate_cabal warnings → status=partial"
                                                   testValidateCabalWarnings
      , test "Envelope #90 Phase B: ghc_validate_cabal errors → status=failed"
                                                   testValidateCabalErrors
      , test "Envelope #90 Phase B: ghc_validate_cabal preserves issues array"
                                                   testValidateCabalBackcompatIssues
      , test "Envelope #90 Phase B: ghc_workflow status emits envelope"
                                                   testWorkflowStatusEnvelope
      , test "Phase 5: workflow status carries scratchpad section"
                                                   testWorkflowStatusHasScratchpad
      , test "Envelope #90 Phase B: ghc_workflow help emits envelope"
                                                   testWorkflowHelpEnvelope
      , test "Envelope #90 Phase B: ghc_workflow rejects unknown action"
                                                   testWorkflowRejectsUnknownAction
      , test "Envelope #90 Phase B: ghc_bootstrap host=claude-code preview emits envelope"
                                                   testBootstrapClaudeCodePreviewEnvelope
      , test "Envelope #90 Phase B: ghc_bootstrap host=generic preview emits envelope"
                                                   testBootstrapGenericPreviewEnvelope
      , test "Envelope #90 Phase B: ghc_bootstrap rejects unknown host"
                                                   testBootstrapRejectsUnknownHost
      , test "Envelope #90 Phase B: ghc_bootstrap rejects missing host"
                                                   testBootstrapRejectsMissingHost
      , test "#165: bootstrap missing-host message lists accepted values"
                                                   testBootstrapMissingHostFriendlyMessage
      , test "Envelope #90 Phase B: ghc_imports emits envelope with count + imports"
                                                   testImportsEnvelopeShape
      , test "Envelope #90 Phase D: legacy 'success' field dropped"
                                                   testEnvelopeLegacySuccessDropped
      , test "Envelope #90 Phase D: legacy 'error_kind' field dropped"
                                                   testEnvelopeLegacyErrorKindDropped
      , test "Envelope #90 Phase B: ghc_browse on project module → status=ok"
                                                   testBrowseProjectModuleOk
      , test "Envelope #90 Phase B: ghc_browse on external module → status=no_match"
                                                   testBrowseExternalModuleNoMatch
      , test "#168: ghc_browse fallback to package env → status=ok (Data.Maybe)"
                                                   testBrowseFallbackOk
      , test "#168: ghc_browse descriptor mentions session-preloaded modules"
                                                   testBrowseDescriptorMentionsSession
      , test "Envelope #90 Phase B: ghc_browse rejects missing module arg"
                                                   testBrowseRejectsMissingArg
      , test "Envelope #90 Phase B: ghc_complete with hits → status=ok"
                                                   testCompleteHitsOk
      , test "Envelope #90 Phase B: ghc_complete with zero hits → status=no_match"
                                                   testCompleteNoMatch
      , test "#145: ghc_complete zero hits + qualified prefix → remediation hint"
                                                   testCompleteQualifiedRemediation
      , test "#225: ghc_complete qualified remediation names module and suggests bare prefix"
                                                   testCompleteQualifiedRemediation225
      , test "#252: ghc_complete description documents qualified prefix support"
                                                   testCompleteDescriptionMentionsQualified
      , test "#252: splitQualifiedPrefix splits at LAST dot — name suffix"
                                                   testSplitQualifiedPrefixWithName
      , test "#252: splitQualifiedPrefix splits at LAST dot — empty suffix"
                                                   testSplitQualifiedPrefixEmptySuffix
      , test "#252: splitQualifiedPrefix returns Nothing for unqualified"
                                                   testSplitQualifiedPrefixUnqualified
      , test "#252: splitQualifiedPrefix handles deep modules (Data.Map.Strict.X)"
                                                   testSplitQualifiedPrefixDeep
      , test "#252: Complete.hs imports lookupModule for fallback"
                                                   testCompleteImportsLookupModule
      , test "Envelope #90 Phase B: ghc_complete refuses newline in prefix"
                                                   testCompleteRefusesNewline
      , test "Envelope #90 Phase B: ghc_goto on local name → status=ok"
                                                   testGotoLocalNameOk
      , test "Envelope #90 Phase B: ghc_goto on unknown name → status=no_match"
                                                   testGotoUnknownNameNoMatch
      , test "Envelope #90 Phase B: ghc_goto refuses newline in name"
                                                   testGotoRefusesNewline
      , test "Envelope #90 Phase B: ghc_doc with Haddock → status=ok"
                                                   testDocHasDocOk
      , test "Envelope #90 Phase B: ghc_doc on unknown name → status=no_match"
                                                   testDocUnknownNameNoMatch
      , test "Envelope #90 Phase B: ghc_doc refuses newline in name"
                                                   testDocRefusesNewline
      , test "Envelope #90 Phase B: ghc_type on valid expr → status=ok"
                                                   testTypeValidExprOk
      , test "Envelope #90 Phase B: ghc_type on ill-typed expr → status=failed (type_error)"
                                                   testTypeIllTypedFailed
      , test "Envelope #90 Phase B: ghc_type refuses newline in expression"
                                                   testTypeRefusesNewline
      , test "#141: ghc_type 'Not in scope' → kind=not_in_scope"
                                                   testTypeNotInScope
      , test "Envelope #90 Phase B: ghc_eval pure expr → status=ok"
                                                   testEvalPureExprOk
      , test "#143: ghc_eval import prefix → compile_error + nextStep=ghc_add_import"
                                                   testEvalImportRedirect
      , test "Envelope #90 Phase B: ghc_eval refuses newline in expression"
                                                   testEvalRefusesNewline
      , test "Envelope #90 Phase B: ghc_eval refuses sentinel string"
                                                   testEvalRefusesSentinel
      , test "#127: ghc_eval refuses 2^64 (oversized integer literal)"
                                                   testEvalRefusesOversizedInteger
      , test "#127: ghc_eval refuses 18446744073709551616 (20-digit literal)"
                                                   testEvalRefusesLargeLiteral
      , test "#134: classifyEvalException: source-text inspection"
                                                   testClassifyEvalExceptionInSource
      , test "Envelope #90 Phase B: ghc_hole on module with hole → status=ok"
                                                   testHoleWithHoleOk
      , test "Envelope #90 Phase B: ghc_hole on hole-free module → status=no_match"
                                                   testHoleNoHoleMatch
      , test "Envelope #90 Phase B: ghc_hole rejects path traversal"
                                                   testHoleRejectsTraversal
      , test "#148: ghc_hole non-existent file → module_path_does_not_exist"
                                                   testHoleNonExistentFile
      , test "Envelope #90 Phase B: ghc_info on real symbol → status=ok (#87)"
                                                   testInfoRealSymbolOk
      , test "Envelope #90 Phase B: ghc_info on unknown name → status=no_match (closes #87)"
                                                   testInfoUnknownNameNoMatch
      , test "Envelope #90 Phase B: ghc_info refuses newline in name"
                                                   testInfoRefusesNewline
      , test "Envelope #90 Phase B: hoogle_search rejects empty query"
                                                   testHoogleRejectsEmpty
      , test "Envelope #90 Phase B: hoogle_search reports unavailable when binary missing"
                                                   testHoogleUnavailable
      , test "Envelope #90 Phase B: ghc_add_import reports unavailable when hoogle missing"
                                                   testAddImportUnavailable
      , test "Envelope #90 Phase B: ghc_add_import rejects missing name arg"
                                                   testAddImportRejectsMissingArg
      , test "parseHlintJson parses list"          testHlintJson
      , test "ghc_lint #81: resolveTarget rejects relative traversal"
                                                   testLintResolveRejectsTraversal
      , test "ghc_lint #81: resolveTarget rejects abs path outside root"
                                                   testLintResolveRejectsAbsoluteOutside
      , test "ghc_lint #81: resolveTarget accepts in-tree path/module_path"
                                                   testLintResolveAcceptsInTree
      , test "#128: stripProjectDirPrefix no-ops on safe path"
                                                   testStripProjectDirPrefixNoOp
      , test "#128: stripProjectDirPrefix strips matching basename"
                                                   testStripProjectDirPrefixStrips
      , test "#128: stripProjectDirPrefix leaves absolute paths unchanged"
                                                   testStripProjectDirPrefixAbsolute
      , test "#128: resolveTarget avoids path doubling (dogfood case)"
                                                   testLintResolveNoDuplication
      , test "validateCabal flags duplicate deps"  testDuplicateDeps
      , test "validateCabal flags missing synopsis" testMissingSynopsis
      , test "parseExposedModules reads modules"   testParseModules
      , test "extractValidFits parses fits"        testValidFits
      , test "extractValidFits: operator-named fit not absorbed (#71)"
                                                                 testValidFitsOperatorBoundary
      , test "isContinuationFitLine: ' :: ' tagged line is a fresh fit (#71)"
                                                                 testHoleContinuationDetector
      , test "parseFitLine: HasCallStack type not truncated (#169)"
                                                                 testParseFitHasCallStack
      , test "splitFitTypeSource: splits bound-at annotation (#169)"
                                                                 testSplitFitTypeBoundAt
      , test "splitFitTypeSource: splits imported-from annotation (#169)"
                                                                 testSplitFitTypeImportedFrom
      , test "splitFitTypeSource: no annotation returns full type (#169)"
                                                                 testSplitFitTypeNoAnnotation
      , test "repairConstraintInSource: moves HasCallStack prefix to type (#196)"
                                                                 testRepairConstraintInSource
      , test "extractValidFits: HasCallStack wrap across continuation lines (#196)"
                                                                 testExtractValidFitsGhc912
      , test "parseSignature simple a -> a"         testSigSimple
      , test "parseSignature with constraint"       testSigConstraint
      , test "parseSignature list"                  testSigList
      , test "suggest matches involutive on a->a"   testSuggestInvolutive
      , test "suggest matches associative on a->a->a" testSuggestAssoc
      , test "suggest associative template applies fn at outer (#52)" testSuggestAssocTemplate
      , test "suggest skips unmatched shapes"       testSuggestNoMatch
      , test "#197: suggest Maybe-totality for a->b->Maybe c"
                                                   testSuggestMaybeReturn2Arg
      , test "#197: suggest Maybe-totality for a->Maybe b"
                                                   testSuggestMaybeReturn1Arg
      , test "#197: no-match hint omits 'arity > 2' for arity-2 sig"
                                                   testSuggestHintNoArityForArity2
      , test "#197: no-match hint includes 'arity > 2' for arity-3 sig"
                                                   testSuggestHintArityForArity3
      , test "batch parses documented {tool,args}"  testBatchParsesToolArgs
      , test "batch accepts MCP {name,arguments}"   testBatchParsesNameArgs
      , test "batch result not double-wrapped (#175)" testBatchResultNotDoubleWrapped
      , test "#249: batch empty actions returns warning"  testBatchEmptyActionsWarning
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
      , test "ghc_eval exposes Control.Concurrent"  testEvalContextHasControlConcurrent
      , test "ghc_eval enforces inner per-call budget" testEvalInnerTimeoutBudget
      , test "load paths derive interactive imports from source" testLoadAutoImports
      , test "Deferred pass writes to MCP-private build dir"      testDeferredIsolatedOutputs
      , test "ghc_deps add: idempotent no-op returns unchanged"  testDepsAddIdempotent
      , test "ghc_switch_project: empty dir -> create_project"   testSwitchProjectEmptyDir
      , test "ghc_check_module: filter diagnostics by file"     testCheckModuleDiagFilter
      , test "ghc_add_modules: accepts stanza param"            testAddModulesStanzaParam
      , test "ghc_check_project: also scans test/app/bench"     testCheckProjectTestDirs
      , test "ghc_quickcheck: widens scope via :m +"            testQuickCheckScopeWidening
      , test "ghc_quickcheck: runner uses :{ do :} not bare <-"  testQuickCheckRunnerDoBrace
      , test "initialize emits instructions field"  testInitializeEmitsInstructions
      , test "instructions mention key tools+flows" testInstructionsMentionCore
      , test "nextStep: create_project -> deps"     testNextStepCreateProject
      , test "nextStep: deps(add) -> load"          testNextStepDepsAdd
      , test "nextStep: load clean -> suggest"      testNextStepLoadClean
      , test "nextStep: load w/ warnings -> hole"   testNextStepLoadWarnings
      , test "nextStep: suggest -> scratch(write)"   testNextStepSuggest
      , test "nextStep: qc passed -> check_module"  testNextStepQcPassed
      , test "nextStep: qc failed -> eval"          testNextStepQcFailed
      , test "nextStep: regression list -> run"     testNextStepRegressionList
      , test "nextStep: refactor -> load"           testNextStepRefactor
      , test "nextStep: check_module -> project"   testNextStepCheckModule
      , test "nextStep: check_project -> coverage" testNextStepCheckProject
      , test "nextStep: errors -> no suggestion"   testNextStepErrorsSuppressed
      , test "#A5: compile_error -> explain_error" testSuggestOnErrorCompileError
      , test "#A5: not_in_scope -> explain_error"  testSuggestOnErrorNotInScope
      , test "#A5: explain_error no self-loop"     testSuggestOnErrorNoSelfLoop
      , test "#A5: unrouted error kind suppresses" testSuggestOnErrorUnroutedKind
      , test "#282: A5 echoes module_path from payload" testSuggestOnErrorEchoesModule
      , test "#282: A5 omits module_path when absent"   testSuggestOnErrorNoModule
      , test "#282: explain_error parses w/o module_path" testExplainErrorOptionalModule
      , test "#266 xsession: ledger round-trips"   testSessionLedgerRoundtrip
      , test "#266 xsession: empty ledger reads empty" testSessionLedgerEmpty
      , test "#A4: info nextStep resolves name from payload" testNextStepInfoNameResolved
      , test "#A4: echoField falls back to placeholder"      testNextStepEchoFieldFallback
      , test "#274: scratch promote resolves target_module"  testNextStepScratchTargetResolved
      , test "#274: scratch promote keeps placeholder w/o module" testNextStepScratchTargetPlaceholder
      -- Phase 5: cross-tool nextStep arms
      , test "Phase 5: hole nextStep → scratch(write) chain"
                                                   testNextStepFromHoleRoutesToScratch
      , test "Phase 5: explain_error nextStep → scratch(write) chain"
                                                   testNextStepFromExplainErrorRoutesToScratch
      , test "Phase 5: scratch check chain has quickcheck after suggest"
                                                   testNextStepSuggestChainHasQuickCheck
      , test "nextStep: exploratory -> no suggestion" testNextStepExploratoryNothing
      , test "#185: ghc_info no_match -> hoogle_search"  testNextStepInfoNoMatchIsHoogle
      , test "#185: ghc_doc no_match -> hoogle_search"   testNextStepDocNoMatchIsHoogle
      , test "#251: ghc_goto no_match -> ghc_load"       testNextStepGotoNoMatchIsGhcLoad
      , test "#185: ghc_info found -> ghc_doc (not hoogle)" testNextStepInfoFoundIsDoc
      , test "nextStep: coverage exhaustive (PR-3)"   testNextStepCoverageExhaustive
      , test "nextStep: action-discriminated coverage" testNextStepActionCoverage
      , test "nextStep: suppressIf suppresses when rule holds (#95)"  testNextStepSuppressIfTrue
      , test "nextStep: suppressIf passes when rule false (#95)"      testNextStepSuppressIfFalse
      , test "nextStep: suppressOnDegraded active for failed (#95)"   testNextStepSuppressOnDegraded
      , test "nextStep: suppressOnZero suppresses zero count (#95)"   testNextStepSuppressOnZero
      , test "nextStep: suppressOnZero passes nonzero count (#95)"    testNextStepSuppressOnZeroPass
      , test "injectNextStep splices into payload" testInjectSplices
      , test "injectNextStep no-op on non-JSON"    testInjectSkipsNonJson
      , test "suggest: functor fmap two laws"      testSuggestFunctorFmap
      , test "suggest: evaluator preservation"     testSuggestEvaluatorPreservation
      , test "suggest: constant-folding soundness" testSuggestConstFoldingSoundness
      , test "suggest: evaluator needs sibling"    testSuggestEvaluatorNoSibling
      , test "gate: tool registered in inventory"  testGateRegistered
      , test "gate: all-skip parses + passes"      testGateAllSkip
      , test "#138: gate all-skip returns refused/validation" testGateAllSkipRefused
      , test "#138: gate summary avoids empty-verbs malform" testGateSummaryNoEmptyVerbs
      , test "#163: coverage default timeout is 5 min"        testCoverageDefaultTimeout
      , test "#163: coverage timeout_minutes clamps to [1,60]" testCoverageTimeoutClamp
      , test "#163: coverage timeout msg reflects actual minutes" testCoverageTimeoutMessage
      , test "#164: gate default test timeout is 5 min"        testGateDefaultTestTimeout
      , test "#164: gate default build timeout is 3 min"       testGateDefaultBuildTimeout
      , test "#164: gate test_timeout_minutes parses"          testGateCustomTestTimeout
      , test "#164: gate build_timeout_minutes parses"         testGateCustomBuildTimeout
      -- Issue #216 — dynamic regression timeout scales with store size
      , test "#216: dynamic regression timeout floors at 2 min"  testDynamicRegressionFloor
      , test "#216: dynamic regression timeout scales above 4 props" testDynamicRegressionScales
      , test "qcexport: tool registered"           testQcExportRegistered
      , test "qcexport: renderTestFile shape"      testQcExportRenderShape
      , test "qcexport: sanitizeLabel strips LF"   testQcExportSanitize
      , test "warnings: categorize common classes" testWarningCategorize
      , test "warnings: bucketize orders by count" testWarningBucketize
      , test "code tools: all 5 registered"        testCodeToolsRegistered
      , test "add_import: qualified renderImportLine" testAddImportQualified
      , test "add_import: missing hoogle returns success=false (#53)" testAddImportMissingHoogle
      , test "#146: addImportToSession rejects invalid import gracefully" testAddImportToSessionInvalid
      , test "#146: addImportToSession accepts valid base import"       testAddImportToSessionValid
      , test "info: renderConstructorsBlock empty (#54)"  testInfoCtorBlockEmpty
      , test "info: renderConstructorsBlock Maybe (#54)"  testInfoCtorBlockMaybe
      , test "info: successResult includes constructors (#54)" testInfoSuccessIncludesCtors
      , test "info: successResult drops field when none (#54)" testInfoSuccessDropsCtorField
      , test "info: renderClassMethodsBlock shape (#70)"        testInfoClassMethodsBlock
      , test "info: successResult emits class_methods on a class (#70)"
                                                                 testInfoSuccessClassMethods
      , test "info: successResult drops class_methods on a data type (#70)"
                                                                 testInfoSuccessDropsClassMethods
      , test "#142: successResult caps instances at 30 and sets instance_count"
                                                                 testInfoInstanceCap
      , test "#142: successResult sets instances_truncated=false when under cap"
                                                                 testInfoInstancesNotTruncated
      , test "check: propertiesGate empty -> ok=true (#42)"   testCheckGateEmpty
      , test "check: propertiesGate all pass -> ok=true (#42)" testCheckGatePass
      , test "check: propertiesGate regressed -> ok=false (#42)" testCheckGateRegressed
      , test "check: propertiesGate skipped -> ok=false (#42)" testCheckGateSkipped
      , test "check: propertiesGate reason matches ok flag (#42)" testCheckGateReasonMatchesOk
      , test "create_project: validateName accepts canonical (#58)"  testCreateValidateAccept
      , test "create_project: validateName rejects empty (#58)"      testCreateValidateEmpty
      , test "create_project: validateName rejects uppercase (#58)"  testCreateValidateUpper
      , test "create_project: validateName rejects double hyphen (#58)" testCreateValidateDoubleHyphen
      , test "create_project: validateName rejects trailing hyphen (#58)" testCreateValidateTrailing
      , test "create_project: validateName rejects leading digit (#58)"  testCreateValidateLeadingDigit
      , test "create_project: validateName rejects symbols (#58)"    testCreateValidateSymbols
      , test "create_project: scaffold cabal file is shippable green-by-default (#69)"
                                                                 testCreateProjectScaffoldGreenCabal
      , test "create_project: validateName error names violation (#58)" testCreateValidateErrorMsg
      -- Issue #233 — all-digit component validation
      , test "#233: validateName rejects date-like all-digit components" testCreateValidateAllDigitComponent
      , test "#233: validateName accepts v-prefixed numeric segments"   testCreateValidateVPrefixedOk
      -- Issue #234 — overwrite=true removes stale .cabal files
      , test "#234: scaffold overwrite=true removes stale .cabal"       testCreateOverwriteRemovesStaleCalab
      -- Issue #126 — path + write fixes
      , test "#126A: scaffold write=false never fails (preview mode)"  testCreateWriteFalseIsPreview
      , test "#126A: scaffold write=false returns preview content"      testCreateWriteFalseContent
      , test "#126B: scaffold targets supplied path not projectDir"    testCreateUsesSuppliedPath
      , test "#256: create with path auto-switches active project"    testCreateAutoSwitchPresent
      , test "#256: create preview (write=false) does not switch"     testCreatePreviewNoSwitch
      , test "#256: create without path does not switch"              testCreateNoPathNoSwitch
      , test "nextStep: add_import count=0 suppresses load (#53)"     testNextStepAddImportZero
      , test "nextStep: add_import count>0 nudges load (#53)"         testNextStepAddImportNonZero
      , test "add_modules: moduleToPath mapping"   testAddModulesPath
      , test "apply_exports: rewriteHeader idempotent" testApplyExportsIdempotent
      , test "apply_exports: injects exports"      testApplyExportsInjects
      , test "#173: rewriteHeader replaces different existing list"
                                                   testApplyExportsReplacesExistingList
      , test "#173: rewriteHeader NoHeader on missing module decl"
                                                   testApplyExportsNoHeader
      , test "#133: successResult includes applied=true"    testApplyExportsSuccessHasApplied
      , test "#133: noChangeResult includes applied=false"  testApplyExportsNoChangeHasApplied
      , test "#133: handle write path returns applied=true" testApplyExportsHandleAppliedTrue
      , test "#133: handle no-op path returns applied=false" testApplyExportsHandleAppliedFalse
      , test "#155: apply_exports write=false → applied=false, file unchanged"
                                                              testApplyExportsWriteFalse
      -- ISSUE-47: module-name validator unit tests
      , test "modname: valid single segment"        testValidModuleNameSingle
      , test "modname: valid dotted name"           testValidModuleNameDotted
      , test "modname: valid underscores"           testValidModuleNameUnderscore
      , test "modname: valid apostrophes"           testValidModuleNameApostrophe
      , test "modname: valid digits after first"    testValidModuleNameDigits
      , test "modname: trims whitespace"            testValidModuleNameTrim
      , test "modname: rejects 'lowercase.module'"  testInvalidLowercaseModule
      , test "modname: rejects bare reserved 'module'"
          testInvalidReservedBare
      , test "modname: rejects reserved second segment"
          testInvalidReservedSecond
      , test "modname: rejects empty input"         testInvalidEmpty
      , test "modname: rejects whitespace-only"     testInvalidWhitespace
      , test "modname: rejects trailing dot"        testInvalidTrailingDot
      , test "modname: rejects leading dot"         testInvalidLeadingDot
      , test "modname: rejects double dot"          testInvalidDoubleDot
      , test "modname: rejects leading digit"       testInvalidLeadingDigit
      , test "modname: rejects hyphen"              testInvalidHyphen
      , test "modname: rejects space"               testInvalidSpace
      , test "modname: bulk preserves order"        testValidateBulkOrderPreserved
      , test "modname: bulk all-good"               testValidateBulkAllGood
      , test "modname: bulk all-bad"                testValidateBulkAllBad
      , test "modname: bulk trims accepted"         testValidateBulkTrimsAccepted
      , test "modname: every reserved keyword refused"
          testReservedKeywordsAllRejected
      , test "modname: keyword set covers issue list"
          testReservedKeywordsCoverIssueList
      , test "modname: isReservedKeyword case-sensitive"
          testReservedKeywordsCaseSensitive
      , test "modname: rendered error is actionable"
          testRenderErrorActionable
      , test "modname: rendered keyword error suggests fix"
          testRenderErrorReservedSuggests
      , test "modname: rendered empty-segment error"
          testRenderErrorEmptySegment
      , test "modname: rendered invalid-char error"
          testRenderErrorInvalidChar
      , test "modname: every error renders non-empty"
          testRenderErrorAllNonEmpty
      -- ISSUE-47: handler-boundary E2E tests
      , test "add_modules: refuses lowercase.module (handler)"
          testHandleAddModulesRefusesLowercaseModule
      , test "add_modules: atomic refusal on mixed batch"
          testHandleAddModulesAtomicRefusal
      , test "add_modules: lists every offender"
          testHandleAddModulesAllOffendersListed
      , test "add_modules: happy path still works"
          testHandleAddModulesHappyPathStillWorks
      , test "remove_modules: refuses invalid name"
          testHandleRemoveModulesRefuses
      , test "remove_modules: happy path still works"
          testHandleRemoveModulesHappyPath
      , test "apply_exports: refuses reserved keyword"
          testHandleApplyExportsRefusesKeyword
      , test "apply_exports: accepts lowercase function"
          testHandleApplyExportsAcceptsLowercase
      , test "fix_warning: plan for unused imports" testFixWarningUnusedImports
      , test "fix_warning: planForCode marks fixable=True for 66111 (#55)" testFixPlanFixable66111
      , test "fix_warning: planForCode marks fixable=False for 40910 (#55)" testFixPlanNotFixable40910
      , test "fix_warning: planForCodeWithName promotes 40910 (#55)" testFixPlanWithNamePromotes
      , test "fix_warning: underscorePrefix replaces token (#55)" testUnderscorePrefixToken
      , test "fix_warning: underscorePrefix respects word boundary (#55)" testUnderscorePrefixWordBoundary
      , test "fix_warning: underscorePrefix idempotent on _name (#55)" testUnderscorePrefixIdempotent
      , test "fix_warning: patchTailBindings renames binding equations (#202)" testPatchTailBindings202
      , test "#221: fix_warning out-of-bounds line returns validation error"  testFixWarningOutOfBounds
      -- Issue #235 — patchPrecedingTypeSig
      , test "#235: isTypeSigLine detects type sig correctly"           testIsTypeSigLine235
      , test "#235: patchPrecedingTypeSig renames type sig"             testPatchPrecedingTypeSig235
      , test "#235: patchPrecedingTypeSig skips blank+comment lines"    testPatchPrecedingTypeSigSkips235
      , test "#235: patchPrecedingTypeSig no-op when no sig found"      testPatchPrecedingTypeSigNoOp235
      , test "#235: writePatched GHC-40910 binding also patches type sig" testWritePatchedAlsoFixesSig235
      , test "#238: renderStored emits module_hint when module=Nothing" testRenderStoredNullModuleHint238
      , test "#238: renderStored no module_hint when module=Just"       testRenderStoredJustModuleNoHint238
      , test "#238: listResult emits null_module_count and hint"        testListResultNullModuleCount238
      , test "#238: listResult no null_module fields when all have modules" testListResultNoNullFields238
      , test "#238: enhanceNullModuleDetail appends hint when null+skipped" testEnhanceNullModuleDetail238
      , test "#238: enhanceNullModuleDetail no-op when both have modules"   testEnhanceNullModuleDetailNoOp238
      , test "#294: fix_warning missingCoords detects module_path-only call" testFixWarningMissingCoords
      , test "remove_modules: scanImportersInBody plain (#41)" testRMScanImportPlain
      , test "remove_modules: scanImportersInBody respects hierarchy (#41)" testRMScanRespectsHierarchy
      , test "remove_modules: scanImportersInBody quiet on no match (#41)" testRMScanQuietOnNoMatch
      , test "move: sliceTopLevelBinding finds signature+body (#62)" testMoveSliceFindsBinding
      , test "move: sliceTopLevelBinding absorbs Haddock (#62)" testMoveSliceAbsorbsHaddock
      , test "move: sliceTopLevelBinding misses unknown (#62)" testMoveSliceMisses
      , test "move: removeSliceFromBody removes range (#62)" testMoveRemoveSlice
      , test "move: insertSliceAtEnd appends + separates (#62)" testMoveInsertSlice
      , test "move: rewriteImports splits selective import (#62)" testMoveRewriteSelective
      , test "move: rewriteImports leaves bare import alone (#62)" testMoveRewriteBare
      , test "move: rewriteImports leaves qualified alone (#62)" testMoveRewriteQualified
      , test "move: moduleNameToPath canonical (#62)" testMoveModulePath
      , test "move: removeFromSourceExportList drops symbol (#62)" testMoveRemoveExport
      , test "move: removeFromSourceExportList no-op on open export (#62)" testMoveRemoveExportOpen
      , test "move: addToDestinationExportList appends symbol (#76)" testMoveAddDestExport
      , test "move: addToDestinationExportList no-op when already present (#76)"
                                                                 testMoveAddDestExportIdempotent
      , test "move: addToDestinationExportList no-op on open export (#76)"
                                                                 testMoveAddDestExportOpen
      , test "move: slicer stops at next binding's Haddock (#76)" testMoveSliceStopsAtHaddock
      , test "#207: addToDestinationExportList handles Type(..) exports"    testMoveAddDestExportTypeCons
      , test "#207: removeFromSourceExportList handles Type(..) exports"    testMoveRemoveExportTypeCons
      , test "#228: collectModuleHeader single-line"                         testCollectModuleHeaderSingle
      , test "#228: collectModuleHeader multi-line"                          testCollectModuleHeaderMulti
      , test "#228: removeFromSourceExportList multi-line header"            testMoveRemoveExportMultiLine
      , test "#228: addToDestinationExportList multi-line header"            testMoveAddDestExportMultiLine
      , test "#236: move sequence: multi-line header + correct slice deletion" testMoveSequenceMultilineHeader236
      , test "#206: hasBareImportOf detects bare import"                    testHasBareImportOfDetects
      , test "#206: hasBareImportOf detects qualified bare import"          testHasBareImportOfQualified
      , test "#206: hasBareImportOf misses selective import"                testHasBareImportOfSelectiveMiss
      , test "deps_explain: parseSolverOutput on real dump (#63)" testDepsExplainParse
      , test "deps_explain: identifyRootCause picks deepest (#63)" testDepsExplainRoot
      , test "deps_explain: extractPackages strips versions (#63)" testDepsExplainPackages
      , test "deps_explain: parseSolverOutput Nothing on clean (#63)" testDepsExplainClean
      , test "#156: pkgSearchTokens aeson → [Aeson]"            testPkgSearchTokensSimple
      , test "#156: pkgSearchTokens data-default → [DataDefault,Data,Default]" testPkgSearchTokensHyphen
      , test "#156: importMatchesPkg aeson import Data.Aeson"   testImportMatchesPkgHit
      , test "#156: importMatchesPkg aeson import Data.Map miss" testImportMatchesPkgMiss
      , test "#156: cabalComponentsMatchingPkg finds library stanza" testCabalComponentsLibrary
      , test "lab: listTopLevelBindings finds simple sigs (#60)" testLabListSimple
      , test "lab: listTopLevelBindings handles multi-line sig (#60)" testLabListMultiline
      , test "lab: listTopLevelBindings skips empty + non-sigs (#60)" testLabListSkips
      , test "lab: confidenceAtLeast threshold (#60)" testLabConfidence
      , test "explain_error: pickDiagnostic default first (#59)" testExplainPickDefault
      , test "explain_error: pickDiagnostic by index (#59)" testExplainPickIndex
      , test "explain_error: pickDiagnostic out of range (#59)" testExplainPickOOR
      , test "explain_error: index out of range gives clear hint, not 'No errors' (#203)" testExplainIndexOutOfRangeHint203
      , test "explain_error: extractImports recognises shapes (#59)" testExplainExtractImports
      , test "explain_error: enclosingLineRange clamps (#59)" testExplainRangeClamps
      , test "perf: aggregate empty -> zeros (#61)" testPerfAggregateEmpty
      , test "perf: aggregate single sample (#61)" testPerfAggregateSingle
      , test "perf: aggregate odd count median (#61)" testPerfAggregateOdd
      , test "perf: aggregate even count median average (#61)" testPerfAggregateEven
      , test "perf: regressionPct positive when slower (#61 Phase2)" testPerfRegressionPctPositive
      , test "perf: regressionPct negative when faster (#61 Phase2)" testPerfRegressionPctNegative
      , test "perf: regressionPct Nothing when zero baseline (#61 Phase2)" testPerfRegressionPctZeroBaseline
      , test "perf: BaselineEntry JSON roundtrip (#61 Phase2)" testPerfBaselineEntryRoundtrip
      , test "#212: audit detects ==> in expression text"       testPAImplicationDetection
      , test "property_audit: pairCombinations 0 elements (#64)" testPACombinationsEmpty
      , test "property_audit: pairCombinations 5 elements (#64)" testPACombinations5
      , test "property_audit: pairCombinations distinct pairs (#64)" testPACombinationsDistinct
      , test "property_audit: buildContradictionProbe shape (#64)" testPABuildProbe
      , test "property_audit: interpretProbeResult QcPassed → contradictory (#77)"
                                                                 testPAInterpretPassed
      , test "property_audit: interpretProbeResult QcFailed → compatible (#77)"
                                                                 testPAInterpretFailed
      , test "property_audit: interpretProbeResult QcGaveUp/Unparsed/Exception → skipped (#77)"
                                                                 testPAInterpretSkipped
      , test "#149: interpretProbeResult QcUnparsed empty raw has non-empty cause"
                                                                 testPAInterpretUnparsedEmptyCause
      , test "property_audit: dedupByExpression keeps first occurrence (#77)"
                                                                 testPADedupByExpression
      , test "property_audit: dedupByExpression preserves singletons (#77)"
                                                                 testPADedupSingletons
      , test "witness: bucketSize boundary cases (#65)" testWitBucketBoundaries
      , test "witness: buildInstrumentedProperty wraps with collect (#65)" testWitBuildInstrumented
      , test "witness: parseLabelDistribution recovers buckets (#65)" testWitParseDistribution
      , test "witness: biasWarnings flags <1% bucket (#65)" testWitBiasWarning
      , test "witness: parseLabelCounts reads tab-separated rows (#78)"
                                                                 testWitParseLabelCounts
      , test "witness: parseLabelCounts skips malformed rows (#78)"
                                                                 testWitParseLabelCountsRobust
      , test "witness: countsToDistribution sums to 100 (#78)"   testWitCountsToDistribution
      , test "witness: countsToDistribution empty input → []  (#78)"
                                                                 testWitCountsEmpty
      , test "witness: buildConstructorProperty wraps with ctor label (#65 Phase2)"
                                                                 testWitBuildConstructorProperty
      , test "#239: buildConstructorProperty uses list-aware extraction"
                                                                 testWitConstructorListAware
      , test "witness: descriptor mentions 'deferred' field (#171)"
                                                                 testWitDeferredDocumented
      , test "witness: timer starts after property build (#171)"
                                                                 testWitTimerAfterBuild
      , test "#199: isPrimitiveBuckets true for numeric ctor labels"
                                                                 testWitIsPrimitiveBucketsTrue
      , test "#199: isPrimitiveBuckets false for ADT ctor labels"
                                                                 testWitIsPrimitiveBucketsFalse
      , test "#199: isPrimitiveBuckets false when empty"
                                                                 testWitIsPrimitiveBucketsEmpty
      , test "#199: primitive fallback adds warning + uses by_size"
                                                                 testWitPrimitiveFallbackWarning
      , test "#199: raw_truncated=true when qc_raw_output > 1000 chars"
                                                                 testWitRawTruncatedFlag
      , test "#199: raw_truncated absent when output <= 1000 chars"
                                                                 testWitNoRawTruncatedWhenShort
      , test "#220: witnessEvalExpr contains sentinel markers and QC calls"
                                                                 testWitnessEvalExprStructure
      , test "#220: Witness.hs uses runQuickCheckWithLabelsInProcess (in-process)"
                                                                 testWitnessUsesInProcessPath
      , test "#220: runQuickCheckWithLabelsInProcess has subprocess fallback"
                                                                 testWitnessInProcessFallback
      , test "#240: compileErrorResult returns status=failed kind=compile_error"
                                                                 testWitnessCompileErrorResult
      , test "property_audit: isVacuousResult true for QcGaveUp (#64 Phase2)"
                                                                 testPAIsVacuousGaveUp
      , test "property_audit: isVacuousResult false for QcPassed (#64 Phase2)"
                                                                 testPAIsVacuousNotPassed
      , test "#241: PropertyAudit.hs uses runQuickCheckWithLabelsInProcess for probe"
                                                                 testAuditUsesInProcessProbe
      , test "#241: enhanceCrossModuleDetail appends hint for cross-module pair"
                                                                 testEnhanceCrossModuleDetailHits
      , test "#241: enhanceCrossModuleDetail no-op when modules match"
                                                                 testEnhanceCrossModuleDetailSameModule
      , test "#241: enhanceCrossModuleDetail no-op when not skipped"
                                                                 testEnhanceCrossModuleDetailNotSkipped
      , test "#241: enhanceCrossModuleDetail no-op when module is null"
                                                                 testEnhanceCrossModuleDetailNullModule
      , test "#241: appendReplStderr surfaces stderr on skipped+load-failure"
                                                                 testAppendReplStderrHits
      , test "#241: appendReplStderr no-op when stderr is empty"
                                                                 testAppendReplStderrEmpty
      , test "#241: appendReplStderr no-op when not skipped"     testAppendReplStderrNotSkipped
      , test "#241: appendReplStderr truncates long stderr to 500 chars"
                                                                 testAppendReplStderrTruncates
      , test "#294: enhanceNotInScopeDetail gives honest skip reason"
                                                                 testEnhanceNotInScopeDetailHits
      , test "#294: enhanceNotInScopeDetail no-op when not skipped"
                                                                 testEnhanceNotInScopeDetailNotSkipped
      , test "#241: allPairsSkipped True when every finding skipped"
                                                                 testAllPairsSkippedTrue
      , test "#241: allPairsSkipped False when at least one compatible"
                                                                 testAllPairsSkippedFalseCompat
      , test "#241: allPairsSkipped False when nPairs=0"         testAllPairsSkippedFalseEmpty
      , test "#230: renderFinding kind=contradictory-pair for contradictory status"
                                                                 testPARenderFindingKindContradictory
      , test "#230: renderFinding kind=skipped-pair for skipped status"
                                                                 testPARenderFindingKindSkipped
      , test "explain_error: applyLinePatch replaces old text (#59 Phase2)"
                                                                 testEEApplyLinePatch
      , test "explain_error: applyLinePatch returns Nothing when old not found (#59 Phase2)"
                                                                 testEEApplyLinePatchMiss
      , test "explain_error: applyLinePatch rejects out-of-bounds line (#59 Phase2)"
                                                                 testEEApplyLinePatchOob
      , test "#222: verify_patch uses loadForTarget not bare loadAndCaptureDiagnostics"
                                                                 testExplainVerifyPatchUsesLoadForTarget
      , test "#189: parseGhcLineCol extracts line+col from error text" testParseGhcLineColBasic
      , test "#189: parseGhcLineCol handles col range (7-15 → 7)"      testParseGhcLineColRange
      , test "#189: parseGhcLineCol falls back to 1:1 on no location"  testParseGhcLineColFallback
      , test "#189: syntheticError uses parsed line+col"                testSyntheticErrorLineCol
      , test "workflow-state: initial empty"       testWorkflowStateInitial
      , test "workflow-state: tracks load + edits" testWorkflowStateTracks
      , test "workflow-state: renderHelp thresholds" testWorkflowStateHelp
      , test "#257: session activity ever-called set"  testSessionActivityEverCalled
      , test "#257: session activity error streak"      testSessionActivityErrorStreak
      , test "#257: session activity unused count"       testSessionActivityUnusedCount
      , test "#263: discover excludes called tools"    testDiscoverExcludesCalled
      , test "#263: discover returns at most 5"         testDiscoverAtMostFive
      , test "#263: discover ranks phase-relevant"      testDiscoverPhaseRelevance
      , test "#266: post-mortem flags missed scratch"  testPostMortemMissedScratch
      , test "#266: post-mortem reports counts"         testPostMortemCounts
      , test "#262: modules(add) -> design-first scratch" testNextStepModulesCreatedScratch
      , test "#270: chain resolves <same module> from payload" testNextStepResolvesSameModule
      , test "#264: plan matches module-with-qc template" testPlanMatchesModuleQc
      , test "#264: plan low-confidence lists alternatives" testPlanLowConfidenceListsAlternatives
      , test "#284: plan scaffolds all named modules"  testPlanMultiModule
      , test "#284: plan caps confidence on complex goal" testPlanComplexGoalCapped
      , test "#258: GHC-32850 suppressed when modules registered" testLoad32850Suppressed
      , test "#258: GHC-32850 retained when module missing" testLoad32850RetainedWhenMissing
      , test "#258: non-GHC-32850 warning never dropped (CI regression)" testLoadNon32850Retained
      , test "#278: test-stanza path detected as non-library"  testLoadNonLibStanzaPath
      , test "#278: library path not flagged as non-library"   testLoadLibStanzaPath
      , test "#278: load failure message points at ghc_gate"   testLoadFailureMessageGate
      , test "#259: GHC-76037 surfaces submodule suggested_import" testSuggestedImportGhc76037
      , test "#260: importsMatchingPackage detects cross-stanza dep" testDepsCrossStanzaMatch
      , test "#261: pathToModule derives module from path" testArbitraryPathToModule
      , test "#261: renderArbitraryModule emits Wno-orphans module" testArbitraryModuleRender
      , test "resources: rules workflow URI resolves" testResourcesRulesRead
      , test "resources: unknown URI returns Nothing" testResourcesUnknown
      , test "baja bundle: 4 tools registered"      testBajaRegistered
      , test "guidance: tool count is dynamic"      testGuidanceDynamicCount
      , test "guidance: text lists every tool"      testGuidanceListsEveryTool
      , test "guidance: markdown lists every tool"  testGuidanceMarkdownListsEveryTool
      , test "guidance: situation table non-empty"  testGuidanceSituationNonEmpty
      , test "guidance: no phantom ghc_session"    testGuidanceNoPhantomSession
      , test "guidance: text drops retired-subprocess vocab (#56)" testGuidanceNoRetiredVocab
      , test "guidance: markdown drops retired-subprocess vocab (#56)" testGuidanceMdNoRetiredVocab
      , test "guidance: text mentions in-process GHC API (#56)" testGuidanceMentionsApi
      , test "guidance: markdown mentions in-process GHC API (#56)" testGuidanceMdMentionsApi
      , test "#124: guidance text has no retired ghc_regression reference"   testGuidanceNoRetiredRegression
      , test "#124: guidance markdown has no retired ghc_regression reference" testGuidanceMdNoRetiredRegression
      , test "deps: description has no phantom"     testDepsDescriptorNoPhantom
      , test "deps: hint text has no phantom"       testDepsHintNoPhantom
      , test "qcexport: modulePathToModule src"     testExportPathSrc
      , test "qcexport: modulePathToModule lib"     testExportPathLib
      , test "qcexport: modulePathToModule test"    testExportPathTest
      , test "qcexport: modulePathToModule nested"  testExportPathNested
      , test "qcexport: modulePathToModule lowercase rejected" testExportPathLowercaseRejected
      , test "qcexport: modulePathToModule no .hs"  testExportPathNoSuffix
      , test "qcexport: render emits valid imports" testExportRenderValidImports
      , test "qcexport: render drops self-import (#40)" testExportRenderDropsSelfImport
      , test "qcexport: render unions library mods (#40)" testExportRenderUnionsLibMods
      , test "qcexport: render dedupes lib + props (#40)" testExportRenderDedupesLibAndProps
      -- #131: export guard
      , test "#131: exportGuard allows new file"                  testExportGuardNewFile
      , test "#131: exportGuard allows generated file"            testExportGuardGeneratedFile
      , test "#131: exportGuard allows scaffold-generated file"   testExportGuardScaffoldFile
      , test "#131: exportGuard blocks hand-written file"         testExportGuardHandWritten
      , test "#131: exportGuard bypassed with force=true"         testExportGuardForce
      , test "#131: export handle refuses hand-written Spec.hs"   testExportHandleRefusesHandWritten
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
      , test "regression: classifyLoadFailure detects scope (#51)" testRegressionClassifyScope
      , test "regression: classifyLoadFailure detects missing mod (#51)" testRegressionClassifyMissing
      , test "regression: classifyLoadFailure ignores quiet stderr (#51)" testRegressionClassifyQuiet
      , test "regression: classifyLoadFailure passthrough on QcPassed (#51)" testRegressionClassifyPassedPassthrough
      , test "regression: summariseLoadError caps at 600 chars (#51)" testRegressionSummariseCap
      , test "suggest: involutive Low for normalizer" testInvolutiveLowForNormalizer
      , test "suggest: involutive Medium for reverse" testInvolutiveMediumForReverse
      , test "suggest: self-inverse-on-lists Low for normalizer (#73)"
                                                                 testSelfInverseLowForNormalizer
      , test "suggest: self-inverse-on-lists Medium for reverse (#73)"
                                                                 testSelfInverseMediumForReverse
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
      , test "#281: clampRuns caps at maxRuns"      testClampRunsCapsHigh
      , test "#281: clampRuns floors at 1"          testClampRunsFloorsLow
      , test "#281: clampRuns passes through normal" testClampRunsPassThrough
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
      , test "#280: identity differs -> stale"        testStalenessIdentityDiffers
      , test "#280: identity matches -> fresh"         testStalenessIdentityMatches
      , test "#265: progress notification has spec shape" testProgressNotificationShape
      , test "#265: progressToken extracted from _meta"   testProgressTokenPresent
      , test "#265: no _meta -> no progress token"        testProgressTokenAbsent
      , test "#265: collecting sink receives events"      testProgressCollectingSink
      , test "#265: no subscription -> noop sink"         testProgressNoSubscriptionNoop
      , test "workflow: history polls ghc_load"      testHistoryPolling
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
      , test "#210: ghc_arbitrary compile-fail returns status=failed" testArbitraryCompileFailShape
      -- Issue #218 — clear error for wired-in types (Bool, not "not in scope")
      , test "#218: ghc_arbitrary wired-in type gives clear message" testArbitraryWiredInMessage
      -- Issue #219 — hasUnboxedConstructor detects I#/C#/W# primops
      , test "#219: hasUnboxedConstructor detects I#/C#/W#"         testArbitraryHasUnboxedConstructor
      -- Issue #226 — renderTyThing tries all names (not just first)
      , test "#226: renderTyThing uses firstJust over all parseName results" testArbitraryFirstJustSource
      , test "#226: parseTypeParams two-param type"                  testArbitraryTwoParamTemplate
      -- Issue #217 — ghc_goto descriptor mentions compiled-mode limitation
      , test "#217: ghc_goto descriptor acknowledges compiled-mode"  testGhcGotoDescriptorAccurate
      , test "remove_modules: tool registered"        testRemoveModulesRegistered
      , test "remove_modules: strips exposed entry"   testRemoveModulesStripsCabal
      , test "remove_modules: idempotent no-op"       testRemoveModulesIdempotent
      , test "remove_modules: preserves other fields" testRemoveModulesPreservesFields
      , test "#157: remove_modules strips other-modules entry"    testRemoveModulesOtherModules
      , test "#157: remove_modules other-modules idempotent"      testRemoveModulesOtherModulesIdempotent
      , test "#157: remove_modules finds both sections"           testRemoveModulesBothSections
      , test "#248: remove_modules not_found for non-existent module" testRemoveModulesNotFoundField
      , test "nextStep: remove_modules -> check+load" testNextStepRemoveModules
      , test "gate: runStep catches exceptions"       testGateRunStepCatchesExceptions
      , test "gate: cabalStep uses bracket + partial safe" testGateCabalStepBracket
      , test "bootstrap: tool registered"             testBootstrapRegistered
      , test "bootstrap: preview returns dynamic content" testBootstrapPreview
      , test "#193: bootstrap default (no write arg) writes to disk" testBootstrapDefaultWrite
      , test "bootstrap: write persists to disk"      testBootstrapWrite
      , test "bootstrap: pathForHost is closed enum"  testBootstrapPathEnum
      , test "#179: bootstrap write=true nextStep says rules written" testBootstrapWriteNextStep
      , test "#179: bootstrap preview nextStep says re-run with write=true" testBootstrapPreviewNextStep
      , test "doc: main README uses haskell-flows-mcp" testDocsMainReadme
      , test "doc: haskell README lists real tools"   testDocsHaskellReadme
      , test "release: workflow file exists + well-formed" testReleaseWorkflow
      , test "ghc-api: GhcSession boots + exprType roundtrip" testGhcSessionBoots
      , test "ghc-api: HscEnv persists across withGhcSession calls" testGhcSessionPersists
      , test "ghc-api: evalIOString runs IO String actions in-process" testEvalIOString
      , test "ghc-api #80: queryExprType resolves 'id' after autoLoadProject"
                                                                 testQueryExprTypeIdAfterAutoLoad
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
      , test "switch_project: handle reopens store at new root (#39)"
                                                                 testSwitchHandleReopensStore
      , test "F-02: switch_project reopens scratchpad at new root"
                                                                 testSwitchHandleReopensScratchpad
      , test "switch_project: empty dir accepted (scaffold-ready)"
                                                                 testSwitchAcceptsEmpty
      , test "PR-4: parseCabalNameField handles canonical input" testParseCabalNameField
      , test "PR-4: detectSelfProject positive (real cabal name)" testDetectSelfProjectPositive
      , test "PR-4: detectSelfProject negative (other name)"     testDetectSelfProjectNegative
      , test "PR-4: detectSelfProject missing cabal → False"     testDetectSelfProjectMissing
      , test "PR-4: withDogfoodHint fires when self+write+path"  testWithDogfoodHintFiresWhenSelf
      , test "PR-4: withDogfoodHint suppressed when not self"    testWithDogfoodHintNotFiresWhenNotSelf
      , test "PR-4: withDogfoodHint suppressed on read-only tool" testWithDogfoodHintNotFiresOnReadTool
      , test "PR-5: every tool description meets the 6-field template"
                                                                 testDescriptionsMeetTemplate
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
      , test "#147: nameHintsInterpreter filters eval/hash"       testSuggestInterpreterNameGuard
      , test "#147: unrelated sibling skipped by evaluator-preservation" testSuggestEvalPreservNoHash
      , test "#147: namesFormPrinterParserPair recognises pairs"  testSuggestPrinterParserPairNames
      , test "#147: unrelated roundtrip sibling filtered by name" testSuggestRoundtripUnrelatedFiltered
      , test "#159: Either return type gets totality suggestion"   testSuggestEitherTotality
      , test "#159: Either parser roundtrip emits Right x"        testSuggestEitherParserRoundtrip
      , test "#159: Either rule in allRules catalog"              testSuggestEitherRuleRegistered
      , test "ghc-api: external cabal edit invalidates stanza cache"
                                                                 testMtimeInvalidation
      , test "ghc-api: withGhcSession ensures stanza flags (#49)"
                                                                 testWithGhcSessionEnsuresStanza
      , test "ghc-api: absolutizePathArg single-token shapes (#43)"
                                                                 testAbsolutizePathArgSingleToken
      , test "ghc-api: absolutizePathArg eq-form (#43)"           testAbsolutizePathArgEqForm
      , test "ghc-api: absolutizeStanzaFlags two-token pairs (#43)"
                                                                 testAbsolutizeStanzaFlagsTwoToken
      , test "ghc-api: absolutizeStanzaFlags idempotent (#43)"    testAbsolutizeStanzaFlagsIdempotent
      , test "ghc-api: absolutizeStanzaFlags preserves order (#43)"
                                                                 testAbsolutizeStanzaFlagsPreservesOrder
      , test "ghc-api: filterArtifacts drops GHC-58427 with peer (#57)"
                                                                 testFilterArtifactsDropsWithPeer
      , test "ghc-api: filterArtifacts keeps lone GHC-58427 (#57)"
                                                                 testFilterArtifactsKeepsLone
      , test "ghc-api: filterArtifacts noop on empty (#57)"      testFilterArtifactsEmpty
      , test "add_modules: unwraps stringified JSON-array (BUG-PLUS-08)"
                                                                 testAddModulesJsonArrayString
      , test "add_modules: plain comma-split preserved for non-JSON strings"
                                                                 testAddModulesPlainStringStillWorks
      , test "check_module: warnings_block=false keeps warnings informational"
                                                                 testCheckModuleWarningsBlockFalse
      , test "check_module: warnings_block default is True"      testCheckModuleWarningsBlockDefault
      , test "check_module: parseModuleHeader simple (#74)"      testParseHeaderSimple
      , test "check_module: parseModuleHeader multi-segment (#74)"
                                                                 testParseHeaderMultiSegment
      , test "check_module: parseModuleHeader exports + multiline (#74)"
                                                                 testParseHeaderExportsMultiline
      , test "check_module: parseModuleHeader skips pragmas + comments + blanks (#74)"
                                                                 testParseHeaderSkipsLeading
      , test "check_module: parseModuleHeader returns Nothing on missing header (#74)"
                                                                 testParseHeaderNoHeader
      , test "check_module: parseModuleHeader rejects lowercase name (#74)"
                                                                 testParseHeaderInvalidName
      , test "quickcheck: summariseStderr filters cabal noise"   testQcSummariseStderrFiltersNoise
      , test "quickcheck: summariseStderr caps at 1600 chars"    testQcSummariseStderrCaps
      -- Issue #132 — not_in_scope classification
      , test "#132: classifyStderrKind NotInScope on Variable not in scope"  testClassifyStderrNotInScope
      , test "#132: classifyStderrKind SubprocessError on generic error"     testClassifyStderrGeneric
      , test "#186: classifyStderrKind CompileError on GHC error line"       testClassifyStderrCompileError
      , test "#186: classifyStderrKind CompileError on GHC-N error code"     testClassifyStderrCompileErrorCode
      , test "#186: isCompileErrorStderr true on ': error:' pattern"         testIsCompileErrorStderrTrue
      , test "#186: isCompileErrorStderr false on generic message"           testIsCompileErrorStderrFalse
      , test "#132: extractNotInScopeSymbol extracts bare name"              testExtractNisBareName
      , test "#132: extractNotInScopeSymbol extracts name before type sig"   testExtractNisTypeSig
      , test "#132: extractNotInScopeSymbol returns Nothing for other text"  testExtractNisAbsent
      , test "#132: summariseStderr strips -Wmissing-home-modules lines"     testQcSummariseStripsWmhm
      , test "nextStep: ghc_load with typed-hole warning \8594 ghc_hole"
                                                                 testNextStepTypedHoleWarn
      , test "nextStep: ghc_load with non-hole warning \8594 ghc_fix_warning"
                                                                 testNextStepFixableWarn
      , test "nextStep: ghc_load with no warnings \8594 ghc_suggest"
                                                                 testNextStepCleanLoad
      -- Issue #98 Phase B · structured logging
      , test "#98B: Logging · redaction truncates strings > 40 chars"
                                                                 testLoggingRedactionPolicy
      , test "#98B: Logging · trace_id is 6 lowercase hex chars"
                                                                 testLoggingTraceIdGeneration
      , test "#98D: Logging · audit path absent when HASKELL_FLOWS_AUDIT unset"
                                                                 testLoggingAuditPathAbsentByDefault
      , test "#98D: Logging · audit path present when HASKELL_FLOWS_AUDIT=1"
                                                                 testLoggingAuditPathPresentWhenEnabled
      -- Issue #96 Phase A · performance budget scaffold
      , test "#96A: Budget · every ToolName has an entry"         testBudgetParsesCleanly
      , test "#96A: Budget · no budget is 0 ms"                   testBudgetNoZeroValues
      , test "#96A: Runner · discardFirst drops cold-start sample" testRunnerDiscardFirstSample
      -- Issue #95 Phase D · nextStep quality gates
      , test "#95D: nextStep Gate D — why ≥ 10 chars + ends in period" testNextStepGateDWhyQuality
      , test "#95D: nextStep Gate E — chain ≤ 4 steps"               testNextStepGateEChainLength
      -- Issue #95 Phase C · golden dispatch snapshot
      , test "#95C: nextStep golden dispatch table"                    testNextStepGoldenDispatch
      -- Issue #94 Phase A · tool taxonomy invariants
      , test "#94A: tool count ≤ 50 (surface-bloat cap)"              testToolCountWithinCap
      , test "#94A: every ToolName has a category"                     testEveryToolHasCategory
      , test "#94A: category counts match taxonomy"                    testCategoryCountsMatchTaxonomy
      , test "#268: TOOL_TAXONOMY.md lists every registered tool"      testTaxonomyDocListsAllTools
      -- Issue #99 Phase B · per-tool version surface
      , test "#99B: every ToolName has a non-empty version"           testEveryToolHasVersion
      , test "#99B: every tool version is valid semver triple"        testToolVersionIsSemverTriple
      -- Issue #94 Phase B · action-discriminated 'modules' primitive
      , test "#94B: ghc_modules registered with category=primitive"   testModulesRegistered
      , test "#94B: ghc_modules rejects unknown action"               testModulesRejectsBadAction
      -- Issue #105: extractModules envelope peeling
      , test "#105: extractModules reads hits inside result envelope"  testExtractModulesEnvelope
      , test "#105: extractModules ignores wrong key 'results'"        testExtractModulesTopLevel
      -- Issue #204: Internal module filter + exact-match priority
      , test "#204: filterInternal removes .Internal modules"         testFilterInternalRemoves
      , test "#204: filterInternal keeps public modules"              testFilterInternalKeeps
      , test "#204: prioritizeModuleMatch exact match first"          testPrioritizeExactFirst
      , test "#204: prioritizeModuleMatch no-op for non-dotted query" testPrioritizeNoDotNoOp
      -- Issue #242 — looksLikeModule bypasses Hoogle for module-path names
      , test "#242: looksLikeModule true for Data.Map"               testLooksLikeModuleTrue
      , test "#242: looksLikeModule false for bare name"             testLooksLikeModuleFalse
      , test "#242: looksLikeModule false for qualified function"    testLooksLikeModuleQualFun
      , test "#242: looksLikeModule false for single-component"      testLooksLikeModuleSingle
      , test "#242: looksLikeModule true for 3-component path"       testLooksLikeModuleThree
      -- Issue #104c: injectTypeAnnotations safety-net
      , test "#104c: injectTypeAnnotations annotates bare x"          testInjectAnnotateBareX
      , test "#104c: injectTypeAnnotations annotates xs as list"      testInjectAnnotateXs
      , test "#104c: injectTypeAnnotations passes through annotated"   testInjectAnnotateAlreadyAnnotated
      , test "#104c: injectTypeAnnotations no-op on non-lambda"        testInjectAnnotateNonLambda
      -- Issue #215: eta-reduced export (no redundant lambda)
      , test "#215: etaReduceLambda bare param"                       testEtaReduceBare
      , test "#215: etaReduceLambda annotated param"                  testEtaReduceAnnotated
      , test "#215: etaReduceLambda list param"                       testEtaReduceList
      , test "#215: etaReduceLambda nested arrow in type is safe"     testEtaReduceNestedArrow
      , test "#215: etaReduceLambda non-lambda returns Nothing"       testEtaReduceNonLambda
      , test "#215: renderPropBinding emits no redundant lambda"      testRenderPropNoLambda
      , test "#215: renderTestFile emits no '= \\\\' pattern"         testRenderTestFileNoLambdaAssign
      -- Issue #215 (GHC-18042 type-default fixes) — splitAtDepthZeroSpaces regression
      , test "#215/td: splitAtDepthZeroSpaces multi-param (GHC-18042 fix)" testSplitAtDepthZeroIssue215
      -- Issue #198 — stale tool name + missing type signatures
      , test "#231: renderTestFile emits OPTIONS_GHC pragma suppressing unused-imports and missing-sigs" testExportOptionsGhcPragma
      , test "#198: generatedHeader says ghc_property_store not ghc_quickcheck_export" testExportHeaderCurrentToolName
      , test "#198: renderPropSignature emits sig for annotated single param"  testRenderPropSigSingle
      , test "#198: renderPropSignature emits sig for annotated multi param"   testRenderPropSigMulti
      , test "#198: renderPropSignature returns Nothing for unannotated param" testRenderPropSigNone
      , test "#198: renderTestFile emits type sig before prop binding"         testRenderTestFileSigPresent
      , test "#104c: injectTypeAnnotations multi-param x y"           testInjectAnnotateMultiParam
      , test "#172: injectTypeAnnotations leaves String-constrained x verbatim"
                                                                       testInjectAnnotateStringConstrained
      , test "#172: injectTypeAnnotations still annotates operator-only x with Int"
                                                                       testInjectAnnotateOperatorOnlyX
      -- Issue #104a: Suggest/Rules.hs annotated lambda output
      , test "#104a: suggest idempotent rule emits :: Int annotation"  testSuggestIdempotentAnnotated
      , test "#104a: suggest involutive rule emits :: Int annotation"  testSuggestInvolutiveAnnotated
      -- Issue #103: extractHaddockAbove source fallback
      , test "#103: extractHaddockAbove finds -- | comment"           testExtractHaddockFindsDoc
      , test "#103: extractHaddockAbove returns Nothing for plain --" testExtractHaddockNoHaddock
      , test "#103: extractHaddockAbove returns Nothing for no comment" testExtractHaddockNoComment
      -- Issue #195 — type-sig skip + nextStep routing
      , test "#195: extractHaddockAbove finds -- | above type signature" testExtractHaddockAboveTypeSig
      , test "#195: noDocInScopePayload has found_in_scope=true"        testNoDocInScopePayloadShape
      , test "#195: hasDocFalse returns True for noDocInScopePayload"   testHasDocFalseDirectly
      , test "#195: nextStep for doc ok+hasDoc=false routes to ghc_info" testDocNoDocNextStepIsInfo
      -- Issue #106 sub-findings
      , test "#106/F-14: mkGhcError propagates code from captureHook" testMkGhcErrorCode
      , test "#180: stripGhcInternalQual removes ghc-internal prefix"  testStripGhcInternalQual
      , test "#180: stripGhcInternalQual multiple occurrences"          testStripGhcInternalQualMulti
      , test "#180: stripGhcInternalQual leaves normal text untouched"  testStripGhcInternalQualNoop
      , test "#106/F-17: previewResult omits patch key when dropLine" testFixWarnNoPatchKey
      , test "#106/F-07: error remediation uses ghc_project(action=create)" testRemediationToolName
      , test "#106/F-20: parseHoogleLine populates hhName field" testHoogleHitName
      , test "#106/F-19: hitsPayload deduplicates by module+signature" testHoogleDedup
      -- Issue #139 — no_match must not set isError
      , test "#139: StatusNoMatch is not a failing status (isError=false)" testNoMatchIsNotFailing
      , test "#139: hoogle_search no-results renderResult has isError=false" testHoogleNoMatchIsError
      -- Issue #158 — 'count' alias for 'limit' must not be silently dropped
      , test "#158: hoogle_search FromJSON accepts 'count' as alias for 'limit'" testHoogleCountAlias
      , test "#106/F-25: ghc_explain_error drops redundant module_source" testExplainErrorNoModuleSource
      , test "#153: ghc_explain_error error_text used directly (not ignored)"
                                                           testExplainErrorTextUsed
      , test "#106/F-11: ghc_doc strips LaTeX delimiters" testDocStripLatex
      , test "#144: ghc_doc strips spurious [ ] brackets from doc string"
                                                           testDocStripBrackets
      , test "#106/F-26: ghc_perf omits samples by default" testPerfSamplesGated
      , test "#106/F-32: ghc_perf regression cause is plain text" testPerfRegressionCausePlain
      , test "#106/F-08: ghc_deps list all stanzas returns stanzas map" testDepsListAllStanzas
      , test "#106/F-34: moduleNameToPath accepts file paths without mangling" testMoveModuleNameToPath
      , test "#106/F-12: ioUnitResult has kind=io_unit_no_output" testEvalIoUnitResult
      , test "#167: ioUnitResult hint is not circular (no putStrLn advice)" testEvalIoUnitHintNotCircular
      , test "#182: evalIOUnitCapture actually captures putStrLn output"    testCaptureStdoutActuallyCaptures
      , test "#194: evalIOUnitCapture via GHC session captures putStrLn"    testEvalIOUnitCaptureViaSess
      , test "#106/F-23: mergeDiags prefers deferred version at same position" testLoadMergeDiagsPreferDeferred
      , test "#106/F-04: toolchain warmup includes gates in response" testWarmupIncludesGates
      , test "#106/F-01: classifyPhase stays PreScaffold beyond 3 calls w/o load" testClassifyPhaseNoLoad
      , test "#106/F-24: enclosingLineRange padding 15 doesn't return whole file" testEnclosingRangePadding
      , test "#106/F-09: parseRejections splits comma-separated versions" testDepsExplainRejectionSplit
      , test "#106/F-06: gitRootOf walks up to .git directory" testBootstrapGitRoot
      , test "#106/F-02: pickModuleLine extracts exposed-modules" testWorkflowPickModuleLine
      , test "#106/F-10: importsPayload has session_preloads field" testImportsHasSessionPreloads
      , test "#106/F-21: compileFailResult has status=failed and dry_run=false" testRefactorCompileFailShape
      , test "#205: compileFailResult dry_run=true propagates to result field"  testRefactorCompileFailDryRunTrue
      , test "#205: extractFreeVarNames picks up not-in-scope variables"        testExtractFreeVarNames
      , test "#205: extractFreeVarNames empty when no not-in-scope errors"      testExtractFreeVarNamesEmpty
      , test "#205: compileFailResult adds note for free-variable errors"       testRefactorFreeVarNote
      , test "#201: extractQcOutputAt slices indexed sentinel output"          testExtractQcOutputAt
      , test "#201: extractQcOutputAt returns empty when sentinel absent"       testExtractQcOutputAtMissing
      , test "#201: qcResultDetail formats counterexample for QcFailed"        testQcResultDetailFailed
      , test "#201: qcResultDetail returns empty for QcPassed"                 testQcResultDetailPassed
      , test "#201: qcResultStatus covers all five constructors"               testQcResultStatusAll
      , test "#237: qcResultStatus QcUnparsed stack-overflow → exception"     testQcResultStatusStackOverflow237
      , test "#237: qcResultStatus QcUnparsed heap-overflow → exception"      testQcResultStatusHeapOverflow237
      , test "#237: qcResultStatus QcUnparsed other raw → unparsed"           testQcResultStatusOtherUnparsed237
      , test "#237: qcResultDetail QcUnparsed non-empty raw → surfaces raw"   testQcResultDetailUnparsedNonEmpty237
      , test "#237: qcResultDetail QcUnparsed empty raw → empty string"       testQcResultDetailUnparsedEmpty237
      , test "#106/F-31: perf renderResult with all errors returns failed" testPerfAllSamplesErrored
      , test "#162: perf renderResult exposes warmup_ns field"            testPerfWarmupNsInPayload
      , test "#162: perf warm samples exclude warmup from mean"           testPerfWarmSamplesNotSkewed
      , test "#136: readBaseline uses strict I/O (no lazy file handle)"    testPerfReadBaselineStrict
      , test "#136: save+compare both flags work sequentially (no lock)"   testPerfSaveAndCompareNoLock
      , test "#161: saveBaseline save→save sequence does not lock"          testPerfSaveBaselinesNoLock
      , test "#174: perf default threshold is 30%, not 10%"               testPerfDefaultThreshold30
      , test "#174: threshold_pct param overrides default"                testPerfCustomThreshold
      , test "#190: perf regression returns status=failed + kind=regression"  testPerfRegressionStatusFailed
      , test "#174: perf threshold_pct clamped to [1,200]"               testPerfThresholdClamped
      -- Issue #200 — regression_pct precision
      , test "#200: roundTo1dp rounds to 1 decimal place"                testPerfRoundTo1dp
      , test "#200: regression_pct in payload has at most 1 decimal"     testPerfRegressionPctPrecision
      , test "#223: perf runtime exception gets 'threw' message not 'module lost'"
                                                                          testPerfRuntimeExceptionMessage
      -- Issue #135 — summariseMeasurementErrors truncation
      , test "#135: summariseMeasurementErrors single error stays short"   testSummariseSingleError
      , test "#135: summariseMeasurementErrors 20 repeated errors omits"  testSummariseRepeatedErrors
      , test "#135: summariseMeasurementErrors long error truncates"       testSummariseLongError
      , test "#135: summariseMeasurementErrors empty list is empty"        testSummariseEmptyList
      , test "#213: check_module holes.reason reflects count when holes present" testCheckModuleHolesReasonCount
      -- Issue #108 — typed-hole reclassification in check_module + refactor
      , test "#108: check_module compileOk true when only hole errors"   testCheckModuleHoleOnlyCompileOk
      , test "#108: check_module realErrors excludes GHC-88464"          testCheckModuleRealErrorsExcludesHoles
      , test "#108: refactor commitResultWithDiff excludes holes from pre_existing_errors"
                                                                         testRefactorPreExistingHolesExcluded
      -- Issue #188 — check_module uses loadSpecificFileForTarget
      , test "#188: check_module uses loadSpecificFileForTarget not loadForTarget" testCheckModuleUsesSpecificLoader
      -- Issue #109 — .cabal comment-stripping in check_project
      , test "#109: parseExposedModules strips inline -- comments"        testParseExposedModulesStripsComments
      , test "#109: parseExposedModules rejects tokens with punctuation"  testParseExposedModulesRejectsPunct
      -- Issue #107 — ghc_info renderDefinition for functions
      , test "#107: renderDefinition AnId produces name :: type"          testInfoAnIdDefinition
      -- Issue #130 — eponymous record TyCon selection
      , test "#130: preferTyCon present in Info.hs source"                testInfoPreferTyConInSource
      , test "#130: queryInfo uses preferTyCon not first-name shortcut"   testInfoQueryUsesPreferTyCon
      -- Issue #184 — AConLike data constructors render as "Name :: Type"
      , test "#184: AConLike branch exists in Info.hs (not catch-all)"    testInfoAConLikeBranchExists
      , test "#184: AConLike uses dataConDisplayType not renderDefinition" testInfoAConLikeUsesDisplayType
      -- Issue #111 — forall stripping in parseSignature
      , test "#111: stripForall handles forall {a}."                      testStripForallInferred
      , test "#111: stripForall handles forall a."                        testStripForallExplicit
      , test "#111: stripForall noop when no forall"                      testStripForallNoop
      , test "#111: parseSignature handles forall {a}. [a] -> [a]"        testParseSigForallList
      , test "#111: rules fire for forall-prefixed reverse signature"     testRulesFireForForallReverse
      -- Issue #137 — Haddock comment stripping in parseSignature
      , test "#137: stripLineComments strips -- ^ Haddock annotation"     testStripLineCommentsHaddock
      , test "#137: stripLineComments strips mid-line -- comment"         testStripLineCommentsMid
      , test "#137: stripLineComments preserves comment-free lines"       testStripLineCommentsClean
      , test "#137: parseSignature handles multiline Haddock-annotated sig" testParseSigHaddockAnnotated
      -- Issue #116 — GHC-66111 category correction in Error.hs
      , test "#116: GHC-66111 routes to WcUnused, not WcDeferredError"    testGhc66111RoutesToUnused
      -- Issue #115 — RuntimeException kind in Envelope.hs + Eval.hs
      , test "#115: Env.RuntimeException exists in enum + wire form"       testRuntimeExceptionKindExists
      -- Issue #117 — ghc_goto InModule returns no_match + has_location
      , test "#117: goto InModule gives no_match + has_location=false"     testGotoLibraryNameNoMatch
      , test "#117: goto InFile gives ok + has_location=true"              testGotoFileHasLocation
      -- Issue #214 — remediation must not claim "no local source file"
      , test "#214: goto InModule remediation says compiled not no-source"  testGotoCompiledModuleRemediation
      -- Issue #224 — qualified preload gives misleading remediation
      , test "#224: qualifiedPreloadPayload names unqualified form and module prefix"
                                                               testGotoQualifiedPreloadPayload
      -- Issue #208 — gate nextStep text must reflect actual steps run
      , test "#208: gate nextStep text uses payload summary not hardcoded names" testGateNextStepTextFromSummary
      -- Issue #118 — removeDep drops blank continuation lines
      , test "#118: removeDep no trailing blank on single-dep line"        testRemoveDepNoTrailingBlank
      , test "#118: removeDep preserves multi-dep block correctly"         testRemoveDepMultiDep
      -- Issue #112 — PropertyAudit pair-probe uses Nothing module context
      , test "#112: contradiction probe is self-contained (no module ref)" testAuditPairProbeIsModuleAgnostic
      -- Issue #113 — Regression cross-stanza retry fallback
      , test "#113: cross-stanza stderr triggers load-failure classification" testRegressionCrossStanzaRetryClassification
      , test "#113: QcPassed with quiet stderr does not trigger retry"     testRegressionSelfContainedNoRetry
      -- Issue #114 — ghc_imports dedup via nubBy importKey
      , test "#114: nubBy dedup removes duplicate module keys"             testImportsNubByDeduplication
      -- Issue #119 — DX paper-cuts batch
      , test "#119: Env.GateFailure exists in enum + wire form"            testGateFailureKindExists
      , test "#119: removeDep unchangedResult has no verb field"           testUnchangedResultNoVerb
      , test "#119: formatIso8601 produces ISO-8601 timestamp"             testFormatIso8601
      , test "#119: ValidateCabal warnings-only returns ok not partial"    testValidateCabalWarningsOk
      -- Issue #110 — ghc_load outside hs-source-dirs validation
      , test "#110: parseHsSourceDirs single stanza"                      testParseHsSourceDirsSingle
      , test "#110: parseHsSourceDirs multiple stanzas union"             testParseHsSourceDirsMultipleStanzas
      , test "#110: parseHsSourceDirs empty → no field found"             testParseHsSourceDirsEmpty
      , test "#110: isUnderAnySourceDir positive and negative"            testIsUnderAnySourceDir
      , test "#110: isUnderAnySourceDir dot matches everything"           testIsUnderAnySourceDirDot
      , test "#110: Env.OutsideSourceDirs exists in enum + wire form"     testOutsideSourceDirsKindExists
      -- Issue #166 — ghc_load must not pick up unregistered src/ files
      , test "#166: ghc_load with module_path ignores stray unregistered files"
                                                              testLoadSpecificFileIgnoresStray
      , test "#166: loadSpecificFileForTarget exported from ApiSession"
                                                              testLoadSpecificFileExported
      -- Issue #232 — ghc_check_module stale-cache warning gap
      , test "#232: StrictFresh is distinct from Strict and Deferred"
                                                              testStrictFreshIsDistinct
      -- Issue #181 — session left broken after ghc_load with compile errors
      , test "#181: resetHscEnvInPlace clears loaded flag"    testResetHscEnvInPlaceClearsLoaded
      , test "#181: resetHscEnvInPlace is no-op on fresh session" testResetHscEnvInPlaceFreshSession
      , test "#181: all 4 load paths have reset-on-failure guard" testLoadPathsHaveResetGuard
      -- Issue #193 — autoLoadProject must not include broken modules in context
      , test "#193: autoLoadProject sets Prelude-only context on failed load (source check)" testAutoLoadFailedBranch
      -- Issue #194 — targetForPath prefix must match flat test/Foo.hs paths
      , test "#194: targetForPath prefix matches flat test/Foo.hs" testTargetForPathFlatFile
      , test "#194: targetForPath prefix matches nested test/foo/Bar.hs" testTargetForPathNestedFile
      , test "#194: targetForPath falls back to library for src/Foo.hs" testTargetForPathLibFallback
      -- Issue #129 — ghc_check_project deadline-based timeout
      , test "#191: check_project delegates to check_module (no own loadForTarget)" testCheckProjectDelegates
      , test "#129: CheckProjectArgs defaults timeout_seconds to 120"    testCheckProjectArgsDefaultTimeout
      , test "#129: renderResult timedOut=True adds timed_out field"     testRenderResultTimedOutFlag
      , test "#129: renderResult timedOut=True lists timed_out_modules"  testRenderResultTimedOutModules
      , test "#129: renderResult timedOut=False omits timed_out field"   testRenderResultNoTimedOutField
      , test "#129: MoTimedOut renders with status=timed_out"            testRenderOutcomeTimedOut
      , test "#129: renderResult overall=false when any module timed out" testRenderResultTimedOutOverallFalse
      , test "#151: timeout summary does not claim 'N/N green' when only k<N checked"
                                                              testRenderResultTimedOutSummary
      , test "#255: check_project mixed results -> status:partial"  testCheckProjectPartialStatus
      , test "#254: lab no-template-matched reason"  testLabNoTemplateMatchedReason
      , test "#254: lab low-confidence reason"       testLabLowConfidenceReason
      , test "#250: renderRunLine uses module name not path"  testRenderRunLineUsesModuleName
      , test "#244: findCommonStanzaWithPkg finds stanza containing pkg" testDepsCommonStanzaPkgFound
      , test "#244: findCommonStanzaWithPkg returns Nothing when pkg absent" testDepsCommonStanzaPkgAbsent
      , test "#244: findCommonStanzaWithPkg returns Nothing when no common stanzas" testDepsCommonStanzaNoCommon
      , test "#244: unchangedResult' emits hint field when mHint=Just" testDepsUnchangedResultHintField
      , test "#243: suggest.hs imports and calls augmentEvalContext"   testSuggestCallsAugmentContext
      , test "#242: add_import bypasses Hoogle for module-path names (source check)" testAddImportBypassesHoogle
      , test "#245: renderResult emits low_precision_warning when mean < 1ms" testPerfLowPrecisionWarning
      , test "#245: renderResult emits warmup_warning when warmup >10x mean"  testPerfWarmupWarning
      , test "#245: renderResult no warnings for healthy 5ms mean"            testPerfNoWarningHealthy
      -- Issue #289 — eliminate partial functions; Util.Safe totality
      , test "#289: safeAt returns Nothing for negative index"        testSafeAtNegative
      , test "#289: safeAt returns Nothing for out-of-bounds index"   testSafeAtOutOfBounds
      , test "#289: safeAt returns Just for valid index"              testSafeAtHit
      , test "#289: safeHead returns Nothing on empty list"           testSafeHeadEmpty
      , test "#289: safeHead returns Just on non-empty list"          testSafeHeadNonEmpty
      , test "#289: safeLast returns Nothing on empty list"           testSafeLastEmpty
      , test "#289: safeLast returns Just on non-empty list"          testSafeLastNonEmpty
      , test "#289: initLast returns Nothing on empty list"           testInitLastEmpty
      , test "#289: initLast returns Just ([],x) on singleton"        testInitLastSingleton
      , test "#289: initLast returns Just (init,last) on multi"       testInitLastMulti
      , test "#289: parseSignature empty input is total"              testParseSignatureEmpty
      , test "#289: parseSignature singleton input is total"          testParseSignatureSingleton
      , test "#289: splitModule empty input is total"                 testSplitModuleEmpty
      , test "#289: splitModule singleton input is total"             testSplitModuleSingleton
      , test "#289: computePercentile empty list returns 0"           testComputePercentileEmpty
      , test "#289: aggregate empty list is total"                    testAggregateEmpty
      -- Issue #287 — centralize timeouts/caps in HaskellFlows.Config
      , test "#287: seconds n = n * 1_000_000 microseconds"           testMicrosSeconds
      , test "#287: minutes n = n * 60_000_000 microseconds"          testMicrosMinutes
      , test "#287: Micros Ord compares by underlying Int"            testMicrosOrd
      , test "#287: Micros Eq compares by underlying Int"             testMicrosEq
      , test "#287: defaultLimits hoogleTimeout = 10 s"               testDefaultHoogleTimeout
      , test "#287: defaultLimits hlintTimeout = 60 s"                testDefaultHlintTimeout
      , test "#287: defaultLimits formatTimeout = 30 s"               testDefaultFormatTimeout
      , test "#287: defaultLimits cabalCheckTimeout = 30 s"           testDefaultCabalCheckTimeout
      , test "#287: defaultLimits versionTimeout = 3 s"               testDefaultVersionTimeout
      , test "#287: defaultLimits evalTimeout = 30 s"                 testDefaultEvalTimeout
      , test "#287: defaultLimits quickCheckTimeout = 30 s"           testDefaultQuickCheckTimeout
      , test "#287: defaultLimits replayTimeout = 30 s"               testDefaultReplayTimeout
      , test "#287: defaultLimits outerToolCeiling = 10 min"          testDefaultOuterToolCeiling
      , test "#287: defaultLimits determinismMaxRuns = 20"            testDefaultDeterminismMaxRuns
      , test "#287: defaultLimits quickCheckMaxSuccess = 300"         testDefaultQcMaxSuccess
      , test "#287: defaultLimits evalOutputCapBytes = 64 KiB"        testDefaultEvalOutputCap
      , test "#287: defaultLimits gateOutputCapBytes = 256 KiB"        testDefaultGateOutputCap
      , test "#287: defaultLimits checkProjectTimeout = 600 s"         testDefaultCheckProjectTimeout
      , test "#287: loadLimits falls back to default when env absent"  testLoadLimitsMissingEnvFallback
      , test "#287: loadLimits overrides from valid env var"           testLoadLimitsValidEnvOverride
      , test "#287: loadLimits falls back on non-numeric env var"      testLoadLimitsInvalidEnvFallback
      , test "#287: loadLimits falls back on zero env var"             testLoadLimitsZeroEnvFallback
      , test "#287: loadLimits falls back on negative env var"         testLoadLimitsNegativeEnvFallback
      , test "#287: loadLimits overrides checkProjectTimeout via env"  testLoadLimitsCheckProjectOverride
      , test "#287: loadLimits overrides outerToolCeiling via env"     testLoadLimitsOuterCeilingOverride
      , test "#287: loadLimits does not touch GHC-session fields"      testLoadLimitsGhcSessionFieldsUnchanged
      -- Issue #285 — uniform ToolEnv dispatch table
      , test "#285: handlerFor covers every ToolName (exhaustive)"     testHandlerForExhaustive
      , test "#285: handlerFor GhcQuickCheck returns handler (QcTool)" testHandlerForGhcQuickCheckIsQcTool
      , test "#285: ToolEnv construction is total (no strict crash)"   testMkToolEnvFields
      -- Issue #286 — ToolSpec registry single source of truth
      , test "#286: registry is total — one ToolSpec per ToolName"         testRegistryTotalOverToolName
      , test "#286: registry has no duplicate ToolName entries"            testRegistryNoDuplicateNames
      , test "#286: Registry.allBudgets keys agree with Budget.allBudgets" testRegistryBudgetKeysAgree
      , test "#286: toolCategory is total over every ToolName"             testToolCategoryTotalOverToolName
      , test "#286: Registry.toolCategory agrees with ToolName.toolCategory" testToolCategoryAgreesWithToolName
      -- Issue #288 — shared runArgv subprocess combinator
      , test "#288: runArgv completes — echo exits 0 with expected stdout" testRunArgvCompletes
      , test "#288: runArgv timeout — sleep 60 killed within 2s budget"    testRunArgvTimeout
      , test "#288: runArgv non-zero — false exits with ExitFailure"       testRunArgvNonZeroExit
      ]
      ++ scratchTests
  if and results then exitSuccess else exitFailure

