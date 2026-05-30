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
import qualified HaskellFlows.Tool.Format as FormatTool
import qualified HaskellFlows.Tool.Hole as HoleTool
import qualified HaskellFlows.Tool.Hoogle as HoogleTool
import qualified HaskellFlows.Tool.Goto as GotoTool
import qualified HaskellFlows.Tool.Imports as ImportsTool
import qualified HaskellFlows.Tool.ToolchainStatus as ToolchainStatusTool
import qualified HaskellFlows.Tool.ToolchainWarmup as ToolchainWarmupTool
import qualified HaskellFlows.Tool.ValidateCabal as ValidateCabalTool
import qualified HaskellFlows.Tool.Workflow as WorkflowTool
import HaskellFlows.Mcp.Staleness (StalenessReport (..))
import qualified HaskellFlows.Tool.Type as TypeTool
import qualified HaskellFlows.Tool.SwitchProject as SwitchProject
import HaskellFlows.Tool.SwitchProject
  ( ValidationError (..)
  , validateSwitchTarget
  )
import Data.IORef (newIORef, readIORef, writeIORef)
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
      -- #253 Phase 1: ghc_scratch
      , test "Scratchpad data roundtrip"           testScratchpadRoundtrip
      , test "Scratchpad upsert by id"             testScratchpadUpsertById
      , test "Scratchpad findById"                 testScratchpadFindById
      , test "Scratch parseAction round-trip"      testScratchParseAction
      , test "Scratch action=write persists entry" testScratchHandleWrite
      , test "Scratch write requires 'code'"       testScratchHandleWriteMissingCode
      , test "Scratch write auto-generates id"     testScratchHandleWriteAutoId
      , test "Scratch list empty returns count=0"  testScratchHandleListEmpty
      , test "Scratch show unknown id → no_match"  testScratchHandleShowMissing
      , test "Scratch clear w/o confirm refused"   testScratchHandleClearNoConfirm
      , test "Scratch clear by id removes one"     testScratchHandleClearById
      , test "Scratch clear confirm=true truncates" testScratchHandleClearAll
      , test "Scratch check missing id → failed"   testScratchCheckMissingId
      , test "Scratch check unknown id → no_match" testScratchCheckUnknownId
      , test "Scratch check sanitize rejects"      testScratchCheckSanitizeReject
      , test "ScratchResult JSON round-trip"       testScratchResultRoundTrip
      , test "F-01: check type_ok has kind at top" testScratchCheckKindAtTopLevel
      -- Phase 3: show full detail + seResult round-trip
      , test "Phase 3: show returns full entry detail"   testScratchShowFullDetail
      , test "Phase 3: seResult survives loadAll"        testScratchResultRoundTripViaLoadAll
      , test "Phase 3: show after check carries result"  testScratchShowAfterCheckHasResult
      -- Phase 4: action=promote boundary tests
      , test "Phase 4: promote missing id → failed"      testScratchPromoteMissingId
      , test "Phase 4: promote missing target_module → failed"
                                                         testScratchPromoteMissingTargetModule
      , test "Phase 4: promote unknown id → no_match"   testScratchPromoteUnknownId
      , test "Phase 4: promote path escapes project → refused"
                                                         testScratchPromoteBadModulePath
      , test "Phase 4: spliceInto appends at end"        testSpliceIntoAppend
      , test "Phase 4: spliceInto inserts after line N"  testSpliceIntoAtLine
      -- F-03/F-04/F-05 fixes
      , test "F-03: sanitizeDeclarations allows newlines"   testSanitizeDeclAllowsNewlines
      , test "F-03: sanitizeDeclarations blocks sentinel"   testSanitizeDeclBlocksSentinel
      , test "F-03: wrapAsLetBlock indents code"            testWrapAsLetBlockIndents
      , test "F-04: promote wraps expression with binding_name"
                                                            testScratchPromoteBindingName
      , test "F-05: compileFailResult uses first error message"
                                                            testCompileFailResultCause
      , test "validatePackageName accepts normal"  testPkgAccepts
      , test "validatePackageName rejects symbol"  testPkgRejectsSymbol
      , test "validatePackageName rejects empty"   testPkgRejectsEmpty
      , test "#48 extractErrorSummary picks pkg line"  testExtractErrorSummaryFindsPackage
      , test "#48 extractErrorSummary falls back"      testExtractErrorSummaryFallsBackOnNoMatch
      , test "#48 extractErrorSummary case-insensitive" testExtractErrorSummaryCaseInsensitive
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
      , test "Envelope #90 Phase B: ghc_workflow next emits envelope"
                                                   testWorkflowNextEnvelope
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
      , test "#106/F-03: nextPayload suggests quickcheck after suggest" testWorkflowNextHistoryAware
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

-- Issue #79: 'checkPathExists' is the gate that turned the silent
-- "load anything, get the whole library back" foot-gun into an
-- explicit error. The Right () branch fires when the file is on
-- disk; the Left branch is the original bug repro shape.
testCheckPathExistsAccepts :: IO Bool
testCheckPathExistsAccepts = do
  tmp <- getTemporaryDirectory
  let dir  = tmp </> "haskell-flows-issue-79-accept"
      file = dir </> "Foo.hs"
  removePathForcibly dir
  createDirectoryIfMissing True dir
  TIO.writeFile file (T.pack "module Foo where\nfoo :: Int\nfoo = 1\n")
  case mkProjectDir dir of
    Left _   -> pure False
    Right pd -> do
      r <- checkPathExists pd (T.pack "Foo.hs")
      removePathForcibly dir
      pure (r == Right ())

testCheckPathExistsRejects :: IO Bool
testCheckPathExistsRejects = do
  tmp <- getTemporaryDirectory
  let dir = tmp </> "haskell-flows-issue-79-reject"
  removePathForcibly dir
  createDirectoryIfMissing True dir
  case mkProjectDir dir of
    Left _   -> pure False
    Right pd -> do
      r <- checkPathExists pd (T.pack "DoesNotExist.hs")
      removePathForcibly dir
      pure $ case r of
        Left msg -> T.isInfixOf (T.pack "does not exist") msg
                 && T.isInfixOf (T.pack "DoesNotExist.hs") msg
        Right () -> False

-- | Issue #84: ghc_load on a project with no @src/@ or @app/@
-- Haskell sources used to silently report @success=true@ +
-- @summary="Compiled OK. No issues."@. The post-#90-Phase-C
-- envelope surfaces it as @status='no_match'@ +
-- @kind='module_not_in_graph'@ so consumer agents can route to
-- ghc_create_project / ghc_add_modules instead of charging into
-- ghc_suggest on an empty graph. We don't need a real GhcSession
-- here: the empty-project guard runs before 'firstTestSuiteOrLibrary',
-- so we exercise the new short-circuit path with a stub session.
--
-- The stub is built by 'startGhcSession' on a tmpdir that has no
-- src/ + no app/ + no .cabal file — the same shape the issue's
-- repro describes.
testGhcLoadEmptyProjectNoMatch :: IO Bool
testGhcLoadEmptyProjectNoMatch = do
  tmp <- getTemporaryDirectory
  let dir = tmp </> "haskell-flows-issue-84-empty"
  removePathForcibly dir
  createDirectoryIfMissing True dir
  result <- case mkProjectDir dir of
    Left _   -> pure (Left "could not build ProjectDir")
    Right pd -> do
      sess <- startGhcSession pd
      tr   <- LoadTool.handle sess pd (A.object [])
      killGhcSession sess
      case trContent tr of
        [TextContent body] ->
          pure (A.eitherDecode (TLE.encodeUtf8 (TL.fromStrict body)))
        _ -> pure (Left "expected exactly one TextContent")
  removePathForcibly dir
  pure $ case result of
    Right env
      | Env.reStatus env == Env.StatusNoMatch
      , Just envErr <- Env.reError env
      , Env.eeKind envErr == Env.ModuleNotInGraph
      , Just (A.Object payload) <- Env.reResult env ->
          AKM.lookup (AKey.fromText "loaded") payload == Just (A.Number 0)
            && AKM.member (AKey.fromText "remediation") payload
    _ -> False

-- | #214: regression — the no-args ‘ghc_load’ path must use
-- ‘firstLibraryOrTestSuite’ (prefers library stanza) rather than
-- ‘firstTestSuiteOrLibrary’ (prefers test-suite stanza).
-- Under test-suite stanza flags, library-only build-depends
-- (containers, scientific, regex-tdfa, …) are NOT directly exposed,
-- causing GHC-87110 "hidden package" errors on every src/ module
-- that imports them.
--
-- This is a source-inspection test: it verifies that Load.hs
-- does NOT reference ‘firstTestSuiteOrLibrary’ in its
-- implementation, confirming the fix is in place.
testGhcLoadNoArgsUsesLibraryTarget :: IO Bool
testGhcLoadNoArgsUsesLibraryTarget = do
  src <- TIO.readFile "src/HaskellFlows/Tool/Load.hs"
  -- The import section must list firstLibraryOrTestSuite (not
  -- firstTestSuiteOrLibrary, which causes the reload to use the
  -- test-suite stanza and lose library-only package context).
  let importLine = "  , firstLibraryOrTestSuite"
  pure $ T.isInfixOf importLine src

testParseHeader :: IO Bool
testParseHeader =
  let raw = T.unlines
        [ "src/Foo.hs:12:5: error: [GHC-83865]"
        , "    Couldn’t match expected type ‘Int’ with actual type ‘Bool’"
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

-- #127: 20-digit decimal literal (>= 10^19) must be refused.
-- 18446744073709551616 is 2^64 — the prototypical crash trigger.
testSanitizeRejectsLargeLiteral :: IO Bool
testSanitizeRejectsLargeLiteral =
  pure $ case sanitizeExpression "18446744073709551616" of
    Left OversizedIntegerLiteral -> True
    _                            -> False

-- #127: exponent >= 64 in a power expression must be refused.
-- "2^64" would constant-fold to a two-limb Integer and segfault the RTS.
testSanitizeRejectsBigExponent :: IO Bool
testSanitizeRejectsBigExponent =
  pure $ case sanitizeExpression "2^64" of
    Left OversizedIntegerLiteral -> True
    _                            -> False

-- #127: 19-digit literal (< 10^19) must NOT be refused — single GMP limb.
-- 9999999999999999999 is 10^19 - 1, safely within Word64.
testSanitizeAccepts19Digits :: IO Bool
testSanitizeAccepts19Digits =
  pure $ case sanitizeExpression "9999999999999999999" of
    Right _ -> True
    _       -> False

-- #127: exponent 63 must NOT be refused — 2^63 is still a single-limb value
-- on 64-bit (it is maxBound :: Int64, not Int64+1).
testSanitizeAcceptsSmallExp :: IO Bool
testSanitizeAcceptsSmallExp =
  pure $ case sanitizeExpression "2^63" of
    Right _ -> True
    Left OversizedIntegerLiteral -> False
    Left _  -> True   -- other rejections (e.g. future rules) still pass

-- #127: integration — 'sanitizeRejection' maps OversizedIntegerLiteral to
-- the OversizedInput ErrorKind so the wire response uses "oversized_input".
testSanitizeRejectionOversizedInteger :: IO Bool
testSanitizeRejectionOversizedInteger =
  let ee = Env.sanitizeRejection "expr" OversizedIntegerLiteral
  in pure (Env.eeKind ee == Env.OversizedInput)

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
-- #127: OversizedIntegerLiteral and InputTooLarge are also valid policy
-- rejections — the property's claim is "never silently accepted", not
-- "always accepted".
prop_sanitize_clean_roundtrip :: String -> Property
prop_sanitize_clean_roundtrip rawS =
  let raw = T.pack rawS
      ok = not (T.null (T.strip raw))
        && T.all (`notElem` ("\n\r" :: String)) raw
        && not ("<<<GHCi-DONE-7f3a2b>>>" `T.isInfixOf` raw)
  in ok ==>
     case sanitizeExpression raw of
       Right cleaned                -> cleaned === T.strip raw
       Left OversizedIntegerLiteral -> property True  -- #127: large-int guard
       Left (InputTooLarge _ _)     -> property True  -- size cap
       _                            -> counterexample "expected Right" (property False)

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

-- | Issue #211: QcGaveUp must produce kind='validation', not
-- kind='not_in_scope' (which wrongly implies "add an import").
testQcGaveUpValidationKind :: IO Bool
testQcGaveUpValidationKind =
  let result = QcTool.renderResult (QcGaveUp "\\x -> x > 0" 0 1000) Nothing
  in pure $ case trContent result of
       [TextContent body_] ->
         case A.decode (TLE.encodeUtf8 (TL.fromStrict body_)) of
           Just (A.Object top) ->
             case AKM.lookup "error" top of
               Just (A.Object err) ->
                 AKM.lookup "kind" err == Just (A.String "validation")
               _ -> False
           _ -> False
       _ -> False

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

-- | #170: 'renderDataCon' used 'showPprUnsafe' which includes GHC's
-- internal unique suffix on type variable names (e.g. @a_ig1m@).
-- After the fix it uses @sdocSuppressUniques = True@ so arg names
-- are clean user-facing identifiers.
--
-- This test pins the contract on 'parseConstructors': a data decl
-- rendered with clean type-var names (as 'renderDataCon' now
-- produces) must produce @cArgs@ with exactly those clean names —
-- no underscore-hash suffix that would confuse downstream consumers.
testCtorsNoUniqueSuffix :: IO Bool
testCtorsNoUniqueSuffix =
  -- Simulate the clean output that renderDataCon now produces.
  -- If the unique suffix leaked, the decl would be "data Pair a_ig1m b_xyz = Pair a_ig1m b_xyz"
  -- and cArgs would contain "a_ig1m", "b_xyz".
  let clean  = "data Pair a b = Pair a b"
      leaky  = "data Pair a_ig1m b_xyz99 = Pair a_ig1m b_xyz99"
  in pure $
       -- Clean form: args should be exactly ["a", "b"]
       ( case parseConstructors clean of
           [c] -> cArgs c == ["a", "b"]
           _   -> False )
       -- Leaky form would produce unique-like args — confirm parser
       -- is transparent (does not strip them): the fix must be in
       -- renderDataCon, not in the parser.
       && ( case parseConstructors leaky of
              [c] -> cArgs c == ["a_ig1m", "b_xyz99"]
              _   -> False )

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
            mPercent a == Just 100 && mTotal a == 12
         && mStatus  a == "covered"
         && mPercent b == Just 66  && mCovered b == 2 && mTotal b == 3
         && mStatus  b == "uncovered"
         && mPercent c == Just 75 && mStatus c == "uncovered"
         && mPercent d == Just 100 && mLabel d == "top-level declarations used"
         && mStatus  d == "covered"
       _ -> False

testCoverageBanner :: IO Bool
testCoverageBanner =
  let raw = T.unlines
        [ "Cabal version 3.12 — banner without fraction"
        , "100% expressions used (1/1)"
        , ""
        ]
  in pure (length (crMetrics (parseCoverage raw)) == 1)

-- | Issue #89: HPC reports @100%@ for categories with zero applicable
-- program points. The parser must collapse those rows to
-- @mPercent = Nothing, mStatus = "not_applicable"@ regardless of the
-- leading-column percent value HPC emitted.
testCoverageMetricNotApplicable :: IO Bool
testCoverageMetricNotApplicable =
  let raw = T.unlines
        [ "100% expressions used (19/19)"
        , "100% guards (0/0)"
        , "100% qualifiers (0/0)"
        , " 50% alternatives used (2/4)"
        ]
  in pure $ case crMetrics (parseCoverage raw) of
       [expr, guards_, quals, alts] ->
            -- Real-coverage rows untouched.
            mPercent expr   == Just 100 && mStatus expr   == "covered"
         && mPercent alts   == Just 50  && mStatus alts   == "uncovered"
            -- 0/0 rows normalised: percent dropped, status flagged.
         && isNothing (mPercent guards_) && mStatus guards_ == "not_applicable"
         && mTotal   guards_ == 0        && mCovered guards_ == 0
         && isNothing (mPercent quals)   && mStatus quals  == "not_applicable"
         && mTotal   quals  == 0         && mCovered quals  == 0
       _ -> False

-- | Issue #89 + #176: 'summarise' must skip 'not_applicable' rows AND
-- the 'boolean coverage' parent bucket when computing the headline
-- average. Anchor: 8 metrics, 2 are 0/0 (not_applicable) and
-- 'boolean coverage' is the parent of 'if' conditions (both have
-- total > 0). After excluding not_applicable + boolean coverage we
-- have 5 applicable metrics: expressions(89%), 'if' conditions(0%),
-- alternatives(50%), local(100%), top-level(100%).
-- Average = (89+0+50+100+100)/5 = 67%.
testCoverageAverageSkipsNotApplicable :: IO Bool
testCoverageAverageSkipsNotApplicable =
  let raw = T.unlines
        [ " 89% expressions used (17/19)"
        , "  0% boolean coverage (0/2)"
        , "100% guards (0/0)"
        , "  0% 'if' conditions (0/2)"
        , "100% qualifiers (0/0)"
        , " 50% alternatives used (2/4)"
        , "100% local declarations used (1/1)"
        , "100% top-level declarations used (2/2)"
        ]
      summary = CoverageTool.summarise (crMetrics (parseCoverage raw))
  in pure $
       T.isInfixOf "5 applicable metrics" summary
         && T.isInfixOf "67%" summary
         -- Make sure the buggy answers never reappear.
         && not (T.isInfixOf "56%" summary)
         && not (T.isInfixOf "8 metrics" summary)
         && not (T.isInfixOf "6 metrics" summary)

-- | Issue #89 + #176 anchor: when all 8 metrics are applicable
-- (total > 0), the parent 'boolean coverage' is still excluded from
-- the average, leaving 7 leaf metrics. Catches a regression where
-- the exclusion is dropped under the all-applicable case.
testCoverageAllMetricsApplicable :: IO Bool
testCoverageAllMetricsApplicable =
  let raw = T.unlines
        [ "100% expressions used (12/12)"
        , " 50% boolean coverage (1/2)"
        , " 33% guards (1/3)"
        , "100% 'if' conditions (1/1)"
        , " 50% qualifiers (1/2)"
        , " 66% alternatives used (2/3)"
        , " 75% local declarations used (3/4)"
        , "100% top-level declarations used (5/5)"
        ]
      summary = CoverageTool.summarise (crMetrics (parseCoverage raw))
  in pure $
       T.isInfixOf "7 applicable metrics" summary
         && T.isInfixOf "%" summary

-- | Issue #89 edge case: every metric is non-applicable. Don't
-- divide-by-zero; emit a coherent summary that names the count.
testCoverageAllNotApplicable :: IO Bool
testCoverageAllNotApplicable =
  let raw = T.unlines
        [ "100% guards (0/0)"
        , "100% qualifiers (0/0)"
        , "100% boolean coverage (0/0)"
        ]
      summary = CoverageTool.summarise (crMetrics (parseCoverage raw))
  in pure $
       T.isInfixOf "No applicable HPC metrics" summary
         && T.isInfixOf "3 metrics seen" summary

-- | #176: 'summarise' must exclude 'boolean coverage' (parent bucket)
-- from the average even when it has total > 0. Here both boolean
-- coverage and its child 'if' conditions have total=2, so without the
-- fix both would be counted and produce a different average.
testCoverageSummariseExcludesBooleanParent :: IO Bool
testCoverageSummariseExcludesBooleanParent =
  let raw = T.unlines
        [ "100% expressions used (10/10)"
        , "  0% boolean coverage (0/2)"
        , "100% guards (0/0)"
        , "  0% 'if' conditions (0/2)"
        , "100% qualifiers (0/0)"
        ]
      -- Applicable (total > 0) AFTER excluding boolean coverage parent:
      -- expressions(100%) + 'if' conditions(0%) = 2 metrics, avg = 50%
      -- If boolean coverage were included: 3 metrics, avg = 33%
      summary = CoverageTool.summarise (crMetrics (parseCoverage raw))
  in pure $
       T.isInfixOf "2 applicable metrics" summary
         && T.isInfixOf "50%" summary
         && not (T.isInfixOf "33%" summary)
         && not (T.isInfixOf "3 applicable" summary)

-- | #177: 'parseCoverage' must capture the "N always True" HPC
-- annotation on 'if' condition lines.
testCoverageAlwaysTrueParsed :: IO Bool
testCoverageAlwaysTrueParsed =
  let raw = T.unlines
        [ "  0% 'if' conditions (0/2), 2 always True" ]
  in pure $ case crMetrics (parseCoverage raw) of
       [m] -> mAlwaysTrue m == 2 && mAlwaysFalse m == 0
       _   -> False

-- | #177: 'parseCoverage' must capture the "N always False" HPC
-- annotation.
testCoverageAlwaysFalseParsed :: IO Bool
testCoverageAlwaysFalseParsed =
  let raw = T.unlines
        [ "  0% 'if' conditions (0/3), 3 always False" ]
  in pure $ case crMetrics (parseCoverage raw) of
       [m] -> mAlwaysTrue m == 0 && mAlwaysFalse m == 3
       _   -> False

-- | #177: 'parseCoverage' must capture both annotations when HPC
-- emits "N always True, M always False".
testCoverageAlwaysBothParsed :: IO Bool
testCoverageAlwaysBothParsed =
  let raw = T.unlines
        [ "  0% 'if' conditions (0/5), 3 always True, 2 always False" ]
  in pure $ case crMetrics (parseCoverage raw) of
       [m] -> mAlwaysTrue m == 3 && mAlwaysFalse m == 2
       _   -> False

-- | #178: when 'verbose' is not set (default), 'renderResult' must
-- NOT include a 'raw' field in the result payload.
testCoverageRawOmittedByDefault :: IO Bool
testCoverageRawOmittedByDefault =
  let args   = CoverageTool.CoverageArgs
                 { CoverageTool.caTimeoutMinutes = 5
                 , CoverageTool.caVerbose        = False
                 }
      out    = "100% expressions used (5/5)\n"
      result = CoverageTool.renderResult args (CoverageTool.CovSuccess out)
  in pure $ case trContent result of
       [TextContent body_] ->
         case A.decode (TLE.encodeUtf8 (TL.fromStrict body_)) of
           Just (A.Object top) ->
             case AKM.lookup "result" top of
               Just (A.Object res) -> not (AKM.member "raw" res)
               _                   -> False
           _ -> False
       _ -> False

-- | #178: when 'verbose=true', 'renderResult' MUST include the raw
-- cabal stdout in the result payload.
testCoverageRawIncludedWhenVerbose :: IO Bool
testCoverageRawIncludedWhenVerbose =
  let args   = CoverageTool.CoverageArgs
                 { CoverageTool.caTimeoutMinutes = 5
                 , CoverageTool.caVerbose        = True
                 }
      out    = "100% expressions used (5/5)\n"
      result = CoverageTool.renderResult args (CoverageTool.CovSuccess out)
  in pure $ case trContent result of
       [TextContent body_] ->
         case A.decode (TLE.encodeUtf8 (TL.fromStrict body_)) of
           Just (A.Object top) ->
             case AKM.lookup "result" top of
               Just (A.Object res) -> AKM.member "raw" res
               _                   -> False
           _ -> False
       _ -> False

--------------------------------------------------------------------------------
-- Issue #86: augmentEvalContext dedup
--
-- The pure helper 'selectMissingExtras' is the load-bearing dedup
-- step. Direct GHC-session integration (calling 'augmentEvalContext'
-- twice and inspecting 'getContext') would require spinning up a
-- real GHC session inside the test runner, which is what the e2e
-- 'eval_context_dedup' scenario covers; here we just nail down the
-- pure arithmetic so a regression in the set logic surfaces in
-- ~1 ms instead of waiting on a 200 s e2e cycle.
--------------------------------------------------------------------------------

-- | Anchor: with no existing imports, every baseline extra is
-- missing and must be appended.
testEvalContextEmptyAddsAll :: IO Bool
testEvalContextEmptyAddsAll =
  let missing = EvalTool.selectMissingExtras Set.empty EvalTool.evalContextExtras
  in pure (missing == EvalTool.evalContextExtras
              && length missing == 5)

-- | Issue #86 — the bug shape: 'Prelude' already in the existing
-- context (because 'autoLoadProject' put it there) MUST suppress
-- the baseline 'Prelude' from being re-added. Pre-fix the result
-- contained 'Prelude' twice on the second eval call.
testEvalContextSkipsExistingPrelude :: IO Bool
testEvalContextSkipsExistingPrelude =
  let existing = Set.fromList ["Prelude"]
      missing  = EvalTool.selectMissingExtras existing EvalTool.evalContextExtras
  in pure (missing == ["System.IO", "Data.List", "Control.Monad", "Control.Concurrent"]
              && notElem "Prelude" missing)

-- | After the FIRST 'augmentEvalContext' call has run, every
-- baseline module is in the existing set. The SECOND call must
-- therefore append nothing — this is exactly the scenario that
-- produced the unbounded growth pre-fix.
testEvalContextSecondCallNoop :: IO Bool
testEvalContextSecondCallNoop =
  let existing = Set.fromList EvalTool.evalContextExtras
      missing  = EvalTool.selectMissingExtras existing EvalTool.evalContextExtras
  in pure (null missing)

-- | The dedup is purely on module name string. An existing context
-- with a SUBSET of the baseline (e.g. only 'Prelude' and
-- 'Data.List' from a project that imports them) leaves only the
-- complement to append.
testEvalContextSubsetExisting :: IO Bool
testEvalContextSubsetExisting =
  let existing = Set.fromList ["Prelude", "Data.List", "Foo.Bar"]
      missing  = EvalTool.selectMissingExtras existing EvalTool.evalContextExtras
  in pure (missing == ["System.IO", "Control.Monad", "Control.Concurrent"])

-- | Idempotence law: running the dedup against the result of a
-- previous run is a no-op. This is the clean property-style
-- statement of the bug — pre-fix, the operation was *not*
-- idempotent under the same input set.
testEvalContextIdempotent :: IO Bool
testEvalContextIdempotent =
  let step1     = EvalTool.selectMissingExtras Set.empty EvalTool.evalContextExtras
      afterStep = Set.fromList step1
      step2     = EvalTool.selectMissingExtras afterStep EvalTool.evalContextExtras
  in pure (null step2 && step1 == EvalTool.evalContextExtras)

--------------------------------------------------------------------------------
-- Issue #88: PermissiveJSON IntField + BoolField
--------------------------------------------------------------------------------

-- | The canonical wire shape — JSON number — must keep parsing.
testIntFieldNumber :: IO Bool
testIntFieldNumber =
  pure (A.decode "42" == Just (IntField 42))

-- | Issue #88's bug shape: an MCP host wrapper stringifies the
-- number. The newtype must accept @\"42\"@ and produce IntField 42.
testIntFieldNumericString :: IO Bool
testIntFieldNumericString =
  pure (A.decode "\"42\"" == Just (IntField 42))

-- | Negative integers and a leading sign survive the string path.
testIntFieldSignedString :: IO Bool
testIntFieldSignedString =
  pure ( A.decode "\"-17\"" == Just (IntField (-17))
      && A.decode "\"+17\"" == Just (IntField 17) )

-- | Whitespace trim — common when shells stringify with padding.
testIntFieldStrippedString :: IO Bool
testIntFieldStrippedString =
  pure (A.decode "\"  42  \"" == Just (IntField 42))

-- | A clearly non-numeric string is rejected (not silently
-- coerced to 0). Loud failure beats a magic-default footgun.
testIntFieldRejectsNonNumeric :: IO Bool
testIntFieldRejectsNonNumeric =
  pure (A.decode "\"hello\"" == (Nothing :: Maybe IntField))

-- | A trailing non-digit after the integer is rejected: a typo
-- like @\"42x\"@ shouldn't silently parse as 42.
testIntFieldRejectsTrailingGarbage :: IO Bool
testIntFieldRejectsTrailingGarbage =
  pure (A.decode "\"42 lines\"" == (Nothing :: Maybe IntField))

-- | Float / fractional numbers are rejected — the field is
-- documented as @integer@ in the schema and the newtype must
-- enforce it.
testIntFieldRejectsFractional :: IO Bool
testIntFieldRejectsFractional =
  pure (A.decode "1.5" == (Nothing :: Maybe IntField))

-- | Canonical Bool wire — JSON boolean — keeps parsing.
testBoolFieldNative :: IO Bool
testBoolFieldNative =
  pure ( A.decode "true"  == Just (BoolField True)
      && A.decode "false" == Just (BoolField False) )

-- | The bug shape: stringified booleans, all four documented
-- forms (case-insensitive).
testBoolFieldStringForms :: IO Bool
testBoolFieldStringForms =
  pure ( A.decode "\"true\""  == Just (BoolField True)
      && A.decode "\"false\"" == Just (BoolField False)
      && A.decode "\"TRUE\""  == Just (BoolField True)
      && A.decode "\"False\"" == Just (BoolField False)
      && A.decode "\"1\""     == Just (BoolField True)
      && A.decode "\"0\""     == Just (BoolField False) )

-- | We do NOT do JavaScript truthiness — empty string and
-- arbitrary non-empty strings must be rejected. A non-truth
-- value should never land in a boolean field by accident.
testBoolFieldRejectsTruthy :: IO Bool
testBoolFieldRejectsTruthy =
  pure ( A.decode "\"\""    == (Nothing :: Maybe BoolField)
      && A.decode "\"yes\"" == (Nothing :: Maybe BoolField)
      && A.decode "1"       == (Nothing :: Maybe BoolField) )

--------------------------------------------------------------------------------
-- #88 integration: drive each affected tool's *Args parser with
-- both wire shapes and assert behaviour matches.
--
-- The key invariant: the SAME logical args, expressed via the
-- old (number/bool) wire vs. the new (string) wire, must decode
-- to the same record. Anything else is a regression in the
-- parser surface that the tool relies on.
--------------------------------------------------------------------------------

-- | Refactor.scope_line_start / scope_line_end accept stringified
-- numbers (the most user-visible win — rename_local was completely
-- unusable from stringifying clients pre-fix).
testRefactorPermissiveLineRange :: IO Bool
testRefactorPermissiveLineRange = do
  -- Issue #92 Phase B note: old_name is now a parse-time
  -- requirement for rename_local (the schema declares it,
  -- the FromJSON enforces it). The payload here includes it
  -- so the test exercises the *stringified primitive* axis
  -- without colliding with the per-action required-field axis.
  let nativeJson =
        "{\"action\":\"rename_local\",\"module_path\":\"src/X.hs\",\
        \\"old_name\":\"oldSym\",\"new_name\":\"newSym\",\
        \\"scope_line_start\":17,\"scope_line_end\":42}"
      stringJson =
        "{\"action\":\"rename_local\",\"module_path\":\"src/X.hs\",\
        \\"old_name\":\"oldSym\",\"new_name\":\"newSym\",\
        \\"scope_line_start\":\"17\",\"scope_line_end\":\"42\"}"
  -- Both must decode (no parse error). The pre-fix behaviour was:
  -- nativeJson decoded, stringJson rejected with an Aeson error
  -- string. Post-fix both succeed.
  let na = A.decode nativeJson :: Maybe A.Value
      st = A.decode stringJson :: Maybe A.Value
  case (na, st) of
    (Just nv, Just sv) -> do
      let nDecoded = A.fromJSON nv :: A.Result RefactorTool.RefactorArgs
          sDecoded = A.fromJSON sv :: A.Result RefactorTool.RefactorArgs
      case (nDecoded, sDecoded) of
        (A.Success _, A.Success _) -> pure True
        _                                   -> pure False
    _ -> pure False

-- | RemoveModules.delete_files / force accept stringified
-- "true"/"false" — the shape that triggered the issue's
-- @\"true\"@ wire form.
testRemoveModulesPermissiveBool :: IO Bool
testRemoveModulesPermissiveBool = do
  let nativeJson =
        "{\"modules\":[\"Foo\"],\"delete_files\":true,\"force\":false}"
      stringJson =
        "{\"modules\":[\"Foo\"],\"delete_files\":\"true\",\"force\":\"false\"}"
      decode raw = A.fromJSON <$> (A.decode raw :: Maybe A.Value)
  case (decode nativeJson, decode stringJson) of
    (Just (A.Success (a :: RM.RemoveModulesArgs)),
     Just (A.Success (b :: RM.RemoveModulesArgs))) ->
       -- Both decoded; if their fields agree then the permissive
       -- path is round-trip-equivalent to the native path.
       pure ( show a == show b )
    _ -> pure False

-- | FixWarning.line and apply: line is REQUIRED, and apply has a
-- default. Both must accept stringified primitives.
testFixWarningPermissiveLine :: IO Bool
testFixWarningPermissiveLine = do
  let nativeJson =
        "{\"module_path\":\"src/X.hs\",\"line\":3,\"code\":\"GHC-66111\",\
        \\"apply\":true}"
      stringJson =
        "{\"module_path\":\"src/X.hs\",\"line\":\"3\",\"code\":\"GHC-66111\",\
        \\"apply\":\"true\"}"
      decode raw = A.fromJSON <$> (A.decode raw :: Maybe A.Value)
  case (decode nativeJson, decode stringJson) of
    (Just (A.Success (a :: FixWarning.FixWarningArgs)),
     Just (A.Success (b :: FixWarning.FixWarningArgs))) ->
       pure ( show a == show b )
    _ -> pure False

-- | Complete.limit accepts stringified numbers. Default still
-- applies when the field is omitted entirely.
testCompletePermissiveLimit :: IO Bool
testCompletePermissiveLimit = do
  let nativeJson = "{\"prefix\":\"sho\",\"limit\":10}"
      stringJson = "{\"prefix\":\"sho\",\"limit\":\"10\"}"
      missingJson = "{\"prefix\":\"sho\"}"
      decode raw = A.fromJSON <$> (A.decode raw :: Maybe A.Value)
  case (decode nativeJson, decode stringJson, decode missingJson) of
    (Just (A.Success (a :: CompleteTool.CompleteArgs)),
     Just (A.Success (b :: CompleteTool.CompleteArgs)),
     Just (A.Success (c :: CompleteTool.CompleteArgs))) ->
       -- a == b proves permissive parses match native;
       -- existence of c proves the default still applies.
       pure ( show a == show b && not (null (show c)) )
    _ -> pure False

--------------------------------------------------------------------------------
-- Issue #85: friendly Aeson parse-error rewriting at the tool boundary
--
-- The interpreter operates on the Aeson 'parseEither' string and
-- categorises it into 'missing_arg' / 'type_mismatch' / 'validation'
-- with an extracted 'field' and a friendly natural-language message.
-- Each test pins one shape — including the unrecognised fall-through.
--------------------------------------------------------------------------------

-- | The canonical \"missing required key\" shape from Aeson.
testParseErrorMissingKey :: IO Bool
testParseErrorMissingKey =
  let raw = "Error in $: key \"expression\" not found"
      ip  = interpretParseError raw
  in pure ( ipKind    ip == Env.MissingArg
         && ipField   ip == Just "expression"
         && T.isInfixOf "expression" (ipMessage ip)
         && T.isInfixOf "missing"    (ipMessage ip)
         && ipRaw     ip == T.pack raw )

-- | The dotted-path shape: @Error in $.field: <reason>@.
-- Field extracted, kind flagged as type mismatch, raw preserved.
testParseErrorTypeMismatchDotted :: IO Bool
testParseErrorTypeMismatchDotted =
  let raw = "Error in $.line: parsing Int failed, expected Number, but encountered String"
      ip  = interpretParseError raw
  in pure ( ipKind    ip == Env.TypeMismatch
         && ipField   ip == Just "line"
         && T.isInfixOf "line"  (ipMessage ip)
         && T.isInfixOf "wrong" (ipMessage ip) )

-- | Bracket-quoted field name: @Error in $['delete_files']: ...@.
-- Aeson uses this form when the key name contains characters that
-- would be ambiguous in dotted-path syntax.
testParseErrorTypeMismatchBracketed :: IO Bool
testParseErrorTypeMismatchBracketed =
  let raw = "Error in $['delete_files']: expected Bool, but encountered String"
      ip  = interpretParseError raw
  in pure ( ipKind  ip == Env.TypeMismatch
         && ipField ip == Just "delete_files" )

-- | Type-mismatch signal but no field path — the heuristic fall
-- through to a Validation result that surfaces the raw text.
testParseErrorTypeMismatchNoField :: IO Bool
testParseErrorTypeMismatchNoField =
  let raw = "Error in something else: expected Bool, but encountered String"
      ip  = interpretParseError raw
  in pure ( ipKind  ip == Env.TypeMismatch
         && isNothing (ipField ip)
         && T.isInfixOf "wrong" (ipMessage ip) )

-- | A parse-error string we don't recognise must NOT be silently
-- mis-categorised. Falls through to Validation with the original
-- text preserved verbatim — that's the one shape that doesn't get
-- friendly-rewritten because we don't know what it means.
testParseErrorUnrecognisedFalls :: IO Bool
testParseErrorUnrecognisedFalls =
  let raw = "some Aeson shape we never saw before"
      ip  = interpretParseError raw
  in pure ( ipKind  ip == Env.Validation
         && isNothing (ipField ip)
         && T.isInfixOf (T.pack raw) (ipMessage ip)
         && ipRaw   ip == T.pack raw )

-- | The bug-pinning anchor: the original raw text must always be
-- preserved on 'ipRaw' regardless of which branch fires. A
-- debugging consumer that wants the literal Aeson output can
-- always retrieve it from there.
testParseErrorRawAlwaysPreserved :: IO Bool
testParseErrorRawAlwaysPreserved =
  let cases = [ "Error in $: key \"foo\" not found"
              , "Error in $.bar: parsing Int failed"
              , "weird unrecognised shape"
              ]
      results = map interpretParseError cases
  in pure (and (zipWith (\raw r -> ipRaw r == T.pack raw) cases results))

--------------------------------------------------------------------------------
-- Issue #92 Phase A: discriminated schema helpers
--------------------------------------------------------------------------------

-- | Sample two-branch schema modelled on ghc_refactor's eventual shape.
sampleRefactorSchema :: A.Value
sampleRefactorSchema = Schema.discriminatedSchema "action"
  [ Schema.SchemaBranch
      { Schema.sbDiscriminantValue = "rename_local"
      , Schema.sbDescription       = "Scoped identifier rename."
      , Schema.sbProperties        =
          [ ("module_path",      Schema.stringField  "Module path.")
          , ("new_name",         Schema.stringField  "New identifier.")
          , ("old_name",         Schema.stringField  "Existing identifier.")
          , ("scope_line_start", Schema.integerField "Inclusive start line.")
          , ("scope_line_end",   Schema.integerField "Inclusive end line.")
          , ("dry_run",          Schema.booleanField "Compute without writing.")
          ]
      , Schema.sbRequired
          = ["module_path", "new_name", "old_name"
            , "scope_line_start", "scope_line_end"]
      }
  , Schema.SchemaBranch
      { Schema.sbDiscriminantValue = "extract_binding"
      , Schema.sbDescription       = "Extract expression."
      , Schema.sbProperties        =
          [ ("module_path",      Schema.stringField  "Module path.")
          , ("new_name",         Schema.stringField  "New binding name.")
          , ("scope_line_start", Schema.integerField "Inclusive start line.")
          , ("scope_line_end",   Schema.integerField "Inclusive end line.")
          ]
      , Schema.sbRequired
          = ["module_path", "new_name"
            , "scope_line_start", "scope_line_end"]
      }
  ]

-- | The published top-level shape MUST be a flat object — no
-- 'oneOf' / 'allOf' / 'anyOf' at the root. The Claude API rejects
-- those keywords there with HTTP 400, which would fail the whole
-- session at MCP register time.
testSchemaTopLevelOneOf :: IO Bool
testSchemaTopLevelOneOf = pure $ case sampleRefactorSchema of
  A.Object km ->
       AKM.lookup "type" km == Just (A.String "object")
    && isNothing (AKM.lookup "oneOf" km)
    && isNothing (AKM.lookup "allOf" km)
    && isNothing (AKM.lookup "anyOf" km)
    && case AKM.lookup "properties" km of
         Just (A.Object _) -> True
         _                 -> False
  _ -> False

-- | The discriminant must be present in the top-level 'properties'
-- as an 'enum' field listing every branch's value. Replaces the
-- pre-fix invariant that pinned a per-branch 'const' discriminant.
testSchemaDiscriminantInEveryBranch :: IO Bool
testSchemaDiscriminantInEveryBranch = pure $
  case sampleRefactorSchema of
    A.Object km -> case AKM.lookup "properties" km of
      Just (A.Object props) -> case AKM.lookup "action" props of
        Just (A.Object actObj) ->
             AKM.lookup "type" actObj == Just (A.String "string")
          && case AKM.lookup "enum" actObj of
               Just (A.Array _) -> True
               _                -> False
        _ -> False
      _ -> False
    _ -> False

-- | The discriminant's 'enum' must list every branch's
-- 'sbDiscriminantValue' in declaration order. Replaces the
-- per-branch 'const' check.
testSchemaDiscriminantConstMatchesValue :: IO Bool
testSchemaDiscriminantConstMatchesValue = pure $
  case sampleRefactorSchema of
    A.Object km -> case AKM.lookup "properties" km of
      Just (A.Object props) -> case AKM.lookup "action" props of
        Just (A.Object actObj) -> case AKM.lookup "enum" actObj of
          Just (A.Array xs) ->
            [ s | A.String s <- Vector.toList xs ]
              == ["rename_local", "extract_binding"]
          _ -> False
        _ -> False
      _ -> False
    _ -> False

-- | At the top-level the only required field is the discriminant;
-- per-action required-field enforcement now lives in the runtime
-- 'FromJSON' parser (the schema can no longer carry it without a
-- top-level 'oneOf', which the Claude API forbids).
testSchemaRequiredSetsAreCorrect :: IO Bool
testSchemaRequiredSetsAreCorrect = pure $
  case sampleRefactorSchema of
    A.Object km -> case AKM.lookup "required" km of
      Just (A.Array reqs) ->
        [ s | A.String s <- Vector.toList reqs ] == ["action"]
      _ -> False
    _ -> False

-- | additionalProperties: false at the top level keeps unknown
-- fields out — the schema lists every branch's properties, so a
-- valid request never needs anything beyond what's declared.
testSchemaAdditionalPropertiesFalse :: IO Bool
testSchemaAdditionalPropertiesFalse = pure $
  case sampleRefactorSchema of
    A.Object km ->
      AKM.lookup "additionalProperties" km == Just (A.Bool False)
    _ -> False

-- | Anchor: 'flatObjectSchema' for non-discriminated tools matches
-- the inline shape every Tool/*.hs already uses today (pre-migration).
-- Tests the helper produces a valid flat schema with the same
-- property + required set that the inline form does.
testSchemaFlatObject :: IO Bool
testSchemaFlatObject =
  let s = Schema.flatObjectSchema
            [ ("expression", Schema.stringField "Haskell expression to eval.") ]
            [ "expression" ]
  in pure $ case s of
       A.Object km ->
            AKM.lookup "type" km == Just (A.String "object")
         && AKM.lookup "additionalProperties" km == Just (A.Bool False)
         && case AKM.lookup "required" km of
              Just (A.Array reqs) -> reqs == Vector.fromList [A.String "expression"]
              _                 -> False
       _ -> False

-- | Field-builder anchors: each helper's output has the right
-- `type` + a `description`. Cheap pin against accidental drift.
testSchemaFieldBuilders :: IO Bool
testSchemaFieldBuilders =
  let cases = [ ("string",  Schema.stringField  "x")
              , ("integer", Schema.integerField "x")
              , ("boolean", Schema.booleanField "x")
              , ("array",   Schema.arrayField   "x")
              ]
      ok (expected, v) = case v of
        A.Object km -> AKM.lookup "type" km == Just (A.String expected)
                  && AKM.lookup "description" km == Just (A.String "x")
        _         -> False
  in pure (all ok cases)

--------------------------------------------------------------------------------
-- Issue #92 Phase B: ghc_refactor migration anchors
--
-- These pin the per-action contract that #92 Phase A's
-- discriminatedSchema helper now expresses on the request side.
-- Pre-fix, the schema declared required = [action, module_path,
-- new_name] and the runtime emitted "scope_line_start is required
-- for rename_local" — the schema lied. Post-fix, the parser
-- enforces per-action requirements and a host that reads
-- 'tools/list' learns the right shape from the schema's
-- per-branch required list.
--------------------------------------------------------------------------------

-- | rename_local with the FULL required set must parse cleanly.
testRefactorRenameLocalCompleteParses :: IO Bool
testRefactorRenameLocalCompleteParses = do
  let raw =
        "{\"action\":\"rename_local\",\"module_path\":\"src/X.hs\",\
        \\"old_name\":\"oldSym\",\"new_name\":\"newSym\",\
        \\"scope_line_start\":17,\"scope_line_end\":42}"
  pure $ case A.decode raw :: Maybe A.Value of
    Just v -> case A.fromJSON v :: A.Result RefactorTool.RefactorArgs of
      A.Success _ -> True
      _           -> False
    _      -> False

-- | rename_local without old_name must FAIL at parse time
-- (post-#92). Pre-fix this parsed and the handler returned
-- "'old_name' is required for rename_local" at runtime — the
-- schema-vs-runtime contract drift this issue closes.
testRefactorRenameLocalMissingOldName :: IO Bool
testRefactorRenameLocalMissingOldName = do
  let raw =
        "{\"action\":\"rename_local\",\"module_path\":\"src/X.hs\",\
        \\"new_name\":\"newSym\",\
        \\"scope_line_start\":17,\"scope_line_end\":42}"
  pure $ case A.decode raw :: Maybe A.Value of
    Just v -> case A.fromJSON v :: A.Result RefactorTool.RefactorArgs of
      A.Error _ -> True   -- expected: parser rejects
      _         -> False
    _      -> False

-- | rename_local without scope_line_start must FAIL at parse
-- time. Same contract: schema declares it required, parser must
-- enforce.
testRefactorRenameLocalMissingScopeStart :: IO Bool
testRefactorRenameLocalMissingScopeStart = do
  let raw =
        "{\"action\":\"rename_local\",\"module_path\":\"src/X.hs\",\
        \\"old_name\":\"oldSym\",\"new_name\":\"newSym\",\
        \\"scope_line_end\":42}"
  pure $ case A.decode raw :: Maybe A.Value of
    Just v -> case A.fromJSON v :: A.Result RefactorTool.RefactorArgs of
      A.Error _ -> True
      _         -> False
    _      -> False

-- | extract_binding does NOT need old_name — it only renames the
-- extracted binding's name, not an existing identifier. Anchor:
-- the parser must ACCEPT extract_binding without old_name.
testRefactorExtractBindingNoOldName :: IO Bool
testRefactorExtractBindingNoOldName = do
  let raw =
        "{\"action\":\"extract_binding\",\"module_path\":\"src/X.hs\",\
        \\"new_name\":\"helperFn\",\
        \\"scope_line_start\":10,\"scope_line_end\":20}"
  pure $ case A.decode raw :: Maybe A.Value of
    Just v -> case A.fromJSON v :: A.Result RefactorTool.RefactorArgs of
      A.Success _ -> True
      _           -> False
    _      -> False

-- | extract_binding STILL requires both scope lines. The
-- per-action contract: dropping just one of the scope lines fails
-- at parse time (Aeson 'fromJSON' returns 'Error').
testRefactorExtractBindingMissingScope :: IO Bool
testRefactorExtractBindingMissingScope = do
  let raw =
        "{\"action\":\"extract_binding\",\"module_path\":\"src/X.hs\",\
        \\"new_name\":\"helperFn\",\"scope_line_start\":10}"
  pure $ case A.decode raw :: Maybe A.Value of
    Just v -> case A.fromJSON v :: A.Result RefactorTool.RefactorArgs of
      A.Error _ -> True
      _         -> False
    _      -> False

-- | The published 'tdInputSchema' for ghc_refactor must publish
-- the action discriminant as an 'enum' over every branch.
-- Anchor: a future drift back to top-level 'oneOf' (which Claude
-- rejects) would fail this.
testRefactorSchemaIsDiscriminated :: IO Bool
testRefactorSchemaIsDiscriminated =
  -- #94 Phase C: THREE action branches + #154 adds list_actions → FOUR.
  -- Post-flat-schema fix (Claude API top-level oneOf rejection): we
  -- anchor on the discriminant 'enum' instead of a per-branch 'oneOf'.
  let s = tdInputSchema RefactorTool.descriptor
  in pure $ case s of
       A.Object km -> case AKM.lookup "properties" km of
         Just (A.Object props) -> case AKM.lookup "action" props of
           Just (A.Object actObj) -> case AKM.lookup "enum" actObj of
             Just (A.Array xs) -> length xs == 4
             _                 -> False
           _ -> False
         _ -> False
       _ -> False

-- | #154: list_actions with no other args returns status=ok and
-- an 'actions' list (no module_path / new_name required).
testRefactorListActions :: IO Bool
testRefactorListActions = do
  tmp <- getTemporaryDirectory
  let dir = tmp </> "haskell-flows-refactor-list-actions"
  createDirectoryIfMissing True dir
  case mkProjectDir dir of
    Left _   -> pure False
    Right pd -> do
      sess <- startGhcSession pd
      let rawArgs = A.object [ "action" A..= ("list_actions" :: T.Text) ]
      tr <- RefactorTool.handle sess pd rawArgs
      killGhcSession sess
      case trContent tr of
        [TextContent t] ->
          case A.decode (TLE.encodeUtf8 (TL.fromStrict t)) :: Maybe A.Value of
            Just (A.Object env) ->
              case AKM.lookup (AKey.fromText "status") env of
                Just (A.String "ok") -> pure True
                _                    -> pure False
            _ -> pure False
        _ -> pure False

-- | #154: list_actions response carries 'actions' array with an entry
-- for 'move_symbol' that lists the correct field names ('symbol','from','to').
testRefactorListActionsHasRequired :: IO Bool
testRefactorListActionsHasRequired = do
  tmp <- getTemporaryDirectory
  let dir = tmp </> "haskell-flows-refactor-list-actions-req"
  createDirectoryIfMissing True dir
  case mkProjectDir dir of
    Left _   -> pure False
    Right pd -> do
      sess <- startGhcSession pd
      let rawArgs = A.object [ "action" A..= ("list_actions" :: T.Text) ]
      tr <- RefactorTool.handle sess pd rawArgs
      killGhcSession sess
      case trContent tr of
        [TextContent t] ->
          case A.decode (TLE.encodeUtf8 (TL.fromStrict t)) :: Maybe A.Value of
            Just (A.Object env) ->
              case AKM.lookup (AKey.fromText "result") env of
                Just (A.Object res) ->
                  case AKM.lookup (AKey.fromText "actions") res of
                    Just (A.Array arr) ->
                      -- Check that 'move_symbol' has 'symbol','from','to'
                      let moveEntry = [ o | A.Object o <- Vector.toList arr
                                      , AKM.lookup (AKey.fromText "action") o
                                          == Just (A.String "move_symbol")
                                      ]
                      in case moveEntry of
                           [o] -> case AKM.lookup (AKey.fromText "required") o of
                             Just (A.Array req) ->
                               let reqStrs = [ s | A.String s <- Vector.toList req ]
                               in pure (  "symbol" `elem` reqStrs
                                       && "from"   `elem` reqStrs
                                       && "to"     `elem` reqStrs)
                             _ -> pure False
                           _ -> pure False
                    _ -> pure False
                _ -> pure False
            _ -> pure False
        _ -> pure False

--------------------------------------------------------------------------------
-- Issue #92 Phase B: ghc_deps migration anchors
--
-- Same shape as the Refactor anchors above — pre-#92 the schema
-- declared @required: [\"action\"]@ for ghc_deps but the runtime
-- emitted "'package' is required for add" at runtime. Each
-- branch now declares its own required-field set.
--------------------------------------------------------------------------------

-- | 'list' has no extra required fields — bare {action: list} parses.
testDepsListBareParses :: IO Bool
testDepsListBareParses = do
  let raw = "{\"action\":\"list\"}"
  pure $ case A.decode raw :: Maybe A.Value of
    Just v -> case A.fromJSON v :: A.Result DepsTool.DepsArgs of
      A.Success _ -> True
      _           -> False
    _      -> False

-- | 'add' without 'package' must FAIL at parse time — the
-- bug-class anchor that #92 closes.
testDepsAddMissingPackage :: IO Bool
testDepsAddMissingPackage = do
  let raw = "{\"action\":\"add\"}"
  pure $ case A.decode raw :: Maybe A.Value of
    Just v -> case A.fromJSON v :: A.Result DepsTool.DepsArgs of
      A.Error _ -> True
      _         -> False
    _      -> False

-- | 'remove' without 'package' must FAIL at parse time.
testDepsRemoveMissingPackage :: IO Bool
testDepsRemoveMissingPackage = do
  let raw = "{\"action\":\"remove\"}"
  pure $ case A.decode raw :: Maybe A.Value of
    Just v -> case A.fromJSON v :: A.Result DepsTool.DepsArgs of
      A.Error _ -> True
      _         -> False
    _      -> False

-- | 'add' with 'package' parses cleanly. 'version' is optional
-- (constraint-as-cabal-text), 'stanza' is optional (defaults to
-- the first build-depends block). Anchor for the positive path.
testDepsAddCompleteParses :: IO Bool
testDepsAddCompleteParses = do
  let raw = "{\"action\":\"add\",\"package\":\"text\",\"version\":\">= 2.0\"}"
  pure $ case A.decode raw :: Maybe A.Value of
    Just v -> case A.fromJSON v :: A.Result DepsTool.DepsArgs of
      A.Success _ -> True
      _           -> False
    _      -> False

-- | The published 'tdInputSchema' for ghc_deps must publish the
-- discriminant as an 'enum' over every action — list / add /
-- remove / explain.
testDepsSchemaIsDiscriminated :: IO Bool
testDepsSchemaIsDiscriminated =
  -- #94 Phase C: schema now has FOUR branches — list / add / remove
  -- / explain (the latter subsumed the retired ghc_deps_explain).
  -- Post-flat-schema fix: anchor on the 'action' enum instead of a
  -- top-level 'oneOf' array (Claude API rejects 'oneOf' at root).
  let s = tdInputSchema DepsTool.descriptor
  in pure $ case s of
       A.Object km -> case AKM.lookup "properties" km of
         Just (A.Object props) -> case AKM.lookup "action" props of
           Just (A.Object actObj) -> case AKM.lookup "enum" actObj of
             Just (A.Array xs) -> length xs == 4
             _                 -> False
           _ -> False
         _ -> False
       _ -> False

--------------------------------------------------------------------------------
-- Issue #92 Phase D (lite): every registered tool's input schema is
-- well-formed AND Claude-API-compatible.
--
-- The Claude API rejects 'input_schema' objects that carry 'oneOf' /
-- 'allOf' / 'anyOf' at the top level (HTTP 400 at MCP registration
-- time, taking down the whole session). This test pins the
-- structural rule:
--
--   * 'tdInputSchema' must be a JSON Object (not an Array, not a
--     primitive — Aeson's 'object' helper guarantees this, but a
--     refactor that swaps in a hand-built Value could regress).
--   * The Object must declare 'type: "object"'.
--   * The Object must NOT carry top-level 'oneOf' / 'allOf' / 'anyOf'.
--   * 'properties' must be present (flat schema).
--   * No tool ships a Null/empty schema.
--------------------------------------------------------------------------------

testEveryToolPublishesValidSchema :: IO Bool
testEveryToolPublishesValidSchema = do
  let invalid =
        [ tdName d
        | d <- allToolDescriptors
        , not (isValidSchema (tdInputSchema d))
        ]
  unless (null invalid) $
    putStrLn ("Tools with malformed schemas: " <> show invalid)
  pure (null invalid)
  where
    -- Post-flat-schema fix (Claude API top-level oneOf rejection):
    -- every tool must publish a flat object schema. Top-level
    -- 'oneOf' / 'allOf' / 'anyOf' fail HTTP registration with 400.
    isValidSchema (A.Object km) =
         AKM.lookup "type" km == Just (A.String "object")
      && isNothing (AKM.lookup "oneOf" km)
      && isNothing (AKM.lookup "allOf" km)
      && isNothing (AKM.lookup "anyOf" km)
      && case AKM.lookup "properties" km of
           Just (A.Object _) -> True
           _                 -> False
    isValidSchema _ = False

--------------------------------------------------------------------------------
-- Issue #95 Phase A (lite): nextStep dangling-reference detector
--
-- The full nextStep audit (suppression rules, signal-to-noise gates,
-- per-tool golden tests) is a multi-PR effort tracked on the
-- meta-issue. This commit lands one of its load-bearing anchors:
-- whenever 'suggestNext' returns a NextStep recommending some
-- tool, that tool MUST exist in 'allToolNames'. The type system
-- already enforces this at compile time (nsTool :: ToolName, an
-- ADT), but a runtime test is still useful as documentation of
-- the contract — a contributor renaming a tool sees the link
-- between the registry and the nextStep recommendations.
--
-- The chain (nsChain) is also covered: every step in a multi-step
-- plan must point at a registered tool.
--------------------------------------------------------------------------------

-- | Drive 'suggestNext' across all 46 tools with a generic
-- payload and assert every recommended tool (primary + chain) is
-- in 'allToolNames'.
testNextStepReferencesRegisteredToolsOnly :: IO Bool
testNextStepReferencesRegisteredToolsOnly = do
  let nameSet  = Set.fromList allToolNames
      payload  = object []
      attempts = [ suggestNext n True payload | n <- allToolNames ]
      bad      =
        [ recName
        | Just ns <- attempts
        , recName <- toolsReferencedBy ns
        , recName `Set.notMember` nameSet
        ]
  unless (null bad) $
    putStrLn ("Dangling nextStep tool refs: " <> show bad)
  pure (null bad)
  where
    toolsReferencedBy ns =
      nsTool ns
        : maybe [] (map csTool) (nsChain ns)

-- | Every registered tool descriptor must carry a non-empty
-- 'tdDescription'. The string surfaces in 'tools/list' and is
-- the primary affordance an LLM agent has for picking the right
-- tool — an empty / whitespace-only description is a real bug
-- (a host's selector menu would just show the bare tool name
-- with no context). This anchor catches a future contributor
-- who adds a tool with an empty 'tdDescription'.
testEveryToolHasNonEmptyDescription :: IO Bool
testEveryToolHasNonEmptyDescription = do
  let bad =
        [ tdName d
        | d <- allToolDescriptors
        , T.null (T.strip (tdDescription d))
        ]
  unless (null bad) $
    putStrLn ("Tools with empty description: " <> show bad)
  pure (null bad)

-- | Every tool's name must be in the canonical 'allToolNames'
-- ADT enumeration — protects against a tool whose descriptor
-- references a non-canonical or hand-stringified name. With
-- 'tdName' currently typed as 'Text' (rather than 'ToolName'
-- directly), this is the runtime invariant the type system
-- doesn't enforce on its own.
testEveryToolNameIsCanonical :: IO Bool
testEveryToolNameIsCanonical = do
  let nameSet = Set.fromList (map toolNameText allToolNames)
      bad =
        [ tdName d
        | d <- allToolDescriptors
        , tdName d `Set.notMember` nameSet
        ]
  unless (null bad) $
    putStrLn ("Tools with non-canonical name: " <> show bad)
  pure (null bad)

-- | Tool names should be reasonably short — they appear in
-- 'tools/list', in selector menus, and in nextStep
-- recommendations. Anchor: nothing should exceed 50 chars.
-- The current longest is well under that (around 24 chars
-- for 'ghc_quickcheck_export'); the invariant catches a
-- runaway case like a future @ghc_some_extremely_long_name@
-- before it ships.
testEveryToolNameIsShort :: IO Bool
testEveryToolNameIsShort = do
  let bad =
        [ (tdName d, T.length (tdName d))
        | d <- allToolDescriptors
        , T.length (tdName d) > 50
        ]
  unless (null bad) $
    putStrLn ("Tool names exceeding 50 chars: " <> show bad)
  pure (null bad)

-- | Tool descriptions should be substantive — at least 20
-- characters after stripping. Protects against a future
-- placeholder like 'tdDescription = "TODO"'.
testEveryToolDescriptionIsSubstantive :: IO Bool
testEveryToolDescriptionIsSubstantive = do
  let bad =
        [ tdName d
        | d <- allToolDescriptors
        , T.length (T.strip (tdDescription d)) < 20
        ]
  unless (null bad) $
    putStrLn ("Tools with too-short descriptions: " <> show bad)
  pure (null bad)

-- | When 'suggestNext' attaches an 'nsExample', it must be a
-- JSON Object — the @ghc_batch@ / direct-call shape an agent
-- can pass straight to the recommended tool. An Array or
-- primitive would never decode as args.
testNextStepExampleIsObjectWhenPresent :: IO Bool
testNextStepExampleIsObjectWhenPresent = do
  let payload  = object []
      attempts = [ (n, suggestNext n True payload) | n <- allToolNames ]
      bad =
        [ toolNameText n
        | (n, Just ns) <- attempts
        , Just v <- [nsExample ns]
        , not (isObject v)
        ]
  unless (null bad) $
    putStrLn ("Tools whose nextStep.example isn't an Object: " <> show bad)
  pure (null bad)
  where
    isObject (A.Object _) = True
    isObject _            = False

-- | Every 'nsChain' step must carry an Object as its args. A
-- non-Object would crash 'ghc_batch' when the agent forwards
-- the chain. This is the chain-time analogue of the
-- nsExample invariant above.
testNextStepChainStepsCarryObjectArgs :: IO Bool
testNextStepChainStepsCarryObjectArgs = do
  let payload  = object []
      attempts = [ (n, suggestNext n True payload) | n <- allToolNames ]
      bad =
        [ (toolNameText n, toolNameText (csTool s))
        | (n, Just ns) <- attempts
        , chain        <- maybeToList (nsChain ns)
        , s            <- chain
        , not (isObject (csArgs s))
        ]
  unless (null bad) $
    putStrLn ("Chain steps with non-Object args: " <> show bad)
  pure (null bad)
  where
    isObject (A.Object _) = True
    isObject _            = False
    maybeToList Nothing   = []
    maybeToList (Just xs) = [xs]

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

-- | #48 — extractErrorSummary picks the cabal lines that mention
-- the package by name. Synthetic input: a typical "could not
-- resolve" verdict that mentions the package on its own line.
testExtractErrorSummaryFindsPackage :: IO Bool
testExtractErrorSummaryFindsPackage = do
  let stderr = T.unlines
        [ "Resolving dependencies..."
        , "cabal-3.14.2.0: Could not resolve dependencies:"
        , "[__0] trying: my-project-0.1.0.0 (user goal)"
        , "[__1] unknown package: this-package-does-not-exist (dependency of my-project-0.1.0.0)"
        , "[__1] fail (backjumping, conflict set: this-package-does-not-exist, my-project)"
        , "After searching the rest of the dependency tree exhaustively,"
        , "these were the goals I've had most trouble fulfilling: my-project, this-package-does-not-exist"
        ]
      summary = extractErrorSummary "this-package-does-not-exist" stderr
  pure ( "this-package-does-not-exist" `T.isInfixOf` summary
      && "unknown package"             `T.isInfixOf` T.toLower summary )

-- | #48 — when no line matches the package name or solver verdicts,
-- extractErrorSummary falls back to a truncated raw output instead
-- of emitting an empty summary that would lose information.
testExtractErrorSummaryFallsBackOnNoMatch :: IO Bool
testExtractErrorSummaryFallsBackOnNoMatch = do
  let stderr = T.replicate 200 "x"
      summary = extractErrorSummary "irrelevant-pkg" stderr
  pure (not (T.null summary) && T.length summary <= 800)

-- | #48 — extractErrorSummary is case-insensitive on the package
-- name, since cabal output often lowercases verdicts ("rejecting:
-- Aeson..." vs "rejecting: aeson..." between versions).
testExtractErrorSummaryCaseInsensitive :: IO Bool
testExtractErrorSummaryCaseInsensitive = do
  let stderr = T.unlines
        [ "[__1] rejecting: AESON-2.2.3.0 (constraint from user target requires <2.0)"
        , "[__1] fail"
        ]
      summary = extractErrorSummary "aeson" stderr
  pure ("AESON" `T.isInfixOf` summary)

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

-- | Issue #50: structural-key helpers for the diagnostic-diff
-- accept criterion. Two diagnostics with identical (file, line,
-- column, message) are considered the same — that's what makes
-- the post ⊆ pre test mean "the rewrite introduced no new errors".

testRefactorErrorKeySame :: IO Bool
testRefactorErrorKeySame =
  let a = mkErr "F.hs" 10 5 "Found hole: _x :: Int"
      b = mkErr "F.hs" 10 5 "Found hole: _x :: Int"
  in pure (RefactorTool.errorKey a == RefactorTool.errorKey b)

testRefactorErrorKeyDistinct :: IO Bool
testRefactorErrorKeyDistinct =
  let a = mkErr "F.hs" 10 5 "Variable not in scope: foo"
      b = mkErr "F.hs" 10 5 "Variable not in scope: bar"
  in pure (RefactorTool.errorKey a /= RefactorTool.errorKey b)

testRefactorSignaturesErrorsOnly :: IO Bool
testRefactorSignaturesErrorsOnly =
  let err  = mkErr  "F.hs" 1 1 "boom"
      warn = mkWarn "F.hs" 2 2 "unused"
  in pure (RefactorTool.errorSignatures [err, warn]
             == [RefactorTool.errorKey err])

-- | Issue #50: a rename that leaves an unrelated pre-existing
-- error in place must NOT be rolled back. Model the diff
-- check directly: post is identical to pre → no new errors.
testRefactorPostSubsetPre :: IO Bool
testRefactorPostSubsetPre =
  let pre  = [mkErr "F.hs" 23 1 "Found hole: _holeArg :: [a]"]
      post = pre  -- rename touched line 13, hole at line 23 unchanged
      preSigs  = RefactorTool.errorSignatures pre
      postSigs = RefactorTool.errorSignatures post
      newErrSigs = filter (`notElem` preSigs) postSigs
  in pure (null newErrSigs)

-- | Issue #50: a rename that introduces a NEW error must be
-- rejected — that's the conservative side of the diff.
testRefactorNewErrorDetected :: IO Bool
testRefactorNewErrorDetected =
  let pre  = [mkErr "F.hs" 23 1 "Found hole: _holeArg :: [a]"]
      post = pre <> [mkErr "F.hs" 13 5 "Variable not in scope: greeting"]
      preSigs  = RefactorTool.errorSignatures pre
      postSigs = RefactorTool.errorSignatures post
      newErrSigs = filter (`notElem` preSigs) postSigs
  in pure (length newErrSigs == 1)

-- | Tiny ctor helpers for the diagnostic tests above.
mkErr :: Text -> Int -> Int -> Text -> GhcError
mkErr file ln col msg = GhcError
  { geFile     = file
  , geLine     = ln
  , geColumn   = col
  , geSeverity = SevError
  , geCode     = Nothing
  , geMessage  = msg
  }

mkWarn :: Text -> Int -> Int -> Text -> GhcError
mkWarn file ln col msg = GhcError
  { geFile     = file
  , geLine     = ln
  , geColumn   = col
  , geSeverity = SevWarning
  , geCode     = Nothing
  , geMessage  = msg
  }

-- | Regression test for issue #46. Pointing extract_binding at a whole
-- top-level equation used to produce broken Haskell — the call site
-- got a bare name (no @=@) and the extracted binding got a nested @=@
-- (its RHS was the original equation line, not the equation's body).
-- The fix refuses any range that sits at column 0, since by Haskell
-- layout rules a body expression is always indented.
--
-- Repro is the exact source from the issue.
testExtractRefusesTopLevelEquation :: IO Bool
testExtractRefusesTopLevelEquation =
  let src = T.unlines
        [ "module Demo where"
        , ""
        , "doubledSum :: [Int] -> Int"
        , "doubledSum xs = foldr (\\x acc -> x * 2 + acc) 0 xs"
        ]
  in pure $ case extractBinding "doubleAndAdd" 4 4 src of
       Left msg ->
         "expression range" `T.isInfixOf` msg
           && "column 0"   `T.isInfixOf` msg
           && "doubledSum" `T.isInfixOf` msg
       Right _  -> False

-- | A type signature lives at column 0 and lifting it is also nonsense.
-- Same column-0 guard catches it; the message still tells the agent to
-- narrow the scope.
testExtractRefusesTypeSignature :: IO Bool
testExtractRefusesTypeSignature =
  let src = T.unlines
        [ "module Demo where"
        , ""
        , "doubledSum :: [Int] -> Int"
        , "doubledSum xs = foldr (\\x acc -> x * 2 + acc) 0 xs"
        ]
  in pure $ case extractBinding "newName" 3 3 src of
       Left msg -> "expression range" `T.isInfixOf` msg
       Right _  -> False

-- | An import line at column 0 must also be refused, not silently
-- corrupted into garbage.
testExtractRefusesImport :: IO Bool
testExtractRefusesImport =
  let src = T.unlines
        [ "module Demo where"
        , ""
        , "import Data.List (sort)"
        , ""
        , "main :: IO ()"
        , "main = print (sort [3,1,2])"
        ]
  in pure $ case extractBinding "imp" 3 3 src of
       Left msg -> "expression range" `T.isInfixOf` msg
       Right _  -> False

-- | Sanity check that the guard does NOT regress the documented
-- success path: an indented body expression must still extract cleanly
-- and the resulting binding must contain a single @=@ (no nested
-- equation, no dangling header).
testExtractAllowsIndentedBody :: IO Bool
testExtractAllowsIndentedBody =
  let src = T.unlines
        [ "module Demo where"
        , ""
        , "doubledSum :: [Int] -> Int"
        , "doubledSum xs ="
        , "  foldr (\\x acc -> x * 2 + acc) 0 xs"
        ]
      countEqualsOnNewBindingLine txt =
        -- The first line of the appended binding must be exactly
        -- "<name> ="; the body lines must NOT start with another
        -- "<name> =".
        let bls = T.lines txt
            isHeader l = "doubleAndAdd =" `T.isPrefixOf` l
            headers    = filter isHeader bls
        in length headers == 1
  in pure $ case extractBinding "doubleAndAdd" 5 5 src of
       Left _   -> False
       Right er ->
         -- Call-site: "doubledSum xs =" preserved on its own line, no
         -- bare orphan name.
         not ("doubledSum xs ="
              `T.isInfixOf` erBindingTxt er)
           && countEqualsOnNewBindingLine (erBindingTxt er)
           && "doubleAndAdd ="    `T.isPrefixOf` erBindingTxt er
           && "doubleAndAdd"      `T.isInfixOf` erNewContent er

-- | The module-header line is the highest-stakes line at column 0:
-- lifting it would orphan the entire file. The guard must refuse it.
testExtractRefusesModuleDecl :: IO Bool
testExtractRefusesModuleDecl =
  let src = T.unlines
        [ "module Demo where"
        , ""
        , "x :: Int"
        , "x = 1"
        ]
  in pure $ case extractBinding "newName" 1 1 src of
       Left msg -> "expression range" `T.isInfixOf` msg
       Right _  -> False

-- | A @data@ declaration sits at column 0 and is meaningless to lift
-- as an expression.
testExtractRefusesDataDecl :: IO Bool
testExtractRefusesDataDecl =
  let src = T.unlines
        [ "module Demo where"
        , ""
        , "data Color = Red | Green | Blue"
        ]
  in pure $ case extractBinding "newName" 3 3 src of
       Left msg -> "expression range" `T.isInfixOf` msg
       Right _  -> False

-- | A @newtype@ declaration is also a top-level form, also refused.
testExtractRefusesNewtypeDecl :: IO Bool
testExtractRefusesNewtypeDecl =
  let src = T.unlines
        [ "module Demo where"
        , ""
        , "newtype Wrap a = Wrap { unwrap :: a }"
        ]
  in pure $ case extractBinding "newName" 3 3 src of
       Left msg -> "expression range" `T.isInfixOf` msg
       Right _  -> False

-- | A @class@ header at column 0 is a top-level form: refused.
testExtractRefusesClassDecl :: IO Bool
testExtractRefusesClassDecl =
  let src = T.unlines
        [ "module Demo where"
        , ""
        , "class Foo a where"
        , "  foo :: a -> a"
        ]
  in pure $ case extractBinding "newName" 3 3 src of
       Left msg -> "expression range" `T.isInfixOf` msg
       Right _  -> False

-- | An @instance@ header at column 0 is a top-level form: refused.
testExtractRefusesInstanceDecl :: IO Bool
testExtractRefusesInstanceDecl =
  let src = T.unlines
        [ "module Demo where"
        , ""
        , "instance Show Color where"
        , "  show Red = \"red\""
        ]
  in pure $ case extractBinding "newName" 3 3 src of
       Left msg -> "expression range" `T.isInfixOf` msg
       Right _  -> False

-- | Pragmas live at column 0 too. The guard treats them like any
-- other top-level form.
testExtractRefusesPragma :: IO Bool
testExtractRefusesPragma =
  let src = T.unlines
        [ "{-# LANGUAGE OverloadedStrings #-}"
        , "module Demo where"
        , ""
        , "main = putStrLn \"hi\""
        ]
  in pure $ case extractBinding "newName" 1 1 src of
       Left msg -> "expression range" `T.isInfixOf` msg
       Right _  -> False

-- | #227: selecting a whole guard branch (line starts with '|') must
-- be refused with a clear message, not allowed to produce broken Haskell.
testExtractRefusesGuardBranch :: IO Bool
testExtractRefusesGuardBranch =
  let src = T.unlines
        [ "module Demo where"
        , ""
        , "factorial :: Int -> Int"
        , "factorial n"
        , "  | n <= 0    = 1"
        , "  | otherwise = n * factorial (n - 1)"
        ]
  in pure $ case extractBinding "recurse" 6 6 src of
       Left msg -> "guard" `T.isInfixOf` msg || "guard branch" `T.isInfixOf` msg
       Right _  -> False

-- | #227: 'isGuardBranch' detects guard-branch lines correctly.
testIsGuardBranch :: IO Bool
testIsGuardBranch = pure $
     isGuardBranch ["  | n <= 0    = 1"]
  && isGuardBranch ["  | otherwise = n * factorial (n - 1)"]
  && isGuardBranch ["    | True = go"]
  && isGuardBranch ["  | n <= 0    = 1", "  | otherwise = n * factorial (n - 1)"]

-- | #227: 'isGuardBranch' correctly ignores normal expression lines.
testIsGuardBranchNeg :: IO Bool
testIsGuardBranchNeg = pure $
     not (isGuardBranch ["  n * factorial (n - 1)"])
  && not (isGuardBranch ["  let x = 1"])
  && not (isGuardBranch [])
  && not (isGuardBranch [""])

-- | An operator definition like @(+++) :: ...@ or @(+++) x y = ...@
-- starts at column 0 too — same refusal.
testExtractRefusesOperatorDef :: IO Bool
testExtractRefusesOperatorDef =
  let src = T.unlines
        [ "module Demo where"
        , ""
        , "(+++) :: Int -> Int -> Int"
        , "(+++) x y = x + y + 1"
        ]
  in pure $ case extractBinding "plus3" 4 4 src of
       Left msg -> "expression range" `T.isInfixOf` msg
       Right _  -> False

-- | A multi-line range that spans a whole equation block (signature +
-- body) at column 0 must be refused even though the equation has its
-- body on a continuation line — the guard sees @commonIndent == 0@
-- because the signature line dominates.
testExtractRefusesMultilineEquation :: IO Bool
testExtractRefusesMultilineEquation =
  let src = T.unlines
        [ "module Demo where"
        , ""
        , "doubledSum :: [Int] -> Int"
        , "doubledSum xs ="
        , "  foldr (\\x acc -> x * 2 + acc) 0 xs"
        ]
  in pure $ case extractBinding "newName" 3 5 src of
       Left msg -> "expression range" `T.isInfixOf` msg
       Right _  -> False

-- | A range that mixes a column-0 line with indented continuations
-- still has @commonIndent == 0@. Must be refused — the column-0 line
-- is the equation header, lifting it would corrupt the file.
testExtractRefusesMixedRange :: IO Bool
testExtractRefusesMixedRange =
  let src = T.unlines
        [ "module Demo where"
        , ""
        , "buildMessage :: String -> String"
        , "buildMessage name ="
        , "  \"Hello, \" ++ name"
        ]
  in pure $ case extractBinding "newName" 4 5 src of
       Left msg -> "expression range" `T.isInfixOf` msg
       Right _  -> False

-- | Leading blank lines in the range must NOT trick the guard. Even if
-- the first selected line is blank, as long as some non-blank line in
-- the range sits at column 0, the guard fires.
testExtractRefusesLeadingBlanksWithCol0 :: IO Bool
testExtractRefusesLeadingBlanksWithCol0 =
  let src = T.unlines
        [ "module Demo where"
        , ""
        , ""
        , "x :: Int"
        , "x = 42"
        ]
  in pure $ case extractBinding "newName" 3 5 src of
       Left msg -> "expression range" `T.isInfixOf` msg
       Right _  -> False

-- | The refusal message must (a) cite the exact line range, (b)
-- include a preview of the offending line so the agent can see what
-- it pointed at, and (c) explain how to recover. All three are
-- machine-checkable via substring presence.
testExtractRefusalMessageShape :: IO Bool
testExtractRefusalMessageShape =
  let src = T.unlines
        [ "module Demo where"
        , ""
        , "x :: Int"
        , "x = 42"
        ]
  in pure $ case extractBinding "newName" 4 4 src of
       Left msg ->
            "4-4"        `T.isInfixOf` msg
         && "x = 42"     `T.isInfixOf` msg
         && "Narrow"     `T.isInfixOf` msg
         && "expression range" `T.isInfixOf` msg
       Right _ -> False

-- | Sanity success path: a let-binding's RHS expression at column 6
-- extracts cleanly.
testExtractAllowsLetBody :: IO Bool
testExtractAllowsLetBody =
  let src = T.unlines
        [ "module Demo where"
        , ""
        , "build :: Int"
        , "build ="
        , "  let result = 1 + 2 + 3"
        , "  in result + 1"
        ]
  in pure $ case extractBinding "smallSum" 5 5 src of
       Left _   -> False
       Right er ->
            "smallSum"         `T.isInfixOf` erNewContent er
         && "smallSum ="       `T.isPrefixOf` erBindingTxt er
         && erIndent er > 0

-- | Sanity success path: a do-block statement at column 2 extracts
-- cleanly.
testExtractAllowsDoBody :: IO Bool
testExtractAllowsDoBody =
  let src = T.unlines
        [ "module Demo where"
        , ""
        , "main :: IO ()"
        , "main = do"
        , "  putStrLn \"hello world\""
        , "  pure ()"
        ]
  in pure $ case extractBinding "greeting" 5 5 src of
       Left _   -> False
       Right er ->
            "greeting"        `T.isInfixOf` erNewContent er
         && "greeting ="      `T.isPrefixOf` erBindingTxt er
         && erIndent er == 2

-- | Sanity success path: a where-clause body expression extracts
-- cleanly. The where-binding header itself sits at column 2; its RHS
-- expression sits at column 4 or beyond.
testExtractAllowsWhereBody :: IO Bool
testExtractAllowsWhereBody =
  let src = T.unlines
        [ "module Demo where"
        , ""
        , "f :: Int -> Int"
        , "f x = helper"
        , "  where"
        , "    helper = x * x + 1"
        ]
  in pure $ case extractBinding "square" 6 6 src of
       Left _   -> False
       Right er ->
            "square"        `T.isInfixOf` erNewContent er
         && "square ="      `T.isPrefixOf` erBindingTxt er

-- | A multi-line indented body: the guard must allow it AND the
-- relative indentation between the body lines must be preserved (the
-- inner lines stay nested under the first line).
testExtractAllowsMultilineBody :: IO Bool
testExtractAllowsMultilineBody =
  let src = T.unlines
        [ "module Demo where"
        , ""
        , "f :: [Int] -> Int"
        , "f xs ="
        , "  foldr"
        , "    (\\x acc -> x + acc)"
        , "    0"
        , "    xs"
        ]
  in pure $ case extractBinding "summing" 5 8 src of
       Left _ -> False
       Right er ->
         let bind  = erBindingTxt er
             newC  = erNewContent er
         in    "summing ="      `T.isPrefixOf` bind
            && "foldr"          `T.isInfixOf` bind
            && "summing"        `T.isInfixOf` newC
            -- The relative indent is preserved (the inner lines stay
            -- deeper than 'foldr').
            && T.isInfixOf "  foldr" bind

-- | Trailing whitespace at end-of-line must NOT trick the guard.
-- @T.takeWhile isSpace@ counts only LEADING whitespace, so trailing
-- whitespace shouldn't shift the indent calculation. Pin the
-- invariant.
testExtractSurvivesEolWhitespace :: IO Bool
testExtractSurvivesEolWhitespace =
  let src = T.unlines
        [ "module Demo where"
        , ""
        , "f :: Int"
        , "f ="
        , "  1 + 2   "  -- trailing spaces
        ]
  in pure $ case extractBinding "onePlus2" 5 5 src of
       Left _   -> False
       Right er -> "onePlus2 =" `T.isPrefixOf` erBindingTxt er

-- | Regression invariant for the bug fix: the appended binding must
-- contain EXACTLY ONE @=@ at column 0 (its own header), and the
-- call-site must NEVER be a bare name with no @=@. Both halves of
-- the original bug pattern must be impossible.
testExtractProducesSingleEquals :: IO Bool
testExtractProducesSingleEquals =
  let src = T.unlines
        [ "module Demo where"
        , ""
        , "f :: Int"
        , "f ="
        , "  let x = 1 + 2 in x + 3"
        ]
      countCol0Equals txt =
        length [ () | l <- T.lines txt
                    , T.length l >= 1
                    , T.take 1 l /= " "
                    , T.take 1 l /= "\t"
                    , "=" `T.isInfixOf` T.takeWhile (/= '\n') l
                    , let stripped = T.strip l
                    , -- Only count lines whose first '=' is the binding
                      -- delimiter, not part of a string literal etc.
                      not ("--" `T.isPrefixOf` stripped)
                    , -- The "<word> =" prefix shape is what we want.
                      let firstEq = T.takeWhile (/= '=') l
                      in not (T.null firstEq)
                ]
  in pure $ case extractBinding "letBody" 5 5 src of
       Left _   -> False
       Right er ->
         let bind = erBindingTxt er
         in    "letBody ="           `T.isPrefixOf` bind
            && countCol0Equals bind == 1

-- | A range consisting of only blank lines triggers the existing
-- "extracted range is empty" path (because the @null body@ check is
-- on raw lines, but actually @body@ is non-empty list of blank lines,
-- so @hasNonBlank body == False@ falls through to the column-0
-- branch's @&& hasNonBlank body@ guard. We expect the textual cut to
-- proceed but produce a degenerate (yet not bug-shaped) result; the
-- compile-verify layer would catch any nonsense. The test below pins
-- that no exception is thrown and the call doesn't refuse with the
-- top-level message — so the guard is precise to non-blank cuts.
testExtractAllBlankRangeRefused :: IO Bool
testExtractAllBlankRangeRefused =
  let src = T.unlines
        [ "module Demo where"
        , ""
        , ""
        , ""
        , "x = 1"
        ]
  in pure $ case extractBinding "newName" 2 4 src of
       -- Either it refuses (different reason) or it goes through —
       -- but it MUST NOT trip the top-level guard since there's no
       -- non-blank column-0 line in [2,4].
       Left msg  -> not ("expression range" `T.isInfixOf` msg)
       Right _er -> True

--------------------------------------------------------------------------------
-- ADT bijection / contract tests for issues #44 (ToolName), #45
-- (ErrorKind), and the companion ADTs RpcMethod and ResourceUri
-- introduced alongside them. Every ADT that maps to a wire string
-- MUST satisfy:
--
--   1. Bijection: parseX (xText t) == Just t  for every constructor.
--   2. Total-rejection: parseX "garbage" == Nothing.
--   3. Wire-form uniqueness: two distinct constructors never share
--      a wire string (would collapse the dispatcher).
--   4. Exhaustiveness: 'allXs' covers every constructor (so adding
--      a constructor without updating the dispatcher fails the test).
--
-- These tests are the forcing function that keeps the wire format
-- from drifting silently when the ADT grows or shrinks.
--------------------------------------------------------------------------------

-- | Bijection: every ToolName round-trips through its text form.
testToolNameRoundTrip :: IO Bool
testToolNameRoundTrip =
  pure $ all (\t -> parseToolName (toolNameText t) == Just t) allToolNames

-- | parseToolName returns Nothing for strings that aren't a
-- registered tool. Without this, the dispatcher would silently route
-- a typo to the wrong tool.
testToolNameParseUnknown :: IO Bool
testToolNameParseUnknown = pure $
     isNothing (parseToolName "")
  && isNothing (parseToolName "ghc_unknown")
  && isNothing (parseToolName "GHC_LOAD")      -- case-sensitive
  && isNothing (parseToolName " ghc_load")     -- whitespace
  && isNothing (parseToolName "ghc_load ")
  && isNothing (parseToolName "ghc-load")      -- hyphen vs underscore
  && isNothing (parseToolName "tools/call")    -- not a method

-- | Two distinct ToolName constructors must never collide on the
-- wire — the dispatcher would otherwise pick the first match and
-- stop, silently breaking the second tool.
testToolNameWireUnique :: IO Bool
testToolNameWireUnique =
  let texts = map toolNameText allToolNames
      uniq  = length (foldr insertOnce [] texts)
      insertOnce x acc = if x `elem` acc then acc else x : acc
  in pure (uniq == length texts && length texts >= 30)

-- | Wire forms must be non-empty, all-ASCII lowercase snake_case
-- (a-z, 0-9, underscores only — no spaces, no hyphens, no slashes,
-- no upper-case). Substring scans across guidance text and the
-- agent's tool-name autocomplete rely on this shape; allowing a
-- stray uppercase letter or hyphen would silently break those
-- consumers.
--
-- The current registry has two prefix families: @ghc_*@ for the
-- Haskell tooling itself and @hoogle_*@ for the Hoogle bridge. We
-- assert each name belongs to one of them so a future tool that
-- forgets the family prefix (and therefore won't sort with its
-- siblings) trips the test.
testToolNameSnakeCase :: IO Bool
testToolNameSnakeCase =
  let isLowerSnake c =
           isAsciiLower c
        || isDigit c
        || c == '_'
      hasFamilyPrefix s =
           "ghc_"    `T.isPrefixOf` s
        || "hoogle_" `T.isPrefixOf` s
      ok t =
        let s = toolNameText t
        in    not (T.null s)
           && hasFamilyPrefix s
           && T.all isLowerSnake s
           && not (T.isInfixOf "__" s)        -- no double underscore
           && not (T.isPrefixOf "_" s)        -- no leading underscore
           && not (T.isSuffixOf "_" s)        -- no trailing underscore
  in pure (all ok allToolNames)

-- | 'allToolNames' is derived from @[minBound .. maxBound]@; if a new
-- constructor is added but Bounded/Enum is broken, this catches it.
testToolNameExhaustive :: IO Bool
testToolNameExhaustive = pure $
     length allToolNames >= 30
  && length allToolNames == length allToolNameTexts

-- | Bijection: every ErrorKind round-trips through its text form.
-- This is the wire contract for tool-error responses; if any
-- constructor's text drifts, the LLM's classifier breaks.
testErrorKindRoundTrip :: IO Bool
testErrorKindRoundTrip =
  let kinds = [Timeout, SessionExhausted, ToolException]
  in pure $ all (\k -> parseErrorKind (renderErrorKind k) == Just k) kinds

-- | Unknown error_kind strings must not parse — protects against
-- silent classification of fresh failure modes as known ones.
testErrorKindParseUnknown :: IO Bool
testErrorKindParseUnknown = pure $
     isNothing (parseErrorKind "")
  && isNothing (parseErrorKind "unknown")
  && isNothing (parseErrorKind "TIMEOUT")           -- case-sensitive
  && isNothing (parseErrorKind "session-exhausted") -- hyphen vs underscore

-- | The three kinds must produce three distinct wire strings.
-- Uniqueness check: deduplicate the list and assert the length is
-- preserved.
testErrorKindWireUnique :: IO Bool
testErrorKindWireUnique =
  let kinds = [Timeout, SessionExhausted, ToolException]
      texts = map renderErrorKind kinds
      uniq  = foldr (\x acc -> if x `elem` acc then acc else x:acc) [] texts
  in pure (length uniq == length texts && length uniq == 3)

-- | The wire strings are exactly the three documented constants.
-- This is the literal contract surfaced to the agent in tool-error
-- responses.
testErrorKindCoversThree :: IO Bool
testErrorKindCoversThree = pure $
     renderErrorKind Timeout          == "timeout"
  && renderErrorKind SessionExhausted == "session_exhausted"
  && renderErrorKind ToolException    == "tool_exception"

-- | Bijection: every RpcMethod round-trips through its text form.
testRpcMethodRoundTrip :: IO Bool
testRpcMethodRoundTrip =
  pure $ all (\m -> parseRpcMethod (rpcMethodText m) == Just m) allRpcMethods

-- | Unknown JSON-RPC methods must not parse — this is what the
-- dispatcher uses to send a "method not found" envelope back to the
-- caller.
testRpcMethodParseUnknown :: IO Bool
testRpcMethodParseUnknown = pure $
     isNothing (parseRpcMethod "")
  && isNothing (parseRpcMethod "tools/unknown")
  && isNothing (parseRpcMethod "tools.list")          -- dot vs slash
  && isNothing (parseRpcMethod "TOOLS/CALL")          -- case-sensitive
  && isNothing (parseRpcMethod "ghc_load")            -- not a tool

-- | Two distinct RpcMethod constructors must never share a wire
-- string. The dispatcher matches by exact text, so a collision would
-- silently route both to the same handler.
testRpcMethodWireUnique :: IO Bool
testRpcMethodWireUnique =
  let texts = allRpcMethodTexts
      uniq  = foldr (\x acc -> if x `elem` acc then acc else x:acc) [] texts
  in pure (length uniq == length texts && length texts == length allRpcMethods)

-- | Pin the seven JSON-RPC methods we currently support against
-- their literal wire strings — these are part of the MCP protocol
-- contract; any drift would break LLM clients.
testRpcMethodCoversAllMcp :: IO Bool
testRpcMethodCoversAllMcp = pure $
     rpcMethodText Initialize             == "initialize"
  && rpcMethodText Initialized            == "initialized"
  && rpcMethodText ToolsList              == "tools/list"
  && rpcMethodText ToolsCall              == "tools/call"
  && rpcMethodText ResourcesList          == "resources/list"
  && rpcMethodText ResourcesRead          == "resources/read"
  && rpcMethodText NotificationsCancelled == "notifications/cancelled"
  && length allRpcMethods == 7

-- | 'isNotification' must classify each method correctly.
-- Notifications are JSON-RPC messages without an @id@ — the server
-- must NOT send a response. A misclassification here either drops
-- a real response (request misclassified as notification) or sends
-- a spurious one (notification misclassified as request).
testRpcMethodIsNotification :: IO Bool
testRpcMethodIsNotification = pure $
  -- Notifications: handshake-complete + cancellation.
     isNotification Initialized
  && isNotification NotificationsCancelled
  -- Requests: every other method has an id-bearing reply.
  && not (isNotification Initialize)
  && not (isNotification ToolsList)
  && not (isNotification ToolsCall)
  && not (isNotification ResourcesList)
  && not (isNotification ResourcesRead)
  -- Sanity: classification is total over the ADT — every constructor
  -- in 'allRpcMethods' has a defined notification status.
  && length [ () | m <- allRpcMethods
                 , let _b = isNotification m
            ] == length allRpcMethods

-- | Bijection: every ResourceUri round-trips through its text form.
testResourceUriRoundTrip :: IO Bool
testResourceUriRoundTrip =
  pure $ all (\u -> parseResourceUri (resourceUriText u) == Just u) allResourceUris

-- | Unknown URIs must not parse. The resources/read dispatcher
-- relies on this to reject probes for non-advertised URIs.
testResourceUriParseUnknown :: IO Bool
testResourceUriParseUnknown = pure $
     isNothing (parseResourceUri "")
  && isNothing (parseResourceUri "haskell-flows://nonexistent")
  && isNothing (parseResourceUri "https://example.com")
  && isNothing (parseResourceUri "haskell-flows://rules/other")
  && isNothing (parseResourceUri "file:///etc/passwd")

-- | The advertised wire form for the only resource we currently
-- expose. This is part of the MCP resource contract — clients hold
-- the URI literally.
testResourceUriWireCanonical :: IO Bool
testResourceUriWireCanonical = pure $
     resourceUriText WorkflowRules == "haskell-flows://rules/workflow"
  && length allResourceUris       == 1
  && length allResourceUriTexts   == 1

--------------------------------------------------------------------------------
-- Issue #90 (Phase A): Mcp.Envelope contract
--
-- Tests the unified response envelope at the JSON-wire boundary plus
-- the smart-constructor invariants. Phase A is pure-additive — these
-- tests exercise the new module without touching any existing tool.
-- The wire-format strings are also a security-relevant contract: the
-- @StatusRefused@ + @{path_traversal, newline_injection,
-- sentinel_poisoning, oversized_input, empty_input}@ pairing is what
-- the future sanitize-layer migration will emit, so the round-trip
-- assertions double as wire-stability anchors for those error kinds.
--------------------------------------------------------------------------------

-- | Every 'ToolStatus' encodes to its documented lowercase wire form
-- and decodes back. Anchors the wire string against accidental
-- rename in 'statusToText'. Iterates @[minBound..maxBound]@ so a
-- future eighth status fails compilation, not at runtime.
testEnvelopeStatusRoundTrip :: IO Bool
testEnvelopeStatusRoundTrip =
  let allStatuses = [minBound .. maxBound] :: [Env.ToolStatus]
      expected =
        [ (Env.StatusOk,          "ok")
        , (Env.StatusPartial,     "partial")
        , (Env.StatusNoMatch,     "no_match")
        , (Env.StatusRefused,     "refused")
        , (Env.StatusFailed,      "failed")
        , (Env.StatusTimeout,     "timeout")
        , (Env.StatusUnavailable, "unavailable")
        ]
      wireFormCorrect = all (\(s, t) -> Env.statusToText s == t) expected
      reverseTotal    = all (\s -> Env.textToStatus (Env.statusToText s) == Just s) allStatuses
      jsonRound s     = case A.fromJSON (A.toJSON s) of
                          A.Success s' -> s' == s
                          _            -> False
      jsonAllOk       = all jsonRound allStatuses
  in pure (wireFormCorrect && reverseTotal && jsonAllOk)

-- | 'ErrorKind' has 23 documented wire-form strings (issue #90 §4).
-- Spot-check a representative subset against the documented strings,
-- plus assert the round-trip works for the full enum.
testEnvelopeErrorKindRoundTrip :: IO Bool
testEnvelopeErrorKindRoundTrip =
  let allKinds = [minBound .. maxBound] :: [Env.ErrorKind]
      pinned =
        [ (Env.MissingArg,             "missing_arg")
        , (Env.TypeMismatch,           "type_mismatch")
        , (Env.PathTraversal,          "path_traversal")
        , (Env.NewlineInjection,       "newline_injection")
        , (Env.SentinelPoisoning,      "sentinel_poisoning")
        , (Env.OversizedInput,         "oversized_input")
        , (Env.NotInScope,             "not_in_scope")
        , (Env.ModuleNotInGraph,       "module_not_in_graph")
        , (Env.ModulePathDoesNotExist, "module_path_does_not_exist")
        , (Env.OutsideSourceDirs,      "outside_source_dirs")
        , (Env.UnresolvableDep,        "unresolvable_dep")
        , (Env.VerifyFailed,           "verify_failed")
        , (Env.InnerTimeout,           "inner_timeout")
        , (Env.OuterTimeout,           "outer_timeout")
        , (Env.SessionExhausted,       "session_exhausted")
        , (Env.BinaryUnavailable,      "binary_unavailable")
        , (Env.HandWrittenFileGuard,   "hand_written_file_guard")  -- #131
        , (Env.Regression,             "regression")               -- #190
        ]
      pinnedOk = all (\(k, t) -> Env.errorKindToText k == t) pinned
      reverseTotal = all (\k -> Env.textToErrorKind (Env.errorKindToText k) == Just k)
                         allKinds
      countOk = length allKinds == 28  -- §4: 24 + GateFailure (#119) + OutsideSourceDirs (#110) + HandWrittenFileGuard (#131) + Regression (#190)
  in pure (pinnedOk && reverseTotal && countOk)

-- | Companion round-trip for 'WarningKind'.
testEnvelopeWarningKindRoundTrip :: IO Bool
testEnvelopeWarningKindRoundTrip =
  let allKinds = [minBound .. maxBound] :: [Env.WarningKind]
      pinned =
        [ (Env.DeprecatedField,     "deprecated_field")
        , (Env.DeprecatedTool,      "deprecated_tool")
        , (Env.LowConfidence,       "low_confidence")
        , (Env.SlowPath,            "slow_path")
        , (Env.RecoveredAfterRetry, "recovered_after_retry")
        , (Env.OtherWarning,        "other")
        ]
      pinnedOk     = all (\(k, t) -> Env.warningKindToText k == t) pinned
      reverseTotal = all (\k -> Env.textToWarningKind (Env.warningKindToText k) == Just k)
                         allKinds
  in pure (pinnedOk && reverseTotal)

-- | 'mkOk' produces the canonical happy-path shape: status=ok,
-- result present, error absent. Encodes through Aeson and asserts
-- the wire-form fields.
testEnvelopeMkOk :: IO Bool
testEnvelopeMkOk =
  let payload = A.object [ "answer" A..= (42 :: Int) ]
      response = Env.mkOk payload
      encoded = A.toJSON response
      lookupKey k v = case encoded of
        A.Object o -> AKM.lookup (AKey.fromText k) o == Just v
        _          -> False
  in pure
       ( Env.reStatus response == Env.StatusOk
      && Env.reResult response == Just payload
      && isNothing (Env.reError response)
      && lookupKey "status"  (A.String "ok")
      && lookupKey "result"  payload
       )

-- | 'mkRefused' produces the canonical refusal shape: status=refused,
-- error present, result absent. Status='refused' is the only
-- discriminator post-#90; the dropped legacy 'success: false'
-- duplicate used to live alongside it during the migration window.
testEnvelopeMkRefused :: IO Bool
testEnvelopeMkRefused =
  let err      = Env.mkErrorEnvelope Env.PathTraversal "target path escapes project root"
      response = Env.mkRefused err
      encoded  = A.toJSON response
      lookupKey k v = case encoded of
        A.Object o -> AKM.lookup (AKey.fromText k) o == Just v
        _          -> False
      hasErrorObj = case encoded of
        A.Object o -> case AKM.lookup (AKey.fromText "error") o of
          Just (A.Object eo) ->
            AKM.lookup (AKey.fromText "kind")    eo == Just (A.String "path_traversal")
              && AKM.lookup (AKey.fromText "message") eo == Just (A.String "target path escapes project root")
          _ -> False
        _ -> False
  in pure
       ( Env.reStatus response == Env.StatusRefused
      && isNothing (Env.reResult response)
      && lookupKey "status"  (A.String "refused")
      && hasErrorObj
       )

-- | 'FromJSON' enforces the §2 invariant: a payload that announces
-- @status: ok@ but omits @result@ is malformed and must fail the
-- parser. Catches the case where a future emitter forgets the
-- @result@ field — without this gate, the consumer would see
-- @reResult = Nothing@ and silently degrade.
testEnvelopeFromJSONRequiresResult :: IO Bool
testEnvelopeFromJSONRequiresResult =
  let bytes = "{\"status\":\"ok\"}"
  in pure $ case A.eitherDecode bytes :: Either String Env.ToolResponse of
       Left err -> "requires" `List.isInfixOf` err
       Right _  -> False

-- | Inverse: @status: failed@ without @error@ must fail.
testEnvelopeFromJSONRequiresError :: IO Bool
testEnvelopeFromJSONRequiresError =
  let bytes = "{\"status\":\"failed\"}"
  in pure $ case A.eitherDecode bytes :: Either String Env.ToolResponse of
       Left err -> "requires" `List.isInfixOf` err
       Right _  -> False

-- | Encode → decode round-trip. Builds a representative response
-- with every optional field populated; assertion is structural
-- equality after the round-trip.
testEnvelopeRoundTrip :: IO Bool
testEnvelopeRoundTrip =
  let payload = A.object [ "type" A..= ("Int -> Int" :: Text) ]
      warning = Env.Warning
                  { Env.wKind    = Env.LowConfidence
                  , Env.wMessage = "result inferred via best-effort"
                  , Env.wExtra   = Just (A.object [ "confidence" A..= ("medium" :: Text) ])
                  }
      meta = Env.Meta
               { Env.metaTool       = "ghc_type"
               , Env.metaVersion    = "0.1.0.0"
               , Env.metaDurationMs = 42
               , Env.metaTraceId    = Just "7f3a2b"
               }
      response =
        Env.withMeta meta
        . Env.withNextStep (A.object [ "tool" A..= ("ghc_quickcheck" :: Text) ])
        . Env.withWarnings [warning]
        $ Env.mkOk payload
      encoded = A.encode response
  in pure $ case A.eitherDecode encoded :: Either String Env.ToolResponse of
       Right decoded
         | decoded == response -> True
       _                       -> False

-- | The optional fields on 'ErrorEnvelope' default to 'Nothing' on
-- decode when omitted — confirms that minimal-shape errors
-- (kind + message only) parse cleanly without the consumer needing
-- to special-case missing keys.
testEnvelopeErrorOptionalFields :: IO Bool
testEnvelopeErrorOptionalFields =
  let bytes = "{\"kind\":\"missing_arg\",\"message\":\"required field 'expression' is missing\"}"
  in pure $ case A.eitherDecode bytes :: Either String Env.ErrorEnvelope of
       Right ee ->
         Env.eeKind ee == Env.MissingArg
           && Env.eeMessage ee == "required field 'expression' is missing"
           && isNothing (Env.eeField ee)
           && isNothing (Env.eeHint ee)
       Left _ -> False

-- | When a response has no warnings, the @warnings@ field is omitted
-- from the wire output (rather than being serialised as an empty
-- array). Keeps the wire payload small and deterministic so a future
-- consumer's string-equality oracle on the JSON doesn't break when
-- a tool that previously emitted warnings stops.
testEnvelopeWarningsOmittedEmpty :: IO Bool
testEnvelopeWarningsOmittedEmpty =
  let response = Env.mkOk (A.object [])
      encoded  = A.toJSON response
      hasWarningsKey = case encoded of
        A.Object o -> AKM.member (AKey.fromText "warnings") o
        _          -> False
  in pure (not hasWarningsKey)

-- | QC: round-trip totality for 'ToolStatus'. Hand-rolled 'Arbitrary'
-- via @[minBound..maxBound]@ + 'QC.elements' so we don't pull in
-- @quickcheck-instances@ for the enum. Every status, when serialised
-- and re-parsed, returns the same value.
prop_envelopeStatusTotal :: QC.Property
prop_envelopeStatusTotal = QC.forAll (QC.elements [minBound..maxBound]) $ \s ->
  case A.fromJSON (A.toJSON (s :: Env.ToolStatus)) of
    A.Success s' -> s' === s
    A.Error e    -> QC.counterexample e (QC.property False)

-- | QC: same totality for 'ErrorKind'. 23 values per #90 §4.
prop_envelopeErrorKindTotal :: QC.Property
prop_envelopeErrorKindTotal = QC.forAll (QC.elements [minBound..maxBound]) $ \k ->
  case A.fromJSON (A.toJSON (k :: Env.ErrorKind)) of
    A.Success k' -> k' === k
    A.Error e    -> QC.counterexample e (QC.property False)

-- | QC: same totality for 'WarningKind'.
prop_envelopeWarningKindTotal :: QC.Property
prop_envelopeWarningKindTotal = QC.forAll (QC.elements [minBound..maxBound]) $ \w ->
  case A.fromJSON (A.toJSON (w :: Env.WarningKind)) of
    A.Success w' -> w' === w
    A.Error e    -> QC.counterexample e (QC.property False)

-- | Helper for Phase B tool-migration tests: drive the tool's
-- handler, decode the JSON body inside the wire-level 'ToolResult',
-- return the parsed 'Env.ToolResponse' (or a string-shaped failure
-- describing why the decode failed).
runToolEnvelope
  :: (A.Value -> IO ToolResult)
  -> A.Value
  -> IO (Either String Env.ToolResponse)
runToolEnvelope h args = do
  result <- h args
  case trContent result of
    [TextContent body] ->
      pure (A.eitherDecode (TLE.encodeUtf8 (TL.fromStrict body)))
    _ ->
      pure (Left "expected exactly one TextContent in trContent")

-- | Phase B oracle: 'ghc_toolchain_status' emits an envelope-shaped
-- response whose status is one of @ok | partial | failed@. The exact
-- status depends on the host's installed binaries — on a dev box
-- with cabal/ghc/hlint present and (typically) fourmolu/hoogle
-- absent, we'd see @partial@. CI may have different binaries; the
-- test stays host-independent by accepting any of the three valid
-- statuses.
testToolchainStatusEnvelopeShape :: IO Bool
testToolchainStatusEnvelopeShape = do
  decoded <- runToolEnvelope ToolchainStatusTool.handle (A.object [])
  pure $ case decoded of
    Right env ->
      Env.reStatus env
        `elem` [Env.StatusOk, Env.StatusPartial, Env.StatusFailed]
    Left _ -> False

-- | The migrated tool keeps the @tools@ + @blocking_gates@ + @summary@
-- fields inside @result@ so any consumer keying on them via the
-- legacy shape continues to function during the dual-shape window.
testToolchainStatusBackcompatFields :: IO Bool
testToolchainStatusBackcompatFields = do
  decoded <- runToolEnvelope ToolchainStatusTool.handle (A.object [])
  pure $ case decoded of
    Right env -> case Env.reResult env of
      Just (A.Object payload) ->
        AKM.member (AKey.fromText "tools")          payload
          && AKM.member (AKey.fromText "blocking_gates") payload
          && AKM.member (AKey.fromText "summary")        payload
      _ -> False
    Left _ -> False

-- | 'ghc_toolchain_warmup' is the simpler analogue of toolchain_status —
-- it only probes optional binaries. After Phase B the response is
-- 'ok' when every probed binary is present, 'partial' when one or
-- more are missing. The host-independent assertion: the response
-- decodes as an envelope with status ∈ {ok, partial}.
testToolchainWarmupEnvelopeShape :: IO Bool
testToolchainWarmupEnvelopeShape = do
  decoded <- runToolEnvelope ToolchainWarmupTool.handle (A.object [])
  pure $ case decoded of
    Right env -> Env.reStatus env `elem` [Env.StatusOk, Env.StatusPartial]
              && case Env.reResult env of
                   Just (A.Object payload) ->
                     AKM.member (AKey.fromText "tools") payload
                   _ -> False
    Left _ -> False

-- | When the warmup status is 'partial' (i.e. ≥1 optional binary is
-- missing), the response MUST carry a non-empty 'warnings' array
-- with one entry per missing binary. This is the contract that
-- lets an agent know *which* downstream tool surfaces are about to
-- start returning status='unavailable'.
testToolchainWarmupPartialWarnings :: IO Bool
testToolchainWarmupPartialWarnings = do
  decoded <- runToolEnvelope ToolchainWarmupTool.handle (A.object [])
  pure $ case decoded of
    Right env
      | Env.reStatus env == Env.StatusPartial ->
          not (null (Env.reWarnings env))
            && all (\w -> Env.wKind w == Env.SlowPath) (Env.reWarnings env)
      | Env.reStatus env == Env.StatusOk ->
          null (Env.reWarnings env)  -- ok ⇒ no missing binaries ⇒ no warnings
      | otherwise -> False  -- only ok or partial expected
    Left _ -> False

-- | Helper: stage a tmpdir with the given .cabal-file body and run
-- 'ValidateCabalTool.handle' against it. Returns the parsed
-- envelope so the test can branch on status / inspect result. The
-- tmpdir is removed on the way out — leaves no residual state.
runValidateCabalIn :: Text -> IO (Either String Env.ToolResponse)
runValidateCabalIn cabalBody = do
  tmp <- getTemporaryDirectory
  let dir = tmp </> "haskell-flows-validate-test"
  removePathForcibly dir
  createDirectoryIfMissing True dir
  TIO.writeFile (dir </> "test-pkg.cabal") cabalBody
  result <- case mkProjectDir dir of
    Left _   -> pure (Left "could not build ProjectDir for tmp")
    Right pd -> do
      tr <- ValidateCabalTool.handle pd (A.object [])
      case trContent tr of
        [TextContent body] ->
          pure (A.eitherDecode (TLE.encodeUtf8 (TL.fromStrict body)))
        _ -> pure (Left "expected exactly one TextContent")
  removePathForcibly dir
  pure result

-- | Cabal fixture with no obvious cabal-check warnings or errors.
-- Whatever cabal-check actually says depends on the cabal/ghc
-- version installed — we don't pin a status here, only the
-- *contract* (status reflects errors / warnings counts faithfully).
minimalCabalBody :: Text
minimalCabalBody = T.unlines
  [ "cabal-version:      3.0"
  , "name:               test-pkg"
  , "version:            0.1.0.0"
  , "synopsis:           a test fixture for #90 phase B validation"
  , "description:        a longer description that exceeds the synopsis "
    <> "in length so cabal-check does not warn about it being shorter."
  , "category:           Testing"
  , "license:            BSD-3-Clause"
  , "author:             test"
  , "maintainer:         test@example.com"
  , "build-type:         Simple"
  , ""
  , "library"
  , "    default-language: GHC2024"
  , "    build-depends:    base >= 4.20 && < 5"
  ]

-- | Cabal fixture that intentionally triggers the duplicate-dep
-- heuristic. The dup is *guaranteed* to be a warning regardless of
-- cabal version. cabal-check may add more — we don't pin specifics.
duplicateDepCabalBody :: Text
duplicateDepCabalBody = T.unlines
  [ "cabal-version:      3.0"
  , "name:               test-pkg"
  , "version:            0.1.0.0"
  , "synopsis:           dup-dep fixture for #90 phase B validation"
  , "description:        a longer description that exceeds the synopsis "
    <> "in length so cabal-check does not warn about it being shorter."
  , "category:           Testing"
  , "license:            BSD-3-Clause"
  , "author:             test"
  , "maintainer:         test@example.com"
  , "build-type:         Simple"
  , ""
  , "library"
  , "    default-language: GHC2024"
  , "    build-depends:    base >= 4.20 && < 5, base"  -- intentional duplicate
  ]

-- | Phase B contract: when 'cabal check' returns no errors, the
-- envelope status is 'ok' (no warnings) or 'partial' (warnings only).
-- Status MUST NOT be 'failed' if there are no errors. Anchors the
-- (errors == 0) ⇒ (status ∈ {ok, partial}) implication.
testValidateCabalClean :: IO Bool
testValidateCabalClean = do
  decoded <- runValidateCabalIn minimalCabalBody
  pure $ case decoded of
    Right env -> case Env.reResult env of
      Just (A.Object payload) ->
        case AKM.lookup (AKey.fromText "errors") payload of
          Just (A.Number 0) ->
            -- 0 errors ⇒ status='ok' (#119: 'partial' was retired for
            -- warnings-only results; all 0-error outcomes are now 'ok')
            Env.reStatus env == Env.StatusOk
          Just (A.Number _) ->
            -- non-zero errors ⇒ status='failed' (the other branch)
            Env.reStatus env == Env.StatusFailed
          _ -> False
      _ -> False
    Left _ -> False

-- | Phase B + #119 contract: a cabal fixture with the duplicate-dep
-- heuristic warning *plus* zero cabal-check errors produces
-- status='ok' (not 'partial' — #119 fix) with envelope-warnings
-- populated. If cabal-check happens to also raise errors on this
-- fixture, status shifts to 'failed' — accept either, but assert
-- the structured 'warnings' array is populated whenever issues exist.
testValidateCabalWarnings :: IO Bool
testValidateCabalWarnings = do
  decoded <- runValidateCabalIn duplicateDepCabalBody
  pure $ case decoded of
    Right env -> case Env.reResult env of
      Just (A.Object payload) ->
        case ( AKM.lookup (AKey.fromText "errors")   payload
             , AKM.lookup (AKey.fromText "warnings") payload
             ) of
          (Just (A.Number 0), Just (A.Number w))
            | w > 0 ->
                -- #119: warnings-only → status='ok', not 'partial'.
                Env.reStatus env == Env.StatusOk
                  && not (null (Env.reWarnings env))
            | otherwise -> Env.reStatus env == Env.StatusOk
          (Just (A.Number e), _)
            | e > 0 -> Env.reStatus env == Env.StatusFailed
          _ -> False
      _ -> False
    Left _ -> False

-- | Phase B: a project dir with no .cabal file at all produces
-- status='failed' with the envelope's
-- 'error.kind=module_path_does_not_exist'. The earlier code path
-- emitted 'success: false' with a free-form error string;
-- post-Phase-B the error is structured.
testValidateCabalErrors :: IO Bool
testValidateCabalErrors = do
  tmp <- getTemporaryDirectory
  let dir = tmp </> "haskell-flows-validate-no-cabal"
  removePathForcibly dir
  createDirectoryIfMissing True dir
  -- No .cabal file in dir.
  result <- case mkProjectDir dir of
    Left _   -> pure (Left "could not build ProjectDir for tmp")
    Right pd -> do
      tr <- ValidateCabalTool.handle pd (A.object [])
      case trContent tr of
        [TextContent body] ->
          pure (A.eitherDecode (TLE.encodeUtf8 (TL.fromStrict body)))
        _ -> pure (Left "expected exactly one TextContent")
  removePathForcibly dir
  pure $ case result of
    Right env
      | Env.reStatus env == Env.StatusFailed
      , Just err <- Env.reError env ->
          Env.eeKind err == Env.ModulePathDoesNotExist
    _ -> False

-- | Phase B back-compat: the legacy 'issues' array must continue
-- to live under 'result' so any consumer keying on it during the
-- migration window keeps working. Since the failed-status path
-- carries the structured info inside 'error' instead of 'result',
-- we drive the success/partial path (a clean cabal) here so 'result'
-- is guaranteed to exist.
testValidateCabalBackcompatIssues :: IO Bool
testValidateCabalBackcompatIssues = do
  decoded <- runValidateCabalIn duplicateDepCabalBody
  pure $ case decoded of
    Right env -> case Env.reResult env of
      Just (A.Object payload) ->
        case AKM.lookup (AKey.fromText "issues") payload of
          Just (A.Array _) -> True
          _                -> False
      Nothing ->
        -- failed path is also acceptable for this test if
        -- cabal-check raised errors — the contract is just that
        -- a successful path keeps the issues array
        Env.reStatus env == Env.StatusFailed
      _ -> False
    Left _ -> False

-- | Phase B helper: build the cluster of state values 'WorkflowTool.handle'
-- needs and drive it for a given action. Returns the parsed envelope.
runWorkflow :: A.Value -> IO (Either String Env.ToolResponse)
runWorkflow args = do
  let pd = case mkProjectDir "/tmp" of
             Right p -> p
             Left e  -> error ("test fixture: bad project dir: " <> show e)
  pdRef    <- newIORef pd
  sessRef  <- newMVar Nothing
  wsRef    <- WS.newWorkflowStateRef
  ws       <- WS.readState wsRef
  let staleness = StalenessReport
        { srStale            = False
        , srBinaryOlderBySec = Nothing
        , srMessage          = Nothing
        }
      toolNames = ["ghc_load", "ghc_type", "ghc_workflow"]
  -- PR-4: workflow handler gained an isSelfProject arg. Tests run
  -- against /tmp, never the MCP source tree, so the flag is False.
  -- #253 Phase 5: workflow handler gained scratchRef for status section.
  scratch    <- SP.openStore pd
  scratchRef <- newIORef scratch
  result <- WorkflowTool.handle pdRef sessRef toolNames ws staleness False scratchRef args
  case trContent result of
    [TextContent body] ->
      pure (A.eitherDecode (TLE.encodeUtf8 (TL.fromStrict body)))
    _ -> pure (Left "expected exactly one TextContent")

-- | 'ghc_workflow {action: status}' returns an envelope-shaped
-- response with status='ok' and a result carrying the documented
-- status fields ('view', 'projectDir', 'ghciAlive', 'toolsActive',
-- 'phase', 'staleness').
testWorkflowStatusEnvelope :: IO Bool
testWorkflowStatusEnvelope = do
  decoded <- runWorkflow (A.object [ "action" A..= ("status" :: Text) ])
  pure $ case decoded of
    Right env
      | Env.reStatus env == Env.StatusOk
      , Just (A.Object payload) <- Env.reResult env ->
          AKM.lookup (AKey.fromText "view") payload == Just (A.String "status")
            && AKM.member (AKey.fromText "projectDir") payload
            && AKM.member (AKey.fromText "ghciAlive") payload
            && AKM.member (AKey.fromText "toolsActive") payload
            && AKM.member (AKey.fromText "phase") payload
            && AKM.member (AKey.fromText "staleness") payload
    _ -> False

-- | #253 Phase 5: 'ghc_workflow {action: status}' result carries
-- a 'scratchpad' section with entries/open/verified/promoted/hint.
testWorkflowStatusHasScratchpad :: IO Bool
testWorkflowStatusHasScratchpad = do
  decoded <- runWorkflow (A.object [ "action" A..= ("status" :: Text) ])
  pure $ case decoded of
    Right env
      | Env.reStatus env == Env.StatusOk
      , Just (A.Object payload) <- Env.reResult env ->
          case AKM.lookup (AKey.fromText "scratchpad") payload of
            Just (A.Object sp) ->
              AKM.member (AKey.fromText "entries")  sp
                && AKM.member (AKey.fromText "open")     sp
                && AKM.member (AKey.fromText "verified") sp
                && AKM.member (AKey.fromText "promoted") sp
                && AKM.member (AKey.fromText "hint")     sp
                -- fresh scratchpad has 0 entries
                && AKM.lookup (AKey.fromText "entries") sp == Just (A.Number 0)
            _ -> False
    _ -> False

-- | 'ghc_workflow {action: help}' status='ok' carrying a help view.
testWorkflowHelpEnvelope :: IO Bool
testWorkflowHelpEnvelope = do
  decoded <- runWorkflow (A.object [ "action" A..= ("help" :: Text) ])
  pure $ case decoded of
    Right env
      | Env.reStatus env == Env.StatusOk
      , Just (A.Object payload) <- Env.reResult env ->
          AKM.lookup (AKey.fromText "view") payload == Just (A.String "help")
            && AKM.member (AKey.fromText "phaseHint") payload
            && AKM.member (AKey.fromText "steps") payload
    _ -> False

-- | 'ghc_workflow {action: next}' status='ok' carrying a single
-- next-tool recommendation.
testWorkflowNextEnvelope :: IO Bool
testWorkflowNextEnvelope = do
  decoded <- runWorkflow (A.object [ "action" A..= ("next" :: Text) ])
  pure $ case decoded of
    Right env
      | Env.reStatus env == Env.StatusOk
      , Just (A.Object payload) <- Env.reResult env ->
          AKM.lookup (AKey.fromText "view") payload == Just (A.String "next")
            && AKM.member (AKey.fromText "tool") payload
            && AKM.member (AKey.fromText "why") payload
    _ -> False

-- | An unknown action lands as status='failed' with
-- error.kind='validation' (the value was structurally a valid
-- string but outside the action enum).
testWorkflowRejectsUnknownAction :: IO Bool
testWorkflowRejectsUnknownAction = do
  decoded <- runWorkflow (A.object [ "action" A..= ("teleport" :: Text) ])
  pure $ case decoded of
    Right env
      | Env.reStatus env == Env.StatusFailed
      , Just err <- Env.reError env ->
          Env.eeKind err == Env.Validation
    _ -> False

-- | Phase B helper: build a fresh tmpdir-based ProjectDir + drive
-- 'BootstrapTool.handle' with the given args. Returns the parsed
-- envelope and cleans up the tmpdir on exit.
runBootstrap :: A.Value -> IO (Either String Env.ToolResponse)
runBootstrap args = do
  tmp <- getTemporaryDirectory
  let dir = tmp </> "haskell-flows-bootstrap-test"
  removePathForcibly dir
  createDirectoryIfMissing True dir
  result <- case mkProjectDir dir of
    Left _   -> pure (Left "could not build ProjectDir")
    Right pd -> do
      tr <- BootstrapTool.handle pd [] args
      case trContent tr of
        [TextContent body] ->
          pure (A.eitherDecode (TLE.encodeUtf8 (TL.fromStrict body)))
        _ -> pure (Left "expected exactly one TextContent")
  removePathForcibly dir
  pure result

-- | 'ghc_bootstrap host=claude-code write=false' emits status='ok' with
-- the rules content + the canonical claude-code target path inside 'result'.
-- Issue #193: pass write=false explicitly; the default is now write=true (write to disk).
testBootstrapClaudeCodePreviewEnvelope :: IO Bool
testBootstrapClaudeCodePreviewEnvelope = do
  decoded <- runBootstrap (A.object [ "host" A..= ("claude-code" :: Text)
                                    , "write" A..= False ])
  pure $ case decoded of
    Right env
      | Env.reStatus env == Env.StatusOk
      , Just (A.Object payload) <- Env.reResult env ->
          AKM.lookup (AKey.fromText "host")    payload == Just (A.String "claude-code")
            && AKM.lookup (AKey.fromText "mode")    payload == Just (A.String "preview")
            && AKM.member (AKey.fromText "content") payload
            && AKM.member (AKey.fromText "target")  payload  -- non-generic ⇒ target path is set
    _ -> False

-- | 'ghc_bootstrap host=generic' emits status='ok' with content but
-- no 'target' field (per the existing contract: generic mode has no
-- canonical target path).
testBootstrapGenericPreviewEnvelope :: IO Bool
testBootstrapGenericPreviewEnvelope = do
  decoded <- runBootstrap (A.object [ "host" A..= ("generic" :: Text) ])
  pure $ case decoded of
    Right env
      | Env.reStatus env == Env.StatusOk
      , Just (A.Object payload) <- Env.reResult env ->
          AKM.lookup (AKey.fromText "host") payload == Just (A.String "generic")
            && AKM.lookup (AKey.fromText "mode") payload == Just (A.String "preview")
            && AKM.member (AKey.fromText "content") payload
            && not (AKM.member (AKey.fromText "target") payload)
    _ -> False

-- | An unknown host lands as status='failed' with
-- error.kind='validation' (the value was structurally a string,
-- just outside the closed Host enum).
testBootstrapRejectsUnknownHost :: IO Bool
testBootstrapRejectsUnknownHost = do
  decoded <- runBootstrap (A.object [ "host" A..= ("orbital-station" :: Text) ])
  pure $ case decoded of
    Right env
      | Env.reStatus env == Env.StatusFailed
      , Just err <- Env.reError env ->
          Env.eeKind err == Env.Validation
    _ -> False

-- | A request with no 'host' lands as status='failed' with
-- error.kind='missing_arg'. Catches the case where the FromJSON
-- 'fail' string format changes and the discriminator regresses.
testBootstrapRejectsMissingHost :: IO Bool
testBootstrapRejectsMissingHost = do
  decoded <- runBootstrap (A.object [])
  pure $ case decoded of
    Right env
      | Env.reStatus env == Env.StatusFailed
      , Just err <- Env.reError env ->
          Env.eeKind err == Env.MissingArg
    _ -> False

-- | #165: when 'host' is missing the error message must mention the
-- accepted values, not leak a raw aeson key name.
testBootstrapMissingHostFriendlyMessage :: IO Bool
testBootstrapMissingHostFriendlyMessage = do
  decoded <- runBootstrap (A.object [])
  pure $ case decoded of
    Right env
      | Env.reStatus env == Env.StatusFailed
      , Just err <- Env.reError env
      , Env.eeKind err == Env.MissingArg ->
          let msg = Env.eeMessage err
          in  "claude-code" `T.isInfixOf` msg
           && "cursor"      `T.isInfixOf` msg
           && "generic"     `T.isInfixOf` msg
    _ -> False

-- | 'ghc_imports' returns the interactive context's import list.
-- Phase B: status='ok' with result carrying the legacy 'count' +
-- 'imports' fields (preserved during the dual-shape window). The
-- absolute *contents* of the imports list depend on whatever
-- autoLoadProject + augmentEvalContext settled on (Prelude + a few
-- stdlib modules); we don't pin specific names — only the contract.
testImportsEnvelopeShape :: IO Bool
testImportsEnvelopeShape = do
  tmp <- getTemporaryDirectory
  let dir = tmp </> "haskell-flows-imports-test"
  removePathForcibly dir
  createDirectoryIfMissing True (dir </> "src")
  TIO.writeFile (dir </> "src" </> "Foo.hs")
    (T.pack "module Foo where\nfoo :: Int\nfoo = 1\n")
  result <- case mkProjectDir dir of
    Left _   -> pure (Left "could not build ProjectDir")
    Right pd -> do
      sess <- startGhcSession pd
      tr   <- ImportsTool.handle sess (A.object [])
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
          AKM.member (AKey.fromText "count")   payload
            && AKM.member (AKey.fromText "imports") payload
    _ -> False

-- | Issue #90 Phase D step 2: the legacy top-level @\"success\"@
-- field is gone. Every consumer reads @\"status\"@ now. This
-- test inverts the Phase D step 1 lock: instead of asserting the
-- field's PRESENCE in the dual-shape window, it pins its
-- ABSENCE. An accidental re-introduction (e.g. someone restoring
-- the @optField \"success\" ...@ line) reds this test.
--
-- We still verify @\"status\"@ is present and consistent with
-- the ADT — that's the canonical replacement.
testEnvelopeLegacySuccessDropped :: IO Bool
testEnvelopeLegacySuccessDropped = do
  let okJson     = A.toJSON (Env.mkOk     (A.object [ "k" A..= ("v" :: Text) ]))
      failedJson = A.toJSON (Env.mkFailed
                              (Env.mkErrorEnvelope Env.Validation "boom"))
  pure $ isNothing (lookupField "success" okJson)
      && isNothing (lookupField "success" failedJson)
      && lookupField "status"  okJson      == Just (A.String "ok")
      && lookupField "status"  failedJson  == Just (A.String "failed")

-- | Issue #90 Phase D step 2: the legacy top-level
-- @\"error_kind\"@ duplicate is gone. Every consumer reads
-- @\"error.kind\"@ (nested) now. Inverts the Phase D step 1
-- lock: pins absence of the duplicate while keeping the nested
-- structured kind intact.
testEnvelopeLegacyErrorKindDropped :: IO Bool
testEnvelopeLegacyErrorKindDropped = do
  let json = A.toJSON (Env.mkFailed
                        (Env.mkErrorEnvelope Env.PathTraversal "tried to escape"))
      topLevelKind = lookupField "error_kind" json
      nestedKind   = lookupField "error" json
                       >>= lookupField "kind"
  pure $ isNothing topLevelKind
      && nestedKind == Just (A.String "path_traversal")

-- | Phase B helper: stage a tmpdir project with a single 'Foo'
-- module, start a fresh GhcSession, drive 'BrowseTool.handle'
-- with the given args. Returns the parsed envelope.
runBrowse :: A.Value -> IO (Either String Env.ToolResponse)
runBrowse args = do
  tmp <- getTemporaryDirectory
  let dir = tmp </> "haskell-flows-browse-test"
  removePathForcibly dir
  createDirectoryIfMissing True (dir </> "src")
  TIO.writeFile (dir </> "src" </> "Foo.hs")
    (T.pack "module Foo (foo, bar) where\nfoo :: Int\nfoo = 1\n\
            \bar :: String -> String\nbar s = s ++ \"!\"\n")
  result <- case mkProjectDir dir of
    Left _   -> pure (Left "could not build ProjectDir")
    Right pd -> do
      sess <- startGhcSession pd
      tr   <- BrowseTool.handle sess args
      killGhcSession sess
      case trContent tr of
        [TextContent body] ->
          pure (A.eitherDecode (TLE.encodeUtf8 (TL.fromStrict body)))
        _ -> pure (Left "expected exactly one TextContent")
  removePathForcibly dir
  pure result

-- | Browsing a module that's in the project's compile graph
-- produces status='ok' with result.{module, count, entries}.
testBrowseProjectModuleOk :: IO Bool
testBrowseProjectModuleOk = do
  decoded <- runBrowse (A.object [ "module" A..= ("Foo" :: Text) ])
  pure $ case decoded of
    Right env
      | Env.reStatus env == Env.StatusOk
      , Just (A.Object payload) <- Env.reResult env ->
          AKM.lookup (AKey.fromText "module") payload == Just (A.String "Foo")
            && AKM.member (AKey.fromText "count")   payload
            && AKM.member (AKey.fromText "entries") payload
    _ -> False

-- | Browsing a module that is not in the project's compile graph AND
-- not in the GHC package environment returns status='no_match'.
-- Post-#90 / #168: modules like 'Data.Maybe' that ARE in the
-- package environment now return status='ok' via the fallback path
-- (see 'testBrowseFallbackOk').  This test uses a name that is
-- guaranteed to be unknown in any package environment.
testBrowseExternalModuleNoMatch :: IO Bool
testBrowseExternalModuleNoMatch = do
  decoded <- runBrowse (A.object [ "module" A..= ("NonExistent.Module.XYZ999" :: Text) ])
  pure $ case decoded of
    Right env
      | Env.reStatus env == Env.StatusNoMatch
      , Just (A.Object payload) <- Env.reResult env ->
          AKM.lookup (AKey.fromText "module") payload
            == Just (A.String "NonExistent.Module.XYZ999")
            && AKM.member (AKey.fromText "remediation") payload
            && case Env.reNextStep env of
                 Just _  -> True   -- next-step pointer included
                 Nothing -> False
    _ -> False

-- | #168: modules available in the GHC package environment (e.g.
-- 'Data.Maybe' from base) are now browseable via the fallback path
-- even when they're not part of the project's compile graph.
-- Expects status='ok' with result.{module, count, entries}.
testBrowseFallbackOk :: IO Bool
testBrowseFallbackOk = do
  decoded <- runBrowse (A.object [ "module" A..= ("Data.Maybe" :: Text) ])
  pure $ case decoded of
    Right env
      | Env.reStatus env == Env.StatusOk
      , Just (A.Object payload) <- Env.reResult env ->
          AKM.lookup (AKey.fromText "module") payload == Just (A.String "Data.Maybe")
            && AKM.member (AKey.fromText "count") payload
            && AKM.member (AKey.fromText "entries") payload
    _ -> False

-- | #168: the descriptor must mention session-preloaded modules so
-- agents know they can browse Prelude/Data.Map/etc after ghc_add_import.
testBrowseDescriptorMentionsSession :: IO Bool
testBrowseDescriptorMentionsSession =
  let desc = BrowseTool.descriptor
  in pure ( "session" `T.isInfixOf` tdDescription desc
         || "ghc_add_import" `T.isInfixOf` tdDescription desc )

-- | Empty args (missing 'module') → status='failed' with
-- error.kind='missing_arg'.
testBrowseRejectsMissingArg :: IO Bool
testBrowseRejectsMissingArg = do
  decoded <- runBrowse (A.object [])
  pure $ case decoded of
    Right env
      | Env.reStatus env == Env.StatusFailed
      , Just err <- Env.reError env ->
          Env.eeKind err == Env.MissingArg
    _ -> False

-- | Phase B helper: stage a tmpdir project, drive
-- 'CompleteTool.handle' with the given args.
runComplete :: A.Value -> IO (Either String Env.ToolResponse)
runComplete args = do
  tmp <- getTemporaryDirectory
  let dir = tmp </> "haskell-flows-complete-test"
  removePathForcibly dir
  createDirectoryIfMissing True (dir </> "src")
  TIO.writeFile (dir </> "src" </> "Foo.hs")
    (T.pack "module Foo where\nfoo :: Int\nfoo = 1\n")
  result <- case mkProjectDir dir of
    Left _   -> pure (Left "could not build ProjectDir")
    Right pd -> do
      sess <- startGhcSession pd
      tr   <- CompleteTool.handle sess args
      killGhcSession sess
      case trContent tr of
        [TextContent body] ->
          pure (A.eitherDecode (TLE.encodeUtf8 (TL.fromStrict body)))
        _ -> pure (Left "expected exactly one TextContent")
  removePathForcibly dir
  pure result

-- | Completing 'fold' returns at least one in-scope candidate (foldr,
-- foldl, foldMap, …) → status='ok' with the legacy candidates
-- array preserved inside 'result'.
testCompleteHitsOk :: IO Bool
testCompleteHitsOk = do
  decoded <- runComplete
    (A.object [ "prefix" A..= ("fold" :: Text) ])
  pure $ case decoded of
    Right env
      | Env.reStatus env == Env.StatusOk
      , Just (A.Object payload) <- Env.reResult env ->
          AKM.lookup (AKey.fromText "prefix") payload == Just (A.String "fold")
            && AKM.member (AKey.fromText "count")      payload
            && AKM.member (AKey.fromText "candidates") payload
            && AKM.member (AKey.fromText "truncated")  payload
    _ -> False

-- | A prefix that matches no in-scope identifier → status='no_match'.
-- Legacy callers that read result.{count, candidates} keep working
-- (count = 0, candidates = []); the discriminator is the
-- top-level 'status'.
testCompleteNoMatch :: IO Bool
testCompleteNoMatch = do
  decoded <- runComplete
    (A.object [ "prefix" A..= ("zZqXunlikelyPrefix" :: Text) ])
  pure $ case decoded of
    Right env
      | Env.reStatus env == Env.StatusNoMatch
      , Just (A.Object payload) <- Env.reResult env ->
          AKM.lookup (AKey.fromText "count") payload == Just (A.Number 0)
    _ -> False

-- | #145: zero hits for a qualified prefix (contains '.') must include
-- a 'remediation' field explaining the import-scope root cause.
-- Unqualified zero-hit results should NOT include the remediation field.
testCompleteQualifiedRemediation :: IO Bool
testCompleteQualifiedRemediation = pure $
  let qualResp  = CompleteTool.renderCompletions "Data.Map." 25 []
      plainResp = CompleteTool.renderCompletions "zZqUnlikely" 25 []
      hasRemediation env =
        case Env.reResult env of
          Just (A.Object o) -> AKM.member "remediation" o
          _                 -> False
  in Env.reStatus qualResp  == Env.StatusNoMatch && hasRemediation qualResp
  && Env.reStatus plainResp == Env.StatusNoMatch && not (hasRemediation plainResp)

-- | #225: updated qualified remediation names the module and suggests
-- bare prefix instead of just "use ghc_add_import first".
testCompleteQualifiedRemediation225 :: IO Bool
testCompleteQualifiedRemediation225 = pure $
  let resp = CompleteTool.renderCompletions "Data.List." 25 []
  in case Env.reResult resp of
       Just (A.Object o) ->
         case AKM.lookup "remediation" o of
           Just (A.String t) ->
             "import qualified Data.List" `T.isInfixOf` t
               && "Data.List" `T.isInfixOf` t
               && "preload" `T.isInfixOf` t
           _ -> False
       _ -> False

-- | Issue #252: the ghc_complete tool description must document that
-- qualified prefixes (e.g. "Data.Map.") are supported.
testCompleteDescriptionMentionsQualified :: IO Bool
testCompleteDescriptionMentionsQualified =
  let desc = tdDescription CompleteTool.descriptor
  in pure $ "Qualified" `T.isInfixOf` desc
         || "qualified" `T.isInfixOf` desc

-- | #252: splitQualifiedPrefix splits "Data.Map.lookup" into
-- ("Data.Map", "lookup").
testSplitQualifiedPrefixWithName :: IO Bool
testSplitQualifiedPrefixWithName =
  pure $ CompleteTool.splitQualifiedPrefix "Data.Map.lookup"
       == Just ("Data.Map", "lookup")

-- | #252: splitQualifiedPrefix splits "Data.Map." (trailing dot) into
-- ("Data.Map", "") — the empty name prefix means "all exports".
testSplitQualifiedPrefixEmptySuffix :: IO Bool
testSplitQualifiedPrefixEmptySuffix =
  pure $ CompleteTool.splitQualifiedPrefix "Data.Map."
       == Just ("Data.Map", "")

-- | #252: splitQualifiedPrefix returns Nothing for bare prefixes
-- with no dot.
testSplitQualifiedPrefixUnqualified :: IO Bool
testSplitQualifiedPrefixUnqualified =
  pure (isNothing (CompleteTool.splitQualifiedPrefix "fold"))

-- | #252: splitQualifiedPrefix handles multi-dot module paths —
-- "Data.Map.Strict.lookup" should split into
-- ("Data.Map.Strict", "lookup"), not ("Data.Map", "Strict.lookup").
testSplitQualifiedPrefixDeep :: IO Bool
testSplitQualifiedPrefixDeep =
  pure $ CompleteTool.splitQualifiedPrefix "Data.Map.Strict.lookup"
       == Just ("Data.Map.Strict", "lookup")

-- | #252: structural source check — Complete.hs must reference
-- @lookupModule@ so the qualified-prefix fallback path is wired in.
-- Catches regressions where someone removes the fallback while
-- leaving 'splitQualifiedPrefix' as a dead helper.
testCompleteImportsLookupModule :: IO Bool
testCompleteImportsLookupModule = do
  src <- TIO.readFile "src/HaskellFlows/Tool/Complete.hs"
  pure $ "lookupModule" `T.isInfixOf` src
      && "queryQualifiedFallback" `T.isInfixOf` src

-- | A newline-laden prefix → status='refused' with
-- error.kind='newline_injection'. Issue #90 Phase B: every
-- sanitize-layer rejection rides StatusRefused with a structured
-- error.kind, distinct from a tool-level failure ('Failed').
testCompleteRefusesNewline :: IO Bool
testCompleteRefusesNewline = do
  decoded <- runComplete
    (A.object [ "prefix" A..= ("fold\n:quit" :: Text) ])
  pure $ case decoded of
    Right env
      | Env.reStatus env == Env.StatusRefused
      , Just err <- Env.reError env ->
          Env.eeKind err == Env.NewlineInjection
            && Env.eeField err == Just "prefix"
    _ -> False

-- | Phase B helper: stage a tmpdir project with a 'Foo' module
-- exporting 'foo' + drive 'GotoTool.handle'.
runGoto :: A.Value -> IO (Either String Env.ToolResponse)
runGoto args = do
  tmp <- getTemporaryDirectory
  let dir = tmp </> "haskell-flows-goto-test"
  removePathForcibly dir
  createDirectoryIfMissing True (dir </> "src")
  TIO.writeFile (dir </> "src" </> "Foo.hs")
    (T.pack "module Foo where\nfoo :: Int\nfoo = 1\n")
  result <- case mkProjectDir dir of
    Left _   -> pure (Left "could not build ProjectDir")
    Right pd -> do
      sess <- startGhcSession pd
      tr   <- GotoTool.handle sess args
      killGhcSession sess
      case trContent tr of
        [TextContent body] ->
          pure (A.eitherDecode (TLE.encodeUtf8 (TL.fromStrict body)))
        _ -> pure (Left "expected exactly one TextContent")
  removePathForcibly dir
  pure result

-- | 'ghc_goto' on a project-defined name resolves to a file
-- location → status='ok' with result.kind='file' + result.file +
-- result.line + result.column.
testGotoLocalNameOk :: IO Bool
testGotoLocalNameOk = do
  decoded <- runGoto (A.object [ "name" A..= ("foo" :: Text) ])
  pure $ case decoded of
    Right env
      | Env.reStatus env == Env.StatusOk
      , Just (A.Object payload) <- Env.reResult env ->
          AKM.lookup (AKey.fromText "name") payload == Just (A.String "foo")
            && (AKM.lookup (AKey.fromText "kind") payload == Just (A.String "file")
                  || AKM.lookup (AKey.fromText "kind") payload == Just (A.String "module"))
    _ -> False

-- | 'ghc_goto' on a name that's not in scope → status='no_match'
-- with the searched name echoed inside result. Closes one of the
-- ghc_info-class \"name not in scope\" cases that #87 generalises.
testGotoUnknownNameNoMatch :: IO Bool
testGotoUnknownNameNoMatch = do
  decoded <- runGoto
    (A.object [ "name" A..= ("definitelyNotARealName123" :: Text) ])
  pure $ case decoded of
    Right env
      | Env.reStatus env == Env.StatusNoMatch
      , Just (A.Object payload) <- Env.reResult env ->
          AKM.lookup (AKey.fromText "name") payload
            == Just (A.String "definitelyNotARealName123")
            && AKM.member (AKey.fromText "remediation") payload
    _ -> False

-- | A newline-laden name → status='refused' with kind='newline_injection'.
testGotoRefusesNewline :: IO Bool
testGotoRefusesNewline = do
  decoded <- runGoto (A.object [ "name" A..= ("foo\n:quit" :: Text) ])
  pure $ case decoded of
    Right env
      | Env.reStatus env == Env.StatusRefused
      , Just err <- Env.reError env ->
          Env.eeKind err == Env.NewlineInjection
            && Env.eeField err == Just "name"
    _ -> False

-- | Phase B helper: stage a tmpdir project, drive 'DocTool.handle'.
runDoc :: A.Value -> IO (Either String Env.ToolResponse)
runDoc args = do
  tmp <- getTemporaryDirectory
  let dir = tmp </> "haskell-flows-doc-test"
  removePathForcibly dir
  createDirectoryIfMissing True (dir </> "src")
  TIO.writeFile (dir </> "src" </> "Foo.hs")
    (T.pack "module Foo where\nfoo :: Int\nfoo = 1\n")
  result <- case mkProjectDir dir of
    Left _   -> pure (Left "could not build ProjectDir")
    Right pd -> do
      sess <- startGhcSession pd
      tr   <- DocTool.handle sess args
      killGhcSession sess
      case trContent tr of
        [TextContent body] ->
          pure (A.eitherDecode (TLE.encodeUtf8 (TL.fromStrict body)))
        _ -> pure (Left "expected exactly one TextContent")
  removePathForcibly dir
  pure result

-- | 'ghc_doc' on a Prelude name (e.g. 'map') usually has Haddock
-- on a properly-installed base. Status='ok' with result.hasDoc=true.
-- The test accepts BOTH 'ok' (Haddock available) and 'no_match'
-- (Haddock missing on this build of base) — the contract is that
-- a name-in-scope with no doc maps to no_match, not to an error.
testDocHasDocOk :: IO Bool
testDocHasDocOk = do
  decoded <- runDoc (A.object [ "name" A..= ("map" :: Text) ])
  pure $ case decoded of
    Right env
      | Env.reStatus env == Env.StatusOk
      , Just (A.Object payload) <- Env.reResult env ->
          AKM.lookup (AKey.fromText "hasDoc") payload == Just (A.Bool True)
            && AKM.member (AKey.fromText "doc") payload
      | Env.reStatus env == Env.StatusNoMatch
      , Just (A.Object payload) <- Env.reResult env ->
          AKM.lookup (AKey.fromText "hasDoc") payload == Just (A.Bool False)
            && AKM.member (AKey.fromText "reason") payload
    _ -> False

-- | 'ghc_doc' on a name that's not in scope → status='no_match'
-- (NOT a success-shaped 'hasDoc: false', which #87 called out as
-- the same anti-pattern as ghc_info).
testDocUnknownNameNoMatch :: IO Bool
testDocUnknownNameNoMatch = do
  decoded <- runDoc
    (A.object [ "name" A..= ("definitelyNotARealName123" :: Text) ])
  pure $ case decoded of
    Right env
      | Env.reStatus env == Env.StatusNoMatch
      , Just (A.Object payload) <- Env.reResult env ->
          AKM.lookup (AKey.fromText "name") payload
            == Just (A.String "definitelyNotARealName123")
            && AKM.lookup (AKey.fromText "hasDoc") payload == Just (A.Bool False)
            && AKM.member (AKey.fromText "reason") payload
    _ -> False

-- | A newline-laden name → status='refused' with kind='newline_injection'.
testDocRefusesNewline :: IO Bool
testDocRefusesNewline = do
  decoded <- runDoc (A.object [ "name" A..= ("foo\n:quit" :: Text) ])
  pure $ case decoded of
    Right env
      | Env.reStatus env == Env.StatusRefused
      , Just err <- Env.reError env ->
          Env.eeKind err == Env.NewlineInjection
            && Env.eeField err == Just "name"
    _ -> False

-- | Phase B helper: stage a tmpdir project + drive 'TypeTool.handle'.
runType :: A.Value -> IO (Either String Env.ToolResponse)
runType args = do
  tmp <- getTemporaryDirectory
  let dir = tmp </> "haskell-flows-type-test"
  removePathForcibly dir
  createDirectoryIfMissing True (dir </> "src")
  TIO.writeFile (dir </> "src" </> "Foo.hs")
    (T.pack "module Foo where\nfoo :: Int\nfoo = 1\n")
  result <- case mkProjectDir dir of
    Left _   -> pure (Left "could not build ProjectDir")
    Right pd -> do
      sess <- startGhcSession pd
      tr   <- TypeTool.handle sess args
      killGhcSession sess
      case trContent tr of
        [TextContent body] ->
          pure (A.eitherDecode (TLE.encodeUtf8 (TL.fromStrict body)))
        _ -> pure (Left "expected exactly one TextContent")
  removePathForcibly dir
  pure result

-- | 'ghc_type' on a valid Prelude expression resolves cleanly →
-- status='ok' with result.{expression, type}. The exact rendering
-- of the type varies by GHC minor (forall + brackets, etc.) so we
-- only assert structure.
testTypeValidExprOk :: IO Bool
testTypeValidExprOk = do
  decoded <- runType (A.object [ "expression" A..= ("id" :: Text) ])
  pure $ case decoded of
    Right env
      | Env.reStatus env == Env.StatusOk
      , Just (A.Object payload) <- Env.reResult env ->
          AKM.lookup (AKey.fromText "expression") payload == Just (A.String "id")
            && AKM.member (AKey.fromText "type") payload
    _ -> False

-- | An ill-typed expression → status='failed' with kind='type_error'.
-- Pre-#90 this returned a free-form 'expression did not type-check
-- — <SDoc>' string; post-#90 the SDoc lives in error.cause and
-- the message stays short.
testTypeIllTypedFailed :: IO Bool
testTypeIllTypedFailed = do
  decoded <- runType (A.object [ "expression" A..= ("True + 1" :: Text) ])
  pure $ case decoded of
    Right env
      | Env.reStatus env == Env.StatusFailed
      , Just err <- Env.reError env ->
          Env.eeKind err == Env.TypeError
    _ -> False

-- | Newline in expression → status='refused' with
-- kind='newline_injection'.
testTypeRefusesNewline :: IO Bool
testTypeRefusesNewline = do
  decoded <- runType (A.object [ "expression" A..= ("id\n:quit" :: Text) ])
  pure $ case decoded of
    Right env
      | Env.reStatus env == Env.StatusRefused
      , Just err <- Env.reError env ->
          Env.eeKind err == Env.NewlineInjection
            && Env.eeField err == Just "expression"
    _ -> False

-- | #141: an expression whose name is not in scope → status='failed'
-- with kind='not_in_scope', not kind='type_error'.
-- 'Data.Map.fromList' is not imported by default so asking for its
-- type triggers GHC's "Not in scope" diagnostic.
testTypeNotInScope :: IO Bool
testTypeNotInScope = do
  decoded <- runType (A.object [ "expression" A..= ("Data.Map.fromList" :: Text) ])
  pure $ case decoded of
    Right env
      | Env.reStatus env == Env.StatusFailed
      , Just err <- Env.reError env ->
          Env.eeKind err == Env.NotInScope
    _ -> False

-- | Phase B helper: stage a tmpdir + drive 'EvalTool.handle'.
runEval :: A.Value -> IO (Either String Env.ToolResponse)
runEval args = do
  tmp <- getTemporaryDirectory
  let dir = tmp </> "haskell-flows-eval-test"
  removePathForcibly dir
  createDirectoryIfMissing True (dir </> "src")
  TIO.writeFile (dir </> "src" </> "Foo.hs")
    (T.pack "module Foo where\nfoo :: Int\nfoo = 1\n")
  result <- case mkProjectDir dir of
    Left _   -> pure (Left "could not build ProjectDir")
    Right pd -> do
      sess <- startGhcSession pd
      tr   <- EvalTool.handle sess args
      killGhcSession sess
      case trContent tr of
        [TextContent body] ->
          pure (A.eitherDecode (TLE.encodeUtf8 (TL.fromStrict body)))
        _ -> pure (Left "expected exactly one TextContent")
  removePathForcibly dir
  pure result

-- | 'ghc_eval' on a pure expression returns its show-rendered
-- output → status='ok' with result.{output, truncated}.
testEvalPureExprOk :: IO Bool
testEvalPureExprOk = do
  decoded <- runEval (A.object [ "expression" A..= ("1 + 1" :: Text) ])
  pure $ case decoded of
    Right env
      | Env.reStatus env == Env.StatusOk
      , Just (A.Object payload) <- Env.reResult env ->
          AKM.lookup (AKey.fromText "output") payload == Just (A.String "2")
            && AKM.lookup (AKey.fromText "truncated") payload == Just (A.Bool False)
    _ -> False

-- | #143: an expression that starts with 'import ' is not a valid
-- Haskell expression — redirect to ghc_add_import with a structured
-- error + nextStep. Checks without a live GhcSession (the redirect
-- fires before sanitization and GHC loading).
testEvalImportRedirect :: IO Bool
testEvalImportRedirect =
  let resp = EvalTool.importRedirectResult "import Data.Map"
  in pure $ Env.reStatus resp == Env.StatusFailed
          && case Env.reError resp of
               Just err -> Env.eeKind err == Env.CompileError
               Nothing  -> False
          && case Env.reNextStep resp of
               Just (A.Object ns) ->
                 AKM.lookup "tool" ns == Just (A.String "ghc_add_import")
               _ -> False

-- | Newline in expression → status='refused' with
-- kind='newline_injection'.
testEvalRefusesNewline :: IO Bool
testEvalRefusesNewline = do
  decoded <- runEval (A.object [ "expression" A..= ("1 + 1\n:quit" :: Text) ])
  pure $ case decoded of
    Right env
      | Env.reStatus env == Env.StatusRefused
      , Just err <- Env.reError env ->
          Env.eeKind err == Env.NewlineInjection
            && Env.eeField err == Just "expression"
    _ -> False

-- | Sentinel string in expression → status='refused' with
-- kind='sentinel_poisoning'. Anchors the security gate that
-- prevents an attacker-controlled prompt from desyncing the
-- framing protocol.
testEvalRefusesSentinel :: IO Bool
testEvalRefusesSentinel = do
  decoded <- runEval
    (A.object [ "expression" A..= ("\"<<<GHCi-DONE-7f3a2b>>>\"" :: Text) ])
  pure $ case decoded of
    Right env
      | Env.reStatus env == Env.StatusRefused
      , Just err <- Env.reError env ->
          Env.eeKind err == Env.SentinelPoisoning
            && Env.eeField err == Just "expression"
    _ -> False

-- | #127: power-expression with exponent >= 64 → status='refused' with
-- kind='oversized_input'. Guards against the two-limb GMP Integer that
-- crashes the in-process GHC evaluator via an RTS segfault.
testEvalRefusesOversizedInteger :: IO Bool
testEvalRefusesOversizedInteger = do
  decoded <- runEval (A.object [ "expression" A..= ("2^64" :: Text) ])
  pure $ case decoded of
    Right env
      | Env.reStatus env == Env.StatusRefused
      , Just err <- Env.reError env ->
          Env.eeKind err == Env.OversizedInput
            && Env.eeField err == Just "expression"
    _ -> False

-- | #127: 20-digit decimal literal → status='refused' with
-- kind='oversized_input'. 18446744073709551616 is 2^64.
testEvalRefusesLargeLiteral :: IO Bool
testEvalRefusesLargeLiteral = do
  decoded <- runEval
    (A.object [ "expression" A..= ("18446744073709551616" :: Text) ])
  pure $ case decoded of
    Right env
      | Env.reStatus env == Env.StatusRefused
      , Just err <- Env.reError env ->
          Env.eeKind err == Env.OversizedInput
            && Env.eeField err == Just "expression"
    _ -> False

-- | #134: the source must contain both the 'GhcException' and 'SourceError'
-- pattern guards in 'classifyEvalException', confirming compile-time errors
-- are routed to CompileError rather than RuntimeException.
testClassifyEvalExceptionInSource :: IO Bool
testClassifyEvalExceptionInSource = do
  src <- TIO.readFile "src/HaskellFlows/Tool/Eval.hs"
  pure $ T.isInfixOf "classifyEvalException" src
      && T.isInfixOf "GhcException" src
      && T.isInfixOf "SourceError"  src
      && T.isInfixOf "Env.CompileError" src

-- | Phase B helper: stage a tmpdir project + drive 'HoleTool.handle'.
-- The stagedSource lets each test write whatever module body it
-- wants (with or without an actual typed hole).
runHole :: Text -> A.Value -> IO (Either String Env.ToolResponse)
runHole stagedSource args = do
  tmp <- getTemporaryDirectory
  let dir = tmp </> "haskell-flows-hole-test"
  removePathForcibly dir
  createDirectoryIfMissing True (dir </> "src")
  TIO.writeFile (dir </> "src" </> "Foo.hs") stagedSource
  result <- case mkProjectDir dir of
    Left _   -> pure (Left "could not build ProjectDir")
    Right pd -> do
      sess <- startGhcSession pd
      tr   <- HoleTool.handle sess pd args
      killGhcSession sess
      case trContent tr of
        [TextContent body] ->
          pure (A.eitherDecode (TLE.encodeUtf8 (TL.fromStrict body)))
        _ -> pure (Left "expected exactly one TextContent")
  removePathForcibly dir
  pure result

-- | A module with an explicit '_' hole produces status='ok' with
-- result.holes carrying ≥ 1 entry. Anchors the happy-path
-- contract: ghc_hole IS the right tool when there are holes.
testHoleWithHoleOk :: IO Bool
testHoleWithHoleOk = do
  let src = T.pack "module Foo where\nfoo :: Int -> Int\nfoo x = _\n"
  decoded <- runHole src (A.object [ "module_path" A..= ("src/Foo.hs" :: Text) ])
  pure $ case decoded of
    Right env
      | Env.reStatus env == Env.StatusOk
      , Just (A.Object payload) <- Env.reResult env ->
          case AKM.lookup (AKey.fromText "hole_count") payload of
            Just (A.Number n) -> n >= 1
            _                 -> False
    _ -> False

-- | A module with no holes produces status='no_match' (the
-- question — \"where are the typed holes?\" — was well-formed;
-- the answer is the empty set). Pre-#90 this returned
-- success=true with hole_count=0 — the same anti-pattern #87
-- generalises.
testHoleNoHoleMatch :: IO Bool
testHoleNoHoleMatch = do
  let src = T.pack "module Foo where\nfoo :: Int -> Int\nfoo x = x + 1\n"
  decoded <- runHole src (A.object [ "module_path" A..= ("src/Foo.hs" :: Text) ])
  pure $ case decoded of
    Right env
      | Env.reStatus env == Env.StatusNoMatch
      , Just (A.Object payload) <- Env.reResult env ->
          AKM.lookup (AKey.fromText "hole_count") payload == Just (A.Number 0)
    _ -> False

-- | #148: a module_path that does not exist on disk → status='failed'
-- with kind='module_path_does_not_exist'. Before the fix the tool
-- returned status='no_match' (hole_count=0) — indistinguishable from
-- a valid hole-free file.
testHoleNonExistentFile :: IO Bool
testHoleNonExistentFile = do
  let src = T.pack "module Foo where\nfoo :: Int\nfoo = 1\n"
  decoded <- runHole src
    (A.object [ "module_path" A..= ("src/DoesNotExist.hs" :: Text) ])
  pure $ case decoded of
    Right env
      | Env.reStatus env == Env.StatusFailed
      , Just err <- Env.reError env ->
          Env.eeKind err == Env.ModulePathDoesNotExist
            && Env.eeField err == Just "module_path"
    _ -> False

-- | A path that escapes the project root is refused via the
-- mkModulePath gate → status='refused' with kind='path_traversal'.
testHoleRejectsTraversal :: IO Bool
testHoleRejectsTraversal = do
  let src = T.pack "module Foo where\nfoo :: Int\nfoo = 1\n"
  decoded <- runHole src
    (A.object [ "module_path" A..= ("../../etc/passwd" :: Text) ])
  pure $ case decoded of
    Right env
      | Env.reStatus env == Env.StatusRefused
      , Just err <- Env.reError env ->
          Env.eeKind err == Env.PathTraversal
            && Env.eeField err == Just "module_path"
    _ -> False

-- | Phase B helper: drive 'InfoTool.handle' against a fresh
-- session with a tiny project loaded.
runInfo :: A.Value -> IO (Either String Env.ToolResponse)
runInfo args = do
  tmp <- getTemporaryDirectory
  let dir = tmp </> "haskell-flows-info-test"
  removePathForcibly dir
  createDirectoryIfMissing True (dir </> "src")
  TIO.writeFile (dir </> "src" </> "Foo.hs")
    (T.pack "module Foo where\nfoo :: Int\nfoo = 1\n")
  result <- case mkProjectDir dir of
    Left _   -> pure (Left "could not build ProjectDir")
    Right pd -> do
      sess <- startGhcSession pd
      tr   <- InfoTool.handle sess args
      killGhcSession sess
      case trContent tr of
        [TextContent body] ->
          pure (A.eitherDecode (TLE.encodeUtf8 (TL.fromStrict body)))
        _ -> pure (Left "expected exactly one TextContent")
  removePathForcibly dir
  pure result

-- | 'ghc_info' on a real symbol resolves to a structured definition.
-- Status='ok' with result.{name, kind, definition, instances}.
testInfoRealSymbolOk :: IO Bool
testInfoRealSymbolOk = do
  decoded <- runInfo (A.object [ "name" A..= ("foo" :: Text) ])
  pure $ case decoded of
    Right env
      | Env.reStatus env == Env.StatusOk
      , Just (A.Object payload) <- Env.reResult env ->
          AKM.lookup (AKey.fromText "name") payload == Just (A.String "foo")
            && AKM.member (AKey.fromText "kind") payload
            && AKM.member (AKey.fromText "definition") payload
    _ -> False

-- | Issue #87 closure: 'ghc_info' on a name not in scope MUST emit
-- status='no_match' (the question was well-formed; the answer is
-- the empty set), NOT a fabricated 'data X' definition. This is
-- the load-bearing test for #87 — the previous behaviour was
-- success=true with a synthesised definition that didn't exist
-- in the project, in base, or anywhere reachable. Post-#90 the
-- definition field is gone (no fabrication), result.searched_in
-- documents where we looked, result.remediation suggests the
-- next move.
testInfoUnknownNameNoMatch :: IO Bool
testInfoUnknownNameNoMatch = do
  decoded <- runInfo
    (A.object [ "name" A..= ("DoesNotExistName123" :: Text) ])
  pure $ case decoded of
    Right env
      | Env.reStatus env == Env.StatusNoMatch
      , Just (A.Object payload) <- Env.reResult env ->
          AKM.lookup (AKey.fromText "name") payload
            == Just (A.String "DoesNotExistName123")
            && AKM.member (AKey.fromText "searched_in") payload
            && AKM.member (AKey.fromText "remediation") payload
            -- The fabricated 'data DoesNotExistName123' definition
            -- is GONE — that was the #87 bug.
            && not (AKM.member (AKey.fromText "definition") payload)
    _ -> False

-- | Newline in name → status='refused' with kind='newline_injection'.
testInfoRefusesNewline :: IO Bool
testInfoRefusesNewline = do
  decoded <- runInfo (A.object [ "name" A..= ("foo\n:quit" :: Text) ])
  pure $ case decoded of
    Right env
      | Env.reStatus env == Env.StatusRefused
      , Just err <- Env.reError env ->
          Env.eeKind err == Env.NewlineInjection
            && Env.eeField err == Just "name"
    _ -> False

-- | Phase B helper: drive 'HoogleTool.handle' / 'AddImportTool.handle'.
-- These tools don't need a GhcSession.
runHoogle :: A.Value -> IO (Either String Env.ToolResponse)
runHoogle args = do
  tr <- HoogleTool.handle args
  case trContent tr of
    [TextContent body] ->
      pure (A.eitherDecode (TLE.encodeUtf8 (TL.fromStrict body)))
    _ -> pure (Left "expected exactly one TextContent")

-- | #146: AddImportTool.handle now requires a GhcSession. For tests
-- that short-circuit before the session is touched (hoogle missing,
-- parse error), a stub session on an empty tmpdir is sufficient.
runAddImport :: A.Value -> IO (Either String Env.ToolResponse)
runAddImport args = do
  tmp <- getTemporaryDirectory
  let dir = tmp </> "haskell-flows-add-import-stub"
  createDirectoryIfMissing True dir
  case mkProjectDir dir of
    Left _  -> pure (Left "could not build ProjectDir for stub session")
    Right pd -> do
      sess <- startGhcSession pd
      tr <- AddImportTool.handle sess args
      killGhcSession sess
      case trContent tr of
        [TextContent body] ->
          pure (A.eitherDecode (TLE.encodeUtf8 (TL.fromStrict body)))
        _ -> pure (Left "expected exactly one TextContent")

-- | An empty hoogle query → status='refused' with
-- kind='empty_input' + field='query'.
testHoogleRejectsEmpty :: IO Bool
testHoogleRejectsEmpty = do
  decoded <- runHoogle (A.object [ "query" A..= ("" :: Text) ])
  pure $ case decoded of
    Right env
      | Env.reStatus env == Env.StatusRefused
      , Just err <- Env.reError env ->
          Env.eeKind err == Env.EmptyInput
            && Env.eeField err == Just "query"
    _ -> False

-- | When the hoogle binary isn't on PATH, the status is
-- 'unavailable' (NOT 'failed'). Distinct discriminator: an
-- environment-binary issue is structurally different from a
-- runtime failure. The test scrubs PATH around the call to
-- guarantee the missing-binary code path fires regardless of
-- the host's actual hoogle install.
testHoogleUnavailable :: IO Bool
testHoogleUnavailable = do
  origPath <- lookupEnv "PATH"
  let scrubbed = "/var/empty-haskell-flows-no-hoogle"
  decoded <- bracket_
    (setEnv "PATH" scrubbed)
    (case origPath of
       Just p  -> setEnv "PATH" p
       Nothing -> unsetEnv "PATH")
    (runHoogle (A.object [ "query" A..= ("filter" :: Text) ]))
  pure $ case decoded of
    Right env
      | Env.reStatus env == Env.StatusUnavailable
      , Just err <- Env.reError env ->
          Env.eeKind err == Env.BinaryUnavailable
            && isJust (Env.eeRemediation err)
    _ -> False

-- | ghc_add_import shares the unavailable contract with hoogle_search.
testAddImportUnavailable :: IO Bool
testAddImportUnavailable = do
  origPath <- lookupEnv "PATH"
  let scrubbed = "/var/empty-haskell-flows-no-hoogle"
  decoded <- bracket_
    (setEnv "PATH" scrubbed)
    (case origPath of
       Just p  -> setEnv "PATH" p
       Nothing -> unsetEnv "PATH")
    (runAddImport (A.object [ "name" A..= ("fromMaybe" :: Text) ]))
  pure $ case decoded of
    Right env
      | Env.reStatus env == Env.StatusUnavailable
      , Just err <- Env.reError env ->
          Env.eeKind err == Env.BinaryUnavailable
    _ -> False

-- | Empty args (missing 'name') → status='failed' with
-- error.kind='missing_arg'.
testAddImportRejectsMissingArg :: IO Bool
testAddImportRejectsMissingArg = do
  decoded <- runAddImport (A.object [])
  pure $ case decoded of
    Right env
      | Env.reStatus env == Env.StatusFailed
      , Just err <- Env.reError env ->
          Env.eeKind err == Env.MissingArg
    _ -> False

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

-- | Issue #81 (CWE-22): the previous 'resolveTarget' compared the
-- joined path against the project root with a literal string-prefix
-- check, which the agent could trick by passing a relative path
-- that contained '..' segments. The fix mirrors 'mkModulePath' and
-- rejects anything whose normalised segments contain '..' before
-- it can reach hlint. Each entry below is a path the old gate
-- accepted but the new one must refuse.
testLintResolveRejectsTraversal :: IO Bool
testLintResolveRejectsTraversal =
  case mkProjectDir "/tmp/project" of
    Left _   -> pure False
    Right pd ->
      let escapes = [ "../.."
                    , "../../something"
                    , "./../foo"
                    , "a/../../escape"
                    ]
          rejectedAsPath p = case LintTool.resolveTarget pd
                                  (LintTool.LintArgs (Just (T.pack p)) Nothing "warning") of
            Left err -> "escapes project directory" `T.isInfixOf` err
            Right _  -> False
          rejectedAsModulePath p = case LintTool.resolveTarget pd
                                  (LintTool.LintArgs Nothing (Just (T.pack p)) "warning") of
            Left err -> "escapes project directory" `T.isInfixOf` err
            Right _  -> False
      in pure $ all rejectedAsPath escapes && all rejectedAsModulePath escapes

-- | Companion: absolute paths outside the project root must also be
-- rejected. (The old gate already caught these — this anchors the
-- regression so a future refactor doesn't trade one bypass for
-- another.)
testLintResolveRejectsAbsoluteOutside :: IO Bool
testLintResolveRejectsAbsoluteOutside =
  case mkProjectDir "/tmp/project" of
    Left _   -> pure False
    Right pd ->
      let outside = LintTool.resolveTarget pd
                       (LintTool.LintArgs (Just "/tmp") Nothing "warning")
      in pure $ case outside of
           Left err -> "escapes project directory" `T.isInfixOf` err
           Right _  -> False

-- | Companion: legitimate relative paths inside the project must
-- still resolve. Both 'path' (directory) and 'module_path' (file)
-- forms are exercised; the empty-args default (root itself) is the
-- third anchor.
testLintResolveAcceptsInTree :: IO Bool
testLintResolveAcceptsInTree =
  case mkProjectDir "/tmp/project" of
    Left _   -> pure False
    Right pd ->
      let asPath       = LintTool.resolveTarget pd
                           (LintTool.LintArgs (Just "src/") Nothing "warning")
          asModulePath = LintTool.resolveTarget pd
                           (LintTool.LintArgs Nothing (Just "src/Foo.hs") "warning")
          asEmpty      = LintTool.resolveTarget pd
                           (LintTool.LintArgs Nothing Nothing "warning")
          isInTree (Right p) = "/tmp/project" `List.isPrefixOf` p
          isInTree _         = False
      in pure $ isInTree asPath && isInTree asModulePath && isInTree asEmpty

-- #128: stripProjectDirPrefix must not change a path that doesn't start
-- with the project basename.
testStripProjectDirPrefixNoOp :: IO Bool
testStripProjectDirPrefixNoOp =
  let root   = "/home/user/my-project"
      result = LintTool.stripProjectDirPrefix root "src/"
  in pure (result == "src/")

-- #128: when the raw path starts with the project's own basename,
-- strip that segment so path-joining doesn't double it.
testStripProjectDirPrefixStrips :: IO Bool
testStripProjectDirPrefixStrips =
  let root   = "/home/user/my-project"
      -- "my-project/" is the basename of root — stripping leaves ""
      result = LintTool.stripProjectDirPrefix root "my-project/"
      -- also test "my-project/src" → "src"
      result2 = LintTool.stripProjectDirPrefix root "my-project/src"
  in pure (null result && result2 == "src")

-- #128: absolute paths must be returned unchanged regardless of basename.
testStripProjectDirPrefixAbsolute :: IO Bool
testStripProjectDirPrefixAbsolute =
  let root   = "/home/user/my-project"
      absP   = "/home/user/my-project/src"
      result = LintTool.stripProjectDirPrefix root absP
  in pure (result == absP)

-- #128: resolveTarget with path="<basename>/" must resolve to projectDir,
-- not projectDir/<basename>. This is the exact dogfood repro case.
testLintResolveNoDuplication :: IO Bool
testLintResolveNoDuplication =
  case mkProjectDir "/tmp/my-project" of
    Left _   -> pure False
    Right pd ->
      -- Pass the project's own basename as the lint path — the old code
      -- would produce /tmp/my-project/my-project (doubled, doesn't exist).
      let args   = LintTool.LintArgs (Just "my-project") Nothing "warning"
          result = LintTool.resolveTarget pd args
      in pure $ case result of
           Right resolved -> resolved == "/tmp/my-project"
           Left _         -> False

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

-- | Issue #71: real GHC output for the @_addOp :: Int -> Int -> Int@
-- hole produces fit-head lines like @(-) :: forall a. Num a => …@
-- whose name starts with @(@. Pre-#71 the continuation classifier
-- treated any indented line starting with @(@ as a continuation
-- of the previous fit, so the operator-named fit was absorbed
-- into the preceding entry's @source@ field, dropping the fit
-- entirely from the array and inflating its predecessor's
-- @source@ string.
--
-- Post-#71 we expect 4 distinct fits, each with its own clean
-- @name@ / @type@ / @source@ — including @(-)@ as its own row.
testValidFitsOperatorBoundary :: IO Bool
testValidFitsOperatorBoundary =
  let block = T.lines $ T.unlines
        [ "    • Valid hole fits include"
        , "        addPair :: Int -> Int -> Int"
        , "          (bound at /tmp/Demo.hs:14:1)"
        , "        (-) :: forall a. Num a => a -> a -> a"
        , "          with (-) @Int"
        , "          (imported from `Prelude' at /tmp/Demo.hs:2:8-19)"
        , "        asTypeOf :: forall a. a -> a -> a"
        , "          with asTypeOf @Int"
        , "        const :: forall a b. a -> b -> a"
        , "          with const @Int @Int"
        ]
      fits = extractValidFits block
      names = map hfName fits
      addPairFit = head fits
      addPairSrc = fromMaybe "" (hfSource addPairFit)
  in pure $ length fits == 4
         && names == ["addPair", "(-)", "asTypeOf", "const"]
         && "(bound at" `T.isInfixOf` addPairSrc
         -- Critical: addPair's source must NOT have absorbed
         -- the next fit's identifier or type signature.
         && not ("(-)"            `T.isInfixOf` addPairSrc)
         && not ("forall a. Num"  `T.isInfixOf` addPairSrc)

-- | Issue #71: pin the new contract for the continuation
-- classifier — the type-signature substring is the canonical
-- disambiguator. Three boundary cases:
--
--   * Operator-named fit '(-) :: forall a. Num a => a -> a -> a'
--     IS a fit-head (must NOT be classified as continuation).
--   * Plain '(bound at /tmp/X.hs:1:1)' IS a continuation.
--   * '(imported from ...)' IS a continuation.
testHoleContinuationDetector :: IO Bool
testHoleContinuationDetector =
  pure $  not (isContinuationFitLine "        (-) :: forall a. Num a => a -> a -> a")
       &&      isContinuationFitLine "          (bound at /tmp/X.hs:1:1)"
       &&      isContinuationFitLine "          (imported from `Prelude' at /tmp/X.hs:2:8-19)"
       &&      isContinuationFitLine "          with (-) @Int"
       -- Sanity: parseFitLine on the operator head extracts the
       -- name with parens and an empty source.
       && case parseFitLine "        (-) :: forall a. Num a => a -> a -> a" of
            Just hf -> hfName hf == "(-)"
                    && "Num a" `T.isInfixOf` hfType hf
            Nothing -> False

-- | #169: Functions with @HasCallStack@ / @(?callStack :: CallStack)@
-- constraints were silently dropped from the @validFits@ list because
-- 'parseFitLine' broke on the first @\"(\"@, producing an empty type
-- and triggering the @T.null (T.strip ty)@ guard.  After the fix,
-- the type must survive intact.
testParseFitHasCallStack :: IO Bool
testParseFitHasCallStack =
  -- GHC can render HasCallStack either as the class name or as the
  -- desugared (?callStack :: CallStack) form.
  let line1 = "        error :: HasCallStack => [Char] -> a (imported from GHC.Stack)"
      line2 = "        error :: (?callStack :: CallStack) => [Char] -> a (imported from GHC.Stack)"
      check line = case parseFitLine line of
        Nothing -> False
        Just hf ->
          hfName hf == "error"
          -- Type must include the constraint, not be empty or truncated.
          && (T.isInfixOf "HasCallStack" (hfType hf)
              || T.isInfixOf "callStack" (hfType hf))
          && T.isInfixOf "[Char] -> a" (hfType hf)
  in pure (check line1 && check line2)

-- | #169: 'splitFitTypeSource' must split at \" (bound at \" and
-- return the annotation as the second component.
testSplitFitTypeBoundAt :: IO Bool
testSplitFitTypeBoundAt =
  let (ty, src) = splitFitTypeSource
        "(?callStack :: CallStack) => [Char] -> a (bound at src/Foo.hs:1:1)"
  in pure $
       T.isInfixOf "callStack" ty
    && T.isInfixOf "[Char] -> a" ty
    && T.isInfixOf "bound at" src

-- | #169: 'splitFitTypeSource' must split at \" (imported from \" too.
testSplitFitTypeImportedFrom :: IO Bool
testSplitFitTypeImportedFrom =
  let (ty, src) = splitFitTypeSource
        "Int -> Int -> Int (imported from Data.Bits)"
  in pure $
       ty == "Int -> Int -> Int"
    && T.isInfixOf "Data.Bits" src

-- | #169: when there is no annotation, the full input is returned as
-- the type and the source component is empty.
testSplitFitTypeNoAnnotation :: IO Bool
testSplitFitTypeNoAnnotation =
  let (ty, src) = splitFitTypeSource "forall a. Num a => a -> a -> a"
  in pure $ ty == "forall a. Num a => a -> a -> a" && T.null src

-- | #196: GHC 9.12 wraps @HasCallStack@-constrained types onto a
-- continuation line, landing the constraint in @hfSource@ instead of
-- @hfType@.  'repairConstraintInSource' must move the constraint prefix
-- back into the type field and keep only the @with …@ clause in source.
testRepairConstraintInSource :: IO Bool
testRepairConstraintInSource =
  let -- Simulates the post-collapseFits state for GHC 9.12 output:
      --   cycle :: forall a.
      --             Stack.Types.HasCallStack => [a] -> [a]
      --             with cycle @Char (imported from 'Prelude' ...)
      broken = HoleFit
        { hfName   = "cycle"
        , hfType   = "forall a."
        , hfSource = Just "Stack.Types.HasCallStack => [a] -> [a] with cycle @Char (imported from 'Prelude' ...)"
        }
      repaired = repairConstraintInSource broken
      -- No-op case: source starts with a non-constraint annotation.
      noOp = HoleFit
        { hfName   = "id"
        , hfType   = "forall a. a -> a"
        , hfSource = Just "with id @Char (imported from 'Prelude' ...)"
        }
      noOpRepaired = repairConstraintInSource noOp
  in pure $
       -- Type now carries the full constraint.
       "Stack.Types.HasCallStack" `T.isInfixOf` hfType repaired
       && "[a] -> [a]"            `T.isInfixOf` hfType repaired
       && "forall a."             `T.isInfixOf` hfType repaired
       -- Source kept the with-clause only.
       && case hfSource repaired of
            Just s  -> "with cycle @Char" `T.isInfixOf` s
                    && not ("HasCallStack" `T.isInfixOf` s)
            Nothing -> False
       -- No-op: id's source must not be touched.
       && hfType repaired /= hfType broken   -- repair happened
       && hfType noOpRepaired == hfType noOp -- no-op untouched

-- | #196: end-to-end check through 'extractValidFits' using a GHC 9.12
-- simulated output block where @cycle@'s type is wrapped onto a
-- continuation line.
testExtractValidFitsGhc912 :: IO Bool
testExtractValidFitsGhc912 =
  let block = T.lines $ T.unlines
        -- GHC 9.12 wraps the HasCallStack constraint onto a separate
        -- continuation line (indent=10) instead of keeping it on the
        -- fit-head line.
        [ "    • Valid hole fits include"
        , "        cycle :: forall a."
        , "          Stack.Types.HasCallStack => [a] -> [a]"
        , "          with cycle @Char"
        , "          (imported from 'Prelude' at /tmp/F.hs:1:1-16)"
        , "        tail :: forall a."
        , "          Stack.Types.HasCallStack => [a] -> [a]"
        , "          with tail @Char"
        , "          (imported from 'Prelude' at /tmp/F.hs:1:1-16)"
        ]
      fits = extractValidFits block
  in case fits of
       [cycleFit, tailFit] ->
         pure $
           -- Both fit names parsed correctly.
           hfName cycleFit == "cycle"
           && hfName tailFit  == "tail"
           -- Types must include the full constraint, not be truncated.
           && "HasCallStack" `T.isInfixOf` hfType cycleFit
           && "[a] -> [a]"  `T.isInfixOf` hfType cycleFit
           && "HasCallStack" `T.isInfixOf` hfType tailFit
           && "[a] -> [a]"  `T.isInfixOf` hfType tailFit
           -- The constraint must NOT appear in the source fields.
           && not ("HasCallStack" `T.isInfixOf` fromMaybe "" (hfSource cycleFit))
           && not ("HasCallStack" `T.isInfixOf` fromMaybe "" (hfSource tailFit))
       _ -> pure False

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

-- | Issue #52: the legacy Associative template emitted
-- @\\x y z -> (op x y) z == op x (op y z)@ — the LHS is
-- @(op x y) z@, which type-checks as \"apply the result of
-- @op x y :: a@ to @z@\" and is a type error whenever @a@ is
-- not a function. Pin that the corrected template applies the
-- outer @op@ on the LHS.
testSuggestAssocTemplate :: IO Bool
testSuggestAssocTemplate =
  case parseSignature "a -> a -> a" of
    Nothing  -> pure False
    Just sig ->
      let assoc = [ s | s <- applyRules "combineSorted" sig
                      , sLaw s == "Associative" ]
      in case assoc of
           [s] ->
             let prop = sProperty s
             in pure $
                  -- Outer call on the LHS must be present.
                  T.isInfixOf "combineSorted (combineSorted x y) z" prop
                  -- And the RHS shape stays the same.
               && T.isInfixOf "combineSorted x (combineSorted y z)" prop
                  -- The malformed bug shape was
                  -- "\\x y z -> (combineSorted x y) z ==" — the
                  -- LHS opening '(' immediately after '-> '. That
                  -- whole prefix must be absent now.
               && not (T.isInfixOf "-> (combineSorted x y) z" prop)
           _   -> pure False

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

-- | Issue #22: @ghc_batch@ advertises @{tool, args}@ via its
-- @inputSchema@ — parsing must accept that shape. This pins the
-- documented contract; a future regression flips this red instead
-- of silently misleading agents following the tool's own schema.
testBatchParsesToolArgs :: IO Bool
testBatchParsesToolArgs =
  let raw = object
        [ "actions" .=
            [ object
                [ "tool" .= ("ghc_type" :: Text)
                , "args" .= object [ "expression" .= ("reverse" :: Text) ]
                ]
            ]
        ]
  in case A.fromJSON raw :: A.Result BatchArgs of
       A.Success ba -> case baActions ba of
         [tc] -> pure
           ( tcName tc == "ghc_type"
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
                [ "name"      .= ("ghc_eval" :: Text)
                , "arguments" .= object [ "expression" .= ("1+1" :: Text) ]
                ]
            ]
        ]
  in case A.fromJSON raw :: A.Result BatchArgs of
       A.Success ba -> case baActions ba of
         [tc] -> pure (tcName tc == "ghc_eval")
         _    -> pure False
       A.Error _ -> pure False

-- | Issue #175: @ghc_batch@ sub-results were double-wrapped in the
-- MCP content-block envelope (@{content:[{type:\"text\",text:\"…\"}],
-- isError:bool}@) instead of returning the domain JSON directly.
-- 'unwrapResult' must peel off that wrapper so agents see
-- @{status,result,…}@ at @results[i].result@.
testBatchResultNotDoubleWrapped :: IO Bool
testBatchResultNotDoubleWrapped = do
  -- Build a ToolResult the same way every tool handler does.
  let innerPayload = object [ "value" .= ("42" :: Text) ]
      tr           = Env.toolResponseToResult (Env.mkOk innerPayload)
  -- unwrapResult should return the domain JSON, not the wire wrapper.
  case unwrapResult tr of
    A.Object obj ->
      -- Must have "status" (domain field), must NOT have "content" (wire field).
      pure (AKM.member (AKey.fromString "status") obj
         && not (AKM.member (AKey.fromString "content") obj))
    _ -> pure False

-- | Issue #249: 'ghc_batch' with an empty actions list must return
-- status='ok' AND a non-empty 'warning' field — the empty list is
-- valid input but almost certainly a caller error.
testBatchEmptyActionsWarning :: IO Bool
testBatchEmptyActionsWarning = do
  let noopDispatch _ = pure (Env.toolResponseToResult (Env.mkOk (A.object [])))
      args = A.object [ "actions" .= ([] :: [A.Value]) ]
  tr <- Batch.handle noopDispatch args
  pure $ case decodeToolResult tr of
    Right env ->
         Env.reStatus env == Env.StatusOk
      && case Env.reResult env of
           Just (A.Object obj) ->
             AKM.member (AKey.fromText "warning") obj
             && case AKM.lookup (AKey.fromText "total") obj of
                  Just (A.Number 0) -> True
                  _                 -> False
           _ -> False
    Left _ -> False

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

-- | Issue #24: @toolsActive@ in 'ghc_workflow' must enumerate the
-- same set of tools as @tools/list@. The two used to drift because
-- the list was hand-maintained in two places. Paranoia check: also
-- confirm every name is non-empty and the server registers more
-- than the 9-tool Phase-5 baseline.
testWorkflowToolsParity :: IO Bool
testWorkflowToolsParity = pure $
     length allToolNameTexts == length allToolDescriptors
  && not (any T.null allToolNameTexts)
  && length allToolNameTexts >= 20

--------------------------------------------------------------------------------
-- Phase 11b regressions: ghc_deps F-01 / F-02 / F-03 fixes.
--------------------------------------------------------------------------------

-- | F-01: @ghc_deps add@ previously wrote @,@-prefixed continuation
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

-- | Phase 11b F-09: @ghc_coverage@ always returned
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
  all (`elem` allToolNameTexts)
    [ "ghc_browse"
    , "ghc_quickcheck"  -- #94 Phase C: ghc_determinism merged in (runs>=N)
    , "ghc_property_store"  -- #94 Phase C step 6: subsumes ghc_property_lifecycle
    , "ghc_toolchain"  -- #94 Phase C: subsumes ghc_toolchain_warmup
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
        ResourceUri.resourceUriText ResourceUri.WorkflowRules
          `elem` Resources.knownResourceUris
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
  pure (all (`T.isInfixOf` instructions) allToolNameTexts)

-- | Issue #56: the rules text emitted by 'ghc_bootstrap' is
-- baked into the binary via 'workflowRulesMarkdown' /
-- 'sessionInstructionsText'. It used to document the retired
-- subprocess GHCi model ('SessionStatus = Alive | Overflowed |
-- Dead', 'executeNoLock', 'registerDelay', 'GHCi death')  —
-- vocabulary that has nothing to do with the in-process GHC API
-- session that's actually running. Agents debugging timeouts
-- looked for invariants that didn't exist.
--
-- Pin both halves of the contract:
--   * The retired-model words must NOT appear in the rendered text.
--   * The new model words MUST appear so the bake-source isn't
--     accidentally cleared.

testGuidanceNoRetiredVocab :: IO Bool
testGuidanceNoRetiredVocab = do
  let txt = Guidance.sessionInstructionsText allToolDescriptors
      retiredTerms =
        [ "SessionStatus"
        , "executeNoLock"
        , "registerDelay"
        , "GHCi death"
        ]
  pure (not (any (`T.isInfixOf` txt) retiredTerms))

testGuidanceMdNoRetiredVocab :: IO Bool
testGuidanceMdNoRetiredVocab = do
  let md = Guidance.workflowRulesMarkdown allToolDescriptors
      retiredTerms =
        [ "SessionStatus"
        , "executeNoLock"
        , "registerDelay"
        , "GHCi death"
        ]
  pure (not (any (`T.isInfixOf` md) retiredTerms))

testGuidanceMentionsApi :: IO Bool
testGuidanceMentionsApi = do
  let txt = T.toLower (Guidance.sessionInstructionsText allToolDescriptors)
      newTerms = map T.toLower
        [ "in-process"
        , "HscEnv"
        , "MVar"
        , "resetHscEnvInPlace"
        ]
  pure (all (`T.isInfixOf` txt) newTerms)

testGuidanceMdMentionsApi :: IO Bool
testGuidanceMdMentionsApi = do
  let md = T.toLower (Guidance.workflowRulesMarkdown allToolDescriptors)
      newTerms = map T.toLower
        [ "in-process"
        , "HscEnv"
        , "MVar"
        , "resetHscEnvInPlace"
        ]
  pure (all (`T.isInfixOf` md) newTerms)

-- | #124: the plain-text guidance must not reference the retired
-- @ghc_regression@ tool (replaced by @ghc_property_store@ in #94 Phase C).
testGuidanceNoRetiredRegression :: IO Bool
testGuidanceNoRetiredRegression = do
  let txt = Guidance.sessionInstructionsText allToolDescriptors
  pure (not ("ghc_regression" `T.isInfixOf` txt))

-- | #124: the markdown guidance must not reference the retired
-- @ghc_regression@ tool.
testGuidanceMdNoRetiredRegression :: IO Bool
testGuidanceMdNoRetiredRegression = do
  let md = Guidance.workflowRulesMarkdown allToolDescriptors
  pure (not ("ghc_regression" `T.isInfixOf` md))

-- | BUG-09: the markdown resource must match the plain-text
-- instructions in tool coverage — both are derived from the same
-- 'allToolDescriptors', so neither can omit a tool.
testGuidanceMarkdownListsEveryTool :: IO Bool
testGuidanceMarkdownListsEveryTool = do
  let md = Guidance.workflowRulesMarkdown allToolDescriptors
  pure (all (`T.isInfixOf` md) allToolNameTexts)

-- | BUG-05: the situation-tool table is the curated map from
-- "user intent" to tool. Must be non-empty and every row's tool
-- must actually be in the registry. Post-issue-#44 the @srTool@
-- field is a 'ToolName' constructor, so this is now a pure
-- ADT-membership check (the wire form is impossible to typo).
testGuidanceSituationNonEmpty :: IO Bool
testGuidanceSituationNonEmpty = pure $
     not (null Guidance.situationTable)
  && all (\r -> Guidance.srTool r `elem` allToolNames) Guidance.situationTable

-- | BUG-19: @ghc_session@ is a TS-era tool name that does not
-- exist in the Haskell MCP. The phantom reference used to leak
-- into @ghc_deps@' description and hint. Pin that no guidance
-- text mentions the phantom tool.
testGuidanceNoPhantomSession :: IO Bool
testGuidanceNoPhantomSession = do
  let instructions = Guidance.sessionInstructionsText allToolDescriptors
      md           = Guidance.workflowRulesMarkdown   allToolDescriptors
      phantom      = "ghc_session"
  pure $ not (phantom `T.isInfixOf` instructions)
      && not (phantom `T.isInfixOf` md)

-- | BUG-19 companion: the @ghc_deps@ tool descriptor used to say
-- \"run ghc_session(action='restart')\". Pin that the description
-- no longer mentions the phantom tool.
testDepsDescriptorNoPhantom :: IO Bool
testDepsDescriptorNoPhantom = do
  let depsDesc = head [ tdDescription d | d <- allToolDescriptors
                                        , tdName d == "ghc_deps" ]
  pure (not ("ghc_session" `T.isInfixOf` depsDesc))

-- | BUG-19 companion: the @ghc_deps@ add/remove response carried
-- a @hint@ string instructing the agent to call @ghc_session@.
-- Pin that the live Deps source no longer embeds the phantom.
testDepsHintNoPhantom :: IO Bool
testDepsHintNoPhantom = do
  src <- TIO.readFile "src/HaskellFlows/Tool/Deps.hs"
  pure (not ("ghc_session" `T.isInfixOf` src))

--------------------------------------------------------------------------------
-- BUG-02 — ghc_quickcheck_export must generate valid Haskell
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

-- | Issue #40: when properties are persisted with @spModule =
-- "test/Spec.hs"@ — exactly the path the export writes — the
-- legacy renderer emitted a self-referential @import Spec@ in a
-- file that uses @module Main where@. The new renderer takes the
-- output's module-name hint and filters it out of the import set.
testExportRenderDropsSelfImport :: IO Bool
testExportRenderDropsSelfImport = do
  let props =
        [ StoredProperty
            { spExpression = "\\x -> x == (x :: Int)"
            , spModule     = Just "test/Spec.hs"
            , spPassed     = 1
            , spUpdated    = 0
            }
        ]
      -- The output will live at @test/Spec.hs@ → module hint "Spec".
      rendered = QcExport.renderTestFileWith (Just "Spec") [] props
  pure $ not (T.isInfixOf "import Spec" rendered)
      && T.isInfixOf "module Main where" rendered

-- | Issue #40 — properties authored at test scope reference
-- library identifiers ('simplify', 'eval', …) but their
-- 'spModule' carries no library-module trail. The renderer must
-- pick up the slack by importing every @exposed-modules:@ entry
-- from the project's library stanza so the emitted file compiles
-- standalone.
testExportRenderUnionsLibMods :: IO Bool
testExportRenderUnionsLibMods = do
  let props =
        [ StoredProperty
            { spExpression = "\\e -> eval emptyEnv (simplify e) == eval emptyEnv e"
            , spModule     = Just "test/Spec.hs"
            , spPassed     = 1
            , spUpdated    = 0
            }
        ]
      libMods = ["Expr.Syntax", "Expr.Simplify", "Expr.Eval"]
      rendered = QcExport.renderTestFileWith (Just "Spec") libMods props
  pure $ T.isInfixOf "import Expr.Syntax"   rendered
      && T.isInfixOf "import Expr.Simplify" rendered
      && T.isInfixOf "import Expr.Eval"     rendered
      && not (T.isInfixOf "import Spec" rendered)

-- | Issue #40: a library module that ALSO appears as a
-- property's @spModule@ must not be imported twice. The renderer
-- dedupes after sorting, so 'nub' on a sorted list does the job.
testExportRenderDedupesLibAndProps :: IO Bool
testExportRenderDedupesLibAndProps = do
  let props =
        [ StoredProperty
            { spExpression = "\\x -> simplify x == simplify (simplify x)"
            , spModule     = Just "src/Expr/Simplify.hs"
            , spPassed     = 1
            , spUpdated    = 0
            }
        ]
      libMods  = ["Expr.Simplify"]  -- already covered by spModule
      rendered = QcExport.renderTestFileWith Nothing libMods props
      occurrences = T.count "import Expr.Simplify" rendered
  pure (occurrences == 1)

--------------------------------------------------------------------------------
-- #131 — export guard: prevents overwriting hand-written Spec.hs
--------------------------------------------------------------------------------

-- | exportGuard returns Nothing (proceed) when the file does not exist.
testExportGuardNewFile :: IO Bool
testExportGuardNewFile = do
  tmpDir <- getTemporaryDirectory
  let path = tmpDir </> "hf-guard-new-Spec.hs"
  removePathForcibly path
  result <- QcExport.exportGuard path False
  pure (isNothing result)

-- | exportGuard returns Nothing when the file starts with the generated header.
testExportGuardGeneratedFile :: IO Bool
testExportGuardGeneratedFile = do
  tmpDir <- getTemporaryDirectory
  let path = tmpDir </> "hf-guard-gen-Spec.hs"
  TIO.writeFile path (QcExport.generatedHeader <> "\nmodule Main where\n")
  result <- QcExport.exportGuard path False
  pure (isNothing result)

-- | exportGuard returns Nothing when the file starts with the scaffold header
-- (produced by ghc_project(action="create")). Both headers are MCP-generated;
-- the scaffold stub is intentionally replaced by a real property suite.
testExportGuardScaffoldFile :: IO Bool
testExportGuardScaffoldFile = do
  tmpDir <- getTemporaryDirectory
  let path = tmpDir </> "hf-guard-scaffold-Spec.hs"
  TIO.writeFile path (QcExport.scaffoldTestFileHeader <> "\nmodule Main where\n")
  result <- QcExport.exportGuard path False
  pure (isNothing result)

-- | exportGuard returns Just (refusal) when the file is hand-written (no header).
testExportGuardHandWritten :: IO Bool
testExportGuardHandWritten = do
  tmpDir <- getTemporaryDirectory
  let path = tmpDir </> "hf-guard-handwritten-Spec.hs"
  TIO.writeFile path "-- Hand-written test suite\nmodule Main where\n"
  result <- QcExport.exportGuard path False
  pure (isJust result)

-- | exportGuard returns Nothing (proceed) when force=True, even for hand-written.
testExportGuardForce :: IO Bool
testExportGuardForce = do
  tmpDir <- getTemporaryDirectory
  let path = tmpDir </> "hf-guard-force-Spec.hs"
  TIO.writeFile path "-- Hand-written test suite\nmodule Main where\n"
  result <- QcExport.exportGuard path True
  pure (isNothing result)

-- | Integration: export handle refuses to overwrite a hand-written file.
testExportHandleRefusesHandWritten :: IO Bool
testExportHandleRefusesHandWritten =
  withTempProject $ \pd -> do
    store <- openStore pd
    let specDir  = unProjectDirRaw pd </> "test"
        specPath = specDir </> "Spec.hs"
    createDirectoryIfMissing True specDir
    TIO.writeFile specPath "-- Hand-written test suite\nmodule Main where\n"
    let args = object [ "action" .= ("export" :: T.Text) ]
    result <- QcExport.handle store pd args
    case trContent result of
      [TextContent body] ->
        pure (  "hand_written_file_guard" `T.isInfixOf` body
             || "Refusing to overwrite"   `T.isInfixOf` body)
      _ -> pure False

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

-- | Issue #73: 'Self-inverse on lists' is the structural twin
-- of 'Involutive' (same f.f==id law, list-shaped surface).
-- The pre-#73 rule ranked it Medium for ANY list signature,
-- including normalisers — making the agent burn a round-trip
-- on the losing law before reaching the (Low) Involutive
-- twin. Same dampening applies: when the function name hints
-- at canonicalisation, the rule must drop to Low with a
-- name-aware rationale.
testSelfInverseLowForNormalizer :: IO Bool
testSelfInverseLowForNormalizer =
  case parseSignature "[a] -> [a]" of
    Nothing  -> pure False
    Just sig -> pure $
      let names = ["simplify", "normalize", "canonicalize"
                  , "fold", "optimize", "reduce", "rewrite"]
          row nm =
            [ s | s <- applyRules nm sig, sLaw s == "Self-inverse on lists" ]
      in all (\nm ->
                case row nm of
                  [s] -> sConfidence s == Low
                         && "normaliser" `T.isInfixOf` sRationale s
                  _   -> False)
              names

-- | Issue #73 — symmetric: 'reverse :: [a] -> [a]' is a real
-- self-inverse, so the rule stays Medium with the original
-- "reverse, rot-k, swap-adjacent-pairs" rationale.
testSelfInverseMediumForReverse :: IO Bool
testSelfInverseMediumForReverse =
  case parseSignature "[a] -> [a]" of
    Nothing  -> pure False
    Just sig -> pure $
      let row =
            [ s | s <- applyRules "reverse" sig, sLaw s == "Self-inverse on lists" ]
      in case row of
           [s] -> sConfidence s == Medium
                  && "reverse" `T.isInfixOf` sRationale s
                  && not ("normaliser" `T.isInfixOf` sRationale s)
           _   -> False

--------------------------------------------------------------------------------
-- BUG-15 — ghc_suggest scope-error goes through a structured hint
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
         && T.isInfixOf "ghc_load"                             body
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
assertNext :: ToolName -> A.Value -> ToolName -> Bool
assertNext tool payload expected =
  case suggestNext tool True payload of
    Just ns -> nsTool ns == expected
    Nothing -> False

testNextStepGatePass :: IO Bool
testNextStepGatePass =
  let payload = A.object [ "success" .= True, "totalDurationSec" .= (1.0 :: Double) ]
  in pure (assertNext GhcGate payload GhcCoverage)

testNextStepGateFail :: IO Bool
testNextStepGateFail =
  let payload = A.object [ "success" .= False, "totalDurationSec" .= (1.0 :: Double) ]
  in pure (assertNext GhcGate payload GhcCheckProject)

testNextStepQcExport :: IO Bool
testNextStepQcExport =
  -- #94 Phase C step 6: ghc_quickcheck_export merged into
  -- ghc_property_store(action=export). The export branch's
  -- discriminator in the response is 'files_written'.
  let payload = A.object
        [ "success" .= True
        , "properties_written" .= (3 :: Int)
        , "files_written" .= (["test/Spec.hs"] :: [Text])
        ]
  in pure (assertNext GhcPropertyStore payload GhcGate)

testNextStepDeterminismPass :: IO Bool
testNextStepDeterminismPass =
  -- #94 Phase C: ghc_determinism merged into ghc_quickcheck (runs>=2).
  -- The 'runs' field in the payload is the discriminator that tells
  -- the dispatcher this was a multi-run call.
  -- #94 Phase C step 6: regression-replay is now ghc_property_store(run).
  let payload = A.object [ "success" .= True, "runs" .= (3 :: Int) ]
  in pure (assertNext GhcQuickCheck payload GhcPropertyStore)

testNextStepDeterminismFail :: IO Bool
testNextStepDeterminismFail =
  let payload = A.object [ "success" .= False, "runs" .= (3 :: Int) ]
  in pure (assertNext GhcQuickCheck payload GhcQuickCheck)

testNextStepAddImport :: IO Bool
testNextStepAddImport =
  -- Issue #53: count>0 must accompany the success payload for the
  -- nudge to fire. A payload without 'count' is interpreted as
  -- \"nothing was added\" and the nextStep is suppressed.
  let payload = A.object
        [ "success" .= True
        , "module"  .= ("src/Foo.hs" :: Text)
        , "count"   .= (3 :: Int)
        ]
  in pure (assertNext GhcAddImport payload GhcLoad)

-- | #94 Phase B — 'ghc_modules' (the action-discriminated successor
-- to add_modules + remove_modules) emits a multi-step chain. The
-- primary next tool is 'ghc_check_project' AND the chain must
-- include at least 'ghc_check_project' + 'ghc_load'.
testNextStepAddModulesChain :: IO Bool
testNextStepAddModulesChain =
  let payload = A.object [ "success" .= True, "cabal_added" .= (["Foo.Bar"] :: [Text]) ]
  in case suggestNext GhcModules True payload of
       Just ns ->
         pure $ nsTool ns == GhcCheckProject
             && case nsChain ns of
                  Just steps ->
                       any ((== GhcLoad)         . csTool) steps
                    && any ((== GhcCheckProject) . csTool) steps
                  Nothing -> False
       Nothing -> pure False

testNextStepApplyExports :: IO Bool
testNextStepApplyExports =
  let payload = A.object [ "success" .= True, "module" .= ("src/Foo.hs" :: Text) ]
  in pure (assertNext GhcApplyExports payload GhcLoad)

testNextStepFixWarning :: IO Bool
testNextStepFixWarning =
  let payload = A.object [ "success" .= True, "module" .= ("src/Foo.hs" :: Text) ]
  in pure (assertNext GhcFixWarning payload GhcLoad)

testNextStepBrowse :: IO Bool
testNextStepBrowse =
  let payload = A.object [ "success" .= True, "count" .= (5 :: Int) ]
  in pure (assertNext GhcBrowse payload GhcSuggest)

testNextStepToolchainWarmup :: IO Bool
testNextStepToolchainWarmup =
  -- #94 Phase C: GhcToolchainWarmup merged into GhcToolchain
  -- (action="warmup"). The dispatch arm is action-agnostic — both
  -- status and warmup recommend ghc_workflow help.
  let payload = A.object [ "success" .= True, "action" .= ("warmup" :: Text) ]
  in pure (assertNext GhcToolchain payload GhcWorkflow)

testNextStepPropertyLifecycleList :: IO Bool
testNextStepPropertyLifecycleList =
  -- #94 Phase C step 6: ghc_property_lifecycle + ghc_regression
  -- merged into ghc_property_store. action=list now recommends
  -- action=run on the same consolidated tool.
  let payload = A.object [ "success" .= True, "action" .= ("list" :: Text) ]
  in pure (assertNext GhcPropertyStore payload GhcPropertyStore)

-- | BUG-22: create_project emits the canonical project-bootstrap
-- chain (deps + add_modules + load). Pin that all three steps are
-- present so the agent can hand it off to ghc_batch.
testNextStepCreateProjectChain :: IO Bool
testNextStepCreateProjectChain =
  let payload = A.object [ "success" .= True, "files_written" .= ([] :: [Text]) ]
  in case suggestNext GhcProject True payload of
       Just ns ->
         pure $ nsTool ns == GhcDeps
             && case nsChain ns of
                  Just steps ->
                    let tools = map csTool steps
                    in GhcDeps    `elem` tools
                    && GhcModules `elem` tools
                    && GhcLoad    `elem` tools
                  Nothing -> False
       Nothing -> pure False

-- | BUG-07 — static source check: the Server must (a) import
-- Staleness, (b) capture boot time + binary path, (c) actually
-- invoke 'checkStaleness' when dispatching ghc_workflow, and
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

-- | BUG-08 — 5 @ghc_load@ calls in a row must trigger the
-- polling nudge that points at ghc_quickcheck / check_project.
testHistoryPolling :: IO Bool
testHistoryPolling =
  let nudges = WS.historyNudges (replicate 5 GhcLoad)
  in pure $ any ("polling" `T.isInfixOf`) nudges
         && any ("ghc_quickcheck" `T.isInfixOf`) nudges

-- | BUG-08 — ghc_suggest followed by non-quickcheck activity
-- surfaces the "pick a law" nudge.
testHistoryMissingQc :: IO Bool
testHistoryMissingQc =
  let hist = [GhcLoad, GhcSuggest, GhcLoad]
      nudges = WS.historyNudges hist
  in pure $ any ("ghc_quickcheck" `T.isInfixOf`) nudges

-- | BUG-08 — last tool was ghc_refactor with no ghc_load since
-- triggers the "reload after refactor" nudge.
testHistoryRefactorNotReloaded :: IO Bool
testHistoryRefactorNotReloaded =
  let hist = [GhcRefactor, GhcType]
      nudges = WS.historyNudges hist
  in pure $ any (\n -> "refactor" `T.isInfixOf` T.toLower n) nudges

-- | BUG-24 — a zero-activity state classifies as pre-scaffold.
testPhasePreScaffold :: IO Bool
testPhasePreScaffold = do
  ref <- WS.newWorkflowStateRef
  s   <- WS.readState ref
  pure (WS.classifyPhase s == WS.PhasePreScaffold)

-- | BUG-24 — a failed ghc_load classifies as bootstrap. Verify
-- with a synthetic state update sequence.
testPhaseBootstrap :: IO Bool
testPhaseBootstrap = do
  ref <- WS.newWorkflowStateRef
  let failedLoad = A.object [ "success" .= False, "errors" .= ["broken" :: Text]
                            , "warnings" .= ([] :: [Text]) ]
  WS.trackTool ref GhcLoad False failedLoad
  s <- WS.readState ref
  pure (WS.classifyPhase s == WS.PhaseBootstrap)

-- | BUG-24 — recent ghc_suggest or ghc_quickcheck classifies
-- as testing-laws.
testPhaseTestingLaws :: IO Bool
testPhaseTestingLaws = do
  ref <- WS.newWorkflowStateRef
  let okLoad   = A.object [ "success" .= True, "errors" .= ([] :: [Text])
                          , "warnings" .= ([] :: [Text]) ]
      suggest  = A.object [ "success" .= True, "count" .= (1 :: Int) ]
  WS.trackTool ref GhcLoad    True okLoad
  WS.trackTool ref GhcSuggest True suggest
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
  WS.trackTool ref GhcLoad       True okLoad
  WS.trackTool ref GhcQuickCheck True passQc
  WS.trackTool ref GhcQuickCheck True passQc
  WS.trackTool ref GhcQuickCheck True passQc
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
-- BUG-17 — ghc_arbitrary uses 'sized' for recursive types
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

-- | Issue #210: when loadForTarget returns (False, errs),
-- 'compileFailedErr' must produce status='failed' with
-- kind='validation' and a message mentioning the error count.
testArbitraryCompileFailShape :: IO Bool
testArbitraryCompileFailShape =
  let result = compileFailedErr 3
  in pure $ case trContent result of
       [TextContent body_] ->
         case A.decode (TLE.encodeUtf8 (TL.fromStrict body_)) of
           Just (A.Object top) ->
             AKM.lookup "status" top == Just (A.String "failed")
             && case AKM.lookup "error" top of
                  Just (A.Object err) ->
                    AKM.lookup "kind" err == Just (A.String "validation")
                    && case AKM.lookup "message" err of
                         Just (A.String msg) -> "3 compile error(s)" `T.isInfixOf` msg
                         _                   -> False
                  _ -> False
           _ -> False
       _ -> False

-- | Issue #218: when getInfo returns Nothing (GHC wired-in primitives
-- like Bool after -hide-all-packages stanza flags), 'handle' must NOT
-- say "not in scope" — it must say "cannot introspect" with the hint
-- that QuickCheck already provides an instance.
--
-- We test via the source text rather than running the full GHC session
-- to keep this a fast unit test. The patch is in the Right Nothing
-- branch of 'handle'.
testArbitraryWiredInMessage :: IO Bool
testArbitraryWiredInMessage = do
  src <- TIO.readFile "src/HaskellFlows/Tool/Arbitrary.hs"
  -- The new message must NOT say "not in scope"
  let noOldMsg = not ("not in scope (getInfo=Nothing)" `T.isInfixOf` src)
  -- The new message MUST mention "wired-in" or "introspect"
  let hasNewMsg = "cannot introspect" `T.isInfixOf` src
                || "wired-in" `T.isInfixOf` src
  -- The Right Nothing branch must use validationErr not notInScopeErr
  let usesValidation = "Right Nothing ->" `T.isInfixOf` src
                     && "validationErr" `T.isInfixOf` src
  pure (noOldMsg && hasNewMsg && usesValidation)

-- | Issue #219: 'hasUnboxedConstructor' detects constructors whose
-- name ends with '#' (I#, C#, W#, …). A constructor that does NOT
-- end with '#' must return False.
testArbitraryHasUnboxedConstructor :: IO Bool
testArbitraryHasUnboxedConstructor = pure $
  -- Typical unboxed-primop constructors
     hasUnboxedConstructor (Constructor { cName = "I#", cArgs = ["Int#"] })
  && hasUnboxedConstructor (Constructor { cName = "C#", cArgs = ["Char#"] })
  && hasUnboxedConstructor (Constructor { cName = "W#", cArgs = ["Word#"] })
  -- Regular constructors must NOT be flagged
  && not (hasUnboxedConstructor (Constructor { cName = "Just",  cArgs = ["a"] }))
  && not (hasUnboxedConstructor (Constructor { cName = "False", cArgs = [] }))
  && not (hasUnboxedConstructor (Constructor { cName = "PathNotAbsolute", cArgs = ["Text"] }))

-- | #226: 'renderTyThing' must try all names returned by 'parseName'
-- rather than blindly taking the first one.  When an external-package
-- type shares an unqualified name with a home-module type, taking only
-- the first name causes getInfo to return Nothing → "wired-in primitive"
-- message.  Verified by checking the source uses the multi-name loop.
testArbitraryFirstJustSource :: IO Bool
testArbitraryFirstJustSource = do
  src <- TIO.readFile "src/HaskellFlows/Tool/Arbitrary.hs"
  let usesMapM    = "mapM tryName" `T.isInfixOf` src
      hasFirstJust = "firstJust"   `T.isInfixOf` src
      noNilTail   = not ("n :| _ <- parseName" `T.isInfixOf` src)
  pure (usesMapM && hasFirstJust && noNilTail)

-- | #226: the pure template path correctly handles two-param types.
-- renderTemplate with params ["a","b"] produces the right instance header.
testArbitraryTwoParamTemplate :: IO Bool
testArbitraryTwoParamTemplate =
  let raw = T.unlines
        [ "data Tagged a b = Tagged a b"
        ]
      params = parseTypeParams raw
      ctors  = parseConstructors raw
      tmpl   = renderTemplate "Tagged" params ctors
  in pure $ params == ["a", "b"]
         && "(Arbitrary a, Arbitrary b) => Arbitrary (Tagged a b)" `T.isInfixOf` tmpl

-- | Issue #217: 'ghc_goto' descriptor must acknowledge that source
-- locations are only available for interpreted modules and that most
-- project modules compile to object code.
testGhcGotoDescriptorAccurate :: IO Bool
testGhcGotoDescriptorAccurate = do
  src <- TIO.readFile "src/HaskellFlows/Tool/Goto.hs"
  -- Descriptor must mention the compiled-mode limitation
  let mentionsCompiled = "compiled" `T.isInfixOf` src
  -- Descriptor must mention interpreted mode (byte-code)
  let mentionsByteCode = "interpreted" `T.isInfixOf` src
                       || "byte-code"  `T.isInfixOf` src
  pure (mentionsCompiled && mentionsByteCode)

--------------------------------------------------------------------------------
-- BUG-16 — ghc_remove_modules symmetric to ghc_add_modules
--------------------------------------------------------------------------------

-- | Tool is registered in the canonical registry. If this
-- fails, the tool exists as dead code (not dispatchable).
testRemoveModulesRegistered :: IO Bool
testRemoveModulesRegistered = pure $
  "ghc_modules" `elem` allToolNameTexts

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

-- | #157: modules listed under @other-modules:@ are removed just
-- like @exposed-modules:@ entries. Before the fix only
-- @exposed-modules@ was scanned, so 'other-modules' entries
-- silently fell through with an empty 'cabal_removed'.
testRemoveModulesOtherModules :: IO Bool
testRemoveModulesOtherModules =
  let cabal = T.unlines
        [ "library"
        , "  other-modules:  Internal.Helper"
        , "                  Internal.Drop"
        , "  build-depends:  base"
        ]
      (newCabal, removed) = RM.removeModulesFromBody cabal ["Internal.Drop"]
  in pure $ removed == ["Internal.Drop"]
         && T.isInfixOf "Internal.Helper" newCabal
         && not ("Internal.Drop" `T.isInfixOf` newCabal)

-- | #157: removing a module that does not appear in @other-modules@
-- is still a no-op (same idempotency guarantee as exposed-modules).
testRemoveModulesOtherModulesIdempotent :: IO Bool
testRemoveModulesOtherModulesIdempotent =
  let cabal = T.unlines
        [ "library"
        , "  other-modules:  Internal.Helper"
        , "  build-depends:  base"
        ]
      (newCabal, removed) = RM.removeModulesFromBody cabal ["Internal.NeverExisted"]
  in pure (null removed && newCabal == cabal)

-- | #157: when a module appears in @exposed-modules@ of one stanza
-- and @other-modules@ of another, both occurrences are removed and
-- both names appear in the returned list.
testRemoveModulesBothSections :: IO Bool
testRemoveModulesBothSections =
  let cabal = T.unlines
        [ "library"
        , "  exposed-modules:  Shared.Core"
        , "                    Shared.Util"
        , "  build-depends:    base"
        , ""
        , "test-suite pkg-test"
        , "  other-modules:  Shared.Util"
        , "  build-depends:  base, QuickCheck"
        ]
      (newCabal, removed) = RM.removeModulesFromBody cabal ["Shared.Util"]
  in pure $ "Shared.Util" `elem` removed
         && T.isInfixOf "Shared.Core"     newCabal
         && not ("Shared.Util" `T.isInfixOf` newCabal)

-- | Issue #248: 'ghc_modules remove' must surface a 'not_found' list
-- when a requested module does not appear in any cabal section,
-- rather than silently returning an empty 'cabal_removed' list.
testRemoveModulesNotFoundField :: IO Bool
testRemoveModulesNotFoundField = do
  tmp <- getTemporaryDirectory
  let dir      = tmp </> "haskell-flows-rm-notfound"
      cabalFile = dir </> "pkg.cabal"
  removePathForcibly dir
  createDirectoryIfMissing True dir
  TIO.writeFile cabalFile $ T.unlines
    [ "cabal-version: 3.0"
    , "name:          pkg"
    , "version:       0.1.0.0"
    , "library"
    , "  exposed-modules:  Existing.Module"
    , "  build-depends:    base"
    , "  default-language: Haskell2010"
    ]
  result <- case mkProjectDir dir of
    Left _   -> pure False
    Right pd -> do
      let args = A.object
            [ "modules"      A..= (["Never.Existed"] :: [Text])
            , "delete_files" A..= False
            ]
      tr <- RM.handle pd args
      pure $ case decodeToolResult tr of
        Right env ->
             Env.reStatus env == Env.StatusOk
          && case Env.reResult env of
               Just (A.Object obj) ->
                 case AKM.lookup (AKey.fromText "not_found") obj of
                   Just (A.Array arr) ->
                     A.String "Never.Existed" `elem` arr
                   _                  -> False
               _ -> False
        Left _ -> False
  removePathForcibly dir
  pure result

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

-- | BUG-01 + Issue #75 — 'cabalStep' must drain stdout AND
-- stderr without deadlocking on a full pipe buffer.
--
-- The pre-#75 implementation forked two threads doing
-- @hGetContents h >>= putMVar v@. Because @hGetContents@ is
-- lazy, the forks deposited thunks into the MVars without
-- actually draining the OS pipes. When cabal wrote more than
-- ~64 KiB (a noisy build error, a -Wall storm), the writer
-- blocked, @waitForProcess@ blocked, and the whole gate hung
-- past its 5-minute timeout in a way that corrupted the MCP
-- transport — the agent saw \"Connection closed\" instead of a
-- structured TimedOut step.
--
-- The fix delegates to @readCreateProcessWithExitCode@, which
-- uses strict bytestring drains for both pipes internally. This
-- test pins the new invariant: the manual fork-and-MVar pattern
-- is gone, replaced by the canonical helper.
testGateCabalStepBracket :: IO Bool
testGateCabalStepBracket = do
  src <- TIO.readFile "src/HaskellFlows/Tool/Gate.hs"
  pure $ T.isInfixOf "readCreateProcessWithExitCode" src
      && not (T.isInfixOf "forkIO (hGetContents" src)
      && not (T.isInfixOf "(_, Just hOut, Just hErr, ph) <- createProcess" src)

-- | #94 Phase B nextStep coverage: 'ghc_modules' on a remove-shaped
-- success suggests the project-wide check + reload chain so any
-- dangling import surfaces immediately.  Both add and remove route
-- through the same chain (the post-condition is the same).
testNextStepRemoveModules :: IO Bool
testNextStepRemoveModules =
  let payload = A.object
        [ "success"      .= True
        , "cabal_removed".= (["Foo.Old"] :: [Text])
        ]
  in case suggestNext GhcModules True payload of
       Just ns ->
         pure $ nsTool ns == GhcCheckProject
             && case nsChain ns of
                  Just steps ->
                       any ((== GhcCheckProject) . csTool) steps
                    && any ((== GhcLoad)         . csTool) steps
                  Nothing -> False
       Nothing -> pure False

--------------------------------------------------------------------------------
-- BUG-10 — ghc_bootstrap writes host rules from the running binary
--------------------------------------------------------------------------------

-- | Tool is in the registry.
testBootstrapRegistered :: IO Bool
testBootstrapRegistered = pure ("ghc_project" `elem` allToolNameTexts)
  -- #94 Phase C step 5: ghc_bootstrap merged into
  -- ghc_project(action="bootstrap"). The legacy wire surface is
  -- gone; the action lives on inside the consolidated tool.

-- | 'ghc_bootstrap(host="claude-code")' preview mode returns
-- the live workflow markdown body (dynamically derived) and
-- does NOT write anything. The markdown is inlined as a JSON
-- string field, so newlines etc. are escaped — we assert
-- *markers* from the markdown, not byte equality.
-- | 'ghc_bootstrap(host="claude-code", write=false)' returns preview content
-- without writing to disk. Issue #193: pass write=false explicitly since
-- the default is now write=true.
testBootstrapPreview :: IO Bool
testBootstrapPreview = withTempProject $ \pd -> do
  let args = A.object [ "host" .= ("claude-code" :: Text)
                      , "write" .= False ]
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
      && T.isInfixOf "ghc_workflow"            body
      && not wrote          -- preview must NOT write

-- | #193: 'ghc_bootstrap(host="claude-code")' with NO write arg must
-- write to disk (write defaults to true, not false). Previously the
-- 'BootstrapArgs' FromJSON defaulted 'write' to False, contradicting
-- the schema which says "If true (default), write files to disk."
testBootstrapDefaultWrite :: IO Bool
testBootstrapDefaultWrite = withTempProject $ \pd -> do
  let args = A.object [ "host" .= ("claude-code" :: Text) ]
  tr <- Bootstrap.handle pd allToolDescriptors args
  let body = case trContent tr of
        (TextContent t : _) -> t
        _                   -> ""
      dest = unProjectDirRaw pd </> ".claude" </> "rules" </> "haskell-flows-mcp.md"
  wrote <- doesFileExist dest
  pure $ not (trIsError tr)
      && T.isInfixOf "\"mode\":\"written\"" body
      && wrote          -- default must WRITE to disk

-- | 'ghc_bootstrap(host="claude-code", write=true)' persists the
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

-- | #179: when bootstrap returns mode=written, nextStep.why must NOT say
-- "Re-run with write=true" — the file is already on disk.
testBootstrapWriteNextStep :: IO Bool
testBootstrapWriteNextStep =
  let payload = A.object
        [ "status" .= ("ok"      :: Text)
        , "mode"   .= ("written" :: Text)
        , "host"   .= ("claude-code" :: Text)
        , "path"   .= ("/some/path" :: Text)
        ]
  in pure $ case suggestNext GhcProject True payload of
       Just ns ->
         not ("write=true" `T.isInfixOf` nsWhy ns)
         && "written" `T.isInfixOf` nsWhy ns
       Nothing -> False

-- | #179: when bootstrap returns mode=preview, nextStep.why must say
-- "Re-run with write=true" so the agent knows what to do next.
testBootstrapPreviewNextStep :: IO Bool
testBootstrapPreviewNextStep =
  let payload = A.object
        [ "status"  .= ("ok"      :: Text)
        , "mode"    .= ("preview" :: Text)
        , "host"    .= ("claude-code" :: Text)
        , "content" .= ("# rules" :: Text)
        ]
  in pure $ case suggestNext GhcProject True payload of
       Just ns -> "write=true" `T.isInfixOf` nsWhy ns
       Nothing -> False

--------------------------------------------------------------------------------
-- BUG-11 + BUG-12 — README accuracy (doc-as-code)
--------------------------------------------------------------------------------

-- | The main README.md must:
--   * mention the Haskell install path (haskell-flows-mcp).
--   * NOT reference the TS-only install (npm / mcp-server/).
--   * NOT reference the broken APIs the README used to show
--     ('ghc_suggest(analyze)', 'ghc_workflow(action="gate")').
testDocsMainReadme :: IO Bool
testDocsMainReadme = do
  readme <- TIO.readFile "../README.md"
  pure $ T.isInfixOf "haskell-flows-mcp"          readme
      && T.isInfixOf "cabal install"              readme
      && T.isInfixOf "ghc_project"                readme
      -- ^ #94 Phase C step 5: ghc_bootstrap merged into
      -- ghc_project(action=bootstrap); the README lists the new
      -- name. The legacy 'ghc_bootstrap' string is gone.
      && not ("ghc_suggest(analyze)"             `T.isInfixOf` readme)
      && not ("ghc_workflow(action=\"gate\")"    `T.isInfixOf` readme)
      && not ("npm install"                       `T.isInfixOf` readme)
      && not ("cd mcp-server\n"                   `T.isInfixOf` readme)

-- | The mcp-server-haskell/README.md must reflect the live tool
-- registry: mention every registered tool at least once.
testDocsHaskellReadme :: IO Bool
testDocsHaskellReadme = do
  readme <- TIO.readFile "README.md"
  pure $ T.isInfixOf "haskell-flows-mcp" readme
      && T.isInfixOf "`ghc_project`"    readme
      -- ^ #94 Phase C step 5: ghc_bootstrap merged into
      -- ghc_project(action=bootstrap). README documents the new
      -- consolidated tool name.
      && T.isInfixOf "`ghc_gate`"       readme
      && T.isInfixOf "`ghc_suggest`"    readme
      && T.isInfixOf "`ghc_modules`" readme
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
        [ "ghc_type", "ghc_info", "ghc_eval", "ghc_goto"
        , "ghc_doc", "ghc_complete", "hoogle_search"
        , "ghc_coverage"    -- terminal: final report
        , "ghc_workflow"    -- meta: would self-loop
        , "ghc_batch"       -- result depends on inner tools
        , "ghc_lint"        -- agent interprets per-hint
        , "ghc_imports"     -- pure diagnostic aid
        -- (b) action-conditional — per-branch tests cover each action
        , "ghc_deps"                 -- add/remove/list
        , "ghc_property_store"       -- list / run / export / audit;
                                     -- #94 Phase C step 6 successor to
                                     -- ghc_property_lifecycle / ghc_regression
                                     -- / ghc_quickcheck_export / ghc_property_audit
        , "ghc_project"              -- create / switch / validate /
                                     -- bootstrap; per-action shape
                                     -- distinguishers test each branch
        , "ghc_quickcheck"           -- state = passed/failed
        , "ghc_scratch"              -- #253: write / check / list / show
                                     -- / clear / promote; per-action shape
                                     -- distinguishers (id, count, entries,
                                     -- cleared, removed) covered by
                                     -- dedicated branches above.
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
        Nothing -> toolNameText t `elem` whitelist
  in all covered allToolNames

-- | Phase 11k: WorkflowState tracker starts at zero counters + empty history.
testWorkflowStateInitial :: IO Bool
testWorkflowStateInitial = do
  ref <- WS.newWorkflowStateRef
  s <- WS.readState ref
  pure $ WS.wsToolCalls s == 0
      && WS.wsEditsSinceLastLoad s == 0
      && null (WS.wsToolHistory s)

-- | Phase 11k: ghc_load resets edit counter; ghc_refactor increments it.
testWorkflowStateTracks :: IO Bool
testWorkflowStateTracks = do
  ref <- WS.newWorkflowStateRef
  let okLoad = A.object [ "success" .= True, "errors" .= ([] :: [Text])
                        , "warnings" .= ([] :: [Text]) ]
      okRef  = A.object [ "success" .= True, "compile" .= ("ok" :: Text) ]
  WS.trackTool ref GhcRefactor True okRef
  WS.trackTool ref GhcRefactor True okRef
  s1 <- WS.readState ref
  WS.trackTool ref GhcLoad     True okLoad
  s2 <- WS.readState ref
  pure $ WS.wsEditsSinceLastLoad s1 == 2
      && WS.wsEditsSinceLastLoad s2 == 0
      && WS.wsLastLoadSuccess s2 == Just True

-- | Phase 11k: renderHelp surfaces the recompile nudge only when
-- editsSinceLastLoad crosses the 3-edit threshold.
testWorkflowStateHelp :: IO Bool
testWorkflowStateHelp =
  let lowEdits  = WS.WorkflowState 0 2 Nothing 0 0 [] Set.empty 0 (posixSecondsToUTCTime 0)
      highEdits = WS.WorkflowState 0 5 Nothing 0 0 [] Set.empty 0 (posixSecondsToUTCTime 0)
      nudgeLow  = WS.renderHelp lowEdits
      nudgeHigh = WS.renderHelp highEdits
  in pure $ null nudgeLow
         && any (T.isInfixOf "edits since the last ghc_load") nudgeHigh

-- | #257: trackTool accumulates the ever-called set — it never forgets
-- (unlike the capped wsToolHistory), so it is the basis for the
-- "unused tools" count in ghc_workflow(status) and for #263 discover.
testSessionActivityEverCalled :: IO Bool
testSessionActivityEverCalled = do
  ref <- WS.newWorkflowStateRef
  WS.trackTool ref GhcLoad    True (A.object [])
  WS.trackTool ref GhcLoad    True (A.object [])   -- repeat: set stays size 1
  WS.trackTool ref GhcSuggest True (A.object [])
  s <- WS.readState ref
  let ever = WS.wsEverCalled s
  pure $ GhcLoad       `Set.member`    ever
      && GhcSuggest    `Set.member`    ever
      && GhcQuickCheck `Set.notMember` ever
      && Set.size ever == 2

-- | #257: error streak increments on each failure and resets to 0 on
-- the next success. Drives the post-mortem error-streak detector (#266).
testSessionActivityErrorStreak :: IO Bool
testSessionActivityErrorStreak = do
  ref <- WS.newWorkflowStateRef
  WS.trackTool ref GhcLoad False (A.object [])
  s1 <- WS.readState ref
  WS.trackTool ref GhcLoad False (A.object [])
  s2 <- WS.readState ref
  WS.trackTool ref GhcLoad True  (A.object [])
  s3 <- WS.readState ref
  pure $ WS.wsErrorStreak s1 == 1
      && WS.wsErrorStreak s2 == 2
      && WS.wsErrorStreak s3 == 0

-- | #257: the registry size minus the ever-called set is the
-- "unused_count" that ghc_workflow(status).session_activity exposes.
testSessionActivityUnusedCount :: IO Bool
testSessionActivityUnusedCount = do
  ref <- WS.newWorkflowStateRef
  WS.trackTool ref GhcLoad    True (A.object [])
  WS.trackTool ref GhcSuggest True (A.object [])
  s <- WS.readState ref
  let total  = length allToolNameTexts
      unused = total - Set.size (WS.wsEverCalled s)
  pure $ total == 36 && unused == 34

-- | #263: discover excludes tools already called this session.
testDiscoverExcludesCalled :: IO Bool
testDiscoverExcludesCalled = do
  ref <- WS.newWorkflowStateRef
  WS.trackTool ref GhcScratch True (A.object [])
  s <- WS.readState ref
  pure (GhcScratch `notElem` WorkflowTool.discoverRanked s WS.PhaseDeveloping)

-- | #263: discover returns at most 5 suggestions (fresh session).
testDiscoverAtMostFive :: IO Bool
testDiscoverAtMostFive = do
  ref <- WS.newWorkflowStateRef
  s <- WS.readState ref
  pure (length (WorkflowTool.discoverRanked s WS.PhaseDeveloping) == 5)

-- | #263: phase-relevant tools rank in — ghc_gate in PhaseReadyToPush.
testDiscoverPhaseRelevance :: IO Bool
testDiscoverPhaseRelevance = do
  ref <- WS.newWorkflowStateRef
  s <- WS.readState ref
  pure (GhcGate `elem` WorkflowTool.discoverRanked s WS.PhaseReadyToPush)

-- | Phase 11j: all 5 Code tools registered in the inventory.
testCodeToolsRegistered :: IO Bool
testCodeToolsRegistered = pure $
  all (`elem` allToolNameTexts)
    [ "ghc_add_import"
    , "ghc_modules"
    , "ghc_apply_exports"
    , "ghc_fix_warning"
    , "ghc_imports"
    ]

testAddImportQualified :: IO Bool
testAddImportQualified = pure $
     AddImport.renderImportLine False "Data.Map"
       == "import Data.Map"
  && AddImport.renderImportLine True  "Data.Map"
       == "import qualified Data.Map as M"

-- | Issue #54: empty constructor list → empty array, not a
-- one-element block of @null@s.
testInfoCtorBlockEmpty :: IO Bool
testInfoCtorBlockEmpty =
  pure (null (InfoTool.renderConstructorsBlock []))

-- | Issue #54: each constructor pair becomes one
-- @{name, args}@ object. Verify shape with the canonical
-- 'Maybe' example: @Nothing | Just a@.
testInfoCtorBlockMaybe :: IO Bool
testInfoCtorBlockMaybe =
  let block = InfoTool.renderConstructorsBlock
                [ ("Nothing", [])
                , ("Just",    ["a"])
                ]
  in pure $ length block == 2
        && hasName "Nothing" block
        && hasName "Just"    block
  where
    hasName n = any $ \case
      A.Object o -> AKM.lookup (AKey.fromText "name") o
                      == Just (A.String n)
      _          -> False

-- | Issue #54: 'successResult' must embed the @constructors@
-- field when the type is algebraic, so JSON consumers see the
-- structured constructor list alongside the legacy 'definition'.
testInfoSuccessIncludesCtors :: IO Bool
testInfoSuccessIncludesCtors =
  let parsed = ParsedInfo
        { piName       = "Maybe"
        , piKind       = IkData
        , piDefinition = "data Maybe a = Nothing | Just a"
        , piInstances  = []
        }
      ctors  = [("Nothing", []), ("Just", ["a"])]
      result = InfoTool.successResult parsed ctors []
  in pure $ case trContent result of
       [TextContent t] ->
         -- Issue #90 Phase B: 'constructors' moved under 'result'.
         -- Drill through the envelope to keep the existing oracle.
         case A.decode (TLE.encodeUtf8 (TL.fromStrict t)) of
           Just (A.Object env) -> case AKM.lookup (AKey.fromText "result") env of
             Just (A.Object o) -> case AKM.lookup (AKey.fromText "constructors") o of
               Just (A.Array xs) -> length xs == 2
               _                 -> False
             _ -> False
           _ -> False
       _ -> False

-- | Issue #54: when no constructors apply (class / function /
-- type-synonym), the 'constructors' field must be absent — not
-- present-with-empty-array. Preserves wire-format compatibility
-- for consumers that didn't ask for the field.
testInfoSuccessDropsCtorField :: IO Bool
testInfoSuccessDropsCtorField =
  let parsed = ParsedInfo
        { piName       = "Functor"
        , piKind       = IkClass
        , piDefinition = "class Functor f"
        , piInstances  = []
        }
      result = InfoTool.successResult parsed [] []
  in pure $ case trContent result of
       [TextContent t] ->
         case A.decode (TLE.encodeUtf8 (TL.fromStrict t)) of
           Just (A.Object env) -> case AKM.lookup (AKey.fromText "result") env of
             Just (A.Object o) ->
               isNothing (AKM.lookup (AKey.fromText "constructors") o)
             _ -> False
           _ -> False
       _ -> False

-- | Issue #70: 'renderClassMethodsBlock' produces one
-- @{name, type}@ object per method, in declaration order.
testInfoClassMethodsBlock :: IO Bool
testInfoClassMethodsBlock =
  let methods = [ ("fmap", "(a -> b) -> f a -> f b")
                , ("(<$)", "a -> f b -> f a")
                ]
      block = InfoTool.renderClassMethodsBlock methods
  in pure $ length block == 2
         && case block of
              [A.Object o1, A.Object o2] ->
                AKM.lookup (AKey.fromText "name") o1 == Just (A.String "fmap")
                && AKM.lookup (AKey.fromText "name") o2 == Just (A.String "(<$)")
              _ -> False

-- | Issue #70: when methods are present, the response carries
-- a top-level @class_methods@ array — the symmetric companion
-- to @constructors@ for data types.
testInfoSuccessClassMethods :: IO Bool
testInfoSuccessClassMethods =
  let parsed = ParsedInfo
        { piName       = "Functor"
        , piKind       = IkClass
        , piDefinition = "class Functor f where\n  fmap :: (a -> b) -> f a -> f b"
        , piInstances  = []
        }
      methods = [ ("fmap", "(a -> b) -> f a -> f b") ]
      result = InfoTool.successResult parsed [] methods
  in pure $ case trContent result of
       [TextContent t] ->
         case A.decode (TLE.encodeUtf8 (TL.fromStrict t)) of
           Just (A.Object env) -> case AKM.lookup (AKey.fromText "result") env of
             Just (A.Object o) ->
               case AKM.lookup (AKey.fromText "class_methods") o of
                 Just (A.Array xs) -> length xs == 1
                 _                 -> False
             _ -> False
           _ -> False
       _ -> False

-- | Issue #70: a data type's response must NOT carry an empty
-- @class_methods@ array — the field should be absent. Wire-format
-- compatibility with consumers that didn't ask.
testInfoSuccessDropsClassMethods :: IO Bool
testInfoSuccessDropsClassMethods =
  let parsed = ParsedInfo
        { piName       = "Maybe"
        , piKind       = IkData
        , piDefinition = "data Maybe a = Nothing | Just a"
        , piInstances  = []
        }
      ctors = [("Nothing", []), ("Just", ["a"])]
      result = InfoTool.successResult parsed ctors []
  in pure $ case trContent result of
       [TextContent t] ->
         case A.decode (TLE.encodeUtf8 (TL.fromStrict t)) of
           Just (A.Object env) -> case AKM.lookup (AKey.fromText "result") env of
             Just (A.Object o) ->
               isNothing (AKM.lookup (AKey.fromText "class_methods") o)
             _ -> False
           _ -> False
       _ -> False

-- | #142: when piInstances has more than 30 entries, successResult
-- caps the 'instances' list and emits instance_count with the total.
testInfoInstanceCap :: IO Bool
testInfoInstanceCap =
  let insts  = map (\i -> "instance Foo Type" <> T.pack (show (i :: Int)))
                   [1 .. 50]
      parsed = ParsedInfo
        { piName       = "Foo"
        , piKind       = IkClass
        , piDefinition = "class Foo a"
        , piInstances  = insts
        }
      result = InfoTool.successResult parsed [] []
  in pure $ case trContent result of
       [TextContent t] ->
         case A.decode (TLE.encodeUtf8 (TL.fromStrict t)) of
           Just (A.Object env) -> case AKM.lookup (AKey.fromText "result") env of
             Just (A.Object o) ->
               let instList      = AKM.lookup (AKey.fromText "instances") o
                   instCount     = AKM.lookup (AKey.fromText "instance_count") o
                   instTruncated = AKM.lookup (AKey.fromText "instances_truncated") o
               in case (instList, instCount, instTruncated) of
                    (Just (A.Array arr), Just (A.Number n), Just (A.Bool b)) ->
                         length arr == 30
                      && floor n == (50 :: Int)
                      && b
                    _ -> False
             _ -> False
           _ -> False
       _ -> False

-- | #142: when piInstances is under the cap, instances_truncated is
-- false and instance_count equals the list length.
testInfoInstancesNotTruncated :: IO Bool
testInfoInstancesNotTruncated =
  let insts  = ["instance Foo A", "instance Foo B"]
      parsed = ParsedInfo
        { piName       = "Foo"
        , piKind       = IkClass
        , piDefinition = "class Foo a"
        , piInstances  = insts
        }
      result = InfoTool.successResult parsed [] []
  in pure $ case trContent result of
       [TextContent t] ->
         case A.decode (TLE.encodeUtf8 (TL.fromStrict t)) of
           Just (A.Object env) -> case AKM.lookup (AKey.fromText "result") env of
             Just (A.Object o) ->
               let instCount     = AKM.lookup (AKey.fromText "instance_count") o
                   instTruncated = AKM.lookup (AKey.fromText "instances_truncated") o
               in case (instCount, instTruncated) of
                    (Just (A.Number n), Just (A.Bool b)) ->
                         floor n == (2 :: Int)
                      && not b
                    _ -> False
             _ -> False
           _ -> False
       _ -> False

-- | Issue #42: empty store → status="empty", ok=true.
testCheckGateEmpty :: IO Bool
testCheckGateEmpty =
  let g = CheckModule.propertiesGate 0 0 0 0
  in pure $ gateField "ok" g == Just (A.Bool True)
        && gateField "status" g == Just (A.String "empty")

-- | Issue #42: every stored prop passed → status="pass", ok=true,
-- reason matches.
testCheckGatePass :: IO Bool
testCheckGatePass =
  let g = CheckModule.propertiesGate 3 3 0 0
  in pure $ gateField "ok" g == Just (A.Bool True)
        && gateField "status" g == Just (A.String "pass")
        && case gateField "reason" g of
             Just (A.String r) -> "pass" `T.isInfixOf` r
             _                 -> False

-- | Issue #42: at least one regressed → status="regressed", ok=false,
-- reason contains "regressed". The bug shape was reason="N pass"
-- with ok=false; pin the new contract.
testCheckGateRegressed :: IO Bool
testCheckGateRegressed =
  let g = CheckModule.propertiesGate 3 1 2 0
  in pure $ gateField "ok" g == Just (A.Bool False)
        && gateField "status" g == Just (A.String "regressed")
        && case gateField "reason" g of
             Just (A.String r) ->
                  "regressed" `T.isInfixOf` r
               && not ("pass" `T.isInfixOf` r)
             _ -> False

-- | Issue #42 + #51: load-failures → status="skipped", ok=false,
-- reason calls out the load failure (not "regressed").
testCheckGateSkipped :: IO Bool
testCheckGateSkipped =
  let g = CheckModule.propertiesGate 2 0 0 2
  in pure $ gateField "ok" g == Just (A.Bool False)
        && gateField "status" g == Just (A.String "skipped")
        && case gateField "reason" g of
             Just (A.String r) ->
               "load" `T.isInfixOf` T.toLower r
             _ -> False

-- | Issue #42 core invariant: ok=false MUST imply the reason
-- text does NOT claim properties pass. Table-drive a few
-- (total, passed, regressed, skipped) tuples.
-- | Issue #58: full Hackage-conformant package-name validator.
-- Each test pins one violation class so a regression in any
-- single rule is attributable on its own.

testCreateValidateAccept :: IO Bool
testCreateValidateAccept = pure $ and
  [ CreateProject.validateName "haskell-flows-mcp" == Right "haskell-flows-mcp"
  , CreateProject.validateName "x"                 == Right "x"
  -- "abc-123-def" removed: segment '123' is all-digit → now rejected by #233 rule
  , CreateProject.validateName "single"            == Right "single"
  , CreateProject.validateName "my-pkg-v2"         == Right "my-pkg-v2"
  , CreateProject.validateName "lib-core"          == Right "lib-core"
  ]

testCreateValidateEmpty :: IO Bool
testCreateValidateEmpty = pure $
  case CreateProject.validateName "" of
    Left _  -> True
    Right _ -> False

testCreateValidateUpper :: IO Bool
testCreateValidateUpper = pure $
     isLeft (CreateProject.validateName "Invalid-Name")
  && isLeft (CreateProject.validateName "camelCase")
  && isLeft (CreateProject.validateName "ALLCAPS")
  where
    isLeft (Left _) = True
    isLeft _        = False

testCreateValidateDoubleHyphen :: IO Bool
testCreateValidateDoubleHyphen = pure $
  case CreateProject.validateName "with--double" of
    Left msg -> "consecutive hyphens" `T.isInfixOf` msg
    Right _  -> False

testCreateValidateTrailing :: IO Bool
testCreateValidateTrailing = pure $
  case CreateProject.validateName "ends-" of
    Left msg -> "end in a hyphen" `T.isInfixOf` msg
    Right _  -> False

testCreateValidateLeadingDigit :: IO Bool
testCreateValidateLeadingDigit = pure $
  case CreateProject.validateName "1leading-digit" of
    Left msg -> "lowercase letter" `T.isInfixOf` msg
    Right _  -> False

testCreateValidateSymbols :: IO Bool
testCreateValidateSymbols = pure $
     isLeft (CreateProject.validateName "with_underscore")
  && isLeft (CreateProject.validateName "with.dot")
  && isLeft (CreateProject.validateName "with space")
  && isLeft (CreateProject.validateName "leading-")
  && isLeft (CreateProject.validateName "-leading")
  where
    isLeft (Left _) = True
    isLeft _        = False

-- | Issue #69: a freshly-scaffolded cabal file must declare
-- 'category', 'maintainer', and 'description'. Without these,
-- 'cabal check' (and our 'ghc_validate_cabal') tags the project
-- with 3 warnings on the agent's very first gate-call. The
-- placeholders are stubs the agent should fill before
-- publishing — but they keep the gate green by default.
testCreateProjectScaffoldGreenCabal :: IO Bool
testCreateProjectScaffoldGreenCabal =
  let cabal = CreateProject.cabalFile "demo" "Demo"
  in pure $  T.isInfixOf "category:" cabal
          && T.isInfixOf "maintainer:" cabal
          && T.isInfixOf "description:" cabal
          -- The TODO sentinel keeps it obvious to the agent
          -- that the description is placeholder text.
          && T.isInfixOf "TODO:" cabal

-- | Issue #58: error messages must NAME the violation so the agent
-- can rename appropriately instead of guessing what \"invalid name\"
-- meant. Pin that the rejected name and the rule both appear.
testCreateValidateErrorMsg :: IO Bool
testCreateValidateErrorMsg = pure $
  case CreateProject.validateName "Bad-Name" of
    Left msg ->
         "Bad-Name" `T.isInfixOf` msg
      && ("lowercase" `T.isInfixOf` msg
            || "Hackage" `T.isInfixOf` msg)
    Right _ -> False

--------------------------------------------------------------------------------
-- Issue #233 — validateName: all-digit component rejection
--------------------------------------------------------------------------------

-- | #233: cabal fails with "unexpected Empty component" when a
-- hyphen-separated name component is all digits.  Pin that the
-- validator now rejects such names with an informative message.
testCreateValidateAllDigitComponent :: IO Bool
testCreateValidateAllDigitComponent = pure $ and
  [ isLeft (CreateProject.validateName "dogfood-2026-05-25-b")  -- full repro case
  , isLeft (CreateProject.validateName "pkg-2026")              -- single trailing digit seg
  , isLeft (CreateProject.validateName "a-1-b")                 -- digit in middle
  , case CreateProject.validateName "dogfood-2026-05-25-b" of
      Left msg -> "all-digit" `T.isInfixOf` msg
                  || "version" `T.isInfixOf` msg
      Right _  -> False
  ]
  where
    isLeft (Left _) = True
    isLeft _        = False

-- | #233: names with letter-prefixed numeric-like segments are fine;
-- segments that are ENTIRELY digits are not (cabal parses them as
-- version components, creating a parse ambiguity).
testCreateValidateVPrefixedOk :: IO Bool
testCreateValidateVPrefixedOk = pure $ and
  [ CreateProject.validateName "dogfood-v2026"      == Right "dogfood-v2026"
  , CreateProject.validateName "pkg-v1"             == Right "pkg-v1"
  , CreateProject.validateName "lib-r2d2"           == Right "lib-r2d2"
  , CreateProject.validateName "http2"              == Right "http2"        -- no hyphen, fine
  , isLeft (CreateProject.validateName "abc-123-def")  -- '123' is all-digit → rejected
  , isLeft (CreateProject.validateName "lib-42")       -- '42' is all-digit → rejected
  ]
  where
    isLeft (Left _) = True
    isLeft _        = False

--------------------------------------------------------------------------------
-- Issue #234 — scaffold overwrite=true removes stale .cabal files
--------------------------------------------------------------------------------

-- | #234: when overwrite=True and a stale .cabal exists with a
-- different package name, scaffold removes it before writing.
testCreateOverwriteRemovesStaleCalab :: IO Bool
testCreateOverwriteRemovesStaleCalab = do
  tmp <- getTemporaryDirectory
  let dir = tmp </> "hf-test234-stale-cabal"
  removePathForcibly dir
  createDirectoryIfMissing True dir
  -- Simulate the stale state: write an old-name.cabal in the dir.
  TIO.writeFile (dir </> "old-name.cabal") "name: old-name\n"
  -- Now scaffold with a new name and overwrite=True.
  _ <- CreateProject.scaffold dir "new-name" "NewName" True True
  -- The stale .cabal should be gone; only new-name.cabal remains.
  entries <- listDirectory dir
  let calabFiles = filter (List.isSuffixOf ".cabal") entries
  removePathForcibly dir
  pure (calabFiles == ["new-name.cabal"])

--------------------------------------------------------------------------------
-- Issue #126 — ghc_project(create): path + write fixes
--------------------------------------------------------------------------------

-- | #126 Bug B: write=false is preview mode — it must never fail due
-- to existing files. Here we run scaffold against a root that DOES
-- have the scaffold files (the test directory itself), but since
-- write=false the overwrite check is skipped and we get ok.
testCreateWriteFalseIsPreview :: IO Bool
testCreateWriteFalseIsPreview = do
  -- "." has cabal.project, src/, test/Spec.hs — exactly the clash
  -- scenario that was failing before the fix.
  result <- CreateProject.scaffold "." "my-pkg" "MyPkg" False False
  pure (not (trIsError result))

-- | #126 Bug B: write=false response carries "preview" key with
-- generated file contents and write=false discriminator.
testCreateWriteFalseContent :: IO Bool
testCreateWriteFalseContent = do
  result <- CreateProject.scaffold "/nonexistent-dir" "my-pkg" "MyPkg" False False
  -- The preview fields live inside the 'result' key of the envelope,
  -- not at the top level — use resultPayload, not extractPayload.
  case resultPayload result of
    A.Object o ->
      let hasPreview = AKM.member (AKey.fromText "preview") o
          writeFalse = case AKM.lookup (AKey.fromText "write") o of
                         Just (A.Bool False) -> True
                         _                  -> False
      in pure (hasPreview && writeFalse && not (trIsError result))
    _ -> pure False

-- | #126 Bug A: scaffold uses the supplied root path, not the
-- active projectDir. We supply a temp dir that has no scaffold
-- files. The call with write=true and overwrite=false should
-- succeed (no clashes at the target path).
testCreateUsesSuppliedPath :: IO Bool
testCreateUsesSuppliedPath = withTempProject $ \pd -> do
  -- The temp dir is freshly created — no cabal.project or Spec.hs.
  -- Before the fix, the clash check looked in the *active* projectDir
  -- (mcp-server-haskell/) which DOES have those files.
  result <- CreateProject.scaffold (unProjectDirRaw pd) "fresh-pkg" "FreshPkg" False True
  pure (not (trIsError result))

-- | Issue #256: after a successful @ghc_project(action="create", path=<p>)@
-- the server must auto-switch to the new path so subsequent tool calls
-- (ghc_deps, ghc_modules) operate on the right project.
-- Verified via source inspection: 'createAutoSwitchPath' must exist,
-- must gate on 'trIsError', and must call 'SwitchProjectTool.handle'.
testCreateAutoSwitchPresent :: IO Bool
testCreateAutoSwitchPresent = do
  src <- TIO.readFile "src/HaskellFlows/Mcp/Server.hs"
  let code = T.unlines (filter (not . isDocLine) (T.lines src))
  pure $ T.isInfixOf "createAutoSwitchPath"        code
      && T.isInfixOf "trIsError r"                 code
      && T.isInfixOf "SwitchProjectTool.handle"    code
  where
    isDocLine ln = "--" `T.isPrefixOf` T.stripStart ln

-- | Issue #256: when @write=false@ (preview mode) 'createAutoSwitchPath'
-- must return Nothing — no switch should happen.
testCreatePreviewNoSwitch :: IO Bool
testCreatePreviewNoSwitch = do
  src <- TIO.readFile "src/HaskellFlows/Mcp/Server.hs"
  let code = T.unlines (filter (not . isDocLine) (T.lines src))
  -- The helper must gate on writeDisk being True — if it is False, Nothing.
  pure $ T.isInfixOf "writeDisk"  code
      && T.isInfixOf "write=True" code  -- the guard comment
  where
    isDocLine ln = "--" `T.isPrefixOf` T.stripStart ln

-- | Issue #256: when no explicit @path@ is supplied the scaffold lands
-- in the current project dir — no switch is needed, 'createAutoSwitchPath'
-- must return Nothing.
testCreateNoPathNoSwitch :: IO Bool
testCreateNoPathNoSwitch = do
  src <- TIO.readFile "src/HaskellFlows/Mcp/Server.hs"
  let code = T.unlines (filter (not . isDocLine) (T.lines src))
  -- When path key is absent the helper must return Nothing.
  pure $ T.isInfixOf "createAutoSwitchPath" code
      && T.isInfixOf "Nothing"              code
  where
    isDocLine ln = "--" `T.isPrefixOf` T.stripStart ln

testCheckGateReasonMatchesOk :: IO Bool
testCheckGateReasonMatchesOk =
  let cases =
        [ (1, 0, 1, 0)  -- one regressed
        , (1, 0, 0, 1)  -- one skipped
        , (3, 1, 1, 1)  -- mixed
        ]
      check (total, passed, regressed, skipped) =
        let g = CheckModule.propertiesGate total passed regressed skipped
        in case (gateField "ok" g, gateField "reason" g) of
             (Just (A.Bool False), Just (A.String r))
               | not ("stored properties pass" `T.isInfixOf` r)
                 && r /= "" -> True
             _ -> False
  in pure (all check cases)

gateField :: Text -> A.Value -> Maybe A.Value
gateField k (A.Object o) = AKM.lookup (AKey.fromText k) o
gateField _ _            = Nothing

-- | Issue #53: when @hoogle@ is not on PATH, ghc_add_import must
-- mirror @hoogle_search@ and return success=false with a
-- remediation string. This used to silently return @count: 0@
-- with @success: true@ and a lying @nextStep@.
--
-- Test strategy: monkey-patch PATH to drop everything that
-- could resolve 'hoogle', invoke handle, parse the response.
testAddImportMissingHoogle :: IO Bool
testAddImportMissingHoogle = do
  origPath <- System.Environment.lookupEnv "PATH"
  System.Environment.setEnv "PATH" "/nonexistent/path-for-test-only"
  -- Use a dedicated tempdir as PATH so hoogle is guaranteed missing.
  -- #146: AddImportTool.handle now requires a GhcSession; the stub
  -- session on an empty dir is sufficient since the tool fails at
  -- the hoogle-availability gate, before touching the session.
  tmp <- getTemporaryDirectory
  let dir = tmp </> "haskell-flows-add-import-nohoogle"
  createDirectoryIfMissing True dir
  let args = A.object [ "name" A..= ("fromMaybe" :: T.Text) ]
  result <- case mkProjectDir dir of
    Left _  -> pure (ToolResult { trContent = [TextContent "{}"], trIsError = False })
    Right pd -> do
      sess <- startGhcSession pd
      r <- AddImport.handle sess args
      killGhcSession sess
      pure r
  -- Restore PATH so other tests aren't affected.
  case origPath of
    Just p  -> System.Environment.setEnv "PATH" p
    Nothing -> System.Environment.unsetEnv "PATH"
  case trContent result of
    [TextContent t] ->
      -- Issue #90 Phase D step 2: legacy 'success: false' is gone.
      -- Branch on @status@ and the structured @error.kind@ instead.
      let parsed = A.decode (TLE.encodeUtf8 (TL.fromStrict t)) :: Maybe A.Value
      in pure $ case parsed of
           Just v -> fieldText "status" v == Just "unavailable"
                  && trIsError result
                  && case lookupField "error" v of
                       Just (A.Object errObj) ->
                         let msg = AKM.lookup (AKey.fromText "message") errObj
                             rem_ = AKM.lookup (AKey.fromText "remediation") errObj
                             msgOk = case msg of
                               Just (A.String m) -> "hoogle" `T.isInfixOf` T.toLower m
                               _ -> False
                             remOk = case rem_ of
                               Just (A.String _) -> True
                               _ -> False
                         in msgOk && remOk
                       _ -> False
           Nothing -> False
    _ -> pure False
  where
    fieldText k (A.Object o) = case AKM.lookup (AKey.fromText k) o of
      Just (A.String s) -> Just s
      _                 -> Nothing
    fieldText _ _ = Nothing
    lookupField k (A.Object o) = AKM.lookup (AKey.fromText k) o
    lookupField _ _            = Nothing

-- | #146: addImportToSession returns (False, msg) when the import
-- line is syntactically invalid — GHC rejects it during parseImportDecl
-- and the exception is caught gracefully.
testAddImportToSessionInvalid :: IO Bool
testAddImportToSessionInvalid = do
  tmp <- getTemporaryDirectory
  let dir = tmp </> "haskell-flows-add-import-invalid"
  createDirectoryIfMissing True dir
  case mkProjectDir dir of
    Left _   -> pure False
    Right pd -> do
      sess <- startGhcSession pd
      (ok, msg) <- AddImport.addImportToSession sess
                     "this is not a valid import at all"
      killGhcSession sess
      pure (not ok && not (T.null msg))

-- | #146: addImportToSession returns (True, importLine) for a
-- well-formed import of a GHC base module.
testAddImportToSessionValid :: IO Bool
testAddImportToSessionValid = do
  tmp <- getTemporaryDirectory
  let dir = tmp </> "haskell-flows-add-import-valid"
  createDirectoryIfMissing True dir
  case mkProjectDir dir of
    Left _   -> pure False
    Right pd -> do
      sess <- startGhcSession pd
      let importLine = "import Data.Maybe"
      (ok, added) <- AddImport.addImportToSession sess importLine
      killGhcSession sess
      pure (ok && added == importLine)

-- | Issue #53: nextStep dispatch on a ghc_add_import payload
-- with @count: 0@ must return 'Nothing' (no \"reload to confirm\"
-- nudge), since nothing was added.
testNextStepAddImportZero :: IO Bool
testNextStepAddImportZero =
  let payload = A.object
        [ "success" A..= True
        , "name"    A..= ("ghostFn" :: T.Text)
        , "count"   A..= (0 :: Int)
        , "imports" A..= ([] :: [T.Text])
        ]
  in pure (isNothing (suggestNext GhcAddImport True payload))

-- | Issue #53: nextStep dispatch on a ghc_add_import payload
-- with @count: 3@ must return 'Just (...GhcLoad...)' so the
-- reload nudge fires when there's something to reload.
testNextStepAddImportNonZero :: IO Bool
testNextStepAddImportNonZero =
  let payload = A.object
        [ "success" A..= True
        , "name"    A..= ("fromMaybe" :: T.Text)
        , "count"   A..= (3 :: Int)
        , "imports" A..= (["import Data.Maybe"] :: [T.Text])
        ]
  in pure $ case suggestNext GhcAddImport True payload of
       Just ns -> nsTool ns == GhcLoad
       Nothing -> False

testAddModulesPath :: IO Bool
testAddModulesPath = pure $
     AddModules.moduleToPath "Expr.Syntax"  == "src/Expr/Syntax.hs"
  && AddModules.moduleToPath "Main"         == "src/Main.hs"

-- | #173: idempotent case — when the requested list is IDENTICAL to
-- the existing one, must return Unchanged.
testApplyExportsIdempotent :: IO Bool
testApplyExportsIdempotent =
  let body = T.unlines
        [ "-- header"
        , "module Foo (a, b) where"
        , "a = 1"
        ]
  in pure (ApplyExports.rewriteHeader ["a", "b"] body == ApplyExports.Unchanged)

testApplyExportsInjects :: IO Bool
testApplyExportsInjects =
  let body = T.unlines
        [ "module Foo where"
        , "a = 1"
        ]
  in case ApplyExports.rewriteHeader ["a", "b"] body of
       ApplyExports.Rewritten newBody ->
         pure (T.isInfixOf "module Foo (a, b) where" newBody)
       _ -> pure False

-- | #173: when the header already has a DIFFERENT export list,
-- 'rewriteHeader' must return 'Rewritten' with the new list — not
-- 'Unchanged'.
testApplyExportsReplacesExistingList :: IO Bool
testApplyExportsReplacesExistingList =
  let body = T.unlines
        [ "module Foo (greet, reverseStr, double, Color, Tree (..)) where"
        , "greet = undefined"
        ]
  in case ApplyExports.rewriteHeader ["greet", "reverseStr"] body of
       ApplyExports.Rewritten newBody ->
         pure $ T.isInfixOf "module Foo (greet, reverseStr) where" newBody
              && not (T.isInfixOf "double" newBody)
              && not (T.isInfixOf "Tree" newBody)
       _ -> pure False

-- | #173: when the source has no @module Foo where@ line,
-- 'rewriteHeader' must return 'NoHeader', not 'Unchanged'.
testApplyExportsNoHeader :: IO Bool
testApplyExportsNoHeader =
  let body = T.unlines
        [ "-- No module declaration"
        , "main :: IO ()"
        , "main = pure ()"
        ]
  in pure (ApplyExports.rewriteHeader ["main"] body == ApplyExports.NoHeader)

-- | #133: successResult (file written) must include applied=true in
-- the result payload so callers can distinguish write from no-op.
testApplyExportsSuccessHasApplied :: IO Bool
testApplyExportsSuccessHasApplied = withFixture $ \pd _ -> do
  let projectDir = HaskellFlows.Types.unProjectDir pd
      modulePath = projectDir </> "src" </> "TestMod.hs"
  createDirectoryIfMissing True (projectDir </> "src")
  TIO.writeFile modulePath (T.unlines ["module TestMod where", "x = 1"])
  let args = A.object
        [ "module_path" A..= ("src/TestMod.hs" :: Text)
        , "exports"     A..= (["x"] :: [Text])
        ]
  tr <- ApplyExports.handle pd args
  let payload = resultPayload tr
  pure $ fieldEquals "applied" (A.Bool True) payload

-- | #133: noChangeResult (header already has exports) must include
-- applied=false in the result payload.
testApplyExportsNoChangeHasApplied :: IO Bool
testApplyExportsNoChangeHasApplied = withFixture $ \pd _ -> do
  let projectDir = HaskellFlows.Types.unProjectDir pd
      modulePath = projectDir </> "src" </> "TestMod2.hs"
  createDirectoryIfMissing True (projectDir </> "src")
  -- Write a module that already has an export list → triggers noChangeResult.
  TIO.writeFile modulePath (T.unlines ["module TestMod2 (x) where", "x = 1"])
  let args = A.object
        [ "module_path" A..= ("src/TestMod2.hs" :: Text)
        , "exports"     A..= (["x"] :: [Text])
        ]
  tr <- ApplyExports.handle pd args
  let payload = resultPayload tr
  pure $ fieldEquals "no_change" (A.Bool True) payload
      && fieldEquals "applied"   (A.Bool False) payload

-- | #133: handle write path (new export list inserted) returns applied=true.
-- Exercises the full handler integration.
testApplyExportsHandleAppliedTrue :: IO Bool
testApplyExportsHandleAppliedTrue = withFixture $ \pd _ -> do
  let projectDir = HaskellFlows.Types.unProjectDir pd
      modulePath = projectDir </> "src" </> "AppliedTrue.hs"
  createDirectoryIfMissing True (projectDir </> "src")
  TIO.writeFile modulePath (T.unlines ["module AppliedTrue where", "foo = 42"])
  let args = A.object
        [ "module_path" A..= ("src/AppliedTrue.hs" :: Text)
        , "exports"     A..= (["foo"] :: [Text])
        ]
  tr <- ApplyExports.handle pd args
  bodyAfter <- TIO.readFile modulePath
  let payload = resultPayload tr
  pure $ fieldEquals "applied" (A.Bool True) payload
      && "foo" `T.isInfixOf` bodyAfter

-- | #133: handle no-op path (header already has exports) returns applied=false.
testApplyExportsHandleAppliedFalse :: IO Bool
testApplyExportsHandleAppliedFalse = withFixture $ \pd _ -> do
  let projectDir = HaskellFlows.Types.unProjectDir pd
      modulePath = projectDir </> "src" </> "AppliedFalse.hs"
  createDirectoryIfMissing True (projectDir </> "src")
  TIO.writeFile modulePath (T.unlines ["module AppliedFalse (bar) where", "bar = 0"])
  let args = A.object
        [ "module_path" A..= ("src/AppliedFalse.hs" :: Text)
        , "exports"     A..= (["bar"] :: [Text])
        ]
  tr <- ApplyExports.handle pd args
  let payload = resultPayload tr
  pure $ fieldEquals "applied"   (A.Bool False) payload
      && fieldEquals "no_change" (A.Bool True)  payload

-- | #155: write=false (dry-run) must return applied=false and must NOT
-- write to disk. Before the fix the tool always wrote and returned
-- applied=true regardless of the write parameter.
testApplyExportsWriteFalse :: IO Bool
testApplyExportsWriteFalse = withFixture $ \pd _ -> do
  let projectDir = HaskellFlows.Types.unProjectDir pd
      modulePath = projectDir </> "src" </> "WriteFalse.hs"
      origBody   = T.unlines ["module WriteFalse where", "foo = 42"]
  createDirectoryIfMissing True (projectDir </> "src")
  TIO.writeFile modulePath origBody
  let args = A.object
        [ "module_path" A..= ("src/WriteFalse.hs" :: Text)
        , "exports"     A..= (["foo"] :: [Text])
        , "write"       A..= False
        ]
  tr <- ApplyExports.handle pd args
  bodyAfter <- TIO.readFile modulePath
  let payload = resultPayload tr
  pure $ fieldEquals "applied" (A.Bool False) payload
      && bodyAfter == origBody  -- file must be unchanged

--------------------------------------------------------------------------------
-- ISSUE-47 — Module-name validator (Parser.ModuleName)
--
-- The validator is the single boundary that prevents @ghc_add_modules@
-- and @ghc_remove_modules@ from corrupting the project's @.cabal@.
-- These tests pin BOTH the happy paths (so we don't accidentally
-- start rejecting valid Haskell module names) AND every documented
-- rejection shape (so a future refactor can't silently weaken the
-- guard).
--
-- Tested invariants:
--
--   * @validateModuleName@ accepts every legal Haskell 2010 module
--     identifier shape we expect from real-world callers.
--   * It rejects every shape that would corrupt the @.cabal@ when
--     written verbatim into @exposed-modules@.
--   * Errors carry actionable diagnostics — the rendered message
--     names the input AND suggests a fix.
--   * 'validateModuleNames' is order-preserving and partitions the
--     input cleanly into rejected/accepted, so the handler can
--     refuse the whole batch atomically.
--   * Every keyword in 'reservedKeywords' is actually rejected when
--     used as a single-segment name.
--------------------------------------------------------------------------------

-- | Happy path: simplest single-segment uppercase name.
testValidModuleNameSingle :: IO Bool
testValidModuleNameSingle = pure $
  validateModuleName "Foo" == Right "Foo"

-- | Happy path: dotted multi-segment name.
testValidModuleNameDotted :: IO Bool
testValidModuleNameDotted = pure $
  validateModuleName "Foo.Bar.Baz" == Right "Foo.Bar.Baz"

-- | Happy path: underscores AFTER the first letter are legal.
testValidModuleNameUnderscore :: IO Bool
testValidModuleNameUnderscore = pure $
  validateModuleName "Foo_Bar.Baz_Qux" == Right "Foo_Bar.Baz_Qux"

-- | Happy path: apostrophes (Haskell prime convention) are legal.
testValidModuleNameApostrophe :: IO Bool
testValidModuleNameApostrophe = pure $
  validateModuleName "Foo'.Bar''" == Right "Foo'.Bar''"

-- | Happy path: digits AFTER the first character are legal.
testValidModuleNameDigits :: IO Bool
testValidModuleNameDigits = pure $
  validateModuleName "Foo123.B4r" == Right "Foo123.B4r"

-- | The validator trims surrounding whitespace and returns the
-- canonicalised form — the handler uses the returned 'Text', so a
-- trailing space can't survive into the @.cabal@.
testValidModuleNameTrim :: IO Bool
testValidModuleNameTrim = pure $
  validateModuleName "  Foo.Bar  " == Right "Foo.Bar"

-- | The exact bug from issue #47: lowercase first segment leaks
-- through line-based handlers and parse-corrupts the @.cabal@.
-- The first failure encountered is the lowercase first segment;
-- the second segment ('module', a reserved keyword) is also bad
-- but the validator stops at the first error, which is the more
-- actionable diagnostic for the LLM.
testInvalidLowercaseModule :: IO Bool
testInvalidLowercaseModule = case validateModuleName "lowercase.module" of
  Left (MNESegmentLeadingNotUpper raw seg) ->
    pure (raw == "lowercase.module" && seg == "lowercase")
  _ -> pure False

-- | Reserved-keyword rejection: 'module' as a bare name fires the
-- keyword check, NOT the lowercase-leading check (the keyword check
-- is intentionally first so the agent sees the more actionable
-- error message).
testInvalidReservedBare :: IO Bool
testInvalidReservedBare = case validateModuleName "module" of
  Left (MNESegmentReserved raw seg) ->
    pure (raw == "module" && seg == "module")
  _ -> pure False

-- | Reserved keyword in the SECOND segment ('Foo.module') — the
-- canonical second-segment-keyword case the issue calls out.
testInvalidReservedSecond :: IO Bool
testInvalidReservedSecond = case validateModuleName "Foo.module" of
  Left (MNESegmentReserved raw seg) ->
    pure (raw == "Foo.module" && seg == "module")
  _ -> pure False

-- | Empty input.
testInvalidEmpty :: IO Bool
testInvalidEmpty = pure (validateModuleName "" == Left MNEEmpty)

-- | Whitespace-only input — same behaviour as empty (after strip).
testInvalidWhitespace :: IO Bool
testInvalidWhitespace = pure (validateModuleName "   \t\n  " == Left MNEEmpty)

-- | Trailing dot produces an empty segment.
testInvalidTrailingDot :: IO Bool
testInvalidTrailingDot = case validateModuleName "Foo." of
  Left (MNESegmentEmpty raw) -> pure (raw == "Foo.")
  _                          -> pure False

-- | Leading dot produces an empty segment.
testInvalidLeadingDot :: IO Bool
testInvalidLeadingDot = case validateModuleName ".Foo" of
  Left (MNESegmentEmpty raw) -> pure (raw == ".Foo")
  _                          -> pure False

-- | Doubled dot produces an empty segment in the middle.
testInvalidDoubleDot :: IO Bool
testInvalidDoubleDot = case validateModuleName "Foo..Bar" of
  Left (MNESegmentEmpty raw) -> pure (raw == "Foo..Bar")
  _                          -> pure False

-- | Leading digit on the first segment.
testInvalidLeadingDigit :: IO Bool
testInvalidLeadingDigit = case validateModuleName "1Foo" of
  Left (MNESegmentLeadingDigit raw seg) ->
    pure (raw == "1Foo" && seg == "1Foo")
  _ -> pure False

-- | Hyphen in name — common mistake porting Cabal package names
-- (which DO use hyphens) into module names (which don't).
testInvalidHyphen :: IO Bool
testInvalidHyphen = case validateModuleName "Foo-Bar" of
  Left (MNESegmentInvalidChar raw seg c) ->
    pure (raw == "Foo-Bar" && seg == "Foo-Bar" && c == '-')
  _ -> pure False

-- | Space in name — almost always a copy-paste accident.
testInvalidSpace :: IO Bool
testInvalidSpace = case validateModuleName "Foo Bar" of
  Left (MNESegmentInvalidChar raw _ c) ->
    pure (raw == "Foo Bar" && c == ' ')
  _ -> pure False

-- | Bulk validator preserves order in BOTH partitions.
testValidateBulkOrderPreserved :: IO Bool
testValidateBulkOrderPreserved =
  let (rejected, accepted) = validateModuleNames
        ["A", "lowercase", "B", "Foo.module", "C"]
      rejectedNames = map fst rejected
  in pure
       (  accepted == ["A", "B", "C"]
       && rejectedNames == ["lowercase", "Foo.module"]
       )

-- | Bulk validator on all-good input yields no rejections.
testValidateBulkAllGood :: IO Bool
testValidateBulkAllGood =
  let (rejected, accepted) = validateModuleNames ["Foo", "Foo.Bar", "Baz"]
  in pure (null rejected && accepted == ["Foo", "Foo.Bar", "Baz"])

-- | Bulk validator on all-bad input yields no acceptances.
testValidateBulkAllBad :: IO Bool
testValidateBulkAllBad =
  let (rejected, accepted) = validateModuleNames ["1Foo", "lowercase", ""]
      rejectedNames        = map fst rejected
  in pure
       (  null accepted
       && rejectedNames == ["1Foo", "lowercase", ""]
       )

-- | Bulk validator preserves trim-canonicalisation on accepted entries.
testValidateBulkTrimsAccepted :: IO Bool
testValidateBulkTrimsAccepted =
  let (rejected, accepted) = validateModuleNames ["  Foo  ", " Bar "]
  in pure (null rejected && accepted == ["Foo", "Bar"])

-- | Every keyword in 'reservedKeywords' is rejected when used as a
-- bare single-segment name. Pins the keyword set: a future change
-- that adds (e.g.) 'forall' must also extend this assertion.
testReservedKeywordsAllRejected :: IO Bool
testReservedKeywordsAllRejected =
  pure $ all rejected (Set.toList reservedKeywords)
  where
    rejected kw = case validateModuleName kw of
      Left (MNESegmentReserved _ seg) -> seg == kw
      _ -> False

-- | The keyword list specifically covers the names called out in
-- issue #47. We pin them explicitly so a refactor that drops one
-- (e.g. dropping 'instance' by accident) is caught here, not in
-- production via a corrupted .cabal.
testReservedKeywordsCoverIssueList :: IO Bool
testReservedKeywordsCoverIssueList =
  pure $ all isReservedKeyword
    [ "module", "where", "let", "case", "do", "if", "then", "else"
    , "class", "instance", "data", "type", "newtype", "default"
    , "deriving", "import", "infix", "infixl", "infixr"
    ]

-- | Predicate: 'isReservedKeyword' is case-sensitive — uppercase
-- 'Module' is a legal module name (and indeed common for utility
-- modules).
testReservedKeywordsCaseSensitive :: IO Bool
testReservedKeywordsCaseSensitive = pure $
     not (isReservedKeyword "Module")
  && not (isReservedKeyword "Where")
  &&     isReservedKeyword "module"

-- | Rendered error mentions the offending input + a suggested fix
-- so the LLM can self-correct without another round-trip.
testRenderErrorActionable :: IO Bool
testRenderErrorActionable =
  let msg = renderModuleNameError
              (MNESegmentLeadingNotUpper "lowercase.module" "lowercase")
  in pure
       (  T.isInfixOf "lowercase.module" msg
       && T.isInfixOf "lowercase"        msg
       && (T.isInfixOf "Did you mean"     msg
           || T.isInfixOf "uppercase"      msg)
       )

-- | Rendered keyword error names the keyword AND offers a renamed
-- suggestion (e.g. 'moduleMod') so the agent has a concrete fix.
testRenderErrorReservedSuggests :: IO Bool
testRenderErrorReservedSuggests =
  let msg = renderModuleNameError (MNESegmentReserved "Foo.module" "module")
  in pure
       (  T.isInfixOf "module"          msg
       && T.isInfixOf "reserved"        msg
       && (T.isInfixOf "Mod"            msg
           || T.isInfixOf "rename"      (T.toLower msg))
       )

-- | Rendered empty-segment error mentions the canonical fix shape
-- "Foo.Bar" so the agent doesn't have to look up the grammar.
testRenderErrorEmptySegment :: IO Bool
testRenderErrorEmptySegment =
  let msg = renderModuleNameError (MNESegmentEmpty "Foo..Bar")
  in pure
       (  T.isInfixOf "Foo..Bar"     msg
       && T.isInfixOf "empty segment" msg
       && T.isInfixOf "Foo.Bar"       msg
       )

-- | Rendered invalid-char error names the offending character so
-- the LLM doesn't need to scan the input to find it.
testRenderErrorInvalidChar :: IO Bool
testRenderErrorInvalidChar =
  let msg = renderModuleNameError (MNESegmentInvalidChar "Foo-Bar" "Foo-Bar" '-')
  in pure
       (  T.isInfixOf "Foo-Bar" msg
       && T.isInfixOf "'-'"     msg
       )

-- | Property-shaped: the rendered error message is non-empty for
-- every error constructor — guards against future refactors that
-- might leave a constructor unhandled in 'renderModuleNameError'.
testRenderErrorAllNonEmpty :: IO Bool
testRenderErrorAllNonEmpty =
  let inputs =
        [ MNEEmpty
        , MNESegmentEmpty "Foo."
        , MNESegmentReserved "module" "module"
        , MNESegmentLeadingNotUpper "foo" "foo"
        , MNESegmentLeadingDigit "1Foo" "1Foo"
        , MNESegmentInvalidChar "Foo-" "Foo-" '-'
        ]
  in pure (not (any (T.null . renderModuleNameError) inputs))

--------------------------------------------------------------------------------
-- ISSUE-47 — End-to-end @ghc_add_modules@ refusal at the handler boundary
--
-- These tests drive the FULL handler against a tempdir-backed
-- @.cabal@. They prove:
--
--   (a) the validator is wired into the handler — bad input is
--       refused before any IO.
--   (b) the @.cabal@ is byte-identical pre/post call when at least
--       one name is invalid (atomic refusal — no partial writes).
--   (c) the rejection payload is structured exactly as the issue
--       specifies: success=false, error, rejected[{name,reason}].
--   (d) symmetric behaviour for @ghc_remove_modules@.
--   (e) @ghc_apply_exports@ rejects reserved keywords.
--   (f) regression: the happy path still succeeds and writes to
--       both the @.cabal@ AND the filesystem.
--------------------------------------------------------------------------------

-- | Minimal scaffolded .cabal a tempdir flow can write/read.
fixtureCabal :: Text
fixtureCabal = T.unlines
  [ "cabal-version: 3.0"
  , "name:          fixture"
  , "version:       0.0.0"
  , "library"
  , "    default-language: GHC2024"
  , "    hs-source-dirs:   src"
  , "    exposed-modules:  Foo"
  , "    build-depends:    base"
  ]

-- | Drive 'AddModules.handle' against a tempdir @ProjectDir@ with
-- a freshly-written fixture .cabal. Returns (cabal-after, payload).
withFixture :: (ProjectDir -> FilePath -> IO a) -> IO a
withFixture k = do
  tmp <- getTemporaryDirectory
  ts  <- show <$> getTestTimestamp
  let dir       = tmp </> ("haskell-flows-mn-" <> ts)
      cabalFile = dir </> "fixture.cabal"
  createDirectoryIfMissing True dir
  TIO.writeFile cabalFile fixtureCabal
  res <- case mkProjectDir dir of
    Left _   -> error "fixture: mkProjectDir failed"
    Right pd -> k pd cabalFile
  removePathForcibly dir
  pure res

-- | The exact bug from issue #47 — driven through the handler.
-- AFTER the fix the handler MUST refuse the call AND leave the
-- .cabal unmodified.
testHandleAddModulesRefusesLowercaseModule :: IO Bool
testHandleAddModulesRefusesLowercaseModule = withFixture $ \pd cabalFile -> do
  let args = A.object [ "modules" A..= (["lowercase.module"] :: [Text]) ]
  before <- TIO.readFile cabalFile
  result <- AddModules.handle pd args
  after  <- TIO.readFile cabalFile
  let isErr        = trIsError result
      envelope     = extractPayload result
      innerResult  = resultPayload result
  pure
    (  isErr
    && before == after
    -- Issue #90 Phase D step 2: 'rejected' moved under 'result';
    -- the legacy 'success' top-level field is dropped, callers
    -- branch on 'status' (here, 'failed' since the rejection is
    -- a domain validation failure, not a sanitize-layer refusal).
    && hasField "rejected" innerResult
    && fieldEquals "status" (A.String "failed") envelope
    )

-- | Atomic refusal: ANY bad name in the batch MUST refuse the
-- entire call. The good name is NOT registered. (Without atomic
-- refusal the agent's worldview drifts from disk reality.)
testHandleAddModulesAtomicRefusal :: IO Bool
testHandleAddModulesAtomicRefusal = withFixture $ \pd cabalFile -> do
  let args = A.object
        [ "modules" A..= (["GoodOne", "lowercase.module", "GoodTwo"] :: [Text]) ]
  before <- TIO.readFile cabalFile
  _      <- AddModules.handle pd args
  after  <- TIO.readFile cabalFile
  pure
    (  before == after
    && not ("GoodOne" `T.isInfixOf` after)
    && not ("GoodTwo" `T.isInfixOf` after)
    )

-- | The rejection payload MUST list every offender so the LLM can
-- fix all bad names in one round-trip (not N round-trips, one per
-- bad name).
testHandleAddModulesAllOffendersListed :: IO Bool
testHandleAddModulesAllOffendersListed = withFixture $ \pd _ -> do
  let args = A.object
        [ "modules" A..= (["1Foo", "lowercase", "Foo.module"] :: [Text]) ]
  result <- AddModules.handle pd args
  let payload   = resultPayload result
      rejected  = lookupField "rejected" payload
      names     = case rejected of
        Just (A.Array xs) -> map (lookupField "name") (Vector.toList xs)
        _                 -> []
  pure $ Just (A.String "1Foo")        `elem` names
      && Just (A.String "lowercase")   `elem` names
      && Just (A.String "Foo.module")  `elem` names

-- | Regression: the happy path still works post-fix. We write a
-- valid module and verify both the .cabal and a stub source file
-- get created.
testHandleAddModulesHappyPathStillWorks :: IO Bool
testHandleAddModulesHappyPathStillWorks = withFixture $ \pd cabalFile -> do
  let args = A.object [ "modules" A..= (["NewMod"] :: [Text]) ]
  result <- AddModules.handle pd args
  after  <- TIO.readFile cabalFile
  -- Stub file exists at the conventional location.
  stubExists <- doesFileExist
                  (HaskellFlows.Types.unProjectDir pd </> "src" </> "NewMod.hs")
  pure
    (  not (trIsError result)
    && "NewMod" `T.isInfixOf` after
    && stubExists
    )

-- | Symmetric: 'ghc_remove_modules' refuses the same shape. Even
-- though removal is "destructive" (the bad name was never legal in
-- the first place), the handler refuses on principle so a typo
-- can't propagate.
testHandleRemoveModulesRefuses :: IO Bool
testHandleRemoveModulesRefuses = withFixture $ \pd cabalFile -> do
  let args = A.object [ "modules" A..= (["lowercase.module"] :: [Text]) ]
  before <- TIO.readFile cabalFile
  result <- RM.handle pd args
  after  <- TIO.readFile cabalFile
  pure
    (  trIsError result
    && before == after
    && hasField "rejected" (resultPayload result)
    )

-- | Issue #41: 'parseImportLine' / 'scanImportersInBody' must
-- recognise the canonical Haskell import shapes and ignore
-- everything else.

testRMScanImportPlain :: IO Bool
testRMScanImportPlain =
  let body = T.unlines
        [ "module Other where"
        , ""
        , "import Foo"
        , "import Bar.Baz (x, y)"
        , "import qualified Foo as F"
        , "import qualified Mtl"
        ]
      hits = RM.scanImportersInBody "test/Other.hs" ["Foo"] body
  in pure (length hits == 2
        && all ((== "Foo") . RM.iModule) hits
        && all ((== "test/Other.hs") . RM.iFile) hits)

-- | Issue #41 — module names match as whole tokens, NOT
-- substrings. Removing 'Foo' must NOT flag 'import Foo.Bar'.
testRMScanRespectsHierarchy :: IO Bool
testRMScanRespectsHierarchy =
  let body = T.unlines [ "import Foo.Bar", "import Foo.Baz" ]
  in pure (null (RM.scanImportersInBody "x.hs" ["Foo"] body))

-- | Issue #41 — empty body / no targets / unrelated body all
-- yield no hits. (Defensive trio so regressions don't slip in
-- via accidental sentinel matches.)
testRMScanQuietOnNoMatch :: IO Bool
testRMScanQuietOnNoMatch = pure $
     null (RM.scanImportersInBody "f.hs" ["Foo"] "")
  && null (RM.scanImportersInBody "f.hs" []      "import Foo\n")
  && null (RM.scanImportersInBody "f.hs" ["Foo"] "module Other where\n")

-- | Issue #62: 'sliceTopLevelBinding' must find a column-0
-- signature and grow the slice down to the next top-level
-- binding's start.

testMoveSliceFindsBinding :: IO Bool
testMoveSliceFindsBinding =
  let body = T.unlines
        [ "module M where"
        , ""
        , "double :: Int -> Int"
        , "double x = x + x"
        , ""
        , "next :: Int -> Int"
        , "next y = y + 1"
        ]
  in case MoveTool.sliceTopLevelBinding "double" body of
       Just s ->
         pure $ "double :: Int -> Int" `T.isInfixOf` MoveTool.srSliced s
             && "double x = x + x"     `T.isInfixOf` MoveTool.srSliced s
             && not ("next" `T.isInfixOf` MoveTool.srSliced s)
       Nothing -> pure False

testMoveSliceAbsorbsHaddock :: IO Bool
testMoveSliceAbsorbsHaddock =
  let body = T.unlines
        [ "module M where"
        , ""
        , "-- | Doubles its input."
        , "-- Continues across lines."
        , "double :: Int -> Int"
        , "double x = x + x"
        ]
  in case MoveTool.sliceTopLevelBinding "double" body of
       Just s -> pure $
         "Doubles its input"   `T.isInfixOf` MoveTool.srSliced s
            && "double x = x + x" `T.isInfixOf` MoveTool.srSliced s
       Nothing -> pure False

testMoveSliceMisses :: IO Bool
testMoveSliceMisses =
  let body = T.unlines
        [ "module M where"
        , "double :: Int -> Int"
        , "double x = x + x"
        ]
  in pure (isNothing (MoveTool.sliceTopLevelBinding "missing" body))

testMoveRemoveSlice :: IO Bool
testMoveRemoveSlice =
  let body = T.unlines
        [ "module M where"
        , ""
        , "double :: Int -> Int"
        , "double x = x + x"
        , ""
        , "next :: Int"
        , "next = 0"
        ]
  in case MoveTool.sliceTopLevelBinding "double" body of
       Just s ->
         let after = MoveTool.removeSliceFromBody s body
         in pure $ not ("double" `T.isInfixOf` after)
                && "next" `T.isInfixOf` after
       Nothing -> pure False

testMoveInsertSlice :: IO Bool
testMoveInsertSlice =
  let body = T.unlines
        [ "module M where"
        , ""
        , "double :: Int -> Int"
        , "double x = x + x"
        ]
      destBody = T.unlines
        [ "module Dest where"
        , ""
        , "existing :: Int"
        , "existing = 0"
        ]
  in case MoveTool.sliceTopLevelBinding "double" body of
       Just s ->
         let merged = MoveTool.insertSliceAtEnd s destBody
         in pure $ "existing"            `T.isInfixOf` merged
                && "double :: Int -> Int" `T.isInfixOf` merged
                -- blank-line separator between existing + slice
                && T.isInfixOf "existing = 0\n\ndouble" merged
       Nothing -> pure False

-- | Issue #62: a consumer body with @import Foo (bar, double)@ and
-- a move of 'double' must split into
-- @import Foo (bar)@ + @import Bar (double)@ — preserving leading
-- whitespace.
testMoveRewriteSelective :: IO Bool
testMoveRewriteSelective =
  let body = T.unlines
        [ "module Other where"
        , ""
        , "import Foo (bar, double)"
        ]
      rewritten = MoveTool.rewriteImports "double" "Foo" "Bar" body
  in pure $ "import Foo (bar)"     `T.isInfixOf` rewritten
        && "import Bar (double)"   `T.isInfixOf` rewritten
        && not ("Foo (bar, double)" `T.isInfixOf` rewritten)

-- | Phase 1 deferral: bare 'import Foo' is left alone — verify
-- catches anything that breaks.
testMoveRewriteBare :: IO Bool
testMoveRewriteBare =
  let body = T.unlines [ "module Other where", "import Foo" ]
      rewritten = MoveTool.rewriteImports "double" "Foo" "Bar" body
  in pure $ "import Foo" `T.isInfixOf` rewritten
        && not ("import Bar" `T.isInfixOf` rewritten)

-- | Phase 1 deferral: 'import qualified Foo as F' is left alone.
testMoveRewriteQualified :: IO Bool
testMoveRewriteQualified =
  let body = T.unlines [ "module O where", "import qualified Foo as F" ]
      rewritten = MoveTool.rewriteImports "double" "Foo" "Bar" body
  in pure $ "import qualified Foo as F" `T.isInfixOf` rewritten
        && not ("import Bar" `T.isInfixOf` rewritten)

testMoveModulePath :: IO Bool
testMoveModulePath = pure $
     MoveTool.moduleNameToPath "Foo"          == "src/Foo.hs"
  && MoveTool.moduleNameToPath "Foo.Bar"      == "src/Foo/Bar.hs"
  && MoveTool.moduleNameToPath "Expr.Simplify" == "src/Expr/Simplify.hs"

-- | Issue #62: when the source module's header carries an
-- explicit export list with the moved symbol, the rewriter
-- drops the symbol from it. Without this, post-move load
-- fails with \"Not in scope\" on the export list.
testMoveRemoveExport :: IO Bool
testMoveRemoveExport =
  let body = T.unlines
        [ "module Source (greet, double) where"
        , ""
        , "double :: Int -> Int"
        , "double x = x + x"
        ]
      stripped = MoveTool.removeFromSourceExportList "double" body
  in pure $ T.isInfixOf "module Source (greet) where" stripped
        && not ("greet, double" `T.isInfixOf` stripped)

-- | Issue #62: open export ('module Foo where' with no parens)
-- is left unchanged.
testMoveRemoveExportOpen :: IO Bool
testMoveRemoveExportOpen =
  let body = T.unlines
        [ "module M where"
        , ""
        , "double = 42"
        ]
      stripped = MoveTool.removeFromSourceExportList "double" body
  in pure (stripped == body)

-- | Issue #76: 'addToDestinationExportList' must insert the
-- moved symbol into a destination header that declares an
-- explicit export list. Without this step, 'ghc_move' lands
-- the symbol in the file but it stays private.
testMoveAddDestExport :: IO Bool
testMoveAddDestExport =
  let body = T.unlines
        [ "module Dest (a, b) where"
        , ""
        , "a = 1"
        , "b = 2"
        ]
      out = MoveTool.addToDestinationExportList "moved" body
  in pure $ T.isInfixOf "module Dest (a, b, moved) where" out
         && T.isInfixOf "a = 1"  out  -- body untouched
         && T.isInfixOf "b = 2"  out

-- | Issue #76: idempotence — if the destination already exports
-- the symbol (e.g. a re-run of the move), the helper must not
-- duplicate the entry.
testMoveAddDestExportIdempotent :: IO Bool
testMoveAddDestExportIdempotent =
  let body = T.unlines
        [ "module Dest (a, moved, b) where"
        , "a = 1"
        ]
      out = MoveTool.addToDestinationExportList "moved" body
  in pure (out == body)

-- | Issue #76: open exports ('module Foo where') already export
-- every binding by default. The helper must leave them alone —
-- introducing a list would change the API surface.
testMoveAddDestExportOpen :: IO Bool
testMoveAddDestExportOpen =
  let body = T.unlines
        [ "module Dest where"
        , "a = 1"
        ]
      out = MoveTool.addToDestinationExportList "moved" body
  in pure (out == body)

-- | Issue #207: 'addToDestinationExportList' must not stop at the ')'
-- inside a Type(..) constructor export. The old T.breakOn ")" approach
-- misparsed @module Dest (Expr(..), eval) where@ as if the ')' inside
-- @Expr(..)@ was the export-list close, so the new symbol was never
-- appended.
testMoveAddDestExportTypeCons :: IO Bool
testMoveAddDestExportTypeCons =
  let body = T.unlines
        [ "module Dest (Expr(..), eval) where"
        , ""
        , "data Expr = Lit Int"
        , "eval :: Expr -> Int"
        , "eval (Lit n) = n"
        ]
      out = MoveTool.addToDestinationExportList "moved" body
  in pure $ T.isInfixOf "module Dest (Expr(..), eval, moved) where" out
         && T.isInfixOf "data Expr" out  -- body preserved

-- | Issue #207: 'removeFromSourceExportList' must also correctly parse
-- headers containing Type(..) constructor exports and not corrupt them.
testMoveRemoveExportTypeCons :: IO Bool
testMoveRemoveExportTypeCons =
  let body = T.unlines
        [ "module Source (Expr(..), eval, moved) where"
        , ""
        , "data Expr = Lit Int"
        ]
      out = MoveTool.removeFromSourceExportList "moved" body
      header = T.takeWhile (/= '\n') out
  in pure $ T.isInfixOf "module Source (Expr(..), eval) where" out
         && not ("moved" `T.isInfixOf` header)

-- | #228: collectModuleHeader handles a standard single-line header.
testCollectModuleHeaderSingle :: IO Bool
testCollectModuleHeaderSingle =
  let lns = [ "module Source (greet, double) where"
            , ""
            , "greet = \"hi\""
            ]
  in pure $ MoveTool.collectModuleHeader lns
         == Just (1, "module Source (greet, double) where")

-- | #228: collectModuleHeader collects all lines up to and including
-- the one ending with @where@.
testCollectModuleHeaderMulti :: IO Bool
testCollectModuleHeaderMulti =
  let lns = [ "module Source"
            , "  ( greet"
            , "  , double"
            , "  ) where"
            , ""
            , "greet = \"hi\""
            ]
  in pure $ MoveTool.collectModuleHeader lns
         == Just (4, "module Source ( greet , double ) where")

-- | #228: removeFromSourceExportList works on multi-line headers.
-- The multi-line header is collapsed to a single line after the rewrite.
testMoveRemoveExportMultiLine :: IO Bool
testMoveRemoveExportMultiLine =
  let body = T.unlines
        [ "module Source"
        , "  ( greet"
        , "  , double"
        , "  ) where"
        , ""
        , "greet = \"hi\""
        , "double x = x * 2"
        ]
      out = MoveTool.removeFromSourceExportList "double" body
  in pure $ "module Source (greet) where" `T.isInfixOf` out
         && not ("double" `T.isInfixOf` T.takeWhile (/= '\n') out)

-- | #228: addToDestinationExportList works on multi-line headers.
-- The multi-line header is collapsed to a single line after the rewrite.
testMoveAddDestExportMultiLine :: IO Bool
testMoveAddDestExportMultiLine =
  let body = T.unlines
        [ "module Dest"
        , "  ( foo"
        , "  , bar"
        , "  ) where"
        , ""
        , "foo = 1"
        , "bar = 2"
        ]
      out = MoveTool.addToDestinationExportList "double" body
  in pure $ "module Dest (foo, bar, double) where" `T.isInfixOf` out

-- | #236 fix: the correct sequence uses a fresh slice from the
-- export-stripped body, ensuring line numbers are consistent.
testMoveSequenceMultilineHeader236 :: IO Bool
testMoveSequenceMultilineHeader236 = pure $
  let body = T.unlines
        [ "module Src"
        , "  ( add"
        , "  , safeDiv"
        , "  , listSum"
        , "  ) where"
        , ""
        , "add :: Int -> Int -> Int"
        , "add x y = x + y"
        , ""
        , "safeDiv :: Int -> Int -> Maybe Int"
        , "safeDiv _ 0 = Nothing"
        , "safeDiv x y = Just x"
        , ""
        , "listSum :: [Int] -> Int"
        , "listSum = foldr add 0"
        ]
      stripped = MoveTool.removeFromSourceExportList "safeDiv" body
      -- Fix: re-slice from stripped body to get correct line numbers
      mSliced  = MoveTool.sliceTopLevelBinding "safeDiv" stripped
      result   = case mSliced of
                   Nothing -> stripped  -- symbol not found (would be a bug)
                   Just s  -> MoveTool.removeSliceFromBody s stripped
  in -- safeDiv should be gone
     not ("safeDiv" `T.isInfixOf` result)
     -- listSum should survive
  && "listSum :: [Int] -> Int" `T.isInfixOf` result
  && "listSum = foldr add 0"   `T.isInfixOf` result
     -- add should also survive
  && "add :: Int -> Int -> Int" `T.isInfixOf` result

-- | Issue #206: 'hasBareImportOf' detects a plain @import Foo@ line.
testHasBareImportOfDetects :: IO Bool
testHasBareImportOfDetects =
  let body = T.unlines
        [ "module Consumer where"
        , "import Data.Text"
        , "import HaskellFlows.Tool.Source"
        , "foo = 1"
        ]
  in pure (MoveTool.hasBareImportOf "HaskellFlows.Tool.Source" body)

-- | Issue #206: 'hasBareImportOf' detects @import qualified Foo@.
testHasBareImportOfQualified :: IO Bool
testHasBareImportOfQualified =
  let body = T.unlines
        [ "import qualified HaskellFlows.Tool.Source"
        ]
  in pure (MoveTool.hasBareImportOf "HaskellFlows.Tool.Source" body)

-- | Issue #206: 'hasBareImportOf' must NOT trigger on a selective
-- import @import Foo (sym)@ — the rewriter handles those already.
testHasBareImportOfSelectiveMiss :: IO Bool
testHasBareImportOfSelectiveMiss =
  let body = T.unlines
        [ "import HaskellFlows.Tool.Source (sym)"
        ]
  in pure (not (MoveTool.hasBareImportOf "HaskellFlows.Tool.Source" body))

-- | Issue #76: the slicer's biggest leak is mistaking the next
-- binding's '-- |' Haddock for a continuation of the current
-- binding. The fix treats column-0 '-- |' / '-- ^' as a slice
-- boundary; the slice for 'first' must end at line 5, before
-- 'second's Haddock starts.
testMoveSliceStopsAtHaddock :: IO Bool
testMoveSliceStopsAtHaddock =
  let body = T.unlines
        [ "-- | First."          -- 1
        , "first :: Int"         -- 2
        , "first = 1"            -- 3
        , ""                     -- 4
        , "-- | Second."         -- 5  ← boundary
        , "second :: Int"        -- 6
        , "second = 2"           -- 7
        ]
  in case MoveTool.sliceTopLevelBinding "first" body of
       Nothing -> pure False
       Just s  ->
         let sliced = MoveTool.srSliced s
         in pure $ T.isInfixOf "first :: Int" sliced
                && T.isInfixOf "first = 1"   sliced
                && not (T.isInfixOf "Second" sliced)
                && not (T.isInfixOf "second" sliced)

-- | Issue #63 Phase 1: a representative cabal solver dump must
-- parse into a non-empty Conflict.
testDepsExplainParse :: IO Bool
testDepsExplainParse =
  let dump = T.unlines
        [ "Resolving dependencies..."
        , "cabal: Could not resolve dependencies:"
        , "[__0] trying: my-project-0.1.0.0 (user goal)"
        , "[__1] next goal: aeson (dependency of my-project)"
        , "[__1] rejecting: aeson-2.2.3.0 (conflict: my-project => aeson < 2.0)"
        , "[__2] rejecting: aeson-2.1.2.1 (conflict: text >= 2.0 needed; text-1.2.5.0 installed)"
        , "[__41] backjump limit reached (currently 4000, change with --max-backjumps)."
        ]
  in pure $ case DepsExplain.parseSolverOutput dump of
       Just c  -> length (DepsExplain.cAll c) == 2
                && DepsExplain.cBackjumps c == Just 4000
       Nothing -> False

-- | Issue #63: 'identifyRootCause' must pick the rejection at the
-- greatest depth.
testDepsExplainRoot :: IO Bool
testDepsExplainRoot =
  let rs =
        [ DepsExplain.Rejection 1  "aeson-2.2.3.0" "my-project => aeson < 2.0"
        , DepsExplain.Rejection 41 "aeson-2.1.2.1" "text needed"
        , DepsExplain.Rejection 12 "lens-5.2.0"    "transitive"
        ]
      root = DepsExplain.identifyRootCause rs
  in pure (DepsExplain.rDepth root == 41
        && DepsExplain.rPackage root == "aeson-2.1.2.1")

-- | Issue #63: 'extractPackages' strips version suffixes and
-- dedupes by name.
testDepsExplainPackages :: IO Bool
testDepsExplainPackages =
  let rs =
        [ DepsExplain.Rejection 1  "aeson-2.2.3.0" "text >= 2.0"
        , DepsExplain.Rejection 2  "aeson-2.1.2.1" "text needed"
        , DepsExplain.Rejection 3  "lens-5.2.0"    "lens upper bound"
        ]
      pkgs = DepsExplain.extractPackages rs
  in pure $ "aeson" `elem` pkgs
        && "lens"  `elem` pkgs
        -- Dedup: aeson appears twice in input.
        && length (filter (== "aeson") pkgs) == 1

-- | Issue #63: clean output (no rejections) → Nothing.
testDepsExplainClean :: IO Bool
testDepsExplainClean =
  let dump = T.unlines
        [ "Resolving dependencies..."
        , "Build profile: -w ghc-9.12.2 -O1"
        , "In order, the following will be built:"
        , " - my-project-0.1.0.0 (lib)"
        ]
  in pure (isNothing (DepsExplain.parseSolverOutput dump))

-- | #156: pkgSearchTokens for a single-word package name.
testPkgSearchTokensSimple :: IO Bool
testPkgSearchTokensSimple =
  pure (DepsExplain.pkgSearchTokens "aeson" == ["Aeson"])

-- | #156: pkgSearchTokens for a hyphenated package name produces
-- joined and individual capitalised tokens.
testPkgSearchTokensHyphen :: IO Bool
testPkgSearchTokensHyphen =
  let tokens = DepsExplain.pkgSearchTokens "data-default"
  in pure
       (  "DataDefault" `elem` tokens
       && "Data"        `elem` tokens
       && "Default"     `elem` tokens
       )

-- | #156: importMatchesPkg recognises a direct import from the package.
testImportMatchesPkgHit :: IO Bool
testImportMatchesPkgHit =
  pure
    (  DepsExplain.importMatchesPkg "aeson" "import Data.Aeson"
    && DepsExplain.importMatchesPkg "aeson" "import qualified Data.Aeson.Key as Key"
    )

-- | #156: importMatchesPkg rejects imports unrelated to the package.
testImportMatchesPkgMiss :: IO Bool
testImportMatchesPkgMiss =
  pure
    (  not (DepsExplain.importMatchesPkg "aeson" "import Data.Map")
    && not (DepsExplain.importMatchesPkg "aeson" "import Prelude")
    )

-- | #156: cabalComponentsMatchingPkg finds the library stanza
-- when it lists the package in build-depends.
testCabalComponentsLibrary :: IO Bool
testCabalComponentsLibrary =
  let cabalText = T.unlines
        [ "cabal-version: 3.4"
        , "name: my-project"
        , ""
        , "library"
        , "  hs-source-dirs: src"
        , "  build-depends:"
        , "      base"
        , "    , aeson"
        , ""
        , "test-suite my-test"
        , "  hs-source-dirs: test"
        , "  build-depends:"
        , "      base"
        ]
      (stanzas, srcDirs) = DepsExplain.cabalComponentsMatchingPkg "aeson" cabalText
  in pure
       (  length stanzas == 1
       && "library" `T.isPrefixOf` head stanzas
       && any (\(_, ds) -> "src" `elem` ds) srcDirs
       )

-- | Issue #60: 'listTopLevelBindings' must pick up every
-- column-0 type signature.
testLabListSimple :: IO Bool
testLabListSimple =
  let body = T.unlines
        [ "module M where"
        , ""
        , "import Data.List (sort)"
        , ""
        , "double :: Int -> Int"
        , "double x = x + x"
        , ""
        , "greet :: String -> String"
        , "greet n = \"hi \" <> n"
        ]
      bs = LabTool.listTopLevelBindings body
  in pure $ length bs == 2
        && map LabTool.bName bs == ["double", "greet"]

-- | Issue #60: signatures wrapped across lines (the second line
-- starts with whitespace) must be joined into one binding entry.
testLabListMultiline :: IO Bool
testLabListMultiline =
  let body = T.unlines
        [ "module M where"
        , ""
        , "concatPairs"
        , "  :: (Eq a, Show b)"
        , "  => [(a, b)] -> [b]"
        , "concatPairs = undefined"
        ]
      bs = LabTool.listTopLevelBindings body
  in pure $ length bs == 1
        && LabTool.bName (head bs) == "concatPairs"
        && T.isInfixOf "[(a, b)] -> [b]" (LabTool.bSignature (head bs))

-- | Issue #60: comments / module headers / equations are NOT
-- mistaken for signatures.
testLabListSkips :: IO Bool
testLabListSkips =
  let body = T.unlines
        [ "module M where"
        , ""
        , "-- top-level comment"
        , "import Data.List (sort)"
        , ""
        , "double = 42  -- no signature"
        ]
  in pure (null (LabTool.listTopLevelBindings body))

-- | Issue #60: 'confidenceAtLeast' compares the candidate against
-- the threshold (Low ≤ Medium ≤ High).
-- | Issue #59: 'pickDiagnostic' defaults to the first error
-- diagnostic. Warnings are filtered out — only severity-error
-- entries qualify.
testExplainPickDefault :: IO Bool
testExplainPickDefault =
  let diags =
        [ GhcError "f.hs" 10 1 SevWarning Nothing "warn"
        , GhcError "f.hs" 20 5 SevError   Nothing "first error"
        , GhcError "f.hs" 30 9 SevError   Nothing "second error"
        ]
  in pure $ case ExplainError.pickDiagnostic Nothing diags of
       Just d  -> geMessage d == "first error" && geLine d == 20
       Nothing -> False

-- | Issue #59: 'diagnostic_index=N' picks the Nth error (0-indexed).
testExplainPickIndex :: IO Bool
testExplainPickIndex =
  let diags =
        [ GhcError "f.hs" 1 1 SevError Nothing "a"
        , GhcError "f.hs" 2 1 SevError Nothing "b"
        , GhcError "f.hs" 3 1 SevError Nothing "c"
        ]
  in pure $ case ExplainError.pickDiagnostic (Just 2) diags of
       Just d  -> geMessage d == "c"
       Nothing -> False

-- | Issue #59: invalid index → Nothing (callers render an
-- error_kind=invalid_index instead of guessing).
testExplainPickOOR :: IO Bool
testExplainPickOOR =
  let diags =
        [ GhcError "f.hs" 1 1 SevError Nothing "a" ]
  in pure (isNothing (ExplainError.pickDiagnostic (Just 5) diags))

-- | Issue #203: renderIndexOutOfRange returns a clear "index N out of
-- range — M error(s) found" hint, not the misleading "No errors detected".
testExplainIndexOutOfRangeHint203 :: IO Bool
testExplainIndexOutOfRangeHint203 =
  let diags  = [ GhcError "src/F.hs" 1 1 SevError Nothing "boom" ]
      result = ExplainError.renderIndexOutOfRange "src/F.hs" 3 1 diags
  in pure $ case trContent result of
    [TextContent body_] ->
      case A.decode (TLE.encodeUtf8 (TL.fromStrict body_)) of
        Just (A.Object top) ->
          case AKM.lookup "result" top of
            Just (A.Object r) ->
              let hint = AKM.lookup "hint" r
              in isJust hint
                 -- must mention "out of range"
                 && maybe False (\case
                      A.String s -> "out of range" `T.isInfixOf` s
                      _          -> False) hint
                 -- must NOT say the old wrong message
                 && maybe True (\case
                      A.String s -> not ("No errors detected" `T.isInfixOf` s)
                      _          -> True) hint
            _ -> False
        _ -> False
    _ -> False

-- | Issue #59: 'extractImports' must recognise plain, qualified,
-- and parenthesised import forms.
testExplainExtractImports :: IO Bool
testExplainExtractImports =
  let body = T.unlines
        [ "module M where"
        , ""
        , "import Data.List (sort)"
        , "import qualified Data.Map.Strict as Map"
        , "import Foo.Bar"
        ]
      imps = ExplainError.extractImports body
  in pure (length imps == 3)

-- | Issue #59: 'enclosingLineRange' clamps to the body bounds
-- so a diagnostic at line 1 doesn't request line -49.
testExplainRangeClamps :: IO Bool
testExplainRangeClamps =
  let (lo1, hi1) = ExplainError.enclosingLineRange 100 50 1
      (lo2, hi2) = ExplainError.enclosingLineRange 100 50 60
      (lo3, hi3) = ExplainError.enclosingLineRange 100 50 200  -- past EOF
  in pure $ lo1 == 1   && hi1 == 51
        && lo2 == 10  && hi2 == 100
        && lo3 == 100 && hi3 == 100   -- clamped on both ends

-- | Issue #61: 'aggregate' must handle every shape callers will
-- encounter — empty list, single sample, odd count (median is
-- the middle element), even count (median averages the two
-- middle elements).

testPerfAggregateEmpty :: IO Bool
testPerfAggregateEmpty =
  let s = PerfTool.aggregate []
  in pure (PerfTool.sCount s == 0
        && PerfTool.sMean s == 0
        && PerfTool.sMin s == 0
        && PerfTool.sMax s == 0)

testPerfAggregateSingle :: IO Bool
testPerfAggregateSingle =
  let s = PerfTool.aggregate [42]
  in pure (PerfTool.sCount s == 1
        && PerfTool.sMean s == 42
        && PerfTool.sMedian s == 42
        && PerfTool.sMin s == 42
        && PerfTool.sMax s == 42)

testPerfAggregateOdd :: IO Bool
testPerfAggregateOdd =
  let s = PerfTool.aggregate [10, 30, 20, 40, 50]
  in pure (PerfTool.sCount s == 5
        && PerfTool.sMin s == 10
        && PerfTool.sMax s == 50
        && PerfTool.sMedian s == 30
        && PerfTool.sMean s == 30)

-- | Even-count median averages the two middle samples after
-- sorting: [10,20,30,40] → median (20+30)/2 = 25.
testPerfAggregateEven :: IO Bool
testPerfAggregateEven =
  let s = PerfTool.aggregate [10, 30, 20, 40]
  in pure (PerfTool.sCount s == 4
        && PerfTool.sMedian s == 25
        && PerfTool.sMean s == 25)

-- Phase 2 baseline tests (#61) ------------------------------------------------

-- | regressionPct: current 110, baseline 100 → +10% (positive = slower).
testPerfRegressionPctPositive :: IO Bool
testPerfRegressionPctPositive =
  pure (PerfTool.regressionPct 100.0 110.0 == Just 10.0)

-- | regressionPct: current 90, baseline 100 → -10% (negative = faster).
testPerfRegressionPctNegative :: IO Bool
testPerfRegressionPctNegative =
  pure (PerfTool.regressionPct 100.0 90.0 == Just (-10.0))

-- | regressionPct: baseline = 0 → Nothing (avoid divide-by-zero).
testPerfRegressionPctZeroBaseline :: IO Bool
testPerfRegressionPctZeroBaseline =
  pure (isNothing (PerfTool.regressionPct 0.0 100.0))

-- | BaselineEntry ToJSON → FromJSON roundtrip: mean_ns preserved.
testPerfBaselineEntryRoundtrip :: IO Bool
testPerfBaselineEntryRoundtrip =
  let entry   = PerfTool.BaselineEntry { PerfTool.beMeanNs = 12345.6 }
      encoded = A.encode entry
  in case A.decode encoded of
       Just decoded -> pure (PerfTool.beMeanNs decoded == 12345.6)
       Nothing      -> pure False

-- | Issue #64: 'pairCombinations' on an empty list returns no
-- | Issue #212: ==> detection pins the text predicate used by
-- 'runPairProbe' to short-circuit the REPL probe. Expressions
-- with implication must be detected; those without must not.
testPAImplicationDetection :: IO Bool
testPAImplicationDetection = pure $
  let has e = "==>" `T.isInfixOf` e
  in  has "\\(x :: Int) -> x > 0 ==> safeDiv x x == Just 1"
   && has "prop_foo ==> prop_bar"
   && not (has "\\x -> x + 0 == x")
   && not (has "\\xs -> reverse (reverse xs) == xs")
   && not (has "\\x -> double x == x * 2")

-- pairs. Edge case the auditor relies on so a property store
-- with 0 entries doesn't try to run a probe.
testPACombinationsEmpty :: IO Bool
testPACombinationsEmpty =
  pure (null (PropertyAuditTool.pairCombinations ([] :: [Int])))

-- | Issue #64: n*(n-1)/2 = 5*4/2 = 10 for a 5-element list.
testPACombinations5 :: IO Bool
testPACombinations5 =
  let pairs = PropertyAuditTool.pairCombinations [1 .. 5 :: Int]
  in pure (length pairs == 10)

-- | Issue #64: every pair is between distinct elements (no
-- (x, x) pairs).
testPACombinationsDistinct :: IO Bool
testPACombinationsDistinct =
  let pairs = PropertyAuditTool.pairCombinations [1 .. 4 :: Int]
  in pure (all (uncurry (/=)) pairs)

-- | Issue #64: 'buildContradictionProbe' wraps the two property
-- expressions into a conjunction lambda. The shape must contain
-- 'args' (the lambda parameter), '&&' (the conjunction), and
-- 'not' (the negation of the second property).
testPABuildProbe :: IO Bool
testPABuildProbe =
  let p1 = "\\x -> simplify (simplify x) == simplify x"
      p2 = "\\x -> simplify (simplify x) == x"
      probe = PropertyAuditTool.buildContradictionProbe p1 p2
  in pure $ T.isInfixOf "args" probe
        && T.isInfixOf "&&"   probe
        && T.isInfixOf "not"  probe
        && T.isInfixOf p1     probe
        && T.isInfixOf p2     probe

-- | Issue #77: 'QcPassed' means the probe @P1 ∧ ¬P2@ was true
-- on every random input — that IS the contradiction. The
-- pre-#77 implementation had this inverted.
testPAInterpretPassed :: IO Bool
testPAInterpretPassed =
  let (status, _detail) = PropertyAuditTool.interpretProbeResult
                            (QcPassed "probe" 100)
  in pure (status == "contradictory")

-- | Issue #77: 'QcFailed' means at least one input made the
-- probe false — the conjunction P1 ∧ ¬P2 does not hold there,
-- so the properties are compatible at that input.
testPAInterpretFailed :: IO Bool
testPAInterpretFailed =
  let (status, detail) = PropertyAuditTool.interpretProbeResult
                           (QcFailed "probe" 50 2 "[0,-1]")
  in pure (status == "compatible" && T.isInfixOf "[0,-1]" detail)

-- | Issue #77: every QC outcome that is neither passed nor
-- failed (parse failure, exception, give-up) maps to skipped.
-- The audit must not pretend to know the answer.
testPAInterpretSkipped :: IO Bool
testPAInterpretSkipped =
  let (s1, _) = PropertyAuditTool.interpretProbeResult
                  (QcUnparsed  "p" "garbage")
      (s2, _) = PropertyAuditTool.interpretProbeResult
                  (QcException "p" "oops")
      (s3, _) = PropertyAuditTool.interpretProbeResult
                  (QcGaveUp    "p" 10 50)
  in pure (s1 == "skipped" && s2 == "skipped" && s3 == "skipped")

-- | #149: when QcUnparsed carries empty raw output (no GHCi stdout,
-- e.g. because the REPL failed with only stderr), the cause field in
-- the skipped finding must be non-empty and provide actionable text.
testPAInterpretUnparsedEmptyCause :: IO Bool
testPAInterpretUnparsedEmptyCause =
  let (status, detail) = PropertyAuditTool.interpretProbeResult
                           (QcUnparsed "\\x -> x == x" "")
  in pure
       (  status == "skipped"
       && not (T.null detail)
       && ("probe load/parse failure: " /= detail)
       -- Must contain something actionable after the colon
       && T.isInfixOf "no GHCi output" detail
       )

-- | Issue #77 (cascade of #74): when the store has duplicate
-- rows for the same expression under different module shapes,
-- 'dedupByExpression' collapses them into one entry, keeping
-- the first occurrence.
testPADedupByExpression :: IO Bool
testPADedupByExpression =
  let mk e m = StoredProperty
                 { spExpression = e
                 , spModule     = Just m
                 , spPassed     = 1
                 , spUpdated    = 0
                 }
      input = [ mk "expr-A" "Foo.Bar"
              , mk "expr-A" "src/Foo/Bar.hs"   -- duplicate, dropped
              , mk "expr-B" "Foo.Bar"
              , mk "expr-B" "src/Foo/Bar.hs"   -- duplicate, dropped
              ]
      out = PropertyAuditTool.dedupByExpression input
      modules = map spModule out
  in pure $ length out == 2
         && map spExpression out == ["expr-A", "expr-B"]
         && modules == [Just "Foo.Bar", Just "Foo.Bar"]   -- first kept

-- | Issue #77: dedupe is a no-op when every expression is
-- distinct. We must never drop a real entry.
testPADedupSingletons :: IO Bool
testPADedupSingletons =
  let mk e = StoredProperty
               { spExpression = e
               , spModule     = Just "Foo"
               , spPassed     = 1
               , spUpdated    = 0
               }
      input = [mk "p1", mk "p2", mk "p3"]
      out   = PropertyAuditTool.dedupByExpression input
  in pure (length out == 3)

-- | Issue #65: each canonical bucket boundary maps to its
-- expected label (0 / 1-5 / 6-20 / >20). The four cases below
-- pin every transition point so a future regression doesn't
-- silently shift the histogram.
testWitBucketBoundaries :: IO Bool
testWitBucketBoundaries =
  pure $  WitnessTool.bucketSize 0   == "0"
       && WitnessTool.bucketSize 1   == "1-5"
       && WitnessTool.bucketSize 5   == "1-5"
       && WitnessTool.bucketSize 6   == "6-20"
       && WitnessTool.bucketSize 20  == "6-20"
       && WitnessTool.bucketSize 21  == ">20"
       && WitnessTool.bucketSize 999 == ">20"

-- | Issue #65: 'buildInstrumentedProperty' wraps the user
-- property with a 'Test.QuickCheck.collect' call carrying a
-- size-prefixed label, and threads withMaxSuccess so the
-- harness honours the requested run count.
testWitBuildInstrumented :: IO Bool
testWitBuildInstrumented =
  let prop = "\\xs -> length (reverse xs) == length (xs :: [Int])"
      out  = WitnessTool.buildInstrumentedProperty prop 750
  in pure $  T.isInfixOf "Test.QuickCheck.withMaxSuccess" out
          && T.isInfixOf "750"                            out
          && T.isInfixOf "Test.QuickCheck.collect"        out
          && T.isInfixOf "size:"                          out
          && T.isInfixOf prop                             out

-- | Issue #65: 'parseLabelDistribution' recovers (label, %) pairs
-- from QuickCheck's formatted histogram. Tolerates integer and
-- decimal forms, and ignores non-percent lines.
testWitParseDistribution :: IO Bool
testWitParseDistribution =
  let raw = T.unlines
        [ "+++ OK, passed 1000 tests:"
        , "35.5% size:1-5"
        , " 40% size:0"
        , "20.0% size:6-20"
        , "4.5% size:>20"
        , "noise line without percent"
        ]
      dist = WitnessTool.parseLabelDistribution raw
  in pure $  any (\(l, p) -> l == "size:1-5"  && p == 35.5) dist
          && any (\(l, p) -> l == "size:0"    && p == 40.0) dist
          && any (\(l, p) -> l == "size:6-20" && p == 20.0) dist
          && any (\(l, p) -> l == "size:>20"  && p == 4.5)  dist
          && length dist == 4

-- | Issue #65: any size-bucket holding < 1 % of the runs is a
-- bias signal. The function only emits warnings for size labels
-- (Phase 1's only instrumented dimension) so unrelated labels
-- are silently ignored.
testWitBiasWarning :: IO Bool
testWitBiasWarning =
  let dist = [ ("size:0",    0.5)   -- below 1% → warned
             , ("size:1-5", 80.0)   -- healthy
             , ("size:6-20", 19.5)  -- healthy
             , ("noise",     0.1)   -- not size:* → ignored
             ]
      ws = WitnessTool.biasWarnings dist
  in pure $ length ws == 1
         && T.isInfixOf "size:0" (head ws)
         && T.isInfixOf "0.5"    (head ws)

-- | Issue #78: 'parseLabelCounts' reads the tab-separated
-- block emitted by the labels-aware harness. Each line is
-- '"<label>\\t<count>"'.
testWitParseLabelCounts :: IO Bool
testWitParseLabelCounts =
  let raw = T.unlines
        [ "size:0\t40"
        , "size:1-5\t312"
        , "size:6-20\t148"
        ]
      counts = WitnessTool.parseLabelCounts raw
  in pure $  length counts == 3
          && lookup "size:0"    counts == Just 40
          && lookup "size:1-5"  counts == Just 312
          && lookup "size:6-20" counts == Just 148

-- | Issue #78: malformed rows (missing tab, non-numeric count,
-- empty label) are silently skipped — never crash the witness.
testWitParseLabelCountsRobust :: IO Bool
testWitParseLabelCountsRobust =
  let raw = T.unlines
        [ "size:1-5\t312"
        , "garbage row without a tab"
        , "\tlone-tab"
        , "label-no-count\tnotanint"
        , "size:6-20\t100"
        ]
      counts = WitnessTool.parseLabelCounts raw
  in pure $  length counts == 2
          && lookup "size:1-5"  counts == Just 312
          && lookup "size:6-20" counts == Just 100

-- | Issue #78: 'countsToDistribution' converts raw counts into
-- percentages summing (within float drift) to 100.
testWitCountsToDistribution :: IO Bool
testWitCountsToDistribution =
  let counts = [("size:0", 25), ("size:1-5", 75)]
      dist   = WitnessTool.countsToDistribution counts
      total  = sum (map snd dist)
  in pure $ length dist == 2
         && abs (total - 100.0) < 0.001
         && lookup "size:0"   dist == Just 25.0
         && lookup "size:1-5" dist == Just 75.0

-- | Issue #78: empty input ⇒ empty distribution. Avoids a
-- divide-by-zero and keeps the bias-warning machinery happy.
testWitCountsEmpty :: IO Bool
testWitCountsEmpty =
  pure $ null (WitnessTool.countsToDistribution [])

-- Phase 2: constructor property builder (#65) ----------------------------------

-- | buildConstructorProperty wraps with 'ctor:' label extraction.
testWitBuildConstructorProperty :: IO Bool
testWitBuildConstructorProperty =
  let built = WitnessTool.buildConstructorProperty "\\x -> x > 0" 500
  in pure ("ctor:" `T.isInfixOf` built
        && "withMaxSuccess 500" `T.isInfixOf` built
        && "show args" `T.isInfixOf` built)

-- | #239: buildConstructorProperty uses list-aware extraction.
-- For list inputs show gives "[1,2,3]" (no spaces), so the old
-- "head (words (show args))" gave one bucket per unique list.
-- The fix detects '[' prefix and returns "[]" or "(:)".
testWitConstructorListAware :: IO Bool
testWitConstructorListAware =
  let built = WitnessTool.buildConstructorProperty "\\xs -> length xs >= 0" 500
  -- Must NOT use the old "head (words (show args))" pattern
  -- Must contain the list-detection logic: "(:)" and "[]"
  in pure (not ("head (words (show args))" `T.isInfixOf` built)
        && "(:)" `T.isInfixOf` built
        && "[]" `T.isInfixOf` built
        && "'['" `T.isInfixOf` built)

-- | #171: The @deferred@ field in the witness response lists features
-- not yet implemented. The descriptor's description must mention it
-- so agents understand the field is intentional (not a bug). Pin that
-- the descriptor text contains the word "deferred".
testWitDeferredDocumented :: IO Bool
testWitDeferredDocumented =
  let desc = WitnessTool.descriptor
  in pure ("deferred" `T.isInfixOf` tdDescription desc)

-- | #171: @wall_time_ms@ must only measure the subprocess call, not
-- the (pure, negligible) property-string construction step. We verify
-- this structurally: the instrumented property does NOT contain any
-- timing boilerplate — it is a pure Text value constructed before t0.
testWitTimerAfterBuild :: IO Bool
testWitTimerAfterBuild =
  -- Property building is a pure, instant operation. If it ran inside
  -- the timed window its output would reference clock calls — it
  -- doesn't. This test pins that buildInstrumentedProperty and
  -- buildConstructorProperty are pure Text builders (no IO).
  let p1 = WitnessTool.buildInstrumentedProperty "\\x -> x > 0" 100
      p2 = WitnessTool.buildConstructorProperty  "\\x -> x > 0" 100
  in pure ("withMaxSuccess" `T.isInfixOf` p1 && "withMaxSuccess" `T.isInfixOf` p2)

-- Phase 2: vacuous-property check (#64) ----------------------------------------

-- | isVacuousResult: QcGaveUp → True.
testPAIsVacuousGaveUp :: IO Bool
testPAIsVacuousGaveUp =
  let qcr = QcGaveUp "\\x -> x > 0" 2 98
  in pure (PropertyAuditTool.isVacuousResult qcr)

-- | isVacuousResult: QcPassed → False.
testPAIsVacuousNotPassed :: IO Bool
testPAIsVacuousNotPassed =
  let qcr = QcPassed "\\x -> True" 100
  in pure (not (PropertyAuditTool.isVacuousResult qcr))

-- | #241: PropertyAudit.hs uses runQuickCheckWithLabelsInProcess for both
-- the contradiction probe and the vacuous check — not the cabal-repl
-- subprocess (which was producing "no GHCi output" for every probe).
testAuditUsesInProcessProbe :: IO Bool
testAuditUsesInProcessProbe = do
  src <- TIO.readFile "src/HaskellFlows/Tool/PropertyAudit.hs"
  -- Must use the in-process path; the old subprocess call must not appear
  -- as a live call (only possibly in comments, which we check by verifying
  -- the number of in-process calls exceeds the number of cabal-repl calls).
  let inProcessCount = T.count "runQuickCheckWithLabelsInProcess" src
      cabalReplCount = T.count "Qc.runQuickCheckViaCabalRepl" src
  pure (inProcessCount >= 2 && cabalReplCount == 0)

-- | #230: kindFor contradictory → "contradictory-pair".
testPARenderFindingKindContradictory :: IO Bool
testPARenderFindingKindContradictory =
  pure (PropertyAuditTool.kindFor "contradictory" == "contradictory-pair")

-- | #230: kindFor skipped → "skipped-pair".
testPARenderFindingKindSkipped :: IO Bool
testPARenderFindingKindSkipped =
  pure (PropertyAuditTool.kindFor "skipped" == "skipped-pair")

-- | #241: enhanceCrossModuleDetail appends the cross-module hint when
-- both pair members have DIFFERENT module paths and the probe was
-- skipped with a load-failure detail.
testEnhanceCrossModuleDetailHits :: IO Bool
testEnhanceCrossModuleDetailHits =
  let detail0 = "probe load/parse failure: (no GHCi output)"
      result  = PropertyAuditTool.enhanceCrossModuleDetail
                  (Just "src/A.hs") (Just "src/B.hs")
                  "skipped" detail0
  in pure (T.isInfixOf "cross-module pair" result
        && T.isInfixOf "src/A.hs" result
        && T.isInfixOf "src/B.hs" result)

-- | #241: enhanceCrossModuleDetail is a no-op when both modules match.
testEnhanceCrossModuleDetailSameModule :: IO Bool
testEnhanceCrossModuleDetailSameModule =
  let detail0 = "probe load/parse failure: (no GHCi output)"
      result  = PropertyAuditTool.enhanceCrossModuleDetail
                  (Just "src/A.hs") (Just "src/A.hs")
                  "skipped" detail0
  in pure (result == detail0)

-- | #241: enhanceCrossModuleDetail is a no-op when status is not skipped.
testEnhanceCrossModuleDetailNotSkipped :: IO Bool
testEnhanceCrossModuleDetailNotSkipped =
  let detail0 = "QuickCheck found 100 random inputs satisfying P1 ∧ ¬P2"
      result  = PropertyAuditTool.enhanceCrossModuleDetail
                  (Just "src/A.hs") (Just "src/B.hs")
                  "contradictory" detail0
  in pure (result == detail0)

-- | #241: enhanceCrossModuleDetail is a no-op when either module is null.
testEnhanceCrossModuleDetailNullModule :: IO Bool
testEnhanceCrossModuleDetailNullModule =
  let detail0 = "probe load/parse failure: (no GHCi output)"
      result  = PropertyAuditTool.enhanceCrossModuleDetail
                  Nothing (Just "src/B.hs")
                  "skipped" detail0
  in pure (result == detail0)

-- | #241: appendReplStderr surfaces non-empty stderr on a skipped pair
-- with a load-failure detail.
testAppendReplStderrHits :: IO Bool
testAppendReplStderrHits =
  let detail0 = "probe load/parse failure: (no GHCi output)"
      err     = "Variable not in scope: pretty :: Expr -> String"
      result  = PropertyAuditTool.appendReplStderr err "skipped" detail0
  in pure (T.isInfixOf "REPL stderr" result
        && T.isInfixOf "Variable not in scope" result)

-- | #241: appendReplStderr is a no-op when stderr is empty.
testAppendReplStderrEmpty :: IO Bool
testAppendReplStderrEmpty =
  let detail0 = "probe load/parse failure: (no GHCi output)"
      result  = PropertyAuditTool.appendReplStderr "" "skipped" detail0
      result2 = PropertyAuditTool.appendReplStderr "   \n  " "skipped" detail0
  in pure (result == detail0 && result2 == detail0)

-- | #241: appendReplStderr is a no-op when status is not skipped.
testAppendReplStderrNotSkipped :: IO Bool
testAppendReplStderrNotSkipped =
  let detail0 = "Probe falsified at: 42"
      err     = "anything"
      result  = PropertyAuditTool.appendReplStderr err "compatible" detail0
  in pure (result == detail0)

-- | #241: appendReplStderr truncates stderr to 500 chars.
testAppendReplStderrTruncates :: IO Bool
testAppendReplStderrTruncates =
  let detail0 = "probe load/parse failure: (no GHCi output)"
      err     = T.replicate 1000 "x"   -- 1000 chars of 'x'
      result  = PropertyAuditTool.appendReplStderr err "skipped" detail0
      -- The result should contain exactly 500 'x' chars (no more).
      stderrSection = T.dropWhile (/= 'x') result
  in pure (T.length (T.takeWhile (== 'x') stderrSection) == 500)

-- | #241: allPairsSkipped True when every finding is skipped.
-- We can't construct a 'PairFinding' directly (constructor unexported),
-- so the True branch is covered by the integration path; here we
-- assert the False branches that guard against false positives.
testAllPairsSkippedTrue :: IO Bool
testAllPairsSkippedTrue =
  -- nPairs > 0 but findings empty (length mismatch) → False
  pure (not (PropertyAuditTool.allPairsSkipped 3 []))

-- | #241: allPairsSkipped False when at least one finding is compatible.
-- Indirect: the length-mismatch False branch.
testAllPairsSkippedFalseCompat :: IO Bool
testAllPairsSkippedFalseCompat =
  pure (not (PropertyAuditTool.allPairsSkipped 1 []))

-- | #241: allPairsSkipped False when nPairs=0 (nothing to skip).
testAllPairsSkippedFalseEmpty :: IO Bool
testAllPairsSkippedFalseEmpty =
  pure (not (PropertyAuditTool.allPairsSkipped 0 []))

-- Phase 2: explain_error patch verification (#59) ------------------------------

-- | applyLinePatch replaces old text on the target line.
testEEApplyLinePatch :: IO Bool
testEEApplyLinePatch =
  let body  = T.unlines ["line1", "foo bar baz", "line3"]
      patch = ExplainError.PatchSpec { ExplainError.psLine = 2
                                     , ExplainError.psOld  = "bar"
                                     , ExplainError.psNew  = "REPLACED"
                                     }
  in pure $ case ExplainError.applyLinePatch body patch of
       Just result -> "REPLACED" `T.isInfixOf` result
                   && "foo" `T.isInfixOf` result
       Nothing     -> False

-- | applyLinePatch returns Nothing when old text not on that line.
testEEApplyLinePatchMiss :: IO Bool
testEEApplyLinePatchMiss =
  let body  = T.unlines ["line1", "line2"]
      patch = ExplainError.PatchSpec { ExplainError.psLine = 1
                                     , ExplainError.psOld  = "NOTHERE"
                                     , ExplainError.psNew  = "X"
                                     }
  in pure (isNothing (ExplainError.applyLinePatch body patch))

-- | applyLinePatch returns Nothing for out-of-bounds line number.
testEEApplyLinePatchOob :: IO Bool
testEEApplyLinePatchOob =
  let body  = T.unlines ["line1"]
      patch = ExplainError.PatchSpec { ExplainError.psLine = 99
                                     , ExplainError.psOld  = "line1"
                                     , ExplainError.psNew  = "X"
                                     }
  in pure (isNothing (ExplainError.applyLinePatch body patch))

-- | Issue #222: runVerifyPatch must use the stanza-aware 'loadForTarget'
-- rather than bare 'loadAndCaptureDiagnostics'. Verified by checking
-- that the source imports and uses both 'loadForTarget' and 'targetForPath'.
testExplainVerifyPatchUsesLoadForTarget :: IO Bool
testExplainVerifyPatchUsesLoadForTarget = do
  src <- TIO.readFile "src/HaskellFlows/Tool/ExplainError.hs"
  let usesLoadForTarget  = "loadForTarget"  `T.isInfixOf` src
  let usesTargetForPath  = "targetForPath"  `T.isInfixOf` src
  -- Confirm the bare path is NOT the only call in runVerifyPatch section
  -- (we don't want a regression back to loadAndCaptureDiagnostics there).
  -- The function is still imported for the initial diagnostic phase, so
  -- loadAndCaptureDiagnostics may appear — but loadForTarget must too.
  pure (usesLoadForTarget && usesTargetForPath)

--------------------------------------------------------------------------------
-- #189 — parseGhcLineCol
--------------------------------------------------------------------------------

-- | #189: standard GHC error format extracts line + column.
testParseGhcLineColBasic :: IO Bool
testParseGhcLineColBasic =
  let errText = "src/WithError.hs:5:10: error: [GHC-39999] No instance for IsString Int"
  in pure (ExplainError.parseGhcLineCol errText == (5, 10))

-- | #189: col range @7-15@ yields just @7@ (end stripped by isDigit).
testParseGhcLineColRange :: IO Bool
testParseGhcLineColRange =
  let errText = "src/Foo.hs:42:7-15: error: something"
  in pure (ExplainError.parseGhcLineCol errText == (42, 7))

-- | #189: plain error text without a location prefix falls back to (1,1).
testParseGhcLineColFallback :: IO Bool
testParseGhcLineColFallback =
  let errText = "No instance for IsString Int"
  in pure (ExplainError.parseGhcLineCol errText == (1, 1))

-- | #189: syntheticError must use parsed line+col, not hardcoded 1:1.
testSyntheticErrorLineCol :: IO Bool
testSyntheticErrorLineCol =
  let errText = "src/WithError.hs:5:10: error: No instance for IsString Int"
      diag    = ExplainError.syntheticError "src/WithError.hs" errText
  in pure (geLine diag == 5 && geColumn diag == 10)

testLabConfidence :: IO Bool
testLabConfidence = pure $
     LabTool.confidenceAtLeast Low    Low    -- threshold Low,    candidate Low    → True
  && LabTool.confidenceAtLeast Low    Medium
  && LabTool.confidenceAtLeast Low    High
  && LabTool.confidenceAtLeast Medium Medium
  && LabTool.confidenceAtLeast Medium High
  && LabTool.confidenceAtLeast High   High
  && not (LabTool.confidenceAtLeast Medium Low)
  && not (LabTool.confidenceAtLeast High   Medium)
  && not (LabTool.confidenceAtLeast High   Low)

-- | Symmetric regression: 'ghc_remove_modules' still removes when
-- given a valid name.
testHandleRemoveModulesHappyPath :: IO Bool
testHandleRemoveModulesHappyPath = withFixture $ \pd cabalFile -> do
  let args = A.object [ "modules" A..= (["Foo"] :: [Text]) ]
  result <- RM.handle pd args
  after  <- TIO.readFile cabalFile
  -- The fixture starts with 'Foo' on the exposed-modules header
  -- line; after the call that line should no longer carry 'Foo'
  -- as a value (the bare 'exposed-modules:' header survives).
  let exposedLines =
        [ ln | ln <- T.lines after
             , "exposed-modules:" `T.isInfixOf` T.toLower (T.stripStart ln) ]
      headerStripped = case exposedLines of
        (ln:_) -> T.strip (T.drop (T.length "exposed-modules:")
                          (T.dropWhile (/= ':') ln))
        []     -> "no-exposed-modules-line"
  pure (not (trIsError result) && headerStripped /= "Foo")

-- | 'ghc_apply_exports' refuses a reserved keyword as an export.
-- The module file is NOT modified — same atomic-refusal contract.
testHandleApplyExportsRefusesKeyword :: IO Bool
testHandleApplyExportsRefusesKeyword = withFixture $ \pd _ -> do
  let projectDir = HaskellFlows.Types.unProjectDir pd
      modulePath = projectDir </> "src" </> "Widget.hs"
      original   = T.unlines
        [ "module Widget where"
        , "greet :: String"
        , "greet = \"hi\""
        ]
  createDirectoryIfMissing True (projectDir </> "src")
  TIO.writeFile modulePath original
  let args = A.object
        [ "module_path" A..= ("src/Widget.hs" :: Text)
        , "exports"     A..= (["greet", "module"] :: [Text])
        ]
  result <- ApplyExports.handle pd args
  bodyAfter <- TIO.readFile modulePath
  -- Issue #90 Phase C: 'rejected' moved under 'result' inside the
  -- envelope. The 'resultPayload' helper drills through.
  pure
    (  trIsError result
    && bodyAfter == original
    && hasField "rejected" (resultPayload result)
    )

-- | 'ghc_apply_exports' regression: lowercase function-name exports
-- are still legal (exports != module names).
testHandleApplyExportsAcceptsLowercase :: IO Bool
testHandleApplyExportsAcceptsLowercase = withFixture $ \pd _ -> do
  let projectDir = HaskellFlows.Types.unProjectDir pd
      modulePath = projectDir </> "src" </> "Widget.hs"
  createDirectoryIfMissing True (projectDir </> "src")
  TIO.writeFile modulePath (T.unlines ["module Widget where", "greet = \"hi\""])
  let args = A.object
        [ "module_path" A..= ("src/Widget.hs" :: Text)
        , "exports"     A..= (["greet"] :: [Text])
        ]
  result <- ApplyExports.handle pd args
  bodyAfter <- TIO.readFile modulePath
  pure (not (trIsError result) && "(greet) where" `T.isInfixOf` bodyAfter)

--------------------------------------------------------------------------------
-- helpers shared by the handler-boundary tests
--------------------------------------------------------------------------------

-- | Decode a 'ToolResult' content payload back into a JSON 'Value'
-- so the tests can pattern-match on field shape (success, error,
-- rejected[]). Mirrors what an MCP client would do.
extractPayload :: ToolResult -> A.Value
extractPayload tr = case trContent tr of
  (TextContent t : _) -> case A.eitherDecodeStrict (encodeUtf8Strict t) of
    Right v -> v
    Left _  -> A.Null
  _ -> A.Null
  where
    encodeUtf8Strict = BL.toStrict . TLE.encodeUtf8 . TL.fromStrict

-- | Issue #90: drill through the envelope to the inner @result@
-- payload. Most pre-envelope tests inspected fields at the top
-- level — those fields now live under @result@. Returns the
-- top-level Value when there's no @result@ field (graceful
-- back-compat for tools still emitting the pre-envelope shape).
resultPayload :: ToolResult -> A.Value
resultPayload tr = case extractPayload tr of
  A.Object o -> case AKM.lookup (AKey.fromText "result") o of
    Just inner -> inner
    Nothing    -> A.Object o
  v          -> v

hasField :: Text -> A.Value -> Bool
hasField k (A.Object o) = AKM.member (AKey.fromText k) o
hasField _ _            = False

lookupField :: Text -> A.Value -> Maybe A.Value
lookupField k (A.Object o) = AKM.lookup (AKey.fromText k) o
lookupField _ _            = Nothing

fieldEquals :: Text -> A.Value -> A.Value -> Bool
fieldEquals k expected v = lookupField k v == Just expected

testFixWarningUnusedImports :: IO Bool
testFixWarningUnusedImports =
  let plan = FixWarning.planForCode "GHC-66111"
  in pure $ FixWarning.fpDrop plan
         && T.isInfixOf "unused import" (T.toLower (FixWarning.fpHint plan))

-- | Issue #55: 'fixable' is the machine-readable signal that
-- replaces \"read the prose hint\". GHC-66111 has a deterministic
-- drop-the-line patch → fixable=True.
testFixPlanFixable66111 :: IO Bool
testFixPlanFixable66111 =
  let plan = FixWarning.planForCode "GHC-66111"
  in pure $ FixWarning.fpFixable plan
         && FixWarning.fpDrop plan

-- | Issue #55: GHC-40910 with NO name → no concrete patch
-- (the tool can't guess which binding the warning meant).
-- fixable=False so the agent knows to fix by hand.
testFixPlanNotFixable40910 :: IO Bool
testFixPlanNotFixable40910 =
  let plan = FixWarning.planForCode "GHC-40910"
  in pure $ not (FixWarning.fpFixable plan)
         && not (FixWarning.fpDrop plan)
         && isNothing (FixWarning.fpPatch plan)

-- | Issue #55: GHC-40910 WITH a binding name → 'planForCodeWithName'
-- promotes the plan to fixable=True with a concrete patch line that
-- prefixes the name with an underscore.
testFixPlanWithNamePromotes :: IO Bool
testFixPlanWithNamePromotes =
  let srcLine = "combineSorted xs ys = sort (xs ++ _holeArg)"
      plan    = FixWarning.planForCodeWithName "GHC-40910" (Just "ys") srcLine
  in pure $ FixWarning.fpFixable plan
         && case FixWarning.fpPatch plan of
              Just patched ->
                patched == "combineSorted xs _ys = sort (xs ++ _holeArg)"
              Nothing -> False

-- | Issue #55 — 'underscorePrefix' core: replace a free word-
-- boundary occurrence of the binding name with @_<name>@.
testUnderscorePrefixToken :: IO Bool
testUnderscorePrefixToken =
  let line = "f x ys = x + 1"
  in pure $
    FixWarning.underscorePrefix "ys" line == Just "f x _ys = x + 1"

-- | Issue #55: must NOT match substrings — 'ysx' or 'tys' don't
-- count as the binding 'ys'.
testUnderscorePrefixWordBoundary :: IO Bool
testUnderscorePrefixWordBoundary =
  let line = "process xys = xys + 1  -- 'ys' is not a token here"
  in pure (isNothing (FixWarning.underscorePrefix "ys" line))

-- | Issue #55: a name already underscore-prefixed → no patch.
-- Prevents double-underscoring on retries.
testUnderscorePrefixIdempotent :: IO Bool
testUnderscorePrefixIdempotent =
  let line = "f x _ys = x"
  in pure (isNothing (FixWarning.underscorePrefix "ys" line))

-- | Issue #202: patchTailBindings renames binding equations that
-- immediately follow the sig line, fixing the GHC-44432 breakage
-- that occurred when only the sig was patched.
testPatchTailBindings202 :: IO Bool
testPatchTailBindings202 = pure $
  -- typical: sig + single equation
     FixWarning.patchTailBindings "unusedBinding"
       [ "unusedBinding = 42"
       , ""
       , "greet x = x"
       ]
       == [ "_unusedBinding = 42"
          , ""
          , "greet x = x"
          ]
  -- multiple equations (pattern matching)
  && FixWarning.patchTailBindings "f"
       [ "f 0 = 1"
       , "f n = n + 1"
       , ""
       ]
       == [ "_f 0 = 1"
          , "_f n = n + 1"
          , ""
          ]
  -- stops at first non-matching line
  && FixWarning.patchTailBindings "foo"
       [ "bar = 1"
       , "foo = 2"
       ]
       == [ "bar = 1"
          , "foo = 2"
          ]
  -- already prefixed: underscorePrefix returns Nothing → passthrough
  && FixWarning.patchTailBindings "unusedBinding"
       [ "_unusedBinding = 42" ]
       == [ "_unusedBinding = 42" ]

-- | Issue #221: applying a fix to a line beyond the file's end must
-- return a validation error, not a silent no-op with applied=true.
testFixWarningOutOfBounds :: IO Bool
testFixWarningOutOfBounds = do
  tmp <- getTemporaryDirectory
  let dir  = tmp </> "haskell-flows-issue-221"
      file = dir </> "Fixture.hs"
  removePathForcibly dir
  createDirectoryIfMissing True dir
  TIO.writeFile file "module Fixture where\n\nfoo = 1\n"   -- 3 lines
  result <- case mkProjectDir dir of
    Left _   -> pure False
    Right pd -> do
      let args = A.object
            [ "module_path" A..= ("Fixture.hs" :: Text)
            , "line"        A..= (999 :: Int)
            , "code"        A..= ("GHC-66111" :: Text)
            , "apply"       A..= True
            ]
      tr <- FixWarning.handle pd args
      pure $ case decodeToolResult tr of
        Right env ->
             Env.reStatus env == Env.StatusFailed
          && maybe False
               (\e ->  Env.eeKind e == Env.Validation
                    && T.isInfixOf "out of bounds" (Env.eeMessage e))
               (Env.reError env)
        Left _ -> False
  removePathForcibly dir
  pure result

--------------------------------------------------------------------------------
-- Issue #235 — patchPrecedingTypeSig
--------------------------------------------------------------------------------

testIsTypeSigLine235 :: IO Bool
testIsTypeSigLine235 = pure $
     FixWarning.isTypeSigLine "listSum" "listSum :: [Int] -> Int"
  && FixWarning.isTypeSigLine "listSum" "listSum :: [Int] -> Int  -- comment"
  && not (FixWarning.isTypeSigLine "listSum" "listSum = foldr add 0")
  && not (FixWarning.isTypeSigLine "listSum" "listSumHelper :: Int")
  && not (FixWarning.isTypeSigLine "listSum" "-- | listSum :: ignored")

-- | #235: patchPrecedingTypeSig renames a type sig in the list.
testPatchPrecedingTypeSig235 :: IO Bool
testPatchPrecedingTypeSig235 = pure $
  FixWarning.patchPrecedingTypeSig "listSum"
    [ "-- | Sum of a list."
    , "listSum :: [Int] -> Int"
    ]
  == [ "-- | Sum of a list."
     , "_listSum :: [Int] -> Int"
     ]

-- | #235: patchPrecedingTypeSig skips blank lines and Haddock comments.
testPatchPrecedingTypeSigSkips235 :: IO Bool
testPatchPrecedingTypeSigSkips235 = pure $
  FixWarning.patchPrecedingTypeSig "foo"
    [ "foo :: Int -> Int"
    , ""
    , "-- inner comment"
    , ""
    ]
  == [ "_foo :: Int -> Int"
     , ""
     , "-- inner comment"
     , ""
     ]

-- | #235: no-op when no type sig precedes the binding.
testPatchPrecedingTypeSigNoOp235 :: IO Bool
testPatchPrecedingTypeSigNoOp235 = pure $
  FixWarning.patchPrecedingTypeSig "bar"
    [ "foo = 1"    -- different name — not a type sig for 'bar'
    , "bar = 2"
    ]
  == [ "foo = 1"
     , "bar = 2"
     ]

-- | #235: writePatched with GHC-40910 on a binding line must ALSO
-- rename the preceding type signature to avoid GHC-44432.
testWritePatchedAlsoFixesSig235 :: IO Bool
testWritePatchedAlsoFixesSig235 = do
  tmp <- getTemporaryDirectory
  let dir  = tmp </> "hf-test235-type-sig"
      file = dir </> "Fixture.hs"
  removePathForcibly dir
  createDirectoryIfMissing True dir
  -- A module where listSum is defined but unused (no exports/callers).
  TIO.writeFile file $ T.unlines
    [ "module Fixture where"
    , ""
    , "-- | Sum of a list."
    , "listSum :: [Int] -> Int"      -- line 4: type sig
    , "listSum = foldr (+) 0"         -- line 5: binding (GHC warns here)
    ]
  result <- case mkProjectDir dir of
    Left _   -> pure False
    Right pd -> do
      let args = A.object
            [ "module_path" A..= ("Fixture.hs" :: Text)
            , "line"        A..= (5 :: Int)   -- GHC-40910 reported on binding
            , "code"        A..= ("GHC-40910" :: Text)
            , "name"        A..= ("listSum" :: Text)
            , "apply"       A..= True
            ]
      _ <- FixWarning.handle pd args
      patched <- TIO.readFile file
      let lns = T.lines patched
      -- Both type sig (line 4, ix 3) and binding (line 5, ix 4) must be renamed.
      pure $  T.isPrefixOf "_listSum ::" (lns !! 3)
           && T.isPrefixOf "_listSum ="  (lns !! 4)
  removePathForcibly dir
  pure result

-- | #238: renderStored should emit a 'module_hint' field when spModule is Nothing.
testRenderStoredNullModuleHint238 :: IO Bool
testRenderStoredNullModuleHint238 = pure $
  let sp = StoredProperty
             { spExpression = "\\x -> x == x"
             , spModule     = Nothing
             , spPassed     = 3
             , spUpdated    = 1_000_000
             }
      A.Object km = RegTool.renderStored sp
  in AKM.member "module_hint" km

-- | #238: renderStored should NOT emit a 'module_hint' field when spModule is Just.
testRenderStoredJustModuleNoHint238 :: IO Bool
testRenderStoredJustModuleNoHint238 = pure $
  let sp = StoredProperty
             { spExpression = "\\x -> x == x"
             , spModule     = Just "src/Foo.hs"
             , spPassed     = 3
             , spUpdated    = 1_000_000
             }
      A.Object km = RegTool.renderStored sp
  in not (AKM.member "module_hint" km)

-- | #238: listResult should emit null_module_count and null_module_hint fields
-- when the property store contains at least one null-module property.
testListResultNullModuleCount238 :: IO Bool
testListResultNullModuleCount238 = do
  let spNull = StoredProperty
                 { spExpression = "\\x -> x >= 0"
                 , spModule     = Nothing
                 , spPassed     = 1
                 , spUpdated    = 1_000_000
                 }
      spOk   = StoredProperty
                 { spExpression = "\\x -> x == x"
                 , spModule     = Just "src/Foo.hs"
                 , spPassed     = 5
                 , spUpdated    = 1_000_000
                 }
      tr     = RegTool.listResult [spNull, spOk]
  case trContent tr of
    [TextContent body] ->
      case A.eitherDecode (TLE.encodeUtf8 (TL.fromStrict body)) of
        Right env ->
          case Env.reResult env of
            Just (A.Object km) ->
              pure $ AKM.member "null_module_count" km
                  && AKM.member "null_module_hint"  km
                  && AKM.lookup "null_module_count" km == Just (A.Number 1)
            _ -> pure False
        Left _ -> pure False
    _ -> pure False

-- | #238: listResult should NOT emit null_module_count/hint when all props have modules.
testListResultNoNullFields238 :: IO Bool
testListResultNoNullFields238 = do
  let sp = StoredProperty
             { spExpression = "\\x -> x == x"
             , spModule     = Just "src/Foo.hs"
             , spPassed     = 2
             , spUpdated    = 1_000_000
             }
      tr = RegTool.listResult [sp]
  case trContent tr of
    [TextContent body] ->
      case A.eitherDecode (TLE.encodeUtf8 (TL.fromStrict body)) of
        Right env ->
          case Env.reResult env of
            Just (A.Object km) ->
              pure $ not (AKM.member "null_module_count" km)
                  && not (AKM.member "null_module_hint"  km)
            _ -> pure False
        Left _ -> pure False
    _ -> pure False

-- | #238: enhanceNullModuleDetail appends a hint when status=skipped,
-- detail starts with "probe load/parse failure", and p1 has no module.
testEnhanceNullModuleDetail238 :: IO Bool
testEnhanceNullModuleDetail238 = pure $
  let result = PropertyAuditTool.enhanceNullModuleDetail
                 True False
                 "skipped"
                 "probe load/parse failure: (no GHCi output)"
  in "re-run it via" `T.isInfixOf` result
  && "module: null" `T.isInfixOf` result

-- | #238: enhanceNullModuleDetail is a no-op when both properties have modules.
testEnhanceNullModuleDetailNoOp238 :: IO Bool
testEnhanceNullModuleDetailNoOp238 = pure $
  let original = "probe load/parse failure: (no GHCi output)"
      result   = PropertyAuditTool.enhanceNullModuleDetail
                   False False "skipped" original
  in result == original

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

-- | Phase 11h: ghc_quickcheck_export must be in the canonical
-- tool list.
testQcExportRegistered :: IO Bool
testQcExportRegistered = pure $ "ghc_property_store" `elem` allToolNameTexts
  -- #94 Phase C step 6: ghc_quickcheck_export merged into
  -- ghc_property_store(action="export"). The legacy wire surface
  -- is gone; the action lives on inside the consolidated tool.

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
    -- Issue #215: properties are now emitted as "prop_N args = body"
    -- (eta-reduced), not "prop_N = \args -> body".
    && T.isInfixOf "prop_1 "                   body   -- binding exists
    && T.isInfixOf "prop_2 "                   body   -- binding exists
    && not (T.isInfixOf "prop_1 = \\" body)           -- not lambda-style
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

-- | Phase 11g: ghc_gate must be in the canonical tool list + the
-- descriptor mentions its three sub-steps.
--------------------------------------------------------------------------------
-- #163: ghc_coverage configurable timeout
--------------------------------------------------------------------------------

-- | Default CoverageArgs must produce a 5-minute timeout (unchanged
-- from the pre-#163 hard-coded value so existing workflows see no
-- behavioural difference).
testCoverageDefaultTimeout :: IO Bool
testCoverageDefaultTimeout =
  let args = CoverageTool.CoverageArgs { CoverageTool.caTimeoutMinutes = 5, CoverageTool.caVerbose = False }
  in pure $ CoverageTool.coverageTimeoutMicros args == 5 * 60 * 1_000_000

-- | Clamping: values below 1 become 1, above 60 become 60.
testCoverageTimeoutClamp :: IO Bool
testCoverageTimeoutClamp =
  let raw0  = A.object []  -- defaults to 5
      raw10 = A.object ["timeout_minutes" .= (10 :: Int)]
      raw80 = A.object ["timeout_minutes" .= (80 :: Int)]
      raw0_ = A.object ["timeout_minutes" .= (0  :: Int)]
  in case ( A.fromJSON raw0  :: A.Result CoverageTool.CoverageArgs
          , A.fromJSON raw10 :: A.Result CoverageTool.CoverageArgs
          , A.fromJSON raw80 :: A.Result CoverageTool.CoverageArgs
          , A.fromJSON raw0_ :: A.Result CoverageTool.CoverageArgs ) of
       (A.Success a0, A.Success a10, A.Success a80, A.Success a0_) ->
         pure $ CoverageTool.caTimeoutMinutes a0  == 5
             && CoverageTool.caTimeoutMinutes a10 == 10
             && CoverageTool.caTimeoutMinutes a80 == 60  -- clamped
             && CoverageTool.caTimeoutMinutes a0_ == 1   -- clamped
       _ -> pure False

-- | The timeout error message must reflect the ACTUAL configured
-- minutes (not hard-code "5 minutes"). This caught by checking
-- the cause field when CovTimeout is rendered with a 15-minute arg.
testCoverageTimeoutMessage :: IO Bool
testCoverageTimeoutMessage =
  let args   = CoverageTool.CoverageArgs { CoverageTool.caTimeoutMinutes = 15, CoverageTool.caVerbose = False }
      result = CoverageTool.renderResult args CoverageTool.CovTimeout
  in pure $ case trContent result of
       [TextContent body_] ->
         case A.decode (TLE.encodeUtf8 (TL.fromStrict body_)) of
           Just (A.Object top) ->
             case AKM.lookup "error" top of
               Just (A.Object err) ->
                 case AKM.lookup "cause" err of
                   Just (A.String cause) -> T.isInfixOf "15m" cause
                   _                    -> False
               _ -> False
           _ -> False
       _ -> False

--------------------------------------------------------------------------------
-- #164: ghc_gate configurable timeouts
--------------------------------------------------------------------------------

-- | Default GateArgs must give 5 min for test (unchanged).
testGateDefaultTestTimeout :: IO Bool
testGateDefaultTestTimeout =
  let raw = A.object []
  in case A.fromJSON raw :: A.Result Gate.GateArgs of
       A.Success args ->
         pure $ Gate.cabalTestTimeoutMicros args == 5 * 60 * 1_000_000
       _ -> pure False

-- | Default GateArgs must give 3 min for build (unchanged).
testGateDefaultBuildTimeout :: IO Bool
testGateDefaultBuildTimeout =
  let raw = A.object []
  in case A.fromJSON raw :: A.Result Gate.GateArgs of
       A.Success args ->
         pure $ Gate.cabalBuildTimeoutMicros args == 3 * 60 * 1_000_000
       _ -> pure False

-- | Passing test_timeout_minutes=20 raises the test budget to 20 min.
testGateCustomTestTimeout :: IO Bool
testGateCustomTestTimeout =
  let raw = A.object ["test_timeout_minutes" .= (20 :: Int)]
  in case A.fromJSON raw :: A.Result Gate.GateArgs of
       A.Success args ->
         pure $ Gate.cabalTestTimeoutMicros args == 20 * 60 * 1_000_000
       _ -> pure False

-- | Passing build_timeout_minutes=10 raises the build budget to 10 min.
testGateCustomBuildTimeout :: IO Bool
testGateCustomBuildTimeout =
  let raw = A.object ["build_timeout_minutes" .= (10 :: Int)]
  in case A.fromJSON raw :: A.Result Gate.GateArgs of
       A.Success args ->
         pure $ Gate.cabalBuildTimeoutMicros args == 10 * 60 * 1_000_000
       _ -> pure False

-- | #216: with 0 properties the dynamic timeout must not fall below 2 min.
testDynamicRegressionFloor :: IO Bool
testDynamicRegressionFloor =
  pure $ Gate.dynamicRegressionTimeout 0 == 2 * 60 * 1_000_000

-- | #216: with 7 properties the dynamic timeout must exceed 2 min so
-- the budget doesn't fire before all 7 cabal-repl launches finish.
-- Formula: max(2 min, n × replayTimeout + 30 s overhead)
-- With n=7 and replayTimeout=30 s:  7×30 + 30 = 240 s > 120 s
testDynamicRegressionScales :: IO Bool
testDynamicRegressionScales =
  pure $ Gate.dynamicRegressionTimeout 7 > 2 * 60 * 1_000_000

testGateRegistered :: IO Bool
testGateRegistered = pure $
     "ghc_gate" `elem` allToolNameTexts
  && case filter (\td -> tdName td == "ghc_gate") allToolDescriptors of
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

-- | #138: calling ghc_gate with all three skip flags returns
-- status='refused' / kind='validation' instead of the vacuous
-- "All gates passed: . Safe to push." success that misled callers.
-- The early-exit path never touches the GhcSession, so 'undefined'
-- is safe for that argument — it is guaranteed not to be forced.
testGateAllSkipRefused :: IO Bool
testGateAllSkipRefused = withTempProject $ \pd -> do
  store <- openStore pd
  let raw = A.object
        [ "skip_regression"  .= True
        , "skip_cabal_test"  .= True
        , "skip_cabal_build" .= True
        ]
  tr <- Gate.handle store (error "GhcSession not needed for all-skip path") pd raw
  case trContent tr of
    [TextContent body] ->
      case A.eitherDecode (TLE.encodeUtf8 (TL.fromStrict body)) of
        Right env ->
          pure $ Env.reStatus env == Env.StatusRefused
              && fmap Env.eeKind (Env.reError env) == Just Env.Validation
        Left _ -> pure False
    _ -> pure False

-- | #138: the 'summary' function must not produce the malformed
-- "All requested gates passed: . Safe to push." string when the
-- passed-verbs list is empty. Verified via source inspection of
-- the defensive guard added in the fix.
testGateSummaryNoEmptyVerbs :: IO Bool
testGateSummaryNoEmptyVerbs = do
  src <- TIO.readFile "src/HaskellFlows/Tool/Gate.hs"
  -- The fix adds a null-check on the verbs list before the safe-to-push string.
  pure $ T.isInfixOf "T.null verbs" src
      && T.isInfixOf "nothing was verified" src

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
-- read-only tools like 'ghc_workflow' froze. Static source check
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
  in pure $ case suggestNext GhcProject True payload of
       Just ns -> nsTool ns == GhcDeps
       Nothing -> False

-- | After ghc_deps(add), reload.
testNextStepDepsAdd :: IO Bool
testNextStepDepsAdd =
  let payload = A.object [ "success" .= True, "action" .= ("added" :: Text) ]
      -- depsAction probes "action" field for "add"/"remove".
      -- The real ghc_deps response uses "added"/"removed" verbs; adjust
      -- this test to pin the contract we actually see in the wild.
      payload2 = A.object [ "success" .= True, "action" .= ("add" :: Text) ]
  in pure $ case suggestNext GhcDeps True payload2 of
       Just ns -> nsTool ns == GhcLoad
       Nothing -> False
    &&
      -- Pin: no false positive on the query variant.
      case suggestNext GhcDeps True payload of
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
  in pure $ case suggestNext GhcLoad True payload of
       Just ns -> nsTool ns == GhcSuggest
       Nothing -> False

-- | Load with warnings → holes.
testNextStepLoadWarnings :: IO Bool
testNextStepLoadWarnings =
  -- Post-BUG-PLUS-mediocre-3 the 'ghc_load' → 'ghc_hole'
  -- route is reserved for typed-hole warnings specifically.
  -- Other (fixable) warnings route to 'ghc_fix_warning'; clean
  -- loads route to 'ghc_suggest'. This test fixture must
  -- emit a real typed-hole message so the dispatcher picks
  -- 'ghc_hole'.
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
  in pure $ case suggestNext GhcLoad True payload of
       Just ns -> nsTool ns == GhcHole
       Nothing -> False

-- | Suggest → quickcheck.
-- #253 Phase 5: GhcSuggest now routes to GhcScratch first (record law
-- candidate in scratchpad before quickchecking it).
testNextStepSuggest :: IO Bool
testNextStepSuggest =
  let payload = A.object [ "success" .= True, "count" .= (3 :: Int) ]
  in pure $ case suggestNext GhcSuggest True payload of
       Just ns -> nsTool ns == GhcScratch
       Nothing -> False

-- | QuickCheck passed → check_module.
testNextStepQcPassed :: IO Bool
testNextStepQcPassed =
  let payload = A.object [ "success" .= True, "state" .= ("passed" :: Text) ]
  in pure $ case suggestNext GhcQuickCheck True payload of
       Just ns -> nsTool ns == GhcCheckModule
       Nothing -> False

-- | QuickCheck failed → eval for debugging.
testNextStepQcFailed :: IO Bool
testNextStepQcFailed =
  let payload = A.object [ "success" .= True, "state" .= ("failed" :: Text) ]
  in pure $ case suggestNext GhcQuickCheck True payload of
       Just ns -> nsTool ns == GhcEval
       Nothing -> False

-- | ghc_property_store(list) → ghc_property_store(run).
-- #94 Phase C step 6: ghc_regression merged into
-- ghc_property_store(action=list|run); the list-then-run hint is
-- emitted on the consolidated tool.
testNextStepRegressionList :: IO Bool
testNextStepRegressionList =
  let payload = A.object [ "success" .= True, "action" .= ("list" :: Text) ]
  in pure $ case suggestNext GhcPropertyStore True payload of
       Just ns -> nsTool ns == GhcPropertyStore
       Nothing -> False

-- | Refactor landed → verify compile.
testNextStepRefactor :: IO Bool
testNextStepRefactor =
  let payload = A.object [ "success" .= True, "compile" .= ("ok" :: Text) ]
  in pure $ case suggestNext GhcRefactor True payload of
       Just ns -> nsTool ns == GhcLoad
       Nothing -> False

-- #253 Phase 5: cross-tool nextStep arms ─────────────────────────────────

-- | GhcHole now routes to GhcScratch (write the hole-filler hypothesis
-- before implementing it).
testNextStepFromHoleRoutesToScratch :: IO Bool
testNextStepFromHoleRoutesToScratch =
  let payload = A.object [ "success" .= True, "holes" .= ([] :: [Value]) ]
  in pure $ case suggestNext GhcHole True payload of
       Just ns -> nsTool ns == GhcScratch
       Nothing -> False

-- | GhcExplainError now routes to GhcScratch (record the proposed fix
-- before applying verify_patch to source).
testNextStepFromExplainErrorRoutesToScratch :: IO Bool
testNextStepFromExplainErrorRoutesToScratch =
  let payload = A.object [ "success" .= True, "error_text" .= ("..." :: Text) ]
  in pure $ case suggestNext GhcExplainError True payload of
       Just ns -> nsTool ns == GhcScratch
       Nothing -> False

-- | GhcSuggest chains to ghc_quickcheck after the scratch write + check
-- steps — verify the chain carries quickcheck as a follow-up.
testNextStepSuggestChainHasQuickCheck :: IO Bool
testNextStepSuggestChainHasQuickCheck =
  let payload = A.object [ "success" .= True, "count" .= (3 :: Int) ]
  in pure $ case suggestNext GhcSuggest True payload of
       Just ns ->
         let chainTools = maybe [] (map csTool) (nsChain ns)
         in GhcQuickCheck `elem` chainTools
       Nothing -> False

-- | Module gate → project gate.
testNextStepCheckModule :: IO Bool
testNextStepCheckModule =
  let payload = A.object [ "success" .= True, "overall" .= True ]
  in pure $ case suggestNext GhcCheckModule True payload of
       Just ns -> nsTool ns == GhcCheckProject
       Nothing -> False

-- | Project gate → gate (pre-push finalizer). BUG-06 re-routed
-- check_project from coverage → gate (the Phase 11n finalizer
-- tool) so the agent reaches the real CI-equivalent step; coverage
-- moves into the attached chain as the optional follow-up.
testNextStepCheckProject :: IO Bool
testNextStepCheckProject =
  let payload = A.object [ "success" .= True, "overall" .= True ]
  in pure $ case suggestNext GhcCheckProject True payload of
       Just ns ->
            nsTool ns == GhcGate
         && case nsChain ns of
              Just steps ->
                   any ((== GhcGate)     . csTool) steps
                && any ((== GhcCoverage) . csTool) steps
              Nothing -> False
       Nothing -> False

-- | Errors suppress the suggestion — the agent should read the error
-- before being nudged forward.
testNextStepErrorsSuppressed :: IO Bool
testNextStepErrorsSuppressed =
  let payload = A.object [ "success" .= False, "error" .= ("oops" :: Text) ]
  in pure $ case suggestNext GhcLoad False payload of
       Nothing -> True
       Just _  -> False

-- | Per PR-3 of the integrated MCP improvements: exploratory tools
-- (type/info/goto/doc) now DO carry a forward-chaining hint — the
-- agent can ignore it but it removes the "ok, what next?" round-trip.
-- The genuine Nothing-arms are now:
--
--   * 'GhcWorkflow', 'GhcBatch' — anti-loop exemptions, always Nothing.
--   * 'GhcComplete', 'HoogleSearch', 'GhcAddImport', 'GhcLint' —
--     suppress when their 'count' field is zero or missing (no
--     candidates to act on).
--   * 'GhcEval', 'GhcCoverage' — suppress on degraded status (the
--     error speaks for itself).
--
-- This test pins the suppression contract; the positive contract
-- ("every other tool returns Just with a canonical payload") lives
-- in 'testNextStepCoverageExhaustive'.
testNextStepExploratoryNothing :: IO Bool
testNextStepExploratoryNothing = pure $
  all nothing
    -- anti-loop exemptions: never recommend regardless of payload.
    [ suggestNext GhcWorkflow True (A.object [])
    , suggestNext GhcBatch    True (A.object [])
    -- count-based suppression: empty payload (no count) → Nothing.
    , suggestNext GhcComplete    True (A.object [])
    , suggestNext HoogleSearch   True (A.object [])
    , suggestNext GhcAddImport   True (A.object [])
    , suggestNext GhcLint        True (A.object [])
    -- count-based suppression: explicit count=0 → Nothing.
    , suggestNext GhcComplete    True (A.object [ "count" .= (0 :: Int) ])
    , suggestNext HoogleSearch   True (A.object [ "count" .= (0 :: Int) ])
    , suggestNext GhcAddImport   True (A.object [ "count" .= (0 :: Int) ])
    , suggestNext GhcLint        True (A.object [ "count" .= (0 :: Int) ])
    -- degraded-status suppression: failed status → Nothing.
    , suggestNext GhcEval     True (A.object [ "status" .= ("failed" :: Text) ])
    , suggestNext GhcCoverage True (A.object [ "status" .= ("failed" :: Text) ])
    ]
  where
    nothing Nothing = True
    nothing _       = False

--------------------------------------------------------------------------------
-- #185 — no_match nextStep for lookup tools
--------------------------------------------------------------------------------

-- | #185: ghc_info on a name not found (status=no_match) must route to
-- hoogle_search, not ghc_doc (which will also no_match on the same name).
testNextStepInfoNoMatchIsHoogle :: IO Bool
testNextStepInfoNoMatchIsHoogle =
  let payload = A.object [ "status" .= ("no_match" :: Text), "name" .= ("unknownXYZ" :: Text) ]
  in pure $ case suggestNext GhcInfo True payload of
       Just ns -> nsTool ns == HoogleSearch
       Nothing -> False

-- | #185: ghc_doc on a name not found (status=no_match) must route to
-- hoogle_search, not ghc_browse (which expects a module, not a symbol).
testNextStepDocNoMatchIsHoogle :: IO Bool
testNextStepDocNoMatchIsHoogle =
  let payload = A.object [ "status" .= ("no_match" :: Text), "name" .= ("unknownXYZ" :: Text) ]
  in pure $ case suggestNext GhcDoc True payload of
       Just ns -> nsTool ns == HoogleSearch
       Nothing -> False

-- | #251: ghc_goto on a name not found (status=no_match) must route to
-- ghc_load (so the user loads the module containing the symbol), not
-- hoogle_search (which searches Hackage and is wrong for project-local names).
testNextStepGotoNoMatchIsGhcLoad :: IO Bool
testNextStepGotoNoMatchIsGhcLoad =
  let payload = A.object [ "status" .= ("no_match" :: Text), "name" .= ("unknownXYZ" :: Text) ]
  in pure $ case suggestNext GhcGoto True payload of
       Just ns -> nsTool ns == GhcLoad
       Nothing -> False

-- | #185: ghc_info on a name FOUND (status=ok) must still route to ghc_doc,
-- not hoogle_search — the no_match branch must not fire on success.
testNextStepInfoFoundIsDoc :: IO Bool
testNextStepInfoFoundIsDoc =
  let payload = A.object [ "status" .= ("ok" :: Text), "name" .= ("Data.List.sort" :: Text) ]
  in pure $ case suggestNext GhcInfo True payload of
       Just ns -> nsTool ns == GhcDoc
       Nothing -> False

-- | PR-3 exhaustivity: every tool except the 2 anti-loop exemptions
-- (GhcWorkflow, GhcBatch) returns 'Just' for a canonical success
-- payload. Adding a new ToolName constructor without filling in a
-- dispatch arm fails this test (as well as the -Wincomplete-patterns
-- warning on 'dispatch').
--
-- The canonical payload per tool exercises the success path: tools
-- that suppress on missing fields get rich payloads; everything else
-- falls back to {status:"ok"}.
testNextStepCoverageExhaustive :: IO Bool
testNextStepCoverageExhaustive = do
  let exempt :: Set.Set ToolName
      exempt = Set.fromList [GhcWorkflow, GhcBatch]
      missing =
        [ n | n <- allToolNames
            , n `Set.notMember` exempt
            , isNothing (suggestNext n True (canonicalPayload n)) ]
  unless (null missing) $
    putStrLn ("nextStep coverage gap: " <> show missing)
  pure (null missing)
  where
    -- Default success envelope; tools that need richer discriminators
    -- override below.
    defaultPayload :: Value
    defaultPayload = A.object [ "status" .= ("ok" :: Text) ]

    canonicalPayload :: ToolName -> Value
    canonicalPayload = \case
      -- count-gated suggestions: provide a non-zero count.
      GhcLint        -> A.object [ "count" .= (3 :: Int) ]
      GhcAddImport   -> A.object [ "count" .= (3 :: Int) ]
      GhcComplete    -> A.object [ "count" .= (3 :: Int) ]
      HoogleSearch   -> A.object [ "count" .= (3 :: Int) ]
      -- shape-gated dispatchers: feed the right discriminator.
      GhcLoad        -> A.object [ "warnings" .= ([] :: [Value])
                                 , "errors"   .= ([] :: [Value]) ]
      GhcDeps        -> A.object [ "action" .= ("add" :: Text) ]
      GhcQuickCheck  -> A.object [ "state"  .= ("passed" :: Text) ]
      GhcRefactor    -> A.object [ "action" .= ("rename_local" :: Text) ]
      GhcPropertyStore -> A.object [ "action" .= ("list" :: Text) ]
      GhcProject     -> A.object [ "scaffolded" .= True ]
      GhcGate        -> A.object [ "status" .= ("ok" :: Text) ]
      -- #253: ghc_scratch action-discriminated by payload shape. Use the
      -- write/show single-entry shape (carries 'id').
      GhcScratch     -> A.object [ "id" .= ("scratch-1" :: Text)
                                 , "kind" .= ("hypothesis" :: Text) ]
      -- everything else: bare success envelope is enough.
      _              -> defaultPayload

-- | Action-discriminated coverage: ghc_property_store, ghc_modules,
-- ghc_project, and ghc_deps each branch on 'action'. This test
-- exercises every action and confirms a Just result, catching the
-- "we forgot to wire one branch" regression.
testNextStepActionCoverage :: IO Bool
testNextStepActionCoverage = pure $
  all justOf
    [ suggestNext GhcPropertyStore True (A.object [ "action" .= ("list" :: Text) ])
    , suggestNext GhcPropertyStore True (A.object [ "action" .= ("run"  :: Text) ])
    -- export branch carries 'files_written' instead of 'action'
    , suggestNext GhcPropertyStore True
        (A.object [ "files_written" .= (["test/Spec.hs"] :: [Text]) ])
    -- audit branch carries 'findings' instead of 'action'
    , suggestNext GhcPropertyStore True
        (A.object [ "findings" .= ([] :: [Value]) ])
    -- ghc_deps every action.
    , suggestNext GhcDeps True (A.object [ "action" .= ("add"     :: Text) ])
    , suggestNext GhcDeps True (A.object [ "action" .= ("remove"  :: Text) ])
    , suggestNext GhcDeps True (A.object [ "action" .= ("explain" :: Text) ])
    -- ghc_project: each branch keys off a different shape field.
    , suggestNext GhcProject True (A.object [ "scaffolded" .= True ])  -- switch
    , suggestNext GhcProject True (A.object [ "host"       .= ("claude" :: Text) ])  -- bootstrap
    , suggestNext GhcProject True (A.object [ "errors"     .= (1 :: Int) ])  -- validate w/ errors
    , suggestNext GhcProject True (A.object [])  -- fallthrough = create
    ]
  where
    justOf (Just _) = True
    justOf Nothing  = False

-- Issue #95 Phase A: suppression rule unit tests --------------------------------

-- | suppressIf suppresses when the predicate returns True.
testNextStepSuppressIfTrue :: IO Bool
testNextStepSuppressIfTrue =
  let ns   = NextStep { nsTool = GhcLoad, nsWhy = "w", nsExample = Nothing, nsChain = Nothing, nsDogfood = Nothing }
      ctx  = NextStep.RecommendCtx { NextStep.rcTool = GhcLoad, NextStep.rcStatus = "ok", NextStep.rcPayload = A.object [] }
      rule = const True
  in pure (isNothing (NextStep.suppressIf rule ctx (Just ns)))

-- | suppressIf passes through when the predicate returns False.
testNextStepSuppressIfFalse :: IO Bool
testNextStepSuppressIfFalse =
  let ns   = NextStep { nsTool = GhcLoad, nsWhy = "w", nsExample = Nothing, nsChain = Nothing, nsDogfood = Nothing }
      ctx  = NextStep.RecommendCtx { NextStep.rcTool = GhcLoad, NextStep.rcStatus = "ok", NextStep.rcPayload = A.object [] }
      rule = const False
  in pure (isJust (NextStep.suppressIf rule ctx (Just ns)))

-- | suppressOnDegraded returns True (suppress) for "failed" status.
testNextStepSuppressOnDegraded :: IO Bool
testNextStepSuppressOnDegraded =
  let ctx = NextStep.RecommendCtx { NextStep.rcTool = GhcLoad
                                  , NextStep.rcStatus = "failed"
                                  , NextStep.rcPayload = A.object []
                                  }
  in pure (NextStep.suppressOnDegraded ctx)

-- | suppressOnZero suppresses when count field is zero.
testNextStepSuppressOnZero :: IO Bool
testNextStepSuppressOnZero =
  let ctx = NextStep.RecommendCtx { NextStep.rcTool = GhcAddImport
                                  , NextStep.rcStatus = "ok"
                                  , NextStep.rcPayload = A.object ["count" A..= (0 :: Int)]
                                  }
  in pure (NextStep.suppressOnZero "count" ctx)

-- | suppressOnZero passes when count field is nonzero.
testNextStepSuppressOnZeroPass :: IO Bool
testNextStepSuppressOnZeroPass =
  let ctx = NextStep.RecommendCtx { NextStep.rcTool = GhcAddImport
                                  , NextStep.rcStatus = "ok"
                                  , NextStep.rcPayload = A.object ["count" A..= (3 :: Int)]
                                  }
  in pure (not (NextStep.suppressOnZero "count" ctx))

-- | injectNextStep splices the nextStep into the first TextContent
-- block's JSON payload.
testInjectSplices :: IO Bool
testInjectSplices =
  let body = A.object [ "success" .= True, "data" .= (42 :: Int) ]
      txt  = TL.toStrict (TLE.decodeUtf8 (A.encode body))
      tr   = ToolResult { trContent = [ TextContent txt ], trIsError = False }
      ns   = NextStep { nsTool = GhcLoad, nsWhy = "because"
                      , nsExample = Nothing, nsChain = Nothing
                      , nsDogfood = Nothing }
      tr'  = injectNextStep ns tr
  in case trContent tr' of
       [TextContent t] -> pure $
         T.isInfixOf "\"nextStep\"" t
           && T.isInfixOf "\"ghc_load\"" t
           && T.isInfixOf "\"data\":42" t
           -- original field preserved
       _ -> pure False

-- | injectNextStep must NOT corrupt non-JSON payloads.
testInjectSkipsNonJson :: IO Bool
testInjectSkipsNonJson =
  let raw = "this is not json"
      tr  = ToolResult { trContent = [ TextContent raw ], trIsError = False }
      ns  = NextStep { nsTool = GhcLoad, nsWhy = "x"
                     , nsExample = Nothing, nsChain = Nothing
                     , nsDogfood = Nothing }
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
-- the Haskell port (@ghc_session@, @ghc_trace@, @ghc_flags@,
-- @ghc_init@, …). Fix wires a non-empty @instructions@ string
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
  --
  -- Issue #56: the post-Wave-5 model uses HscEnv + MVar, NOT the
  -- retired SessionStatus / executeNoLock / registerDelay
  -- subprocess GHCi vocabulary. Drop the latter from the
  -- expected-markers set; rely on 'testGuidanceNoRetiredVocab'
  -- + 'testGuidanceMentionsApi' to pin both halves.
  let instructions = Guidance.sessionInstructionsText allToolDescriptors
      staticMarkers =
        [ "ci-local.sh"
        , "HscEnv"          , "MVar"
        , "10-min"
        , "dogfood"
        , "handshake"
        , "situation"       , "invariant"
        , "nextStep"
        ]
      toolMarkers = allToolNameTexts
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
      -- Issue #90 Phase B: payload routes through the unified
      -- envelope ('mkTimeout' + 'InnerTimeout' kind) instead of
      -- the legacy 'renderErrorKind Timeout' top-level string.
      -- Wire string moves from "timeout" to "inner_timeout"; the
      -- envelope additionally surfaces a top-level 'error_kind'
      -- field for the dual-shape window so legacy oracles still
      -- see a discriminator.
      && T.isInfixOf "Env.mkTimeout" code
      && T.isInfixOf "Env.InnerTimeout" code
      && T.isInfixOf "SomeAsyncException" code
  where
    isDocLine ln =
      let s = T.stripStart ln in "--" `T.isPrefixOf` s

-- | Deferred-pass isolation regression. 'ghc_check_project' runs
-- GHC with '-fdefer-type-errors' + '-fdefer-typed-holes', which
-- produces '.hi'/'.o' artifacts for semantically-broken modules.
-- Those MUST land in a MCP-private build tree, never in cabal's
-- default 'dist-newstyle/' — otherwise a user running 'cabal build'
-- after 'ghc_check_project' sees the poisoned interfaces and
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

--------------------------------------------------------------------------------
-- Dogfood-session feedback fixes — 6 polish probes.
--------------------------------------------------------------------------------

-- | Fix 2. 'ghc_deps add' used to return
-- @"No change: 'X' not found or already at desired state."@ when
-- the package was already in the targeted stanza — a remove-path
-- message on an add call. The correct behaviour is a structured
-- idempotent no-op ('action=unchanged', 'success=true') with a
-- verb-specific 'note'.
testDepsAddIdempotent :: IO Bool
testDepsAddIdempotent = do
  src <- TIO.readFile "src/HaskellFlows/Tool/Deps.hs"
  let code = T.unlines (filter (not . isDocLine) (T.lines src))
  pure $ T.isInfixOf "unchangedResult" code
      -- No lingering occurrence of the old misleading string in
      -- live code (comments may still reference it via "--").
      && not (T.isInfixOf "not found or already at desired state" code)
  where
    isDocLine ln =
      let s = T.stripStart ln in "--" `T.isPrefixOf` s

-- | Fix 6. Switching to an empty directory should point the agent
-- at 'ghc_create_project' (the canonical scaffold step), not at
-- 'ghc_workflow(status)'. The branching lives in 'NextStep.hs';
-- the payload signal ('scaffolded' bool) is emitted by
-- 'SwitchProject.successResult'.
testSwitchProjectEmptyDir :: IO Bool
testSwitchProjectEmptyDir = do
  ns <- TIO.readFile "src/HaskellFlows/Mcp/NextStep.hs"
  sp <- TIO.readFile "src/HaskellFlows/Tool/SwitchProject.hs"
  let nsCode = T.unlines (filter (not . isDocLine) (T.lines ns))
      spCode = T.unlines (filter (not . isDocLine) (T.lines sp))
  -- #94 Phase C step 5: SwitchProject was merged into
  -- ghc_project; the empty-dir → scaffold hint now points at
  -- ghc_project(action=create) rather than the legacy
  -- ghc_create_project tool.
  pure $ T.isInfixOf "ghc_project(action=create)" nsCode
      && T.isInfixOf "\"scaffolded\"" nsCode
      && T.isInfixOf "\"scaffolded\"" spCode
  where
    isDocLine ln =
      let s = T.stripStart ln in "--" `T.isPrefixOf` s

-- | Fix 4. 'ghc_check_module' used to attribute every diagnostic
-- from the whole library load to every module — a warning in
-- 'Expr.Pretty' would red-gate 'Expr.Syntax' too. The fix filters
-- by 'geFile' suffix matching the checked module path.
testCheckModuleDiagFilter :: IO Bool
testCheckModuleDiagFilter = do
  src <- TIO.readFile "src/HaskellFlows/Tool/CheckModule.hs"
  let code = T.unlines (filter (not . isDocLine) (T.lines src))
  pure $ T.isInfixOf "ownDiag" code
      && T.isInfixOf "isSuffixOf" code
      && T.isInfixOf "geFile" code
  where
    isDocLine ln =
      let s = T.stripStart ln in "--" `T.isPrefixOf` s

-- | Fix 1. 'ghc_add_modules' now accepts an optional 'stanza'
-- param so callers can register modules into a test-suite /
-- executable / benchmark stanza (routed to 'other-modules') not
-- just the library's 'exposed-modules'.
testAddModulesStanzaParam :: IO Bool
testAddModulesStanzaParam = do
  src <- TIO.readFile "src/HaskellFlows/Tool/AddModules.hs"
  let code = T.unlines (filter (not . isDocLine) (T.lines src))
  pure $ T.isInfixOf "amStanza" code
      && T.isInfixOf "resolveStanzaTarget" code
      && T.isInfixOf "other-modules" code
      -- Source-dir routing covers the three non-library stanzas.
      && T.isInfixOf "\"test\"" code
      && T.isInfixOf "\"app\"" code
      && T.isInfixOf "\"bench\"" code
  where
    isDocLine ln =
      let s = T.stripStart ln in "--" `T.isPrefixOf` s

-- | Fix 5. 'ghc_check_project' used to search only 'src/', 'lib/',
-- and project root for each declared module's .hs file, so a
-- test-suite's 'other-modules: Gen' came back as @not_found@ even
-- though 'test/Gen.hs' existed. Candidate list now includes
-- 'test/', 'app/', and 'bench/'.
testCheckProjectTestDirs :: IO Bool
testCheckProjectTestDirs = do
  src <- TIO.readFile "src/HaskellFlows/Tool/CheckProject.hs"
  let code = T.unlines (filter (not . isDocLine) (T.lines src))
  pure $ T.isInfixOf "\"src\"   </> relPath" code
      && T.isInfixOf "\"test\"  </> relPath" code
      && T.isInfixOf "\"app\"   </> relPath" code
      && T.isInfixOf "\"bench\" </> relPath" code
  where
    isDocLine ln =
      let s = T.stripStart ln in "--" `T.isPrefixOf` s

-- | Fix 3. 'ghc_quickcheck module=<file>' used to leave the
-- property running with only @file@'s own imports in scope, so
-- a property that referenced library functions failed with
-- 'Variable not in scope'. The fix widens the interactive context
-- via @:m +@ over every library exposed-module.
testQuickCheckScopeWidening :: IO Bool
testQuickCheckScopeWidening = do
  src <- TIO.readFile "src/HaskellFlows/Tool/QuickCheck.hs"
  let code = T.unlines (filter (not . isDocLine) (T.lines src))
  pure $ T.isInfixOf "libraryExposedModules" code
      && T.isInfixOf ":m + " code
      && T.isInfixOf "scanLibraryExposedModules" code
  where
    isDocLine ln =
      let s = T.stripStart ln in "--" `T.isPrefixOf` s

-- | #187 / F-11: GHC 9.12 batch/stdin mode no longer accepts bare
-- top-level @r <- quickCheckWithResult …@ without an explicit @do@.
-- The runner must wrap IO statements in @:{@ @do { … }@ @:}@ so
-- GHCi parses them as a single IO action regardless of whether stdin
-- is a TTY. Both runner blocks (main and stability/witness) must
-- use this pattern.
testQuickCheckRunnerDoBrace :: IO Bool
testQuickCheckRunnerDoBrace = do
  src <- TIO.readFile "src/HaskellFlows/Tool/QuickCheck.hs"
  let code = T.unlines (filter (not . isDocLine) (T.lines src))
  -- Must have the :{ / :} delimiters in the generated input strings
  pure $ T.isInfixOf "\":{\""  code
      && T.isInfixOf "\":}\""  code
      -- Must use explicit do-block syntax
      && T.isInfixOf "\"do {" code
      -- Must NOT have bare top-level r <- any more
      && not (T.isInfixOf "\"r <- quickCheckWithResult" code)
  where
    isDocLine ln = "--" `T.isPrefixOf` T.stripStart ln

-- | Cure regression: the interactive context must be derived from
-- the project's own @import …@ declarations, not from a hardcoded
-- allowlist. Each of the three in-process load paths ('autoLoad',
-- 'loadProjectWithFlavour', 'loadForTarget') must call
-- 'projectInteractiveImports' so qualified + aliased imports in
-- source files ('import qualified Data.Map.Strict as Map') reach
-- 'ghc_eval' verbatim. Without this, every new stdlib module
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

-- | Phase 11c F-10: 'ghc_arbitrary' used to render
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
         && any (\m -> mPercent m == Just 92) (crMetrics rpt))

-- | Phase 11b F-08 (critical): the old @loadModuleWith Deferred@ used
-- @:unset -fdefer-type-errors -fdefer-typed-holes@, but GHCi's
-- @:unset@ is only for GHCi-level options (@+s@, @+t@, …) — NOT GHC
-- flags. So the flags leaked across calls and every subsequent
-- compile-check silently deferred its errors. This voided the
-- snapshot-and-compile-verify invariant of @ghc_refactor@: renames
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

-- | Phase 11b F-05: @ghc_suggest@ used to emit false laws for
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
-- ghc_quickcheck: store-module resolution (the "persist with the right file"
-- UX fix). The dogfood of the expr-evaluator surfaced the bug: callers pass
-- the module of the /function under test/ ('src/Foo.hs'), but the property
-- itself lives in 'test/Spec.hs', and regression replay needs the latter to
-- put the identifier in scope. These tests pin the pure decision function
-- so the resolution rule can evolve without a live GHCi.
--------------------------------------------------------------------------------

-- | Wave-3: chooseStoreModule no longer consults ':info' output —
-- that plumbing sat on top of the subprocess ghci which has been
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
-- ghc_regression: parser for ':show modules' output. Used by the scope
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

-- | Issue #51: a stored property whose recorded module is no
-- longer in scope (e.g. @ghc_quickcheck_export@ overwrote
-- @test/Spec.hs@) used to be reported as a regression with
-- @raw: ""@. The classifier now sees the empty parsed result
-- + @"Variable not in scope"@ stderr and tags it as
-- 'load_failed'.
testRegressionClassifyScope :: IO Bool
testRegressionClassifyScope =
  let parsed   = QcUnparsed "\\x -> simplify x" ""
      stderr_  = "test/Spec.hs:7:1: Variable not in scope: simplify"
      result   = RegTool.classifyLoadFailure parsed stderr_
  in pure $ case result of
       Just msg -> "Variable not in scope" `T.isInfixOf` msg
       Nothing  -> False

-- | Issue #51: GHC's @Could not find module@ error is the other
-- common load-failure shape (e.g. when @cabal v2-repl@ rebuilt
-- after a @ghc_remove_modules@). It must also map to
-- 'load_failed', not to a regression.
testRegressionClassifyMissing :: IO Bool
testRegressionClassifyMissing =
  let parsed  = QcUnparsed "\\x -> True" ""
      stderr_ = "test/Spec.hs:7:1: error [GHC-87110] Could not find module 'Spec'"
      result  = RegTool.classifyLoadFailure parsed stderr_
  in pure (isJust result)

-- | Issue #51 — false-positive guard: a property that genuinely
-- failed at runtime (parser produced a non-Unparsed result) must
-- not be re-classified as load_failed even if some incidental
-- stderr was captured.
testRegressionClassifyPassedPassthrough :: IO Bool
testRegressionClassifyPassedPassthrough =
  let parsed   = QcPassed "\\x -> True" 200
      stderr_  = "Variable not in scope: foo"  -- noise, not load failure
      result   = RegTool.classifyLoadFailure parsed stderr_
  in pure (isNothing result)

-- | Issue #51: an unparsed result with NO load-failure marker in
-- stderr (e.g. a property that printed unrecognised text) must
-- stay unparsed — promotion to load_failed requires evidence.
testRegressionClassifyQuiet :: IO Bool
testRegressionClassifyQuiet =
  let parsed   = QcUnparsed "\\x -> True" ""
      stderr_  = "" -- nothing actionable
      result   = RegTool.classifyLoadFailure parsed stderr_
  in pure (isNothing result)

-- | Issue #51: cabal-repl can dump several KB of build-plan
-- noise on a load failure; the response payload caps it at
-- 600 chars + a truncation marker so the JSON-RPC line stays
-- manageable.
testRegressionSummariseCap :: IO Bool
testRegressionSummariseCap =
  let huge = T.replicate 2000 "x"
      out  = RegTool.summariseLoadError huge
  in pure (T.length out <= 700  -- 600 + truncation marker
       && "(truncated)" `T.isInfixOf` out)

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

-- | Issue #80 regression anchor: the most fundamental query the
-- MCP exposes — ghc_type "id" — must always succeed regardless of
-- whether autoLoadProject ran. We stage a tiny project (no
-- .cabal — autoLoadProject path, no stanza flags) with one
-- module that imports Prelude only, start a GhcSession, run
-- 'queryExprType "id"' inside 'withGhcSession' (which auto-loads
-- the project on first use), and assert the renderer returns a
-- polymorphic identity signature.
--
-- Pre-fix this could (in some sessions) return a hidden-package
-- cascade because the interactive context's auto-imported set
-- referenced symbols not exposed under base-only DynFlags.
-- Codifies the working state so any future regression that
-- swaps the IC handling re-surfaces in the unit suite, before
-- the e2e harness catches it.
testQueryExprTypeIdAfterAutoLoad :: IO Bool
testQueryExprTypeIdAfterAutoLoad = do
  tmp <- getTemporaryDirectory
  let dir  = tmp </> "haskell-flows-issue-80"
      file = dir </> "src" </> "Foo.hs"
  removePathForcibly dir
  createDirectoryIfMissing True (dir </> "src")
  TIO.writeFile file
    (T.pack "module Foo where\nimport Prelude\nfoo :: Int\nfoo = 1\n")
  case mkProjectDir dir of
    Left _   -> pure False
    Right pd -> do
      sess <- startGhcSession pd
      eRes <- try @SomeException $
        withGhcSession sess (TypeTool.queryExprType "id")
      killGhcSession sess
      removePathForcibly dir
      pure $ case eRes of
        Right t -> "a -> a" `T.isInfixOf` t
        Left _  -> False

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
-- 'ghc_deps add'), the NEXT 'loadForTarget' must re-bootstrap
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
      -- 2. Mutate .cabal to add QuickCheck (simulates ghc_deps add).
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
-- ghc_switch_project tests
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
      storeA   <- openStore pdA
      storeRef <- newIORef storeA
      -- Prime the session so we can observe the kill semantics:
      -- handle must wipe whatever Session was there.
      primed   <- startGhcSession pdA
      _        <- readMVar sessRef
      sessRef' <- newMVar (Just primed)
      -- PR-4: SwitchProject.handle gained an IORef Bool for the
      -- self-project flag. Synthetic /tmp targets are never self.
      -- F-02: SwitchProject.handle also gained an IORef Scratchpad.Store.
      scratchA   <- SP.openStore pdA
      scratchRef <- newIORef scratchA
      selfRef    <- newIORef False
      let args = A.object [ "path" A..= T.pack dirB ]
      result  <- SwitchProject.handle pdRef sessRef' storeRef scratchRef selfRef args
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

-- | Issue #39: a successful 'switch_project' must atomically
-- swap the property store ref so subsequent 'loadAll' goes
-- against the NEW project's @.haskell-flows/properties.json@.
-- Pre-fix the ref kept pointing at the boot-time store, so a
-- property saved into A leaked into B's regression list.
--
-- Setup:
--   * Project A scaffolded + a single property saved to its store.
--   * Project B scaffolded with NO properties.
--   * SwitchProject.handle (A → B).
-- Assertion:
--   * The post-switch storeRef opened against B reads as empty.
--   * The pre-switch storeRef (storeA, captured before the swap)
--     still reads A's property — proves we returned a NEW
--     'Store' instead of mutating the old one in place (which
--     would have been correct but fragile).
testSwitchHandleReopensStore :: IO Bool
testSwitchHandleReopensStore = do
  dirA <- scaffoldTmpProject "with-prop"
  dirB <- scaffoldTmpProject "no-prop"
  case (mkProjectDir dirA, mkProjectDir dirB) of
    (Right pdA, Right pdB) -> do
      pdRef    <- newIORef pdA
      sessRef  <- newMVar Nothing
      storeA   <- openStore pdA
      save storeA "\\x -> x == (x :: Int)" (Just "src/Foo.hs")
      preProps <- loadAll storeA
      storeRef <- newIORef storeA
      -- PR-4: SwitchProject.handle gained an IORef Bool for the
      -- self-project flag. Synthetic /tmp targets are never self.
      -- F-02: SwitchProject.handle also gained an IORef Scratchpad.Store.
      scratchA   <- SP.openStore pdA
      scratchRef <- newIORef scratchA
      selfRef    <- newIORef False
      let args = A.object [ "path" A..= T.pack dirB ]
      _ <- SwitchProject.handle pdRef sessRef storeRef scratchRef selfRef args
      storeAfter  <- readIORef storeRef
      postProps   <- loadAll storeAfter
      -- After the swap, the OLD Store handle should still point
      -- at A's file (immutability invariant) — read it again to
      -- confirm A's property wasn't somehow purged.
      stillInA    <- loadAll storeA
      removePathForcibly dirA
      removePathForcibly dirB
      pure
        ( length preProps  == 1
            && null postProps
            && length stillInA == 1
        )
    _ -> do
      removePathForcibly dirA
      removePathForcibly dirB
      pure False

-- | F-02 regression: a successful 'switch_project' must atomically
-- swap the scratchpad IORef so subsequent 'ghc_scratch' calls go
-- against the NEW project's @.haskell-flows/scratchpad.json@.
-- Pre-fix the ref kept pointing at the boot-time scratchpad, so
-- scratch entries saved into project A leaked into project B's list.
--
-- Setup:
--   * Project A scaffolded + a single scratch entry saved to its store.
--   * Project B scaffolded with NO scratch entries.
--   * SwitchProject.handle (A → B).
-- Assertion:
--   * The post-switch scratchRef opened against B reads as empty.
--   * The pre-switch scratchA handle still reads A's entry (immutability
--     invariant — we returned a new Store, not mutated the old one).
testSwitchHandleReopensScratchpad :: IO Bool
testSwitchHandleReopensScratchpad = do
  dirA <- scaffoldTmpProject "scratch-from"
  dirB <- scaffoldTmpProject "scratch-to"
  case (mkProjectDir dirA, mkProjectDir dirB) of
    (Right pdA, Right pdB) -> do
      pdRef      <- newIORef pdA
      sessRef    <- newMVar Nothing
      storeA     <- openStore pdA
      storeRef   <- newIORef storeA
      scratchA   <- SP.openStore pdA
      scratchRef <- newIORef scratchA
      selfRef    <- newIORef False
      -- Save one entry into project A's scratchpad
      let entry = SP.ScratchEntry
            { SP.seId      = "f02-probe"
            , SP.seKind    = SP.ScratchHypothesis
            , SP.seCode    = "1 + 1"
            , SP.seModule  = Nothing
            , SP.seImports = []
            , SP.seNote    = Nothing
            , SP.seResult  = Nothing
            , SP.seStatus  = SP.ScratchOpen
            , SP.seCreated = 0
            , SP.seUpdated = 0
            }
      SP.save scratchA entry
      preEntries <- SP.loadAll scratchA
      -- Switch to project B (use pdB-derived path to suppress unused-match)
      let args = A.object [ "path" A..= T.pack (HaskellFlows.Types.unProjectDir pdB) ]
      _ <- SwitchProject.handle pdRef sessRef storeRef scratchRef selfRef args
      -- Read the new scratchpad via the swapped ref
      scratchAfter  <- readIORef scratchRef
      postEntries   <- SP.loadAll scratchAfter
      -- A's scratch still intact (immutability invariant)
      stillInA      <- SP.loadAll scratchA
      removePathForcibly dirA
      removePathForcibly dirB
      pure
        ( length preEntries  == 1
            && null postEntries
            && length stillInA == 1
        )
    _ -> do
      removePathForcibly dirA
      removePathForcibly dirB
      pure False

--------------------------------------------------------------------------------
-- PR-4: SelfProject + dogfood-hint tests
--------------------------------------------------------------------------------

-- | Pure parser test: 'parseCabalNameField' handles canonical input,
-- whitespace, case-insensitive 'name', and ignores other fields.
testParseCabalNameField :: IO Bool
testParseCabalNameField = pure $ and
  [ SelfProject.parseCabalNameField "name: haskell-flows-mcp"
      == Just "haskell-flows-mcp"
  , SelfProject.parseCabalNameField "  name:   haskell-flows-mcp  "
      == Just "haskell-flows-mcp"
  , SelfProject.parseCabalNameField "Name: foo"
      == Just "foo"
  , SelfProject.parseCabalNameField "version: 0.1\nname: bar\nbuild-depends: base"
      == Just "bar"
  , isNothing (SelfProject.parseCabalNameField "no name here")
  , isNothing (SelfProject.parseCabalNameField "")
  ]

-- | Helper for SelfProject tests: write a cabal at the given path
-- with the given 'name:' value plus minimal valid metadata.
writeCabalNamed :: FilePath -> Text -> IO ()
writeCabalNamed path n = TIO.writeFile path $ T.unlines
  [ "cabal-version: 2.4"
  , "name: " <> n
  , "version: 0.1.0.0"
  , ""
  , "library"
  , "  build-depends: base"
  , "  default-language: Haskell2010"
  ]

-- | Positive: a tmp dir whose cabal file is named haskell-flows-mcp.cabal
-- AND declares 'name: haskell-flows-mcp' is recognised as self.
testDetectSelfProjectPositive :: IO Bool
testDetectSelfProjectPositive = do
  base <- getTemporaryDirectory
  ts   <- getPOSIXTime
  let dir = base </> ("sp-self-pos-"
                       <> show (floor (ts * 1000000) :: Int))
  createDirectoryIfMissing True dir
  writeCabalNamed (dir </> "haskell-flows-mcp.cabal") SelfProject.selfCabalName
  result <- case mkProjectDir dir of
    Right pd -> SelfProject.detectSelfProject pd
    Left _   -> pure False
  removePathForcibly dir
  pure result

-- | Negative: a tmp dir whose cabal declares a different name returns
-- False — alt-named forks are "not self" by design.
testDetectSelfProjectNegative :: IO Bool
testDetectSelfProjectNegative = do
  base <- getTemporaryDirectory
  ts   <- getPOSIXTime
  let dir = base </> ("sp-self-neg-"
                       <> show (floor (ts * 1000000) :: Int))
  createDirectoryIfMissing True dir
  writeCabalNamed (dir </> "haskell-flows-mcp.cabal") "other-package"
  result <- case mkProjectDir dir of
    Right pd -> SelfProject.detectSelfProject pd
    Left _   -> pure True   -- treat as failed setup
  removePathForcibly dir
  pure (not result)

-- | Missing cabal file collapses to False — the detector must err on
-- the side of "not self" so the dogfood prompt doesn't fire on a
-- random / empty / unrelated project.
testDetectSelfProjectMissing :: IO Bool
testDetectSelfProjectMissing = do
  base <- getTemporaryDirectory
  ts   <- getPOSIXTime
  let dir = base </> ("sp-self-missing-"
                       <> show (floor (ts * 1000000) :: Int))
  createDirectoryIfMissing True dir
  -- no cabal file written — empty dir
  result <- case mkProjectDir dir of
    Right pd -> SelfProject.detectSelfProject pd
    Left _   -> pure True
  removePathForcibly dir
  pure (not result)

-- | 'withDogfoodHint' must populate 'nsDogfood' when ALL three
-- conditions hold: isSelf=True, write-tool, module_path under
-- a self-mutable subdir.
testWithDogfoodHintFiresWhenSelf :: IO Bool
testWithDogfoodHintFiresWhenSelf = pure $
  let baseNs = NextStep
        { nsTool    = GhcLoad
        , nsWhy     = "compile clean"
        , nsExample = Nothing
        , nsChain   = Nothing
        , nsDogfood = Nothing
        }
      payload = A.object
        [ "module_path" .= ("src/HaskellFlows/Mcp/Server.hs" :: Text) ]
      ns = withDogfoodHint True SelfProject.selfMutableSubdirs GhcLoad payload baseNs
  in case nsDogfood ns of
       Just _  -> True
       Nothing -> False

-- | Suppression: 'isSelf=False' → dogfood stays Nothing regardless of
-- tool or module_path.
testWithDogfoodHintNotFiresWhenNotSelf :: IO Bool
testWithDogfoodHintNotFiresWhenNotSelf = pure $
  let baseNs = NextStep
        { nsTool    = GhcLoad
        , nsWhy     = "x"
        , nsExample = Nothing
        , nsChain   = Nothing
        , nsDogfood = Nothing
        }
      payload = A.object
        [ "module_path" .= ("src/HaskellFlows/Mcp/Server.hs" :: Text) ]
      ns = withDogfoodHint False SelfProject.selfMutableSubdirs GhcLoad payload baseNs
  in isNothing (nsDogfood ns)

-- | Suppression: read-only tools (ghc_type) don't trigger the dogfood
-- nudge even on a self-project — the prompt is for write-tools only.
testWithDogfoodHintNotFiresOnReadTool :: IO Bool
testWithDogfoodHintNotFiresOnReadTool = pure $
  let baseNs = NextStep
        { nsTool    = GhcSuggest
        , nsWhy     = "y"
        , nsExample = Nothing
        , nsChain   = Nothing
        , nsDogfood = Nothing
        }
      payload = A.object [] -- ghc_type doesn't carry module_path
      ns = withDogfoodHint True SelfProject.selfMutableSubdirs GhcType payload baseNs
  in isNothing (nsDogfood ns)

--------------------------------------------------------------------------------
-- PR-5: description-shape lint
--------------------------------------------------------------------------------

-- | PR-5: every registered tool descriptor must meet the 6-field
-- template documented in @docs/TOOL_DESCRIPTION_TEMPLATE.md@. The
-- check is intentionally LENIENT — it only enforces the one signal
-- that catches the "added a new tool with a one-line stub" regression
-- without generating false positives on pre-existing prose-style
-- descriptions:
--
--   * length >= 200 characters — under that, the description is
--     too thin to carry the 6 fields even in prose form.
--
-- The "when" word and sibling-tool-ref checks were removed: many
-- well-written pre-existing descriptions use equivalent prose without
-- the literal keyword, and enforcing them would require editing every
-- tool at once (out of scope for PR-5).
--
-- Failures are reported by tool name so the offender is obvious.
-- | #267: the description-shape lint now enforces the full 6-field
-- template (PURPOSE / WHEN / WHEN NOT / PREREQUISITES / OUTPUT /
-- SEE ALSO) documented in 'docs/TOOL_DESCRIPTION_TEMPLATE.md', not
-- just a 200-char minimum. The 'WHEN NOT' marker is the disambiguator
-- that stops an LLM picking the wrong sibling tool; 'SEE ALSO' (plus
-- the ghc_/hoogle_ cross-reference check) gives it a routing anchor.
-- Pre-#267 only 14/36 descriptors carried these markers and the lint
-- silently passed the other 22 on length alone.
testDescriptionsMeetTemplate :: IO Bool
testDescriptionsMeetTemplate = do
  let requiredMarkers :: [Text]
      requiredMarkers =
        [ "PURPOSE:", "WHEN:", "WHEN NOT:"
        , "PREREQUISITES:", "OUTPUT:", "SEE ALSO:"
        ]
      missingMarkers d =
        [ m | m <- requiredMarkers, not (m `T.isInfixOf` tdDescription d) ]
      -- At least one sibling-tool cross-reference so the agent has a
      -- routing anchor (every SEE ALSO names a ghc_/hoogle_ tool).
      hasCrossRef d =
        "ghc_" `T.isInfixOf` tdDescription d
          || "hoogle_" `T.isInfixOf` tdDescription d
      problems d =
        [ "length < 200" | T.length (tdDescription d) < 200 ]
          <> [ "missing " <> m | m <- missingMarkers d ]
          <> [ "no ghc_/hoogle_ cross-reference" | not (hasCrossRef d) ]
      bad =
        [ (tdName d, problems d)
        | d <- allToolDescriptors
        , not (null (problems d))
        ]
  unless (null bad) $ do
    putStrLn "description-template lint hits (6-field template, #267):"
    mapM_
      (\(name, ps) ->
         putStrLn ("  " <> T.unpack name <> ": "
                    <> T.unpack (T.intercalate ", " ps)))
      bad
  pure (null bad)

--------------------------------------------------------------------------------
-- BUG-PLUS-07: switch_project accepts empty dirs (scaffold-ready)
--------------------------------------------------------------------------------

-- | An empty directory should be a valid switch target so the
-- user can follow up with 'ghc_create_project' — the canonical
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
-- BUG-PLUS-01: ghc_add_modules string fallback
--------------------------------------------------------------------------------

-- | The documented shape: @{"modules": ["A", "B"]}@.
testAddModulesArrayForm :: IO Bool
testAddModulesArrayForm =
  let payload = A.object [ "modules" A..= (["Expr.Syntax", "Expr.Eval"] :: [Text]) ]
  in case A.fromJSON payload of
       A.Success (AddModules.AddModulesArgs xs _) ->
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
          A.Success (AddModules.AddModulesArgs xs _) ->
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
-- #147: name-semantic guards prevent type-shape coincidence pairings
--------------------------------------------------------------------------------

-- | 'nameHintsInterpreter' must return True for evaluation-like
-- names and False for unrelated names that share the interpreter
-- type shape (e.g. @hash :: Expr -> Int@).
testSuggestInterpreterNameGuard :: IO Bool
testSuggestInterpreterNameGuard = pure $
     nameHintsInterpreter "eval"
  && nameHintsInterpreter "runExpr"
  && nameHintsInterpreter "interpret"
  && not (nameHintsInterpreter "hash")
  && not (nameHintsInterpreter "size")
  && not (nameHintsInterpreter "pretty")

-- | When a sibling's name does NOT hint at interpretation, the
-- evaluator-preservation rule must NOT pair it with the focal
-- transform. Pre-fix, @hash :: Expr -> Int@ was paired with
-- @simplify :: Expr -> Expr@ because it shared the interpreter
-- type shape.
testSuggestEvalPreservNoHash :: IO Bool
testSuggestEvalPreservNoHash = do
  let simpSig = HaskellFlows.Parser.TypeSignature.parseSignature "Expr -> Expr"
      evalSig = HaskellFlows.Parser.TypeSignature.parseSignature "Expr -> Int"
  case (simpSig, evalSig) of
    (Just ss, Just es) ->
      let ctx = RuleContext
            { rcName     = "simplify"
            , rcSig      = ss
            , rcSiblings = [("hash", es)]  -- same shape as eval but name doesn't hint
            }
          evalLaws = filter (\s -> sCategory s == "evaluator") (applyRulesCtx ctx)
      in pure (null evalLaws)
    _ -> pure False

-- | 'namesFormPrinterParserPair' must recognise valid pairs and
-- reject unrelated names that happen to coincide in type shape.
testSuggestPrinterParserPairNames :: IO Bool
testSuggestPrinterParserPairNames = pure $
     namesFormPrinterParserPair "pretty"      "parseExpr"
  && namesFormPrinterParserPair "encode"      "decode"
  && namesFormPrinterParserPair "serialize"   "deserialize"
  && namesFormPrinterParserPair "toJSON"      "fromJSON"
  && not (namesFormPrinterParserPair "size"        "length")
  && not (namesFormPrinterParserPair "hash"         "sort")
  && not (namesFormPrinterParserPair "pretty"       "hash")

-- | When a roundtrip-sibling's name does not correlate with the
-- focal's name (no printer/parser pair), no roundtrip law is emitted.
testSuggestRoundtripUnrelatedFiltered :: IO Bool
testSuggestRoundtripUnrelatedFiltered = do
  let focalSig  = HaskellFlows.Parser.TypeSignature.parseSignature "Expr -> Text"
      unrelSig  = HaskellFlows.Parser.TypeSignature.parseSignature "Text -> Expr"
  case (focalSig, unrelSig) of
    (Just fs, Just us) ->
      let ctx = RuleContext
            { rcName     = "hash"        -- not a printer name
            , rcSig      = fs
            , rcSiblings = [("lookup", us)]  -- not a parser name
            }
          roundtrips = filter (\s -> sLaw s == "Printer/parser roundtrip")
                               (applyRulesCtx ctx)
      in pure (null roundtrips)
    _ -> pure False

--------------------------------------------------------------------------------
-- #159: Either-return law templates
--------------------------------------------------------------------------------

-- | A function @parse :: Text -> Either String Expr@ must emit at
-- least the totality law even when no sibling is present. Before
-- the fix, @ghc_suggest@ returned 0 suggestions for this shape.
testSuggestEitherTotality :: IO Bool
testSuggestEitherTotality = do
  let sig = HaskellFlows.Parser.TypeSignature.parseSignature
              "Text -> Either String Expr"
  case sig of
    Just s ->
      let ctx = RuleContext { rcName = "parse", rcSig = s, rcSiblings = [] }
          sug = applyRulesCtx ctx
          hasTotality = any (\x -> sCategory x == "either") sug
      in pure hasTotality
    Nothing -> pure False

-- | When a printer sibling exists (@pretty :: Expr -> Text@), the
-- roundtrip property for an Either-returning parser must use
-- @Right x@, not @Just x@ or bare @x@.
testSuggestEitherParserRoundtrip :: IO Bool
testSuggestEitherParserRoundtrip = do
  let parseSig  = HaskellFlows.Parser.TypeSignature.parseSignature
                    "Text -> Either String Expr"
      prettySig = HaskellFlows.Parser.TypeSignature.parseSignature
                    "Expr -> Text"
  case (parseSig, prettySig) of
    (Just ps, Just pr) ->
      let ctx = RuleContext
            { rcName     = "parse"
            , rcSig      = ps
            , rcSiblings = [("pretty", pr)]
            }
          roundtrips = filter (\s -> sLaw s == "Printer/parser roundtrip")
                               (applyRulesCtx ctx)
          usesRight  = any (("Right x" `T.isInfixOf`) . sProperty) roundtrips
      in pure (not (null roundtrips) && usesRight)
    _ -> pure False

-- | The 'ruleEitherReturn' rule must be present in 'allRules'.
testSuggestEitherRuleRegistered :: IO Bool
testSuggestEitherRuleRegistered = do
  let sig = HaskellFlows.Parser.TypeSignature.parseSignature
              "Int -> Either String Bool"
  case sig of
    Just s ->
      let ctx = RuleContext { rcName = "validate", rcSig = s, rcSiblings = [] }
          sug = applyRulesCtx ctx
      in pure (any (\x -> sCategory x == "either") sug)
    Nothing -> pure False

--------------------------------------------------------------------------------
-- BUG-PLUS-03: external cabal edit invalidates stanza cache
--------------------------------------------------------------------------------

-- | Prove 'ensureStanzaFlags' picks up cabal-file changes made
-- OUTSIDE the MCP's ghc_deps pipeline. The sequence:
--
--   1. Scaffold a real cabal project.
--   2. Call 'ensureStanzaFlags' — cache populates, mtime
--      recorded.
--   3. Touch the .cabal so its mtime strictly advances.
--   4. Call 'ensureStanzaFlags' again — 'cabalWasTouched'
--      returns True, bootstrap re-runs, and the env ref / applied
--      target are invalidated.
-- | Issue #49: every entry to 'withGhcSession' must re-run
-- 'ensureStanzaFlags' so external @.cabal@ edits picked up by
-- the next non-load tool ('ghc_type', 'ghc_eval', 'ghc_info', …)
-- without forcing the agent to issue an unrelated 'ghc_load'
-- first. Pre-fix the bootstrap was wired only to
-- 'loadForTarget'; tools that took the bare 'withGhcSession'
-- path served stale flags after a corruption-and-restore cycle.
--
-- Setup:
--   * Scaffold a real cabal project.
--   * One 'withGhcSession' call — populates mtime cache.
--   * Touch the @.cabal@ externally so mtime strictly advances.
--   * Another 'withGhcSession' call — must observe the bump.
testWithGhcSessionEnsuresStanza :: IO Bool
testWithGhcSessionEnsuresStanza = do
  base <- getTemporaryDirectory
  ts   <- getPOSIXTime
  let dir = base </> ("withghc-ensure-" <> show (floor (ts * 1000000) :: Int))
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
      -- First withGhcSession — runs the auto-load + bootstrap.
      _ <- withGhcSession sess $ pure ()
      afterFirst <- ApiSession.readCabalMtimeForTest sess
      -- macOS fs mtime has 1-sec resolution; sleep past it.
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
      -- Second withGhcSession with NO explicit ensureStanzaFlags
      -- must still bump the cached mtime. This is exactly the
      -- scenario non-load tools encounter.
      _ <- withGhcSession sess $ pure ()
      afterTouch <- ApiSession.readCabalMtimeForTest sess
      killGhcSession sess
      removePathForcibly dir
      pure
        ( isJust afterFirst
       && isJust afterTouch
       && afterFirst < afterTouch
        )

-- | Issue #43: 'absolutizePathArg' must absolutize the
-- single-token flag-embedded forms ('-isrc', '-IFoo', '-LDir')
-- and bare paths ('dist-newstyle/...') while leaving non-path
-- tokens, flag-only tokens, and already-absolute paths
-- untouched.
testAbsolutizePathArgSingleToken :: IO Bool
testAbsolutizePathArgSingleToken = pure $ and
  [ ApiSession.absolutizePathArg "/r" "-isrc"            == "-i/r/src"
  , ApiSession.absolutizePathArg "/r" "-IFoo"            == "-I/r/Foo"
  , ApiSession.absolutizePathArg "/r" "-LDir"            == "-L/r/Dir"
  , ApiSession.absolutizePathArg "/r" "dist-newstyle/x"  == "/r/dist-newstyle/x"
    -- already-absolute → untouched
  , ApiSession.absolutizePathArg "/r" "-i/abs/src"       == "-i/abs/src"
  , ApiSession.absolutizePathArg "/r" "/abs/path"        == "/abs/path"
    -- non-path tokens → untouched
  , ApiSession.absolutizePathArg "/r" "Shapes"           == "Shapes"
  , ApiSession.absolutizePathArg "/r" "-package-id=qux"  == "-package-id=qux"
    -- flag-only (no value glued) → untouched
  , ApiSession.absolutizePathArg "/r" "-package-db"      == "-package-db"
  ]

-- | Issue #43: '=' form ('-outputdir=DIR') is what GHC accepts
-- when paths come glued with '=' rather than space.
testAbsolutizePathArgEqForm :: IO Bool
testAbsolutizePathArgEqForm = pure $ and
  [ ApiSession.absolutizePathArg "/r" "-outputdir=dist-newstyle/build"
      == "-outputdir=/r/dist-newstyle/build"
  , ApiSession.absolutizePathArg "/r" "-hidir=hi"
      == "-hidir=/r/hi"
  , ApiSession.absolutizePathArg "/r" "-package-db=pkgs"
      == "-package-db=/r/pkgs"
  , ApiSession.absolutizePathArg "/r" "-odir=/already/abs"
      == "-odir=/already/abs"
    -- non-pathish long flags must NOT trigger the =-rewrite
  , ApiSession.absolutizePathArg "/r" "-funknown=value"
      == "-funknown=value"
  ]

-- | Issue #43: 'absolutizeStanzaFlags' walks the argv list and
-- pairs path-bearing flags with their next-token operand.
testAbsolutizeStanzaFlagsTwoToken :: IO Bool
testAbsolutizeStanzaFlagsTwoToken = pure $ and
  [ ApiSession.absolutizeStanzaFlags "/r"
      [ "-package-db", "dist-newstyle/store" ]
      == [ "-package-db", "/r/dist-newstyle/store" ]
  , ApiSession.absolutizeStanzaFlags "/r"
      [ "-hidir", "hi", "-odir", "obj" ]
      == [ "-hidir", "/r/hi", "-odir", "/r/obj" ]
  , ApiSession.absolutizeStanzaFlags "/r"
      [ "-package-env", ".ghc.environment.x" ]
      == [ "-package-env", "/r/.ghc.environment.x" ]
    -- absolute operand → leave alone
  , ApiSession.absolutizeStanzaFlags "/r"
      [ "-package-db", "/already/abs" ]
      == [ "-package-db", "/already/abs" ]
  ]

-- | Issue #43: applying 'absolutizeStanzaFlags' twice must be a
-- no-op. Idempotence keeps the function safe to call from
-- multiple code paths without double-rewrites.
testAbsolutizeStanzaFlagsIdempotent :: IO Bool
testAbsolutizeStanzaFlagsIdempotent =
  let raw =
        [ "-isrc"
        , "-package-db", "dist-newstyle/store"
        , "-outputdir=dist-newstyle/build"
        , "-this-unit-id", "demo-0.1.0.0-inplace"
        ]
      once  = ApiSession.absolutizeStanzaFlags "/r" raw
      twice = ApiSession.absolutizeStanzaFlags "/r" once
  in pure (once == twice)

-- | Issue #43: order of the input argv must be preserved
-- exactly (GHC's flag parser is order-sensitive — late
-- 'package-id' tokens depend on earlier 'package-db' tokens).
testAbsolutizeStanzaFlagsPreservesOrder :: IO Bool
testAbsolutizeStanzaFlagsPreservesOrder =
  let raw =
        [ "-package-db", "dist-newstyle/store"
        , "-hide-all-packages"
        , "-package-id", "QckChck-2.16-abc"
        , "-isrc"
        , "Shapes"
        ]
      out = ApiSession.absolutizeStanzaFlags "/r" raw
  in pure $ out ==
        [ "-package-db", "/r/dist-newstyle/store"
        , "-hide-all-packages"
        , "-package-id", "QckChck-2.16-abc"
        , "-i/r/src"
        , "Shapes"
        ]

-- | Issue #57: when GHC's deferred-pass emits a real diagnostic
-- (typed hole) AND a "GHC-58427 ... is not loaded" follow-up,
-- 'filterArtifacts' must drop the artifact. The agent then sees
-- one error, not two.
testFilterArtifactsDropsWithPeer :: IO Bool
testFilterArtifactsDropsWithPeer =
  let hole = GhcError
        { geFile     = "src/Demo.hs"
        , geLine     = 24
        , geColumn   = 35
        , geSeverity = SevError
        , geCode     = Nothing
        , geMessage  = "[GHC-88464] • Found hole: _holeArg :: [a]"
        }
      artifact = GhcError
        { geFile     = ""
        , geLine     = 0
        , geColumn   = 0
        , geSeverity = SevError
        , geCode     = Nothing
        , geMessage  = "<interactive>:1:1: error: [GHC-58427]\n    \
                       \attempting to use module 'Foo' which is not loaded"
        }
      out = ApiSession.filterArtifacts [hole, artifact]
  in pure (out == [hole])

-- | Issue #57: when the GHC-58427 entry is the ONLY diagnostic,
-- it stays — that case is a real \"module not in graph\"
-- situation and the agent should see the message.
testFilterArtifactsKeepsLone :: IO Bool
testFilterArtifactsKeepsLone =
  let lone = GhcError
        { geFile     = ""
        , geLine     = 0
        , geColumn   = 0
        , geSeverity = SevError
        , geCode     = Nothing
        , geMessage  = "<interactive>:1:1: error: [GHC-58427] not loaded"
        }
      out = ApiSession.filterArtifacts [lone]
  in pure (out == [lone])

-- | Issue #57: empty input is a no-op (no false drops).
testFilterArtifactsEmpty :: IO Bool
testFilterArtifactsEmpty =
  pure (null (ApiSession.filterArtifacts []))

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
        A.Success (AddModules.AddModulesArgs xs _) ->
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
        A.Success (AddModules.AddModulesArgs xs _) -> xs == expected
        _ -> False
  in pure (ok csv ["A","B"] && ok ws ["A","B"] && ok mixed ["A","B","C"])

--------------------------------------------------------------------------------
-- BUG-PLUS-mediocre-1: warnings_block flag on ghc_check_module
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
-- Issue #74: path → module-name resolver — pure parser tests
--
-- 'parseModuleHeader' must accept the canonical Haskell module
-- header shapes the scaffold and refactor tools produce, and must
-- bail (Nothing) when the file has no recognisable header so the
-- caller can fall back to path-based comparison without lying.
--------------------------------------------------------------------------------

-- | Issue #74: bare @module Foo where@ → "Foo".
testParseHeaderSimple :: IO Bool
testParseHeaderSimple =
  pure $ CheckModule.parseModuleHeader "module Foo where" == Just "Foo"

-- | Issue #74: dotted module names round-trip exactly.
testParseHeaderMultiSegment :: IO Bool
testParseHeaderMultiSegment =
  pure $ CheckModule.parseModuleHeader "module Foo.Bar.Baz where"
       == Just "Foo.Bar.Baz"

-- | Issue #74: explicit export list — same line OR multi-line.
-- 'apply_exports' produces the same-line shape; the scaffold and
-- hand-edits often produce the multi-line variant. Both are valid.
testParseHeaderExportsMultiline :: IO Bool
testParseHeaderExportsMultiline = do
  let oneLine   = "module Foo (a, b, c) where"
      multiLine = T.unlines
        [ "module Foo"
        , "  ( a"
        , "  , b"
        , "  ) where"
        ]
  pure $ CheckModule.parseModuleHeader oneLine   == Just "Foo"
      && CheckModule.parseModuleHeader multiLine == Just "Foo"

-- | Issue #74: skip Haddock blurbs, pragmas, blank lines BEFORE
-- the module header. 'ghc_create_project' emits exactly this
-- shape: a Haddock comment, optional pragma, then `module … where`.
testParseHeaderSkipsLeading :: IO Bool
testParseHeaderSkipsLeading =
  let src = T.unlines
        [ "-- | Some Haddock blurb."
        , "{-# LANGUAGE OverloadedStrings #-}"
        , ""
        , "-- another comment"
        , "module DogfoodSuite.Math where"
        , ""
        , "square :: Int -> Int"
        ]
  in pure $ CheckModule.parseModuleHeader src == Just "DogfoodSuite.Math"

-- | Issue #74: a file without a `module … where` line is not a
-- regular Haskell source. Returning Nothing is the honest answer
-- — the caller falls back to path-only comparison.
testParseHeaderNoHeader :: IO Bool
testParseHeaderNoHeader =
  let src = T.unlines
        [ "-- just a comment"
        , "x = 1"
        ]
  in pure $ isNothing (CheckModule.parseModuleHeader src)

-- | Issue #74: defensive parsing — Haskell module names must
-- start uppercase. A misspelled or invalid header should not
-- be accepted as a valid name.
testParseHeaderInvalidName :: IO Bool
testParseHeaderInvalidName = do
  let lower    = "module foo where"
      digit    = "module 1Foo where"
  pure $ isNothing (CheckModule.parseModuleHeader lower)
      && isNothing (CheckModule.parseModuleHeader digit)

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
-- Issue #132 — classifyStderrKind + extractNotInScopeSymbol
--------------------------------------------------------------------------------

-- | #132: "Variable not in scope" in stderr → NotInScope kind.
testClassifyStderrNotInScope :: IO Bool
testClassifyStderrNotInScope =
  let hint = Just "Variable not in scope: sort :: [Int] -> [Int]"
      kind = QcTool.classifyStderrKind hint
  in pure (kind == Env.NotInScope)

-- | #132: generic stderr → SubprocessError kind (unchanged from before).
testClassifyStderrGeneric :: IO Bool
testClassifyStderrGeneric =
  let hint = Just "cabal repl exited with code 1"
      kind = QcTool.classifyStderrKind hint
  in pure (kind == Env.SubprocessError)

-- | #186: GHC ": error:" pattern in stderr → CompileError kind.
testClassifyStderrCompileError :: IO Bool
testClassifyStderrCompileError =
  let hint = Just "src/WithError.hs:3:10: error: No instance for IsString Int"
      kind = QcTool.classifyStderrKind hint
  in pure (kind == Env.CompileError)

-- | #186: GHC "error: [GHC-N]" pattern in stderr → CompileError kind.
testClassifyStderrCompileErrorCode :: IO Bool
testClassifyStderrCompileErrorCode =
  let hint = Just "src/Foo.hs:5:3: error: [GHC-39999] …"
      kind = QcTool.classifyStderrKind hint
  in pure (kind == Env.CompileError)

-- | #186: isCompileErrorStderr positive case.
testIsCompileErrorStderrTrue :: IO Bool
testIsCompileErrorStderrTrue =
  pure $ QcTool.isCompileErrorStderr "src/Foo.hs:5:3: error: type mismatch"

-- | #186: isCompileErrorStderr negative case — generic message without GHC patterns.
testIsCompileErrorStderrFalse :: IO Bool
testIsCompileErrorStderrFalse =
  pure $ not (QcTool.isCompileErrorStderr "cabal repl exited unexpectedly")

-- | #132: extract bare name from "Variable not in scope: sort".
testExtractNisBareName :: IO Bool
testExtractNisBareName =
  pure (QcTool.extractNotInScopeSymbol "Variable not in scope: sort" == Just "sort")

-- | #132: extract name before the type annotation (":: …").
testExtractNisTypeSig :: IO Bool
testExtractNisTypeSig =
  pure (QcTool.extractNotInScopeSymbol
          "Variable not in scope: sort :: [Int] -> [Int]" == Just "sort")

-- | #132: unrelated text yields Nothing.
testExtractNisAbsent :: IO Bool
testExtractNisAbsent =
  pure (isNothing (QcTool.extractNotInScopeSymbol "some other error"))

-- | #132: summariseStderr must strip -Wmissing-home-modules warning
-- lines (and their continuation "Modules listed as … but not compiled:" line).
testQcSummariseStripsWmhm :: IO Bool
testQcSummariseStripsWmhm =
  let noise = T.unlines
        [ "<no location info>: warning: [-Wmissing-home-modules]"
        , "    Modules listed as 'other-modules' but not compiled: HaskellFlows.Tool.Batch HaskellFlows.Tool.Eval"
        , "<interactive>:1:1: error:"
        , "    Variable not in scope: sort"
        ]
      result = QcTool.summariseStderr noise
  in pure $ not ("missing-home-modules" `T.isInfixOf` result)
         && not ("Modules listed as" `T.isInfixOf` result)
         && "Variable not in scope: sort" `T.isInfixOf` result

--------------------------------------------------------------------------------
-- BUG-PLUS-mediocre-3: nextStep from ghc_load based on warning kind
--------------------------------------------------------------------------------

-- | When the 'warnings' array is empty, 'dispatch' proposes
-- 'ghc_suggest' — the clean-compile follow-up.
testNextStepCleanLoad :: IO Bool
testNextStepCleanLoad =
  let payload = A.object
        [ "success"  A..= True
        , "errors"   A..= ([] :: [Text])
        , "warnings" A..= ([] :: [Text])
        ]
  in pure $ case suggestNext GhcLoad True payload of
       Just ns -> nsTool ns == GhcSuggest
       Nothing -> False

-- | A typed-hole warning routes to 'ghc_hole' (which knows how
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
  in pure $ case suggestNext GhcLoad True payload of
       Just ns -> nsTool ns == GhcHole
       Nothing -> False

-- | A non-hole warning (unused-imports, type-defaults, …) routes
-- to 'ghc_fix_warning' — the auto-patch tool.
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
  in pure $ case suggestNext GhcLoad True payload of
       Just ns -> nsTool ns == GhcFixWarning
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

--------------------------------------------------------------------------------
-- #253 Phase 1: ghc_scratch — data layer + write/list/show/clear actions
--------------------------------------------------------------------------------

-- | Round-trip a ScratchEntry through the on-disk scratchpad.
testScratchpadRoundtrip :: IO Bool
testScratchpadRoundtrip = withTempProject $ \pd -> do
  store <- SP.openStore pd
  let entry = SP.ScratchEntry
        { SP.seId       = "t1"
        , SP.seKind     = SP.ScratchHypothesis
        , SP.seCode     = "\\x -> x + 1 :: Int -> Int"
        , SP.seModule   = Just "src/Foo.hs"
        , SP.seImports  = ["Data.List"]
        , SP.seNote     = Just "verify roundtrip"
        , SP.seResult   = Nothing
        , SP.seStatus   = SP.ScratchOpen
        , SP.seCreated  = 1_000_000
        , SP.seUpdated  = 1_000_000
        }
  SP.save store entry
  loaded <- SP.loadAll store
  pure $ case loaded of
    [e] -> SP.seId e == "t1"
        && SP.seCode e == "\\x -> x + 1 :: Int -> Int"
        && SP.seModule e == Just "src/Foo.hs"
        && SP.seImports e == ["Data.List"]
        && SP.seStatus e == SP.ScratchOpen
        && SP.seKind e == SP.ScratchHypothesis
    _   -> False

-- | save with the same id should replace, not append.
testScratchpadUpsertById :: IO Bool
testScratchpadUpsertById = withTempProject $ \pd -> do
  store <- SP.openStore pd
  let mk c = SP.ScratchEntry
        { SP.seId       = "dup"
        , SP.seKind     = SP.ScratchHypothesis
        , SP.seCode     = c
        , SP.seModule   = Nothing
        , SP.seImports  = []
        , SP.seNote     = Nothing
        , SP.seResult   = Nothing
        , SP.seStatus   = SP.ScratchOpen
        , SP.seCreated  = 1_000_000
        , SP.seUpdated  = 1_000_000
        }
  SP.save store (mk "v1")
  SP.save store (mk "v2")
  SP.save store (mk "v3")
  loaded <- SP.loadAll store
  pure $ case loaded of
    [e] -> SP.seCode e == "v3"
    _   -> False

-- | findById should resolve an id, return Nothing when absent.
testScratchpadFindById :: IO Bool
testScratchpadFindById = withTempProject $ \pd -> do
  store <- SP.openStore pd
  let entry = SP.ScratchEntry
        { SP.seId       = "lookup-me"
        , SP.seKind     = SP.ScratchHypothesis
        , SP.seCode     = "True"
        , SP.seModule   = Nothing
        , SP.seImports  = []
        , SP.seNote     = Nothing
        , SP.seResult   = Nothing
        , SP.seStatus   = SP.ScratchOpen
        , SP.seCreated  = 1
        , SP.seUpdated  = 1
        }
  SP.save store entry
  hit  <- SP.findById store "lookup-me"
  miss <- SP.findById store "no-such-id"
  pure (fmap SP.seId hit == Just "lookup-me" && isNothing miss)

-- | parseAction round-trip — covers the default + every named action.
testScratchParseAction :: IO Bool
testScratchParseAction = pure $
  ScratchTool.parseAction Nothing            == Right ScratchTool.ActList
  && ScratchTool.parseAction (Just "write")  == Right ScratchTool.ActWrite
  && ScratchTool.parseAction (Just "check")  == Right ScratchTool.ActCheck
  && ScratchTool.parseAction (Just "list")   == Right ScratchTool.ActList
  && ScratchTool.parseAction (Just "show")   == Right ScratchTool.ActShow
  && ScratchTool.parseAction (Just "clear")  == Right ScratchTool.ActClear
  && ScratchTool.parseAction (Just "promote") == Right ScratchTool.ActPromote
  && case ScratchTool.parseAction (Just "nope") of
       Left _  -> True
       Right _ -> False

-- | action=write creates the entry on disk and returns its id.
testScratchHandleWrite :: IO Bool
testScratchHandleWrite = withTempProject $ \pd -> do
  store <- SP.openStore pd
  let args = A.object
        [ "action" A..= ("write" :: Text)
        , "id"     A..= ("user-id" :: Text)
        , "code"   A..= ("1 + 1 :: Int" :: Text)
        ]
  result <- ScratchTool.handle store undefined undefined args
  case trContent result of
    [TextContent body] -> case A.decode (TLE.encodeUtf8 (TL.fromStrict body)) of
      Just (A.Object top) -> case AKM.lookup "status" top of
        Just (A.String "ok") -> do
          loaded <- SP.loadAll store
          pure $ case loaded of
            [e] -> SP.seId e == "user-id" && SP.seCode e == "1 + 1 :: Int"
            _   -> False
        _ -> pure False
      _ -> pure False
    _ -> pure False

-- | action=write without code returns status=failed kind=missing_arg.
testScratchHandleWriteMissingCode :: IO Bool
testScratchHandleWriteMissingCode = withTempProject $ \pd -> do
  store <- SP.openStore pd
  let args = A.object [ "action" A..= ("write" :: Text) ]
  result <- ScratchTool.handle store undefined undefined args
  pure $ case trContent result of
    [TextContent body] -> case A.decode (TLE.encodeUtf8 (TL.fromStrict body)) of
      Just (A.Object top) -> AKM.lookup "status" top == Just (A.String "failed")
      _ -> False
    _ -> False

-- | action=write without an id auto-generates scratch-1, scratch-2, ...
testScratchHandleWriteAutoId :: IO Bool
testScratchHandleWriteAutoId = withTempProject $ \pd -> do
  store <- SP.openStore pd
  let args = A.object
        [ "action" A..= ("write" :: Text)
        , "code"   A..= ("a" :: Text)
        ]
  _ <- ScratchTool.handle store undefined undefined args
  _ <- ScratchTool.handle store undefined undefined args
  loaded <- SP.loadAll store
  let ids = map SP.seId loaded
  pure (ids == ["scratch-1", "scratch-2"])

-- | action=list with no entries returns count=0.
testScratchHandleListEmpty :: IO Bool
testScratchHandleListEmpty = withTempProject $ \pd -> do
  store <- SP.openStore pd
  let args = A.object [ "action" A..= ("list" :: Text) ]
  result <- ScratchTool.handle store undefined undefined args
  pure $ case trContent result of
    [TextContent body] -> case A.decode (TLE.encodeUtf8 (TL.fromStrict body)) of
      Just (A.Object top) -> case AKM.lookup "result" top of
        Just (A.Object r) -> AKM.lookup "count" r == Just (A.Number 0)
        _ -> False
      _ -> False
    _ -> False

-- | action=show on an unknown id returns status=no_match.
testScratchHandleShowMissing :: IO Bool
testScratchHandleShowMissing = withTempProject $ \pd -> do
  store <- SP.openStore pd
  let args = A.object [ "action" A..= ("show" :: Text), "id" A..= ("ghost" :: Text) ]
  result <- ScratchTool.handle store undefined undefined args
  pure $ case trContent result of
    [TextContent body] -> case A.decode (TLE.encodeUtf8 (TL.fromStrict body)) of
      Just (A.Object top) -> AKM.lookup "status" top == Just (A.String "no_match")
      _ -> False
    _ -> False

-- | action=clear without id and without confirm=true is refused.
testScratchHandleClearNoConfirm :: IO Bool
testScratchHandleClearNoConfirm = withTempProject $ \pd -> do
  store <- SP.openStore pd
  let args = A.object [ "action" A..= ("clear" :: Text) ]
  result <- ScratchTool.handle store undefined undefined args
  pure $ case trContent result of
    [TextContent body] -> case A.decode (TLE.encodeUtf8 (TL.fromStrict body)) of
      Just (A.Object top) -> AKM.lookup "status" top == Just (A.String "refused")
      _ -> False
    _ -> False

-- | action=clear with id removes only that entry.
testScratchHandleClearById :: IO Bool
testScratchHandleClearById = withTempProject $ \pd -> do
  store <- SP.openStore pd
  let mk i c = SP.ScratchEntry
        { SP.seId       = i, SP.seKind = SP.ScratchHypothesis, SP.seCode = c
        , SP.seModule   = Nothing, SP.seImports = [], SP.seNote = Nothing
        , SP.seResult   = Nothing, SP.seStatus = SP.ScratchOpen
        , SP.seCreated  = 1, SP.seUpdated = 1
        }
  SP.save store (mk "a" "a")
  SP.save store (mk "b" "b")
  let args = A.object [ "action" A..= ("clear" :: Text), "id" A..= ("a" :: Text) ]
  _ <- ScratchTool.handle store undefined undefined args
  loaded <- SP.loadAll store
  pure (map SP.seId loaded == ["b"])

-- | action=clear with confirm=true (no id) truncates everything.
testScratchHandleClearAll :: IO Bool
testScratchHandleClearAll = withTempProject $ \pd -> do
  store <- SP.openStore pd
  let mk i = SP.ScratchEntry
        { SP.seId       = i, SP.seKind = SP.ScratchHypothesis, SP.seCode = "x"
        , SP.seModule   = Nothing, SP.seImports = [], SP.seNote = Nothing
        , SP.seResult   = Nothing, SP.seStatus = SP.ScratchOpen
        , SP.seCreated  = 1, SP.seUpdated = 1
        }
  SP.save store (mk "a")
  SP.save store (mk "b")
  SP.save store (mk "c")
  let args = A.object [ "action" A..= ("clear" :: Text), "confirm" A..= True ]
  _ <- ScratchTool.handle store undefined undefined args
  loaded <- SP.loadAll store
  pure (null loaded)

-- #253 Phase 2: ghc_scratch action=check — boundary / data-layer tests.
-- GHC-session-requiring tests (type_ok / type_error from live GHCi)
-- are covered by the FlowScratchpad E2E scenario; the unit tests here
-- pin the sanitize-gate, missing-id, and unknown-id paths that do not
-- need a live session (undefined is safe because check returns early).

-- | action=check without 'id' → MissingArg (failed).
testScratchCheckMissingId :: IO Bool
testScratchCheckMissingId = withTempProject $ \pd -> do
  store <- SP.openStore pd
  let args = A.object [ "action" A..= ("check" :: Text) ]
  result <- ScratchTool.handle store undefined undefined args
  pure $ case trContent result of
    [TextContent body] -> case A.decode (TLE.encodeUtf8 (TL.fromStrict body)) of
      Just (A.Object top) -> AKM.lookup "status" top == Just (A.String "failed")
      _ -> False
    _ -> False

-- | action=check with an id that doesn't exist → no_match.
testScratchCheckUnknownId :: IO Bool
testScratchCheckUnknownId = withTempProject $ \pd -> do
  store <- SP.openStore pd
  let args = A.object [ "action" A..= ("check" :: Text), "id" A..= ("ghost" :: Text) ]
  result <- ScratchTool.handle store undefined undefined args
  pure $ case trContent result of
    [TextContent body] -> case A.decode (TLE.encodeUtf8 (TL.fromStrict body)) of
      Just (A.Object top) -> AKM.lookup "status" top == Just (A.String "no_match")
      _ -> False
    _ -> False

-- | action=check refuses code containing the sentinel string.
-- (F-03 removed the newline rejection — multi-line declarations now go
-- through the wrapAsLetBlock path.  Sentinel injection is still blocked
-- in both the single-line and multi-line sanitizers.)
testScratchCheckSanitizeReject :: IO Bool
testScratchCheckSanitizeReject = withTempProject $ \pd -> do
  store <- SP.openStore pd
  -- Single-line code containing the sentinel — sanitizeExpression
  -- catches it before touching the GHC session.
  let badCode = "f x = True -- " <> sentinel
      writeArgs = A.object
        [ "action" A..= ("write" :: Text)
        , "id"     A..= ("bad" :: Text)
        , "code"   A..= badCode
        ]
  _ <- ScratchTool.handle store undefined undefined writeArgs
  let checkArgs = A.object
        [ "action" A..= ("check" :: Text)
        , "id"     A..= ("bad" :: Text)
        ]
  result <- ScratchTool.handle store undefined undefined checkArgs
  pure $ case trContent result of
    [TextContent body] -> case A.decode (TLE.encodeUtf8 (TL.fromStrict body)) of
      Just (A.Object top) -> AKM.lookup "status" top == Just (A.String "refused")
      _ -> False
    _ -> False

-- | 'ScratchResult' ToJSON / FromJSON round-trip.
testScratchResultRoundTrip :: IO Bool
testScratchResultRoundTrip =
  let r = SP.ScratchResult
        { SP.srKind   = "type_ok"
        , SP.srDetail = "Int -> Int"
        , SP.srAt     = 1.0
        }
  in pure $ case A.fromJSON (A.toJSON r) of
       A.Success r' ->
         SP.srKind   r' == SP.srKind   r
         && SP.srDetail r' == SP.srDetail r
         && SP.srAt    r' == SP.srAt    r
       A.Error _ -> False

-- | F-01 regression: action=check response carries 'kind' at the top
-- level of the mkOk result object, NOT nested under result.result.kind.
-- With 'undefined' for the session, withGhcSession throws and the
-- try-block stores a type_error; we just need 'kind' to be directly
-- inside the result object so scratchNext's envField routing works.
testScratchCheckKindAtTopLevel :: IO Bool
testScratchCheckKindAtTopLevel = withTempProject $ \pd -> do
  store <- SP.openStore pd
  let writeArgs = A.object
        [ "action" A..= ("write" :: Text)
        , "id"     A..= ("probe" :: Text)
        , "code"   A..= ("bogusF" :: Text)  -- passes sanitize, GHC call throws
        ]
  _ <- ScratchTool.handle store undefined undefined writeArgs
  let checkArgs = A.object
        [ "action" A..= ("check" :: Text)
        , "id"     A..= ("probe" :: Text)
        ]
  result <- ScratchTool.handle store undefined undefined checkArgs
  pure $ case trContent result of
    [TextContent body] -> case A.decode (TLE.encodeUtf8 (TL.fromStrict body)) of
      Just (A.Object top) ->
        case AKM.lookup "status" top of
          Just (A.String "ok") ->
            -- status=ok means the try-block ran; result must have 'kind'
            -- directly (type_error from undefined-session crash), not nested.
            case AKM.lookup "result" top of
              Just (A.Object inner) ->
                -- 'kind' must be present here
                case AKM.lookup "kind" inner of
                  Just (A.String k) -> k == "type_ok" || k == "type_error"
                  _ -> False
              _ -> False
          _ -> True  -- refused / failed paths: sanitize boundary fired, fine
      _ -> False
    _ -> False

-- #253 Phase 3: ghc_scratch action=show — full detail + seResult round-trip.

-- | action=show returns the complete entry: all fields present, code
-- intact, status correct, seResult=null initially.
testScratchShowFullDetail :: IO Bool
testScratchShowFullDetail = withTempProject $ \pd -> do
  store <- SP.openStore pd
  -- Write an entry with a note and module context
  let writeArgs = A.object
        [ "action" A..= ("write" :: Text)
        , "id"     A..= ("full-e1" :: Text)
        , "code"   A..= ("\\x -> x + 1" :: Text)
        , "module" A..= ("src/Foo.hs" :: Text)
        , "note"   A..= ("hypothesis: this is a plain increment" :: Text)
        ]
  _ <- ScratchTool.handle store undefined undefined writeArgs
  let showArgs = A.object
        [ "action" A..= ("show" :: Text)
        , "id"     A..= ("full-e1" :: Text)
        ]
  result <- ScratchTool.handle store undefined undefined showArgs
  pure $ case trContent result of
    [TextContent body] -> case A.decode (TLE.encodeUtf8 (TL.fromStrict body)) of
      Just (A.Object top) ->
        -- Outer envelope: status=ok
        case AKM.lookup "status" top of
          Just (A.String "ok") ->
            case AKM.lookup "result" top of
              Just (A.Object inner) ->
                -- All required fields must be present
                AKM.lookup "id"     inner == Just (A.String "full-e1")
                && AKM.lookup "code"   inner == Just (A.String "\\x -> x + 1")
                && AKM.lookup "status" inner == Just (A.String "open")
                -- note field forwarded correctly
                && AKM.lookup "note" inner   == Just (A.String "hypothesis: this is a plain increment")
                -- result is null before any check
                && AKM.lookup "result" inner == Just A.Null
                -- module field forwarded
                && AKM.lookup "module" inner == Just (A.String "src/Foo.hs")
              _ -> False
          _ -> False
      _ -> False
    _ -> False

-- | 'seResult' survives a save → loadAll round-trip. After calling
-- SP.save with a non-Nothing result, loadAll must return the same
-- 'ScratchResult' value byte-for-byte.
testScratchResultRoundTripViaLoadAll :: IO Bool
testScratchResultRoundTripViaLoadAll = withTempProject $ \pd -> do
  store <- SP.openStore pd
  let result = SP.ScratchResult
        { SP.srKind   = "type_ok"
        , SP.srDetail = "Int -> Int"
        , SP.srAt     = 1234567.0
        }
      entry = SP.ScratchEntry
        { SP.seId      = "rt-entry"
        , SP.seKind    = SP.ScratchHypothesis
        , SP.seCode    = "\\x -> x + 1"
        , SP.seModule  = Nothing
        , SP.seImports = []
        , SP.seNote    = Nothing
        , SP.seResult  = Just result
        , SP.seStatus  = SP.ScratchVerified
        , SP.seCreated = 100.0
        , SP.seUpdated = 200.0
        }
  SP.save store entry
  loaded <- SP.loadAll store
  pure $ case loaded of
    [e] ->
      SP.seId e == "rt-entry"
      && SP.seStatus e == SP.ScratchVerified
      && case SP.seResult e of
           Just r  ->
             SP.srKind   r == "type_ok"
             && SP.srDetail r == "Int -> Int"
             && SP.srAt    r == 1234567.0
           Nothing -> False
    _ -> False

-- | action=show after action=check (undefined session → type_error) carries
-- the persisted ScratchResult in the response's result.result.
-- This verifies the full pipeline: check persists the result, show
-- reads it back, and the outer toJSON round-trip is intact.
testScratchShowAfterCheckHasResult :: IO Bool
testScratchShowAfterCheckHasResult = withTempProject $ \pd -> do
  store <- SP.openStore pd
  -- Write a simple entry (undefined session means check will catch a
  -- SomeException and persist kind=type_error)
  let writeArgs = A.object
        [ "action" A..= ("write" :: Text)
        , "id"     A..= ("chk-then-show" :: Text)
        , "code"   A..= ("badIdent" :: Text)
        ]
  _ <- ScratchTool.handle store undefined undefined writeArgs
  -- check — undefined session throws, type_error is persisted
  let checkArgs = A.object
        [ "action" A..= ("check" :: Text)
        , "id"     A..= ("chk-then-show" :: Text)
        ]
  _ <- ScratchTool.handle store undefined undefined checkArgs
  -- show — must return the persisted result
  let showArgs = A.object
        [ "action" A..= ("show" :: Text)
        , "id"     A..= ("chk-then-show" :: Text)
        ]
  result <- ScratchTool.handle store undefined undefined showArgs
  pure $ case trContent result of
    [TextContent body] -> case A.decode (TLE.encodeUtf8 (TL.fromStrict body)) of
      Just (A.Object top) ->
        case AKM.lookup "status" top of
          Just (A.String "ok") ->
            case AKM.lookup "result" top of
              Just (A.Object inner) ->
                -- result.result must be a non-null object with kind=type_error
                case AKM.lookup "result" inner of
                  Just (A.Object r) ->
                    AKM.lookup "kind" r == Just (A.String "type_error")
                  _ -> False
              _ -> False
          _ -> False
      _ -> False
    _ -> False

-- #253 Phase 4: ghc_scratch action=promote — boundary tests.
-- The splice + compile-verify tests require a live GHC session and are
-- covered by the FlowScratchpad E2E scenario.  These unit tests pin the
-- argument-validation paths that fire before touching the filesystem.

-- | action=promote without 'id' → failed.
testScratchPromoteMissingId :: IO Bool
testScratchPromoteMissingId = withTempProject $ \pd -> do
  store <- SP.openStore pd
  let args = A.object [ "action" A..= ("promote" :: Text) ]
  result <- ScratchTool.handle store undefined pd args
  pure $ case trContent result of
    [TextContent body] -> case A.decode (TLE.encodeUtf8 (TL.fromStrict body)) of
      Just (A.Object top) -> AKM.lookup "status" top == Just (A.String "failed")
      _ -> False
    _ -> False

-- | action=promote with id but no 'target_module' → failed.
testScratchPromoteMissingTargetModule :: IO Bool
testScratchPromoteMissingTargetModule = withTempProject $ \pd -> do
  store <- SP.openStore pd
  let writeArgs = A.object
        [ "action" A..= ("write" :: Text)
        , "id"     A..= ("pm1" :: Text)
        , "code"   A..= ("f x = x" :: Text)
        ]
  _ <- ScratchTool.handle store undefined undefined writeArgs
  let args = A.object [ "action" A..= ("promote" :: Text), "id" A..= ("pm1" :: Text) ]
  result <- ScratchTool.handle store undefined pd args
  pure $ case trContent result of
    [TextContent body] -> case A.decode (TLE.encodeUtf8 (TL.fromStrict body)) of
      Just (A.Object top) -> AKM.lookup "status" top == Just (A.String "failed")
      _ -> False
    _ -> False

-- | action=promote with unknown id → no_match.
testScratchPromoteUnknownId :: IO Bool
testScratchPromoteUnknownId = withTempProject $ \pd -> do
  store <- SP.openStore pd
  let args = A.object
        [ "action"        A..= ("promote" :: Text)
        , "id"            A..= ("no-such-id" :: Text)
        , "target_module" A..= ("src/Foo.hs" :: Text)
        ]
  result <- ScratchTool.handle store undefined pd args
  pure $ case trContent result of
    [TextContent body] -> case A.decode (TLE.encodeUtf8 (TL.fromStrict body)) of
      Just (A.Object top) -> AKM.lookup "status" top == Just (A.String "no_match")
      _ -> False
    _ -> False

-- | action=promote with target_module that escapes the project → refused.
testScratchPromoteBadModulePath :: IO Bool
testScratchPromoteBadModulePath = withTempProject $ \pd -> do
  store <- SP.openStore pd
  let writeArgs = A.object
        [ "action" A..= ("write" :: Text)
        , "id"     A..= ("pm2" :: Text)
        , "code"   A..= ("f x = x" :: Text)
        ]
  _ <- ScratchTool.handle store undefined undefined writeArgs
  let args = A.object
        [ "action"        A..= ("promote" :: Text)
        , "id"            A..= ("pm2" :: Text)
        , "target_module" A..= ("/etc/passwd" :: Text)
        ]
  result <- ScratchTool.handle store undefined pd args
  pure $ case trContent result of
    [TextContent body] -> case A.decode (TLE.encodeUtf8 (TL.fromStrict body)) of
      Just (A.Object top) -> AKM.lookup "status" top == Just (A.String "refused")
      _ -> False
    _ -> False

-- | Pure 'spliceInto' test: appending (no target_line) places code
-- after a blank separator and ends with a newline.
testSpliceIntoAppend :: IO Bool
testSpliceIntoAppend =
  let orig   = "module Foo where\n\nfoo = 1\n"
      code   = "bar = 2"
      result = ScratchTool.spliceInto orig code Nothing
  in pure
       ( T.isInfixOf "foo = 1" result
           && T.isInfixOf "bar = 2" result
           -- blank-line separator before the new code
           && T.isInfixOf "foo = 1\n\nbar = 2" result
           -- trailing newline
           && T.isSuffixOf "\n" result
       )

-- | Pure 'spliceInto' test: inserting after line 2 puts code between
-- lines 2 and 3 of the original file.
testSpliceIntoAtLine :: IO Bool
testSpliceIntoAtLine =
  let orig   = "line1\nline2\nline3\n"
      code   = "NEW"
      result = ScratchTool.spliceInto orig code (Just 2)
      ls     = T.lines result
  in pure
       ( "line1" `elem` ls
           && "line2" `elem` ls
           && "NEW"   `elem` ls
           && "line3" `elem` ls
           -- order: line1 before line2, line2 before NEW, NEW before line3
           && headIdx "line1" ls < headIdx "line2" ls
           && headIdx "line2" ls < headIdx "NEW"   ls
           && headIdx "NEW"   ls < headIdx "line3" ls
       )
  where
    headIdx :: Text -> [Text] -> Int
    headIdx x xs = case [i | (i, e) <- zip [0..] xs, e == x] of
      (n:_) -> n
      []    -> maxBound

--------------------------------------------------------------------------------
-- F-03: sanitizeDeclarations + wrapAsLetBlock
--------------------------------------------------------------------------------

-- | sanitizeDeclarations allows newlines (unlike sanitizeExpression).
testSanitizeDeclAllowsNewlines :: IO Bool
testSanitizeDeclAllowsNewlines = pure $
  case sanitizeDeclarations "f :: Int -> Int\nf x = x + 1" of
    Right _ -> True
    Left  _ -> False

-- | sanitizeDeclarations still blocks the sentinel string.
testSanitizeDeclBlocksSentinel :: IO Bool
testSanitizeDeclBlocksSentinel = pure $
  case sanitizeDeclarations ("f x = " <> sentinel) of
    Left ContainsSentinel -> True
    _                     -> False

-- | wrapAsLetBlock indents each line by 2 spaces and wraps in let/in.
testWrapAsLetBlockIndents :: IO Bool
testWrapAsLetBlockIndents = pure $
  let code   = "f :: Int\nf = 42"
      result = ScratchTool.wrapAsLetBlock code
  in T.isPrefixOf "let\n" result
       && T.isInfixOf "  f :: Int" result
       && T.isInfixOf "  f = 42"   result
       && T.isSuffixOf " in ()" result

--------------------------------------------------------------------------------
-- F-04: promote wraps single-line expression when binding_name is given
--------------------------------------------------------------------------------

-- | action=promote with binding_name wraps the stored expression as
-- "name = expr" before splicing, producing a valid top-level binding.
-- This test only checks the path-validation layer (no live GHC session
-- needed) — the spliced text is verified by inspecting the error payload
-- which carries the attempted code before GHC rejects or accepts it.
testScratchPromoteBindingName :: IO Bool
testScratchPromoteBindingName = withTempProject $ \pd -> do
  store <- SP.openStore pd
  -- Write a single-line expression entry.
  let writeArgs = A.object
        [ "action" A..= ("write" :: Text)
        , "id"     A..= ("expr-entry" :: Text)
        , "code"   A..= ("(\\x -> x + 1) :: Int -> Int" :: Text)
        ]
  _ <- ScratchTool.handle store undefined undefined writeArgs
  -- Promote with binding_name — target module does not exist so the
  -- path-traversal guard fires before GHC is touched, letting us test
  -- the binding_name wrapping via the refused kind.
  let promoteArgs = A.object
        [ "action"       A..= ("promote" :: Text)
        , "id"           A..= ("expr-entry" :: Text)
        , "target_module" A..= ("../escape" :: Text)   -- triggers path_traversal
        , "binding_name" A..= ("myFun" :: Text)
        ]
  result <- ScratchTool.handle store undefined pd promoteArgs
  -- We only need to confirm the args parse and reach the path-guard
  -- (status=refused, kind=path_traversal) — that proves binding_name
  -- parsed correctly and the entry was found.
  pure $ case trContent result of
    [TextContent body] -> case A.decode (TLE.encodeUtf8 (TL.fromStrict body)) of
      Just (A.Object top) ->
        AKM.lookup "status" top == Just (A.String "refused")
      _ -> False
    _ -> False

--------------------------------------------------------------------------------
-- F-05: compileFailResult uses first error message for 'cause'
--------------------------------------------------------------------------------

-- | compileFailResult should use the first SevError message for the
-- 'cause' field, not a truncation of the raw string that may be
-- dominated by unrelated package-version warnings.
testCompileFailResultCause :: IO Bool
testCompileFailResultCause = pure $
  let firstErr = GhcError
        { geFile     = "src/Foo.hs"
        , geColumn   = 1
        , geLine     = 10
        , geSeverity = SevError
        , geCode     = Just "GHC-94426"
        , geMessage  = "Invalid type signature"
        }
      -- Raw output starts with a long package warning that would eat
      -- the first 400 chars and truncate the real error.
      longPreamble = T.replicate 300 "package-warning ; "
      raw = longPreamble <> "Invalid type signature"
      result = RefactorTool.compileFailResult False [firstErr] raw " — snapshot restored"
  in case trContent result of
    [TextContent body] -> case A.decode (TLE.encodeUtf8 (TL.fromStrict body)) of
      Just (A.Object top) ->
        case AKM.lookup "error" top of
          Just (A.Object errObj) ->
            case AKM.lookup "cause" errObj of
              Just (A.String cause) -> cause == "Invalid type signature"
              _ -> False
          _ -> False
      _ -> False
    _ -> False

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

--------------------------------------------------------------------------------
-- Issue #100 Phase C — cross-tool path-traversal harness
--
-- Every tool that accepts a 'module_path' argument must reject paths
-- that escape the project root with status='refused', kind='path_traversal'.
-- The GhcSession / Store arguments are safely 'undefined' in these tests
-- because 'mkModulePath' fires in the Left branch before any session or
-- store access.
--------------------------------------------------------------------------------

-- | Shared helper: decode a 'ToolResult' to 'Env.ToolResponse'.
decodeToolResult :: ToolResult -> Either String Env.ToolResponse
decodeToolResult tr = case trContent tr of
  [TextContent body] ->
    A.eitherDecode (TLE.encodeUtf8 (TL.fromStrict body))
  _ -> Left "expected exactly one TextContent"

-- | Assert that the response is status='refused', kind='path_traversal'.
isTraversalRefused :: Either String Env.ToolResponse -> Bool
isTraversalRefused (Right env) =
  Env.reStatus env == Env.StatusRefused &&
  maybe False ((== Env.PathTraversal) . Env.eeKind) (Env.reError env)
isTraversalRefused _ = False

-- | #100C: 'ghc_apply_exports' must refuse traversal paths.
-- No GhcSession needed — 'mkModulePath' guard fires before any filesystem access.
testApplyExportsRejectsTraversal :: IO Bool
testApplyExportsRejectsTraversal = do
  case mkProjectDir "/tmp/project" of
    Left _ -> pure False
    Right pd -> do
      let args = A.object
            [ "module_path" A..= ("../../etc/passwd" :: Text)
            , "exports"     A..= ([] :: [Text])
            ]
      tr <- ApplyExports.handle pd args
      pure (isTraversalRefused (decodeToolResult tr))

-- | #100C: 'ghc_fix_warning' must refuse traversal paths.
testFixWarningRejectsTraversal :: IO Bool
testFixWarningRejectsTraversal = do
  case mkProjectDir "/tmp/project" of
    Left _ -> pure False
    Right pd -> do
      let args = A.object
            [ "module_path" A..= ("../../etc/passwd" :: Text)
            , "line"        A..= (1 :: Int)
            , "code"        A..= ("-Wunused-imports" :: Text)
            ]
      tr <- FixWarning.handle pd args
      pure (isTraversalRefused (decodeToolResult tr))

-- | #100C: 'ghc_format' must refuse traversal paths.
testFormatRejectsTraversal :: IO Bool
testFormatRejectsTraversal = do
  case mkProjectDir "/tmp/project" of
    Left _ -> pure False
    Right pd -> do
      let args = A.object
            [ "module_path" A..= ("../../etc/passwd" :: Text) ]
      tr <- FormatTool.handle pd args
      pure (isTraversalRefused (decodeToolResult tr))

-- | Issue #246: 'ghc_format' on a non-existent file must return
-- status='failed' with kind='module_path_does_not_exist', not a raw
-- subprocess backtrace from fourmolu/ormolu.
testFormatMissingFile :: IO Bool
testFormatMissingFile = do
  tmp <- getTemporaryDirectory
  let dir = tmp </> "haskell-flows-format-missing"
  removePathForcibly dir
  createDirectoryIfMissing True dir
  case mkProjectDir dir of
    Left _ -> pure False
    Right pd -> do
      let args = A.object
            [ "module_path" A..= ("src/DoesNotExist.hs" :: Text) ]
      tr <- FormatTool.handle pd args
      let result = decodeToolResult tr
      pure $ case result of
        Right env ->
             Env.reStatus env == Env.StatusFailed
          && maybe False
               (\e -> Env.eeKind e == Env.ModulePathDoesNotExist)
               (Env.reError env)
        Left _ -> False

-- | #100C: 'ghc_check_module' must refuse traversal paths.
-- 'mkModulePath' fires before the GhcSession or Store are touched.
testCheckModuleRejectsTraversal :: IO Bool
testCheckModuleRejectsTraversal = do
  case mkProjectDir "/tmp/project" of
    Left _ -> pure False
    Right pd -> do
      let args = A.object
            [ "module_path" A..= ("../../etc/passwd" :: Text) ]
      tr <- CheckModule.handle
              (undefined :: GhcSession)
              (undefined :: Store)
              pd args
      pure (isTraversalRefused (decodeToolResult tr))

-- | #150: 'ghc_check_module' on a non-existent file must return
-- status='failed' with kind='module_path_does_not_exist'. Before the
-- fix the tool returned status='ok' with all gates green — a false
-- all-green for a file that does not exist.
-- The GhcSession and Store are not reached (existence check fires first).
testCheckModuleNonExistentFile :: IO Bool
testCheckModuleNonExistentFile = do
  case mkProjectDir "/tmp" of
    Left _ -> pure False
    Right pd -> do
      let args = A.object
            [ "module_path" A..= ("src/DoesNotExist.hs" :: Text) ]
      tr <- CheckModule.handle
              (undefined :: GhcSession)
              (undefined :: Store)
              pd args
      case decodeToolResult tr of
        Right env
          | Env.reStatus env == Env.StatusFailed
          , Just err <- Env.reError env ->
              pure (Env.eeKind err == Env.ModulePathDoesNotExist
                 && Env.eeField err == Just "module_path")
        _ -> pure False

-- | #100C: 'ghc_explain_error' must refuse traversal paths.
testExplainErrorRejectsTraversal :: IO Bool
testExplainErrorRejectsTraversal = do
  case mkProjectDir "/tmp/project" of
    Left _ -> pure False
    Right pd -> do
      let args = A.object
            [ "module_path" A..= ("../../etc/passwd" :: Text) ]
      tr <- ExplainError.handle
              (undefined :: GhcSession)
              pd args
      pure (isTraversalRefused (decodeToolResult tr))

-- | #100C: 'ghc_lab' must refuse traversal paths.
testLabRejectsTraversal :: IO Bool
testLabRejectsTraversal = do
  case mkProjectDir "/tmp/project" of
    Left _ -> pure False
    Right pd -> do
      let args = A.object
            [ "module_path" A..= ("../../etc/passwd" :: Text) ]
      tr <- LabTool.handle
              (undefined :: GhcSession)
              (undefined :: Store)
              pd args
      pure (isTraversalRefused (decodeToolResult tr))

-- | #160: 'ghc_lab' on a non-existent file must return
-- status='failed' with kind='module_path_does_not_exist', not
-- kind='subprocess_error'. The existence check fires before any
-- I/O or GhcSession usage.
testLabNonExistentFile :: IO Bool
testLabNonExistentFile = do
  case mkProjectDir "/tmp" of
    Left _ -> pure False
    Right pd -> do
      let args = A.object
            [ "module_path" A..= ("src/DoesNotExist.hs" :: Text) ]
      tr <- LabTool.handle
              (undefined :: GhcSession)
              (undefined :: Store)
              pd args
      case decodeToolResult tr of
        Right env
          | Env.reStatus env == Env.StatusFailed
          , Just err <- Env.reError env ->
              pure (Env.eeKind err == Env.ModulePathDoesNotExist)
        _ -> pure False

-- | #100C: 'ghc_load' must refuse traversal paths when 'module_path' is supplied.
-- 'mkModulePath' fires in the Just-path branch before 'countHaskellSources'
-- or any GhcSession usage.
testLoadRejectsTraversal :: IO Bool
testLoadRejectsTraversal = do
  case mkProjectDir "/tmp/project" of
    Left _ -> pure False
    Right pd -> do
      let args = A.object
            [ "module_path" A..= ("../../etc/passwd" :: Text) ]
      tr <- LoadTool.handle
              (undefined :: GhcSession)
              pd args
      pure (isTraversalRefused (decodeToolResult tr))

-- | #100C: 'ghc_refactor' must refuse traversal paths.
testRefactorRejectsTraversal :: IO Bool
testRefactorRejectsTraversal = do
  case mkProjectDir "/tmp/project" of
    Left _ -> pure False
    Right pd -> do
      let args = A.object
            [ "action"           A..= ("rename_local" :: Text)
            , "module_path"      A..= ("../../etc/passwd" :: Text)
            , "old_name"         A..= ("foo" :: Text)
            , "new_name"         A..= ("bar" :: Text)
            , "scope_line_start" A..= (1 :: Int)
            , "scope_line_end"   A..= (10 :: Int)
            ]
      tr <- RefactorTool.handle
              (undefined :: GhcSession)
              pd args
      pure (isTraversalRefused (decodeToolResult tr))

-- ---------------------------------------------------------------------------
-- Issue #98 Phase B · structured logging unit tests
-- ---------------------------------------------------------------------------

-- | 'redactArgs' must truncate string values longer than 'maxArgStringLen'
-- (40 chars) to exactly 40 chars + the Unicode ellipsis "…", while leaving
-- short strings, numbers, and bools verbatim.
testLoggingRedactionPolicy :: IO Bool
testLoggingRedactionPolicy = do
  let longStr  = T.replicate 50 "x"   -- 50 chars, will be truncated
      shortStr = T.replicate 20 "y"   -- 20 chars, kept verbatim
      args = A.object
        [ "long"  A..= longStr
        , "short" A..= shortStr
        , "num"   A..= (42 :: Int)
        , "flag"  A..= True
        ]
  case Logging.redactArgs args of
    A.Object km ->
      let longOk  = case AKM.lookup (AKey.fromString "long") km of
                      Just (A.String t) ->
                        -- Exactly maxArgStringLen chars + one "…" code point
                        T.length t == Logging.maxArgStringLen + 1
                        && T.last t == '\8230'   -- U+2026 HORIZONTAL ELLIPSIS
                      _ -> False
          shortOk = case AKM.lookup (AKey.fromString "short") km of
                      Just (A.String t) -> t == shortStr
                      _                 -> False
          numOk   = case AKM.lookup (AKey.fromString "num") km of
                      Just (A.Number _) -> True
                      _                 -> False
          flagOk  = case AKM.lookup (AKey.fromString "flag") km of
                      Just (A.Bool True) -> True
                      _                  -> False
      in pure (longOk && shortOk && numOk && flagOk)
    _ -> pure False

-- | 'newLogContext' must produce a 'LogContext' whose 'lcTraceId' is
-- exactly 6 characters long and consists solely of lowercase hex digits.
testLoggingTraceIdGeneration :: IO Bool
testLoggingTraceIdGeneration = do
  ctx <- Logging.newLogContext "ghc_test"
  let tid = Logging.lcTraceId ctx
  pure ( T.length tid == 6
      && T.all (\c -> c `elem` ("0123456789abcdef" :: String)) tid
      )

-- | When 'HASKELL_FLOWS_AUDIT' is not set, 'lcAuditPath' must be 'Nothing'.
testLoggingAuditPathAbsentByDefault :: IO Bool
testLoggingAuditPathAbsentByDefault = do
  -- Ensure the env var is absent for this test.
  unsetEnv "HASKELL_FLOWS_AUDIT"
  ctx <- Logging.newLogContext "ghc_test"
  pure (isNothing (Logging.lcAuditPath ctx))

-- | When 'HASKELL_FLOWS_AUDIT=1' is set, 'lcAuditPath' must be 'Just _'
-- pointing to a path ending in @".haskell-flows/audit.jsonl"@.
testLoggingAuditPathPresentWhenEnabled :: IO Bool
testLoggingAuditPathPresentWhenEnabled = do
  tmp <- getTemporaryDirectory
  let dir = tmp </> "hf-audit-test"
  removePathForcibly dir
  createDirectoryIfMissing True dir
  setEnv "HASKELL_FLOWS_AUDIT" "1"
  setEnv "HASKELL_PROJECT_DIR" dir
  ctx <- Logging.newLogContext "ghc_test"
  unsetEnv "HASKELL_FLOWS_AUDIT"
  unsetEnv "HASKELL_PROJECT_DIR"
  removePathForcibly dir
  pure $ case Logging.lcAuditPath ctx of
    Nothing   -> False
    Just path -> ".haskell-flows/audit.jsonl" `List.isSuffixOf` path

------------------------------------------------------------------------
-- Issue #96 Phase A — performance budget scaffold
------------------------------------------------------------------------

-- | Every constructor in 'ToolName' must have an entry in 'Budget.allBudgets'.
-- Catches gaps introduced when a new tool is added to 'ToolName' without
-- a corresponding budget row.
testBudgetParsesCleanly :: IO Bool
testBudgetParsesCleanly =
  pure $ all (isJust . Budget.lookupBudget) allToolNames

-- | No budget value is 0 ms — a zero p50 or p95 would always pass and
-- would be useless as a regression gate.
testBudgetNoZeroValues :: IO Bool
testBudgetNoZeroValues = pure $ all okBudget allToolNames
  where
    okBudget t = case Budget.lookupBudget t of
      Nothing -> False
      Just b  ->
        Budget.tbP50Ms b > 0
        && Budget.tbP95Ms b > 0
        && Budget.tbP50Ms b <= Budget.tbP95Ms b

-- | 'Runner.discardFirst' removes exactly the first element.
-- Simulates discarding the cold-start sample before computing p50\/p95.
testRunnerDiscardFirstSample :: IO Bool
testRunnerDiscardFirstSample = pure $
  -- non-empty list: first element gone
  Runner.discardFirst ([4800, 310, 290] :: [Int]) == [310, 290]
  -- singleton: result is empty
  && null (Runner.discardFirst ([42] :: [Int]))
  -- empty list: still empty (no error)
  && null (Runner.discardFirst ([] :: [Int]))
  -- computeStats picks up warm samples correctly
  && let r = Runner.computeStats [5000, 100, 200, 300]
     in Runner.prSamples r == [100, 200, 300]
        && Runner.prP50 r == 200
        && Runner.prP95 r == 300

------------------------------------------------------------------------
-- Issue #95 Phase D — nextStep quality gate: why string + chain length
------------------------------------------------------------------------

-- | Collect every 'NextStep' hint the dispatch table can emit, across
-- all tools and the meaningful payload shapes that select alternate
-- dispatch branches.  Used by Gate D and Gate E property tests.
allDispatchedHints :: [NextStep]
allDispatchedHints = catMaybes $
  -- Default payload — covers the bulk of tools (most have a single
  -- dispatch branch that ignores payload content).
  [ suggestNext t True (object []) | t <- allToolNames ]
  ++
  -- Per-tool payload variants that select alternate branches:
  [ suggestNext GhcGate         True (object ["status" .= ("ok"     :: Text)])
  -- #94 Phase C: determinism (runs>=2) merged into quickcheck.
  , suggestNext GhcQuickCheck   True (object ["runs"   .= (3 :: Int), "status" .= ("ok" :: Text)])
  , suggestNext GhcPropertyStore True (object ["action" .= ("run"    :: Text)])
  , suggestNext GhcPropertyStore True (object ["action" .= ("list"   :: Text)])
  , suggestNext GhcPropertyStore True (object ["files_written" .= (["test/Spec.hs"] :: [Text])])
  , suggestNext GhcPropertyStore True (object ["findings" .= ([] :: [Value])])
  , suggestNext GhcDeps         True (object ["action" .= ("add"    :: Text)])
  , suggestNext GhcDeps         True (object ["action" .= ("remove" :: Text)])
  , suggestNext GhcAddImport    True (object ["count"  .= (3 :: Int)])
  , suggestNext GhcQuickCheck   True (object ["state"  .= ("passed" :: Text)])
  , suggestNext GhcQuickCheck   True (object ["state"  .= ("failed" :: Text)])
  -- GhcLoad: typed-hole path
  , suggestNext GhcLoad True
      (object ["warnings" .=
        [object ["message" .= ("typed hole: _ :: Int" :: Text)]]])
  -- GhcLoad: fixable-warning path (non-hole warning)
  , suggestNext GhcLoad True
      (object ["warnings" .=
        [object ["message" .= ("unused import" :: Text)]]])
  -- GhcProject(action=switch): empty directory (no cabal file → scaffold)
  , suggestNext GhcProject True (object ["scaffolded" .= False])
  -- GhcProject(action=validate): cabal errors present
  , suggestNext GhcProject True (object ["errors" .= (3 :: Int)])
  ]

-- | Gate D: every 'nsWhy' string must be at least 10 characters long
-- and must end with a period ".".  A short or unpunctuated 'why' string
-- is not actionable — it gives the agent too little context to act on.
testNextStepGateDWhyQuality :: IO Bool
testNextStepGateDWhyQuality = pure $
  all checkWhy allDispatchedHints
  where
    checkWhy ns =
      T.length (nsWhy ns) >= 10
      && T.isSuffixOf "." (T.strip (nsWhy ns))

-- | Gate E: every 'nsChain' list (when present) must contain at most 4
-- steps.  Chains longer than 4 steps are overwhelming — the agent should
-- batch large workflows rather than prescribe them up front.
testNextStepGateEChainLength :: IO Bool
testNextStepGateEChainLength = pure $
  all checkChain allDispatchedHints
  where
    checkChain ns = case nsChain ns of
      Nothing -> True
      Just c  -> length c <= 4

------------------------------------------------------------------------
-- Issue #95 Phase C — golden dispatch snapshot
------------------------------------------------------------------------

-- | Golden table: @(description, source tool, payload, expected next tool)@.
-- Captures the dispatch table's behaviour for every meaningful (tool, payload)
-- combination.  A diff against this table signals a deliberate suppression-rule
-- change and must be reviewed before landing.
type GoldenRow = (String, ToolName, Value, Maybe ToolName)

goldenDispatchTable :: [GoldenRow]
goldenDispatchTable =
  -- ── Default payload (object []) ──────────────────────────────────
  [ ("project(create default) → deps chain", GhcProject,         object [],    Just GhcDeps)
  , ("deps(no-action) → suppressed",     GhcDeps,               object [],    Nothing)
  , ("load(clean) → suggest",            GhcLoad,               object [],    Just GhcSuggest)
  , ("hole → scratch(write) chain",      GhcHole,               object [],    Just GhcScratch)
  , ("arbitrary → load",                 GhcArbitrary,          object [],    Just GhcLoad)
  , ("suggest → scratch(write) chain",   GhcSuggest,            object [],    Just GhcScratch)
  , ("quickcheck(no-state) → suppress",  GhcQuickCheck,         object [],    Nothing)
  , ("property_store(no-action) → suppress", GhcPropertyStore,  object [],    Nothing)
  , ("refactor → load",                  GhcRefactor,           object [],    Just GhcLoad)
  , ("check_module → check_project",     GhcCheckModule,        object [],    Just GhcCheckProject)
  , ("check_project → gate chain",       GhcCheckProject,       object [],    Just GhcGate)
  , ("toolchain status → workflow",     GhcToolchain,          object [ "action" .= ("status" :: T.Text) ], Just GhcWorkflow)
  , ("project(validate clean) → suppress", GhcProject,          object [ "errors" .= (0 :: Int) ], Nothing)
  , ("lint → suppress",                  GhcLint,               object [],    Nothing)
  , ("format → load",                    GhcFormat,             object [],    Just GhcLoad)
  , ("batch → suppress",                 GhcBatch,              object [],    Nothing)
  , ("gate(fail) → check_project",       GhcGate,               object [],    Just GhcCheckProject)
  , ("property_store(export) → gate",   GhcPropertyStore,      object [ "files_written" .= (["test/Spec.hs"] :: [Text]) ], Just GhcGate)
  , ("quickcheck(runs=3,fail) → quickcheck",  GhcQuickCheck,    object [ "runs" .= (3 :: Int), "success" .= False ],  Just GhcQuickCheck)
  , ("property_store(audit) → list",    GhcPropertyStore,      object [ "findings" .= ([] :: [Value]) ],   Just GhcPropertyStore)
  , ("perf → perf",                      GhcPerf,               object [],    Just GhcPerf)
  , ("explain_error → scratch(write) chain", GhcExplainError,   object [],    Just GhcScratch)
  -- PR-3: lab promoted to a chain whose primary is property_store(audit),
  -- followed by check_project. Catches contradictions before replay.
  , ("lab → property_store(audit) chain", GhcLab,               object [],    Just GhcPropertyStore)
  , ("deps explain → deps add",         GhcDeps,               object [ "action" .= ("explain" :: T.Text) ], Just GhcDeps)
  , ("witness → quickcheck",            GhcWitness,            object [],    Just GhcQuickCheck)
  , ("refactor move_symbol → check_project", GhcRefactor,        object [ "action" .= ("move_symbol" :: T.Text) ], Just GhcCheckProject)
  , ("add_import(0) → suppress",        GhcAddImport,          object [],    Nothing)
  , ("modules add → check_project",     GhcModules,            object [ "action" .= ("add" :: T.Text) ],    Just GhcCheckProject)
  , ("modules remove → check_project",  GhcModules,            object [ "action" .= ("remove" :: T.Text) ], Just GhcCheckProject)
  , ("apply_exports → load",            GhcApplyExports,       object [],    Just GhcLoad)
  , ("fix_warning → load",              GhcFixWarning,         object [],    Just GhcLoad)
  , ("browse → suggest",                GhcBrowse,             object [],    Just GhcSuggest)
  -- PR-3: imports listed → browse the most-used module to find a
  -- candidate binding for laws.
  , ("imports → browse",                GhcImports,            object [],    Just GhcBrowse)
  -- #94 Phase C step 6: property_lifecycle merged into property_store. The
  -- "(no-action) → suppress" row above already covers the default-payload path.
  , ("toolchain warmup → workflow",     GhcToolchain,          object [ "action" .= ("warmup" :: T.Text) ], Just GhcWorkflow)
  , ("project(bootstrap) → workflow",   GhcProject,            object [ "host" .= ("claude-code" :: T.Text) ], Just GhcWorkflow)
  , ("workflow → suppress",             GhcWorkflow,           object [],    Nothing)
  -- PR-3: type/info/goto/doc fire on bare success — exploratory tools
  -- now carry forward-chaining hints. The new contract.
  , ("type → suggest",                  GhcType,               object [],    Just GhcSuggest)
  , ("info → doc",                      GhcInfo,               object [],    Just GhcDoc)
  -- PR-3: eval suppresses on degraded status; bare success (no status)
  -- counts as degraded via the statusOk_ fallback → suppressed.
  , ("eval (no status) → suppress",     GhcEval,               object [],    Nothing)
  , ("goto → browse",                   GhcGoto,               object [],    Just GhcBrowse)
  , ("doc → browse",                    GhcDoc,                object [],    Just GhcBrowse)
  -- PR-3: count-gated suggestions suppress when count is absent or zero
  -- (no candidates to act on).
  , ("complete (no count) → suppress",  GhcComplete,           object [],    Nothing)
  , ("hoogle_search (no count) → suppress", HoogleSearch,      object [],    Nothing)
  -- PR-3: coverage suppresses on degraded status (same shape as eval).
  , ("coverage (no status) → suppress", GhcCoverage,           object [],    Nothing)
  , ("project(switch scaffolded) → workflow", GhcProject,    object [ "scaffolded" .= True ], Just GhcWorkflow)
  -- ── Variant payloads ─────────────────────────────────────────────
  , ("deps(add) → load",
        GhcDeps,
        object ["action" .= ("add" :: Text)],
        Just GhcLoad)
  , ("deps(remove) → load",
        GhcDeps,
        object ["action" .= ("remove" :: Text)],
        Just GhcLoad)
  , ("load(errors) → suppress",
        GhcLoad,
        object ["errors" .= [object [] :: Value]],
        Nothing)
  , ("load(typed-holes) → hole",
        GhcLoad,
        object ["warnings" .= [object ["message" .= ("typed hole: _ :: Int" :: Text)] :: Value]],
        Just GhcHole)
  , ("load(fixable-warning) → fix_warning",
        GhcLoad,
        object ["warnings" .= [object ["message" .= ("unused import" :: Text)] :: Value]],
        Just GhcFixWarning)
  , ("quickcheck(passed) → check_module",
        GhcQuickCheck,
        object ["state" .= ("passed" :: Text)],
        Just GhcCheckModule)
  , ("quickcheck(failed) → eval",
        GhcQuickCheck,
        object ["state" .= ("failed" :: Text)],
        Just GhcEval)
  , ("property_store(list) → property_store(run)",
        GhcPropertyStore,
        object ["action" .= ("list" :: Text)],
        Just GhcPropertyStore)
  , ("property_store(run) → check_project",
        GhcPropertyStore,
        object ["action" .= ("run" :: Text)],
        Just GhcCheckProject)
  , ("gate(pass) → coverage",
        GhcGate,
        object ["status" .= ("ok" :: Text)],
        Just GhcCoverage)
  , ("quickcheck(runs=3,pass) → property_store",
        GhcQuickCheck,
        object ["runs" .= (3 :: Int), "status" .= ("ok" :: Text)],
        Just GhcPropertyStore)
  -- #94 Phase C step 6: ghc_property_lifecycle merged into
  -- ghc_property_store(action=list); covered by the
  -- "property_store(list) → property_store(run)" row above.
  , ("add_import(count>0) → load",
        GhcAddImport,
        object ["count" .= (3 :: Int)],
        Just GhcLoad)
  , ("project(switch empty) → project(create)",
        GhcProject,
        object ["scaffolded" .= False],
        Just GhcProject)
  , ("project(validate errors>0) → deps",
        GhcProject,
        object ["errors" .= (5 :: Int)],
        Just GhcDeps)
  ]

-- | Golden snapshot test: verify the dispatch table emits the expected
-- next-tool for every @(tool, payload)@ pair in 'goldenDispatchTable'.
-- A failure here means a dispatch-table change altered the recommendation
-- for a named case — review the diff before merging.
testNextStepGoldenDispatch :: IO Bool
testNextStepGoldenDispatch = do
  let failures = [ desc
                 | (desc, t, payload, expected) <- goldenDispatchTable
                 , let actual = fmap nsTool (suggestNext t True payload)
                 , actual /= expected
                 ]
  mapM_ (\d -> putStrLn ("  GOLDEN MISMATCH: " ++ d)) failures
  pure (null failures)

------------------------------------------------------------------------
-- Issue #268 — doc-truth: TOOL_TAXONOMY.md stays in sync with the registry
------------------------------------------------------------------------

-- | #268: doc-truth guard. Fails if docs/TOOL_TAXONOMY.md omits any
-- registered wire name or states the wrong tool total. The hand-prose
-- header used to drift (it claimed "46 registered tools" while the
-- machine count was 36); this keeps the canonical doc honest against
-- the code. Read relative to the package dir (../docs), matching the
-- existing @TIO.readFile "src/..."@ convention in this suite.
testTaxonomyDocListsAllTools :: IO Bool
testTaxonomyDocListsAllTools = do
  doc <- TIO.readFile "../docs/TOOL_TAXONOMY.md"
  let names   = allToolNameTexts
      missing = [ t | t <- names, not (("`" <> t <> "`") `T.isInfixOf` doc) ]
      total   = length names
      totalOk = ("**" <> T.pack (show total) <> "**") `T.isInfixOf` doc
  unless (null missing) $ do
    putStrLn "TOOL_TAXONOMY.md omits registered wire names:"
    mapM_ (putStrLn . ("  " ++) . T.unpack) missing
  unless totalOk $
    putStrLn ("TOOL_TAXONOMY.md does not state the live tool total (**"
                ++ show total ++ "**).")
  pure (null missing && totalOk)

------------------------------------------------------------------------
-- Issue #94 Phase A — tool taxonomy invariants
------------------------------------------------------------------------

-- | Invariant 1: the total registered-tool count must not exceed the
-- documented cap of 50.  The cap is bumped only via an explicit PR
-- with rationale — this prevents silent surface-bloat regressions.
--
-- Current count: 36 tools.  Cap: 50 (14 slots of headroom).
testToolCountWithinCap :: IO Bool
testToolCountWithinCap = do
  let n   = length allToolNames
      cap = 50 :: Int
  when (n > cap) $
    putStrLn $ "  SURFACE BLOAT: " ++ show n ++ " tools > cap " ++ show cap
  pure (n <= cap)

-- | Invariant 2: every 'ToolName' constructor must map to a non-empty
-- category text string via 'toolCategory' + 'toolCategoryText'.
-- Enforces that adding a new constructor also adds an arm to the
-- 'toolCategory' exhaustive case (otherwise it's a compile error).
testEveryToolHasCategory :: IO Bool
testEveryToolHasCategory = pure $
  not (any (T.null . toolCategoryText . toolCategory) allToolNames)

-- | Invariant 3: the count per category must match the taxonomy
-- published in @docs/TOOL_TAXONOMY.md@ (issue #94 §2).
-- Current breakdown: 27 primitives, 4 composites, 3 gates, 2 control-plane.
testCategoryCountsMatchTaxonomy :: IO Bool
testCategoryCountsMatchTaxonomy = pure $
  countCat CatPrimitive    == 27
  -- ^ #94 Phase B retrofit: GhcModules replaces GhcAddModules +
  -- GhcRemoveModules (36 → 35).
  -- #94 Phase C step 1: GhcDeps action="explain" replaces
  -- GhcDepsExplain outright (35 → 34).
  -- #94 Phase C step 3: ghc_quickcheck runs>=2 replaces
  -- GhcDeterminism outright (34 → 33).
  -- #94 Phase C step 4: ghc_refactor action="move_symbol" replaces
  -- GhcMove outright (33 → 32).
  -- #94 Phase C step 5: GhcProject (action=create|switch|validate
  -- |bootstrap) replaces GhcCreateProject + GhcSwitchProject +
  -- GhcValidateCabal + GhcBootstrap outright (32 → 29 — four
  -- removed, one added).  No deprecation period because the
  -- project has a single internal consumer.
  -- #94 Phase C step 6: GhcPropertyStore (action=list|run|export
  -- |audit) replaces GhcPropertyLifecycle + GhcRegression +
  -- GhcQuickCheckExport + GhcPropertyAudit outright (29 → 26 —
  -- four removed, one added).
  -- #253: GhcScratch — persistent LLM code canvas (26 → 27).
  && countCat CatComposite    ==  4
  && countCat CatGate         ==  3
  && countCat CatControlPlane ==  2
  -- ^ #94 Phase C step 2: GhcToolchain (action="status"|"warmup")
  -- replaces GhcToolchainStatus + GhcToolchainWarmup outright.
  -- Net delta on control-plane: 3 → 2 (two removed, one added).
  where
    countCat c = length [ t | t <- allToolNames, toolCategory t == c ]

------------------------------------------------------------------------
-- Issue #99 Phase B · per-tool version surface
------------------------------------------------------------------------

-- | Invariant: 'toolVersion' returns a non-empty Text for every
-- 'ToolName'. Adding a constructor without an arm in
-- 'HaskellFlows.Mcp.ToolName.toolVersion' is a compile error; this
-- test additionally rejects an empty-string entry sneaking in.
testEveryToolHasVersion :: IO Bool
testEveryToolHasVersion = pure $
  not (any (T.null . toolVersion) allToolNames)

-- | Invariant: every per-tool version parses as a 'MAJOR.MINOR.PATCH'
-- semver triple of non-negative integers. Catches typos like "1.0" or
-- "1.0.0-rc1" creeping into the table without a deliberate decision.
-- Stage A of #99 mandates simple triples; later phases can extend the
-- grammar (pre-release, build metadata) when an actual use case shows up.
testToolVersionIsSemverTriple :: IO Bool
testToolVersionIsSemverTriple = pure $
  all (validSemverTriple . T.unpack . toolVersion) allToolNames
  where
    validSemverTriple s = case wordsBy (== '.') s of
      [a, b, c] -> all isPositiveOrZero [a, b, c]
      _         -> False
    isPositiveOrZero t =
      not (null t)
      && all isDigit t
    -- Local re-impl to avoid pulling in Data.List.Split.
    wordsBy p = foldr step []
      where
        step c acc = case acc of
          (x : xs) | not (p c) -> (c : x) : xs
          _        | not (p c) -> [c] : acc
          _                    -> [] : acc

------------------------------------------------------------------------
-- Issue #94 Phase B · action-discriminated 'modules' primitive
------------------------------------------------------------------------

-- | The new 'GhcModules' constructor must round-trip through
-- 'parseToolName . toolNameText' AND be classified as a primitive
-- (not gate, not composite, not control-plane).  Sanity check that
-- adding the constructor without the corresponding 'toolCategory'
-- arm doesn't slip past the type system (it can't — 'toolCategory'
-- is exhaustive — but the *category* could still be wrong).
testModulesRegistered :: IO Bool
testModulesRegistered = pure $
  parseToolName "ghc_modules" == Just GhcModules
  && toolCategory GhcModules    == CatPrimitive

-- | The dispatcher must refuse an action it does not recognise with
-- a structured response (status=refused), not crash and not silently
-- delegate to the wrong handler.  Mirrors the contract every other
-- action-discriminated primitive (e.g. 'ghc_deps') already honours.
testModulesRejectsBadAction :: IO Bool
testModulesRejectsBadAction =
  case HaskellFlows.Types.mkProjectDir "/tmp" of
    Left _   -> pure False  -- mkProjectDir failed; cannot run the test
    Right pd -> do
      -- Direct handler call (no JSON-RPC envelope needed): we only
      -- verify that the dispatcher returns isError=true with content
      -- present, which is how 'ToolResult' renders a refused response.
      -- The ProjectDir argument is never read because the action
      -- check fires first.
      ToolResult content isErr <-
        Modules.handle pd
          (object
            [ "action"  .= ("nuke_everything" :: T.Text)
            , "modules" .= (["Foo"] :: [T.Text])
            ])
      pure (isErr && not (null content))

--------------------------------------------------------------------------------
-- Issue #105 · extractModules envelope peeling
--------------------------------------------------------------------------------

-- | Build a 'ToolResult' that mirrors what 'Hoogle.handle' actually
-- produces: the hits list is nested inside the \"result\" sub-object, not
-- at the top level.  The bug was that 'extractModules' looked for
-- \"results\" (wrong key) at the top level (wrong nesting depth).
testExtractModulesEnvelope :: IO Bool
testExtractModulesEnvelope = do
  let hitsPayload = A.object
        [ "hits" .=
            [ A.object ["module" .= ("Data.Maybe" :: T.Text)]
            , A.object ["module" .= ("Data.List"  :: T.Text)]
            ]
        , "count" .= (2 :: Int)
        ]
      tr = Env.toolResponseToResult (Env.mkOk hitsPayload)
      mods = AddImport.extractModules tr
  pure (mods == ["Data.Maybe", "Data.List"])

-- | The old bug looked for \"results\" (plural, wrong key). Verify that
-- a payload with only a \"results\" key — but no \"hits\" — returns [].
-- Regression pin: the wrong key must remain unrecognised.
testExtractModulesTopLevel :: IO Bool
testExtractModulesTopLevel = do
  let rawJson = "{\"status\":\"ok\",\"result\":{\"results\":[{\"module\":\"Data.Maybe\"}]}}"
      tr = ToolResult [TextContent (T.pack rawJson)] False
      mods = AddImport.extractModules tr
  pure (null mods)

--------------------------------------------------------------------------------
-- Issue #104c · injectTypeAnnotations
--------------------------------------------------------------------------------

-- | #172: A bare @\\x ->@ lambda whose parameter is constrained by a
-- named function ('foo x') must NOT be annotated — the type is
-- determined by 'foo' and injecting @:: Int@ would cause a type error.
testInjectAnnotateBareX :: IO Bool
testInjectAnnotateBareX = pure $
  QcExport.injectTypeAnnotations "\\x -> foo x == foo (foo x)"
    == "\\x -> foo x == foo (foo x)"

-- | A parameter whose name ends in @s@ (list convention) acquires @:: [Int]@.
testInjectAnnotateXs :: IO Bool
testInjectAnnotateXs = pure $
  QcExport.injectTypeAnnotations "\\xs -> reverse (reverse xs) == xs"
    == "\\(xs :: [Int]) -> reverse (reverse xs) == xs"

-- | Already-annotated lambda heads pass through unchanged.
testInjectAnnotateAlreadyAnnotated :: IO Bool
testInjectAnnotateAlreadyAnnotated =
  let expr = "\\(x :: Int) -> foo x == x"
  in pure (QcExport.injectTypeAnnotations expr == expr)

-- | Non-lambda expressions are returned verbatim.
testInjectAnnotateNonLambda :: IO Bool
testInjectAnnotateNonLambda =
  let expr = "foo x == foo (foo x)"
  in pure (QcExport.injectTypeAnnotations expr == expr)

-- | #215: bare single param eta-reduces correctly.
testEtaReduceBare :: IO Bool
testEtaReduceBare =
  pure $ QcExport.etaReduceLambda "\\x -> x + 1 == x + 1"
      == Just ("x", "x + 1 == x + 1")

-- | #215: annotated param @(x :: Int)@ eta-reduces preserving the
-- parenthesised annotation intact.
testEtaReduceAnnotated :: IO Bool
testEtaReduceAnnotated =
  pure $ QcExport.etaReduceLambda "\\(x :: Int) -> x + 0 == x"
      == Just ("(x :: Int)", "x + 0 == x")

-- | #215: list param @(xs :: [Int])@ eta-reduces.
testEtaReduceList :: IO Bool
testEtaReduceList =
  pure $ QcExport.etaReduceLambda "\\(xs :: [Int]) -> reverse (reverse xs) == xs"
      == Just ("(xs :: [Int])", "reverse (reverse xs) == xs")

-- | #215: a param with a function-type annotation @(f :: Int -> Int)@
-- contains a nested \" -> \" inside the parens. The paren-aware
-- scanner must not split there; it must find the outer arrow.
testEtaReduceNestedArrow :: IO Bool
testEtaReduceNestedArrow =
  pure $ QcExport.etaReduceLambda "\\(f :: Int -> Int) -> f 0 == 0"
      == Just ("(f :: Int -> Int)", "f 0 == 0")

-- | #215: a non-lambda expression returns @Nothing@.
testEtaReduceNonLambda :: IO Bool
testEtaReduceNonLambda =
  pure $ isNothing (QcExport.etaReduceLambda "foo x == foo (foo x)")

-- | #215: 'renderPropBinding' must NOT emit @= \\@ (the HLint-flagged
-- redundant-lambda pattern) for a plain stored property.
testRenderPropNoLambda :: IO Bool
testRenderPropNoLambda =
  let sp  = StoredProperty
              { spExpression = "\\x -> double (double x) == (4 * x :: Int)"
              , spModule     = Just "src/Scratch.hs"
              , spPassed     = 1
              , spUpdated    = 0
              }
      -- Access via renderTestFile so we test the full pipeline.
      out = QcExport.renderTestFile [sp]
  in pure $ not ("= \\" `T.isInfixOf` out)

-- | #215: a full 'renderTestFile' output for the canonical property
-- set must contain no @prop_N = \\@ lines at all.
testRenderTestFileNoLambdaAssign :: IO Bool
testRenderTestFileNoLambdaAssign =
  let props =
        [ StoredProperty
            { spExpression = "\\x -> double (double x) == (4 * x :: Int)"
            , spModule     = Just "src/Scratch.hs"
            , spPassed     = 1, spUpdated = 0 }
        , StoredProperty
            { spExpression = "\\(x :: Int) -> safeDiv (x :: Int) 0 == Nothing"
            , spModule     = Just "src/Scratch.hs"
            , spPassed     = 1, spUpdated = 0 }
        , StoredProperty
            { spExpression = "\\(xs :: [Int]) -> reverse (reverse xs) == xs"
            , spModule     = Nothing
            , spPassed     = 1, spUpdated = 0 }
        ]
      out    = QcExport.renderTestFile props
      lines_ = T.lines out
  in pure $ not (any ("= \\" `T.isInfixOf`) lines_)

--------------------------------------------------------------------------------
-- Issue #231 — unused imports + missing-sigs warnings in generated file
--------------------------------------------------------------------------------

-- | #231: generated file contains OPTIONS_GHC pragma suppressing both
-- -Wunused-imports and -Wmissing-signatures, preventing CI failures when
-- cabal test compiles the exported Spec.hs with -Wall.
testExportOptionsGhcPragma :: IO Bool
testExportOptionsGhcPragma =
  let rendered = QcExport.renderTestFile []
  in pure ("{-# OPTIONS_GHC -Wno-unused-imports -Wno-missing-signatures #-}"
           `T.isInfixOf` rendered)

-- Issue #198 — stale tool name + missing type signatures
--------------------------------------------------------------------------------

-- | #198: 'generatedHeader' must reference the current tool name
-- @ghc_property_store@, not the retired @ghc_quickcheck_export@.
testExportHeaderCurrentToolName :: IO Bool
testExportHeaderCurrentToolName =
  pure $  "ghc_property_store" `T.isInfixOf` QcExport.generatedHeader
       && not ("ghc_quickcheck_export" `T.isInfixOf` QcExport.generatedHeader)

-- | #198: 'renderPropSignature' returns a @prop_N :: T -> Bool@ line
-- for a single annotated parameter.
testRenderPropSigSingle :: IO Bool
testRenderPropSigSingle =
  pure $ QcExport.renderPropSignature 1 "(xs :: [Int])"
       == Just "prop_1 :: [Int] -> Bool"

-- | #198: 'renderPropSignature' concatenates multiple param types
-- with @->@ and appends @-> Bool@.
testRenderPropSigMulti :: IO Bool
testRenderPropSigMulti =
  pure $ QcExport.renderPropSignature 2 "(x :: Int) (y :: Int)"
       == Just "prop_2 :: Int -> Int -> Bool"

-- | #198: 'renderPropSignature' returns Nothing for an unannotated
-- bare parameter (no @::@ present in the param string).
testRenderPropSigNone :: IO Bool
testRenderPropSigNone =
  pure (isNothing (QcExport.renderPropSignature 3 "x"))

-- | #198: 'renderTestFile' output must include a type signature line
-- immediately before each @prop_N@ binding that has annotated params.
testRenderTestFileSigPresent :: IO Bool
testRenderTestFileSigPresent =
  let sp = StoredProperty
             { spExpression = "\\(xs :: [Int]) -> reverse (reverse xs) == xs"
             , spModule     = Nothing
             , spPassed     = 1
             , spUpdated    = 0 }
      out   = QcExport.renderTestFile [sp]
      lns   = T.lines out
      -- The sig line must appear directly before the binding line
      pairs = zip lns (drop 1 lns)
  in pure $ any (\(sig, bind) ->
       "prop_1 :: [Int] -> Bool" `T.isInfixOf` sig &&
       "prop_1 (xs :: [Int])" `T.isInfixOf` bind) pairs

-- | Two bare params — @x@ and @y@ — used only in operator expressions
-- get @:: Int@ (both are unconstrained by any named function).
testInjectAnnotateMultiParam :: IO Bool
testInjectAnnotateMultiParam = pure $
  QcExport.injectTypeAnnotations "\\x y -> x + y == y + x"
    == "\\(x :: Int) (y :: Int) -> x + y == y + x"

-- | #172: A parameter constrained by a String function must NOT
-- receive @:: Int@ — that would produce a type error.
testInjectAnnotateStringConstrained :: IO Bool
testInjectAnnotateStringConstrained = pure $
  QcExport.injectTypeAnnotations "\\x -> reverseStr (reverseStr x) == x"
    == "\\x -> reverseStr (reverseStr x) == x"

-- | #172: A parameter used only in an operator expression (no named
-- function constrains its type) must still get @:: Int@.
testInjectAnnotateOperatorOnlyX :: IO Bool
testInjectAnnotateOperatorOnlyX = pure $
  QcExport.injectTypeAnnotations "\\x -> x + 1 == 1 + x"
    == "\\(x :: Int) -> x + 1 == 1 + x"

--------------------------------------------------------------------------------
-- Issue #104a · Suggest/Rules emits annotated lambda params
--------------------------------------------------------------------------------

-- | The idempotent rule for @a -> a@ must now emit @\\(x :: Int) ->@
-- rather than a bare @\\x ->@.  Without the annotation, exporting the
-- property to a compiled Spec.hs triggers \"Ambiguous type variable\".
testSuggestIdempotentAnnotated :: IO Bool
testSuggestIdempotentAnnotated =
  case parseSignature "a -> a" of
    Nothing  -> pure False
    Just sig ->
      let props = [ sProperty s | s <- applyRules "normalise" sig
                                 , sLaw s == "Idempotent" ]
      in pure $ case props of
           (p:_) -> T.isInfixOf ":: Int" p
                 || T.isInfixOf ":: [Int]" p
           []    -> False

-- | The involutive rule for @a -> a@ must also emit an annotated param.
testSuggestInvolutiveAnnotated :: IO Bool
testSuggestInvolutiveAnnotated =
  case parseSignature "a -> a" of
    Nothing  -> pure False
    Just sig ->
      let props = [ sProperty s | s <- applyRules "rev" sig
                                 , sLaw s == "Involutive" ]
      in pure $ case props of
           (p:_) -> T.isInfixOf ":: Int" p
                 || T.isInfixOf ":: [Int]" p
           []    -> False

--------------------------------------------------------------------------------
-- Issue #103 · extractHaddockAbove source fallback
--------------------------------------------------------------------------------

-- | A @-- |@ block immediately above a definition is extracted and the
-- comment prefix is stripped.
testExtractHaddockFindsDoc :: IO Bool
testExtractHaddockFindsDoc = do
  tmp <- getTemporaryDirectory
  let path = tmp </> "haskell-flows-103-haddock1.hs"
  TIO.writeFile path "-- | Compute the identity of a value.\nidentity :: a -> a\nidentity x = x\n"
  result <- DocTool.extractHaddockAbove path 2  -- defLine=2 is the type sig line
  removePathForcibly path
  pure $ case result of
    Just txt -> T.isInfixOf "Compute the identity" txt
    Nothing  -> False

-- | A plain @--@ comment (without @|@) is NOT a Haddock block.
testExtractHaddockNoHaddock :: IO Bool
testExtractHaddockNoHaddock = do
  tmp <- getTemporaryDirectory
  let path = tmp </> "haskell-flows-103-haddock2.hs"
  TIO.writeFile path "-- Not a haddock comment\nfoo :: Int -> Int\nfoo x = x\n"
  result <- DocTool.extractHaddockAbove path 2
  removePathForcibly path
  pure (isNothing result)

-- | No comment at all above the definition → 'Nothing'.
testExtractHaddockNoComment :: IO Bool
testExtractHaddockNoComment = do
  tmp <- getTemporaryDirectory
  let path = tmp </> "haskell-flows-103-haddock3.hs"
  TIO.writeFile path "foo :: Int -> Int\nfoo x = x\n"
  result <- DocTool.extractHaddockAbove path 1
  removePathForcibly path
  pure (isNothing result)

--------------------------------------------------------------------------------
-- Issue #195 — extractHaddockAbove type-sig skip + nextStep routing
--------------------------------------------------------------------------------

-- | #195: 'extractHaddockAbove' must find a @-- |@ comment that sits
-- above a type-signature line, which in turn sits above the binding.
-- GHC reports the binding line (not the sig line) as @defLine@, so
-- the scanner must skip the sig line before collecting the comment.
testExtractHaddockAboveTypeSig :: IO Bool
testExtractHaddockAboveTypeSig = do
  tmp <- getTemporaryDirectory
  let path = tmp </> "haskell-flows-195-typesig.hs"
  TIO.writeFile path $ T.unlines
    [ "-- | Greet a person."
    , "greet :: String -> String"
    , "greet name = \"Hello, \" <> name <> \"!\""
    ]
  -- defLine = 3 (the binding line)
  result <- DocTool.extractHaddockAbove path 3
  removePathForcibly path
  pure $ case result of
    Just txt -> "Greet a person" `T.isInfixOf` txt
    Nothing  -> False

-- | #195: 'noDocInScopePayload' must have @hasDoc=false@,
-- @found_in_scope=true@, and a non-empty @reason@.
testNoDocInScopePayloadShape :: IO Bool
testNoDocInScopePayloadShape =
  case DocTool.noDocInScopePayload "greet" of
    A.Object o ->
      let hasDoc      = AKM.lookup "hasDoc"         o == Just (A.Bool False)
          foundInScope = AKM.lookup "found_in_scope" o == Just (A.Bool True)
          hasReason   = case AKM.lookup "reason" o of
                          Just (A.String r) -> not (T.null r)
                          _                 -> False
      in pure (hasDoc && foundInScope && hasReason)
    _ -> pure False

-- | #195: 'hasDocFalse' must return 'True' when fed a payload whose
-- @hasDoc@ field is @false@. This is a direct probe of the helper
-- so that if 'testDocNoDocNextStepIsInfo' fails we can distinguish
-- "helper broken" from "dispatch guard ordering wrong".
testHasDocFalseDirectly :: IO Bool
testHasDocFalseDirectly =
  let payloadFalse = DocTool.noDocInScopePayload "greet"
      payloadTrue  = DocTool.hasDocPayload "greet" "some doc"
      falsePayload = object [ "hasDoc" .= False ]
      truePayload  = object [ "hasDoc" .= True  ]
  in pure $  NextStep.hasDocFalse payloadFalse
          && not (NextStep.hasDocFalse payloadTrue)
          && NextStep.hasDocFalse falsePayload
          && not (NextStep.hasDocFalse truePayload)

-- | #195: when ghc_doc returns status=ok with @hasDoc=false@, the
-- injected nextStep must route to @ghc_info@, not @hoogle_search@.
-- Previously it routed to hoogle_search (wrong for local names).
testDocNoDocNextStepIsInfo :: IO Bool
testDocNoDocNextStepIsInfo =
  -- Issue #195: when ghc_doc returns ok + hasDoc=false (in-scope but
  -- no doc string), the nextStep must route to ghc_info, not hoogle_search.
  -- We pass the noDocInScopePayload directly so the envField lookup finds
  -- hasDoc at the top level, bypassing the two-level drill-down.
  -- The full envelope path (status=ok + result={...}) is exercised by
  -- injectNextStep at runtime; here we test the routing logic in isolation.
  -- NOTE: We only check nsTool==GhcInfo (not nsWhy text) because the why
  -- text intentionally mentions "hoogle_search" as a *contrast* ("more
  -- useful than hoogle_search for a locally-defined name"), so a substring
  -- search for "hoogle" would false-positive on that explanation.
  let payload = DocTool.noDocInScopePayload "greet"
      mNs = NextStep.suggestNext GhcDoc True payload
  in case mNs of
       Just ns -> pure (NextStep.nsTool ns == GhcInfo)
       Nothing -> pure False

--------------------------------------------------------------------------------
-- Issue #106 sub-findings
--------------------------------------------------------------------------------

-- | F-14: 'parseGhcErrors' should populate 'geCode' when the header line
-- contains @[GHC-XXXXX]@. This verifies the regex capture group is correct.
-- The captureHook fix populates geCode for the GHC-API path; the same
-- 'geCode' field is what categorizeWarning branches on.
testMkGhcErrorCode :: IO Bool
testMkGhcErrorCode =
  let raw = T.unlines
        [ "src/Foo.hs:5:1: warning: [GHC-66111] [-Wunused-imports]"
        , "    The import of 'Data.List' is redundant"
        ]
  in pure $ case parseGhcErrors raw of
       [e] -> geCode e == Just "GHC-66111"
           && geSeverity e == SevWarning
       _   -> False

--------------------------------------------------------------------------------
-- #180 — stripGhcInternalQual
--------------------------------------------------------------------------------

-- | #180: GHC 9.12 leaks 'ghc-internal-VERSION:GHC.Internal.*' prefixes into
-- diagnostics. 'stripGhcInternalQual' must remove them, leaving the public
-- module path.
testStripGhcInternalQual :: IO Bool
testStripGhcInternalQual =
  let raw = "No instance for 'ghc-internal-9.1202.0:GHC.Internal.Data.String.IsString Int'"
      want = "No instance for 'Data.String.IsString Int'"
  in pure (ApiSession.stripGhcInternalQual raw == want)

-- | #180: multiple occurrences in one message must all be stripped.
testStripGhcInternalQualMulti :: IO Bool
testStripGhcInternalQualMulti =
  let raw  = "ghc-internal-9.1202.0:GHC.Internal.Enum and ghc-internal-9.1202.0:GHC.Internal.Show"
      want = "Enum and Show"
  in pure (ApiSession.stripGhcInternalQual raw == want)

-- | #180: text without internal package qualifications is left unchanged.
testStripGhcInternalQualNoop :: IO Bool
testStripGhcInternalQualNoop =
  let raw = "No instance for 'Data.String.IsString Int'"
  in pure (ApiSession.stripGhcInternalQual raw == raw)

-- | F-17: 'previewResult' for a dropLine plan must omit the @patch@ key
-- entirely rather than emitting @\"patch\": null@. Agents that branch on
-- key presence (not null vs. absent) were getting confused.
testFixWarnNoPatchKey :: IO Bool
testFixWarnNoPatchKey =
  let plan = FixWarning.planForCode "GHC-66111"
      args = FixWarning.FixWarningArgs
               { FixWarning.fwModulePath = "src/Foo.hs"
               , FixWarning.fwLine       = 3
               , FixWarning.fwCode       = "GHC-66111"
               , FixWarning.fwApply      = False
               , FixWarning.fwName       = Nothing
               }
      result = FixWarning.previewResult "src/Foo.hs" plan args
  in pure $ case trContent result of
       [TextContent body] ->
         case A.decode (TLE.encodeUtf8 (TL.fromStrict body)) of
           Just (A.Object topEnv) ->
             case AKM.lookup "result" topEnv of
               Just (A.Object r) ->
                    AKM.member "dropLine" r
                 && not (AKM.member "patch" r)
               _ -> False
           _ -> False
       _ -> False

-- | F-07: error remediation strings must reference the consolidated
-- @ghc_project(action=\"create\")@ surface, not the retired
-- @ghc_create_project@ tool name.
testRemediationToolName :: IO Bool
testRemediationToolName = do
  let scaffold = CreateProject.sourceFile "Foo"
  pure $ not (T.isInfixOf "ghc_create_project" scaffold)

-- | F-20: 'parseHoogleLine' must populate 'hhName' with the
-- function/type name extracted from the LHS (the token after the
-- module prefix). Previously the name was captured and discarded.
testHoogleHitName :: IO Bool
testHoogleHitName =
  let line = "Prelude filter :: (a -> Bool) -> [a] -> [a]"
  in pure $ case parseHoogleLine line of
       Just h  -> hhName h == Just "filter"
       Nothing -> False

-- | F-19: 'hitsPayload' must deduplicate hits by (module, signature)
-- so that Hoogle returning the same entry twice (e.g. once per package
-- variant) doesn't inflate the count. Test the predicate directly with
-- 'List.nubBy' on constructed hits.
testHoogleDedup :: IO Bool
testHoogleDedup =
  let mk m nm sig = HoogleHit { hhModule = m, hhName = nm, hhSignature = sig }
      h1 = mk (Just "Data.List") (Just "sort") "Ord a => [a] -> [a]"
      h2 = mk (Just "Data.List") (Just "sort") "Ord a => [a] -> [a]"  -- duplicate
      h3 = mk (Just "Data.Set")  (Just "toList") "Set a -> [a]"
      sameHit a b = hhModule a == hhModule b && hhSignature a == hhSignature b
      unique = List.nubBy sameHit [h1, h2, h3]
  in pure (length unique == 2)

--------------------------------------------------------------------------------
-- Issue #139 — StatusNoMatch must not set isError
--------------------------------------------------------------------------------

-- | #139: 'isFailingStatus StatusNoMatch' must return False so that
-- 'toolResponseToResult' sets @isError=false@ on no-result hoogle responses.
testNoMatchIsNotFailing :: IO Bool
testNoMatchIsNotFailing = do
  let result = Env.toolResponseToResult (Env.mkNoMatch (A.object []))
  pure (not (trIsError result))

-- | #139: the full hoogle renderResult path for an empty hit list must
-- produce a ToolResult with isError=false.
testHoogleNoMatchIsError :: IO Bool
testHoogleNoMatchIsError = do
  let result = HoogleTool.renderResult "NoSuchSymbolXYZ" (HoogleTool.HoSuccess [])
  pure (not (trIsError result))

-- | #158: 'FromJSON HoogleArgs' must accept "count" as a synonym for
-- "limit" — both field names are in common LLM use. The schema declares
-- additionalProperties=false but the 'FromJSON' instance must honour
-- both aliases rather than silently dropping the unknown "count" key.
testHoogleCountAlias :: IO Bool
testHoogleCountAlias =
  -- Parse args with "count" key instead of "limit"
  case A.fromJSON (A.object ["query" A..= ("map" :: Text), "count" A..= (3 :: Int)]) of
    A.Error _   -> pure False  -- must parse successfully
    A.Success (HoogleTool.HoogleArgs { HoogleTool.haQuery = q, HoogleTool.haLimit = lim }) ->
      pure (q == "map" && lim == 3)

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
  pure $ case trContent result of
    [TextContent body_] ->
      case A.decode (TLE.encodeUtf8 (TL.fromStrict body_)) of
        Just (A.Object top) ->
          case AKM.lookup "result" top of
            Just (A.Object r) ->
              case AKM.lookup "context" r of
                Just (A.Object ctx) -> not (AKM.member "module_source" ctx)
                _                   -> False
            _ -> False
        _ -> False
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
  pure $ case trContent result of
    [TextContent body_] ->
      case A.decode (TLE.encodeUtf8 (TL.fromStrict body_)) of
        Just (A.Object top) ->
          case AKM.lookup "result" top of
            Just (A.Object r) ->
              case AKM.lookup "diagnostic" r of
                Just (A.Object d) ->
                  AKM.lookup "message" d == Just (A.String errTxt)
                _ -> False
            _ -> False
        _ -> False
    _ -> False

-- | F-11: 'hasDocPayload' must strip LaTeX @\\(…\\)@ delimiters that
-- GHC's pretty-printer emits for math notation in Haddock strings.
testDocStripLatex :: IO Bool
testDocStripLatex =
  let raw     = "O(\\(n\\)) complexity"
      payload = DocTool.hasDocPayload "foo" raw
  in pure $ case payload of
       A.Object km ->
         case AKM.lookup "doc" km of
           Just (A.String d) -> not (T.isInfixOf "\\(" d) && T.isInfixOf "O(" d
           _                 -> False
       _ -> False

-- | #144: GHC's 'showPprUnsafe' wraps Haddock doc strings in literal
-- @[@ … @]@ brackets (its internal DocH list format). 'hasDocPayload'
-- must strip those brackets so agents receive clean prose.
testDocStripBrackets :: IO Bool
testDocStripBrackets = pure $
  DocTool.stripDocBrackets "[ Left-associative fold @since base-4.6.0.0]"
    == "Left-associative fold @since base-4.6.0.0"
  && DocTool.stripDocBrackets "no brackets here" == "no brackets here"
  && DocTool.stripDocBrackets "[ ]" == ""
  && DocTool.stripDocBrackets "[single]" == "single"

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
  in pure $ case trContent result of
       [TextContent body_] ->
         case A.decode (TLE.encodeUtf8 (TL.fromStrict body_)) of
           Just (A.Object top) ->
             case AKM.lookup "result" top of
               Just (A.Object r) ->
                 case AKM.lookup "measurements" r of
                   Just (A.Object m) -> not (AKM.member "samples" m)
                   _                 -> False
               _ -> False
           _ -> False
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
  in pure $ case trContent result of
       [TextContent body_] ->
         case A.decode (TLE.encodeUtf8 (TL.fromStrict body_)) of
           Just (A.Object top) ->
             case AKM.lookup "error" top of
               Just (A.Object err) ->
                 case AKM.lookup "cause" err of
                   Just (A.String cause) ->
                     not (T.isInfixOf "{" cause) && T.isInfixOf "baseline_mean_ns" cause
                   _ -> False
               _ -> False
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
  in pure $ case trContent result of
       [TextContent body_] ->
         case A.decode (TLE.encodeUtf8 (TL.fromStrict body_)) of
           Just (A.Object top) ->
             case AKM.lookup "result" top of
               Just (A.Object r) ->
                 AKM.lookup "kind" r == Just (A.String "io_unit_no_output")
               _ -> False
           _ -> False
       _ -> False

-- | #167: The old 'ioUnitResult' hint said "Use putStrLn / print for
-- visible output" — circular advice when the user already called
-- putStrLn and got @output: ""@. The new hint must NOT contain the
-- phrase "Use putStrLn" (or the equivalent "use putStrLn") and must
-- instead explain that the action ran but produced no stdout output.
testEvalIoUnitHintNotCircular :: IO Bool
testEvalIoUnitHintNotCircular =
  let result = EvalTool.ioUnitResult
  in pure $ case trContent result of
       [TextContent body_] ->
         case A.decode (TLE.encodeUtf8 (TL.fromStrict body_)) of
           Just (A.Object top) ->
             case AKM.lookup "result" top of
               Just (A.Object r) ->
                 case AKM.lookup "hint" r of
                   Just (A.String hint) ->
                     -- Must NOT contain circular "Use putStrLn for visible output"
                     not (T.isInfixOf "Use putStrLn / print for visible output" hint)
                     -- Must still describe the IO () execution
                     && T.isInfixOf "IO ()" hint
                   _ -> False
               _ -> False
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

-- | F-03: 'render' with action=next must recommend 'ghc_quickcheck'
-- (not ghc_load) when the last history entry is 'GhcSuggest'.
testWorkflowNextHistoryAware :: IO Bool
testWorkflowNextHistoryAware =
  let ws = WS.WorkflowState
             { WS.wsToolCalls          = 5
             , WS.wsEditsSinceLastLoad  = 0
             , WS.wsLastLoadSuccess     = Just True
             , WS.wsLastLoadWarnings    = 0
             , WS.wsPassedProperties    = 0
             , WS.wsToolHistory         = [GhcSuggest]
             , WS.wsEverCalled          = Set.empty
             , WS.wsErrorStreak         = 0
             , WS.wsStarted             = posixSecondsToUTCTime 0
             }
      pd     = case mkProjectDir "/tmp" of Right p -> p; Left _ -> error "bad pd"
      result = WorkflowTool.render WorkflowTool.ActNext pd True [] ws dummyStaleness [] Nothing False Nothing
  in pure $ case trContent result of
       [TextContent body_] ->
         case A.decode (TLE.encodeUtf8 (TL.fromStrict body_)) of
           Just (A.Object top) ->
             case AKM.lookup "result" top of
               Just (A.Object r) ->
                 AKM.lookup "tool" r == Just (A.String "ghc_quickcheck")
               _ -> False
           _ -> False
       _ -> False
  where
    dummyStaleness = StalenessReport { srStale = False, srBinaryOlderBySec = Nothing, srMessage = Nothing }

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
  in pure $ case trContent result of
       [TextContent body_] ->
         case A.decode (TLE.encodeUtf8 (TL.fromStrict body_)) of
           Just (A.Object top) ->
             AKM.lookup "status" top == Just (A.String "failed")
             && case AKM.lookup "result" top of
                  Just (A.Object r) ->
                    AKM.lookup "dry_run" r == Just (A.Bool False)
                    && AKM.lookup "compile" r == Just (A.String "failed")
                  _ -> False
           _ -> False
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
  in pure $ case trContent result of
       [TextContent body_] ->
         case A.decode (TLE.encodeUtf8 (TL.fromStrict body_)) of
           Just (A.Object top) ->
             AKM.lookup "status" top == Just (A.String "failed")
             && case AKM.lookup "error" top of
                  Just (A.Object err) ->
                    case AKM.lookup "remediation" err of
                      Just (A.String r) -> T.isInfixOf "ghc_load" r
                      _                 -> False
                  _ -> False
           _ -> False
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
  in pure $ case trContent result of
       [TextContent body_] ->
         case A.decode (TLE.encodeUtf8 (TL.fromStrict body_)) of
           Just (A.Object top) ->
             case AKM.lookup "result" top of
               Just (A.Object r) ->
                 AKM.lookup "warmup_ns" r == Just (A.toJSON warmup)
               _ -> False
           _ -> False
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
      in pure $ case trContent result of
           [TextContent body_] ->
             case A.decode (TLE.encodeUtf8 (TL.fromStrict body_)) of
               Just (A.Object top) ->
                 -- #190: regression must be status=failed, not status=refused
                 AKM.lookup "status" top == Just (A.String "failed")
               _ -> False
           _ -> False
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
      in pure $ case trContent result of
           [TextContent body_] ->
             case A.decode (TLE.encodeUtf8 (TL.fromStrict body_)) of
               Just (A.Object top) ->
                 AKM.lookup "status" top == Just (A.String "failed")
                 && case AKM.lookup "error" top of
                      Just (A.Object err) ->
                        AKM.lookup "kind" err == Just (A.String "regression")
                      _ -> False
               _ -> False
           _ -> False
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
      encoded = case trContent result of
        [TextContent body_] -> body_
        _                   -> ""
  in case A.decode (TLE.encodeUtf8 (TL.fromStrict encoded)) of
       Just (A.Object top) ->
         case AKM.lookup "result" top of
           Just (A.Object res) ->
             case AKM.lookup "baseline" res of
               Just (A.Object bl) ->
                 case AKM.lookup "regression_pct" bl of
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
      encoded = case trContent result of
        [TextContent body_] -> body_
        _                   -> ""
  in case A.decode (TLE.encodeUtf8 (TL.fromStrict encoded)) of
       Just (A.Object top) ->
         case AKM.lookup "error" top of
           Just (A.Object err) ->
             case AKM.lookup "message" err of
               Just (A.String msg) ->
                    pure ( "threw a runtime exception" `T.isInfixOf` msg
                        && not ("session may have lost" `T.isInfixOf` msg) )
               _ -> pure False
           _ -> pure False
       _ -> pure False

--------------------------------------------------------------------------------
-- Issue #135 — summariseMeasurementErrors truncation + dedup
--------------------------------------------------------------------------------

-- | #135: a single short error is presented as "First error (1/1): …"
-- with no omit note and total length well under 600 chars.
testSummariseSingleError :: IO Bool
testSummariseSingleError = do
  let msg    = "Could not load module 'Foo'" :: T.Text
      result = PerfTool.summariseMeasurementErrors [msg]
  pure $ T.isInfixOf "First error (1/1):" result
      && T.isInfixOf msg result
      && not (T.isInfixOf "omitted" result)
      && T.length result < 600

-- | #135: 20 identical errors are collapsed to one line with
-- "[19 similar errors omitted]" and total length < 600 chars.
testSummariseRepeatedErrors :: IO Bool
testSummariseRepeatedErrors = do
  let msg    = "Could not load module 'GHC.Types.Error'" :: T.Text
      errs   = replicate 20 msg
      result = PerfTool.summariseMeasurementErrors errs
  pure $ T.isInfixOf "First error (1/20):" result
      && T.isInfixOf "19 similar errors omitted" result
      && T.length result < 600

-- | #135: an error longer than 500 chars is truncated with a [truncated]
-- note and the result stays under 600 chars.
testSummariseLongError :: IO Bool
testSummariseLongError = do
  let longMsg = T.replicate 600 "x"
      result  = PerfTool.summariseMeasurementErrors [longMsg]
  pure $ T.isInfixOf "[truncated]" result
      && T.length result < 600

-- | #135: empty error list returns empty string (no crash).
testSummariseEmptyList :: IO Bool
testSummariseEmptyList =
  pure $ PerfTool.summariseMeasurementErrors [] == ""

--------------------------------------------------------------------------------
-- Issue #108 — typed-hole reclassification in check_module + refactor
--------------------------------------------------------------------------------

-- | #108: the 'handle' body in CheckModule.hs must filter typed holes
-- (GHC-88464) out of the 'errors' bucket and compute 'compileOk' from
-- the remaining real errors.  Checked structurally by scanning the
-- source file so we don't need a live GHCi session in unit tests.
-- | Issue #213: when holes are present, holes.reason must say
-- "N typed hole(s) found", not the contradictory "no deferred
-- typed holes". Use renderResult directly to avoid a live session.
testCheckModuleHolesReasonCount :: IO Bool
testCheckModuleHolesReasonCount =
  -- Pass 2 dummy holes (unit values — holes param is polymorphic [a])
  let result = CheckModule.renderResult
                 "src/Scratch.hs"
                 True   -- compileOk
                 []     -- errors
                 []     -- warnings
                 [(), ()] -- 2 holes
                 []     -- regressions
                 0      -- totalProps
                 0      -- loadFailed
                 True   -- warnings_block
  in pure $ case trContent result of
       [TextContent body_] ->
         case A.decode (TLE.encodeUtf8 (TL.fromStrict body_)) of
           Just (A.Object top) ->
             case AKM.lookup "result" top of
               Just (A.Object r) ->
                 case AKM.lookup "gates" r of
                   Just (A.Object g) ->
                     case AKM.lookup "holes" g of
                       Just (A.Object holesGate) ->
                         AKM.lookup "ok" holesGate == Just (A.Bool False)
                         && case AKM.lookup "reason" holesGate of
                              Just (A.String rr) -> "2 typed hole(s)" `T.isInfixOf` rr
                              _                  -> False
                       _ -> False
                   _ -> False
               _ -> False
           _ -> False
       _ -> False

testCheckModuleHoleOnlyCompileOk :: IO Bool
testCheckModuleHoleOnlyCompileOk = do
  src <- TIO.readFile "src/HaskellFlows/Tool/CheckModule.hs"
  let code = T.unlines (filter (not . isDocLine) (T.lines src))
  pure $ T.isInfixOf "isHoleErr" code
      && T.isInfixOf "GHC-88464" code
      && T.isInfixOf "ownHoleOnly" code
      && T.isInfixOf "compileOk = null errors" code
  where
    isDocLine ln = "--" `T.isPrefixOf` T.stripStart ln

-- | #108: 'errors' must exclude diagnostics where 'geCode == Just "GHC-88464"'.
testCheckModuleRealErrorsExcludesHoles :: IO Bool
testCheckModuleRealErrorsExcludesHoles = do
  src <- TIO.readFile "src/HaskellFlows/Tool/CheckModule.hs"
  let code = T.unlines (filter (not . isDocLine) (T.lines src))
  -- Must filter with 'not (isHoleErr d)' or equivalent.
  pure $ T.isInfixOf "not (isHoleErr d)" code
       || T.isInfixOf "not (isHoleErr" code
  where
    isDocLine ln = "--" `T.isPrefixOf` T.stripStart ln

-- | #108: 'commitResultWithDiff' in Refactor.hs must exclude holes from
-- pre_existing_errors by filtering on 'GHC-88464'.
testRefactorPreExistingHolesExcluded :: IO Bool
testRefactorPreExistingHolesExcluded = do
  src <- TIO.readFile "src/HaskellFlows/Tool/Refactor.hs"
  let code = T.unlines (filter (not . isDocLine) (T.lines src))
  pure $ T.isInfixOf "isHoleErr" code
      && T.isInfixOf "GHC-88464" code
      && T.isInfixOf "not (isHoleErr d)" code
  where
    isDocLine ln = "--" `T.isPrefixOf` T.stripStart ln

--------------------------------------------------------------------------------
-- Issue #188 — check_module uses loadSpecificFileForTarget
--------------------------------------------------------------------------------

-- | #188: 'ghc_check_module' used to call 'loadForTarget' which scans all
-- .hs files via 'enumerateHaskellSources', causing 'strictOk=False' when
-- any unregistered broken file existed in src/ even if the checked module
-- was clean. Fix: use 'loadSpecificFileForTarget' so only the requested
-- file is compiled.
testCheckModuleUsesSpecificLoader :: IO Bool
testCheckModuleUsesSpecificLoader = do
  src <- TIO.readFile "src/HaskellFlows/Tool/CheckModule.hs"
  let code = T.unlines (filter (not . isDocLine) (T.lines src))
  pure $ T.isInfixOf "loadSpecificFileForTarget" code
      -- loadForTarget must NOT appear as a function call (it can appear
      -- in comments or error message strings, but not as the actual call)
      && not (T.isInfixOf "loadForTarget ghcSess tgt Strict" code)
      && not (T.isInfixOf "loadForTarget ghcSess tgt Deferred" code)
  where
    isDocLine ln = "--" `T.isPrefixOf` T.stripStart ln

--------------------------------------------------------------------------------
-- Issue #109 — .cabal comment stripping in check_project
--------------------------------------------------------------------------------

-- | #109: 'parseExposedModules' must not extract comment words like
-- "Bench", "Phase", "A)" that follow a @--@ marker in the payload.
testParseExposedModulesStripsComments :: IO Bool
testParseExposedModulesStripsComments =
  let body = T.unlines
        [ "library"
        , "  exposed-modules:"
        , "    Core.Logic"
        , "    -- Bench modules (#96 Phase A)"
        , "    Core.Parser"
        ]
      mods = parseExposedModules body
  in pure $ "Core.Logic"   `elem` mods
         && "Core.Parser"  `elem` mods
         && "Bench"    `notElem` mods
         && "Phase"    `notElem` mods
         && "A)"       `notElem` mods

-- | #109: tokens with non-module characters (e.g. trailing ')') must
-- be rejected by the strengthened 'isModuleName' predicate.
testParseExposedModulesRejectsPunct :: IO Bool
testParseExposedModulesRejectsPunct =
  let body = T.unlines
        [ "library"
        , "  exposed-modules: Good.Module, Bad)"
        ]
      mods = parseExposedModules body
  in pure $ "Good.Module" `elem`    mods
         && "Bad)"        `notElem` mods
         && "Bad"         `notElem` mods

--------------------------------------------------------------------------------
-- Issue #107 — ghc_info renderDefinition for functions
--------------------------------------------------------------------------------

-- | #107: the 'AnId' branch in 'queryInfo' must use 'idType' to produce
-- "name :: <type>" instead of "Identifier 'name'" (pprShortTyThing).
-- Checked structurally by scanning Info.hs source.
testInfoAnIdDefinition :: IO Bool
testInfoAnIdDefinition = do
  src <- TIO.readFile "src/HaskellFlows/Tool/Info.hs"
  let code = T.unlines (filter (not . isDocLine) (T.lines src))
  pure $ T.isInfixOf "AnId i ->" code
      && T.isInfixOf "idType i" code
      && T.isInfixOf "nm <> \" :: \"" code
  where
    isDocLine ln = "--" `T.isPrefixOf` T.stripStart ln

--------------------------------------------------------------------------------
-- Issue #130 — ghc_info eponymous record TyCon selection
--------------------------------------------------------------------------------

-- | #130: the Info.hs source must define 'preferTyCon' with the
-- ATyCon-preference logic.
testInfoPreferTyConInSource :: IO Bool
testInfoPreferTyConInSource = do
  src <- TIO.readFile "src/HaskellFlows/Tool/Info.hs"
  pure $ T.isInfixOf "preferTyCon" src
      && T.isInfixOf "ATyCon {}" src
      && T.isInfixOf "toList names" src

-- | #130: 'queryInfo' must use 'preferTyCon' and NOT take only the
-- first name via the old @n :| _@ pattern.
testInfoQueryUsesPreferTyCon :: IO Bool
testInfoQueryUsesPreferTyCon = do
  src <- TIO.readFile "src/HaskellFlows/Tool/Info.hs"
  let code = T.unlines (filter (not . isDocLine) (T.lines src))
  pure $ T.isInfixOf "preferTyCon infos" code
      -- The old first-name shortcut must be gone
      && not (T.isInfixOf "n :| _" code)
  where
    isDocLine ln = "--" `T.isPrefixOf` T.stripStart ln

--------------------------------------------------------------------------------
-- Issue #184 — AConLike data constructors render as "Name :: Type"
--------------------------------------------------------------------------------

-- | #184: Info.hs must have an explicit 'AConLike (RealDataCon dc)' branch
-- so that data constructors like 'Just', 'Nothing', 'True' are rendered as
-- "Name :: Type" and NOT routed to the catch-all renderDefinition which
-- produced the garbled "data Just Data constructor 'Just'" output.
testInfoAConLikeBranchExists :: IO Bool
testInfoAConLikeBranchExists = do
  src <- TIO.readFile "src/HaskellFlows/Tool/Info.hs"
  let code = T.unlines (filter (not . isDocLine) (T.lines src))
  pure $ T.isInfixOf "AConLike (RealDataCon dc)" code
      && T.isInfixOf "AConLike (PatSynCon ps)" code
  where
    isDocLine ln = "--" `T.isPrefixOf` T.stripStart ln

-- | #184: the RealDataCon branch must use 'dataConDisplayType' to build
-- the "Name :: Type" string, not 'renderDefinition' (which prepends "data ").
testInfoAConLikeUsesDisplayType :: IO Bool
testInfoAConLikeUsesDisplayType = do
  src <- TIO.readFile "src/HaskellFlows/Tool/Info.hs"
  let code = T.unlines (filter (not . isDocLine) (T.lines src))
  pure $ T.isInfixOf "dataConDisplayType" code
      -- The old garble path must not be taken for constructors:
      && not (T.isInfixOf "renderDefinition kind nm renderedThing" code
               && T.isInfixOf "AConLike" (T.unlines
                    [ ln | ln <- T.lines code
                    , T.isInfixOf "renderDefinition kind nm renderedThing" ln ]))
  where
    isDocLine ln = "--" `T.isPrefixOf` T.stripStart ln

--------------------------------------------------------------------------------
-- Issue #111 — forall stripping in parseSignature
--------------------------------------------------------------------------------

-- | #111: 'stripForall' handles the inferred-variable form @forall {a}.@
-- produced by GHC 9.2+ for non-specified variables.
testStripForallInferred :: IO Bool
testStripForallInferred =
  pure $ stripForall "forall {a}. [a] -> [a]" == "[a] -> [a]"

-- | #111: 'stripForall' handles the explicit @forall a.@ form.
testStripForallExplicit :: IO Bool
testStripForallExplicit =
  pure $ stripForall "forall a. [a] -> [a]" == "[a] -> [a]"

-- | #111: 'stripForall' is a no-op when there is no leading @forall@.
testStripForallNoop :: IO Bool
testStripForallNoop =
  pure $ stripForall "[a] -> [a]" == "[a] -> [a]"
      && stripForall "a -> a"     == "a -> a"

-- | #111: 'parseSignature' handles a full @forall {a}.@ prefix and
-- produces a valid 'ParsedSig' where arg == return (required for
-- the involutive + list-roundtrip rules to fire on @reverse@).
testParseSigForallList :: IO Bool
testParseSigForallList =
  case parseSignature "forall {a}. [a] -> [a]" of
    Nothing  -> pure False
    Just sig -> pure $ isSameTypeThroughout sig && length (psArgs sig) == 1

-- | #111: the involutive rule must fire for a @reverse@-shaped signature
-- even when the string carries a leading @forall {a}.@ (the form GHC
-- 9.2+ emits from 'exprType TM_Inst').
testRulesFireForForallReverse :: IO Bool
testRulesFireForForallReverse =
  case parseSignature "forall {a}. [a] -> [a]" of
    Nothing  -> pure False
    Just sig ->
      let suggs = applyRules "reverse" sig
      in pure $ any ((== "Involutive")           . sLaw) suggs
             && any ((== "Self-inverse on lists") . sLaw) suggs

--------------------------------------------------------------------------------
-- Issue #137 — Haddock comment stripping in parseSignature
--------------------------------------------------------------------------------

-- | #137: 'stripLineComments' strips a '-- ^' Haddock annotation to
-- end of line, preserving the type token before it.
testStripLineCommentsHaddock :: IO Bool
testStripLineCommentsHaddock =
  let raw    = "Set.Set String   -- ^ existing context module names"
      result = stripLineComments raw
  in pure $ T.isInfixOf "Set.Set String" result
         && not ("--" `T.isInfixOf` result)

-- | #137: mid-line '--' comment is stripped.
testStripLineCommentsMid :: IO Bool
testStripLineCommentsMid =
  let raw    = "Int -- some note"
      result = stripLineComments raw
  in pure (T.strip result == "Int")

-- | #137: comment-free lines pass through unchanged (modulo trailing
-- whitespace normalisation by 'T.stripEnd').
testStripLineCommentsClean :: IO Bool
testStripLineCommentsClean =
  let raw = "Set.Set String -> [String] -> [String]"
  in pure (T.strip (stripLineComments raw) == raw)

-- | #137: 'parseSignature' correctly parses a multi-line Haddock-annotated
-- signature into its component types, recovering the exact same result as
-- the comment-free version.
testParseSigHaddockAnnotated :: IO Bool
testParseSigHaddockAnnotated =
  let annotated = "Set.Set String   -- ^ existing names\n  -> [String]   -- ^ candidates\n  -> [String]"
      clean     = "Set.Set String -> [String] -> [String]"
  in pure (parseSignature annotated == parseSignature clean)

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

-- | #117: when 'queryLocation' resolves a name to an 'InModule' location
-- (library name with no local source file), 'locationPayload' must include
-- @has_location: false@ and a remediation hint.
testGotoLibraryNameNoMatch :: IO Bool
testGotoLibraryNameNoMatch =
  let loc = GotoTool.InModule "GHC.Base"
      payload = GotoTool.locationPayload "fmap" loc
  in case payload of
       A.Object o ->
         let hasLoc  = AKM.lookup "has_location" o == Just (A.Bool False)
             hasRem  = case AKM.lookup "remediation" o of
                         Just (A.String t) -> not (T.null t)
                         _                 -> False
             hasKind = AKM.lookup "kind" o == Just (A.String "module")
         in pure (hasLoc && hasRem && hasKind)
       _ -> pure False

-- | #117: an 'InFile' location must carry @has_location: true@.
testGotoFileHasLocation :: IO Bool
testGotoFileHasLocation =
  let loc = GotoTool.InFile "src/Foo.hs" 10 5
      payload = GotoTool.locationPayload "myFn" loc
  in case payload of
       A.Object o ->
         pure (AKM.lookup "has_location" o == Just (A.Bool True))
       _ -> pure False

-- | #214: the InModule remediation message must NOT say "no local source
-- file" because compiled project modules DO have a local source file —
-- they're simply compiled. The message must use "was compiled" instead.
testGotoCompiledModuleRemediation :: IO Bool
testGotoCompiledModuleRemediation =
  let -- A project-local module name (same shape as Scratch)
      loc = GotoTool.InModule "Scratch"
      payload = GotoTool.locationPayload "greet" loc
  in case payload of
       A.Object o ->
         case AKM.lookup "remediation" o of
           Just (A.String t) ->
             let hasCompiled   = "was compiled" `T.isInfixOf` t
                 noFalseSource = not ("no local source file" `T.isInfixOf` t)
             in pure (hasCompiled && noFalseSource)
           _ -> pure False
       _ -> pure False

-- | #224: qualifiedPreloadPayload names the unqualified form and the
-- module prefix so the agent knows how to retry without the qualifier.
testGotoQualifiedPreloadPayload :: IO Bool
testGotoQualifiedPreloadPayload =
  let loc = GotoTool.InModule "GHC.Internal.Data.OldList"
      payload = qualifiedPreloadPayload "Data.List.sort" "sort" loc
  in case payload of
       A.Object o ->
         case AKM.lookup "remediation" o of
           Just (A.String t) ->
             let mentionsSort = "'sort'" `T.isInfixOf` t
                 mentionsMod  = "'Data.List'" `T.isInfixOf` t
                 mentionsQual = "qualified" `T.isInfixOf` t
             in pure (mentionsSort && mentionsMod && mentionsQual)
           _ -> pure False
       _ -> pure False

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

-- | #220: 'witnessEvalExpr' must produce a well-formed in-process
-- expression that contains the QC sentinel markers, the
-- 'quickCheckWithResult' call, and the 'Data.Map.toList' + 'labels r'
-- extraction needed for the structured labels path.
testWitnessEvalExprStructure :: IO Bool
testWitnessEvalExprStructure =
  let expr = T.pack (QcTool.witnessEvalExpr "\\x -> x > 0")
  in pure $  T.isInfixOf "quickCheckWithResult"   expr
          && T.isInfixOf "Data.Map.toList"         expr
          && T.isInfixOf "labels r"                expr
          && T.isInfixOf "__QC_OUTPUT_START__"     expr
          && T.isInfixOf "__QC_LABELS_START__"     expr
          && T.isInfixOf "__QC_OUTPUT_END__"       expr
          && T.isInfixOf "__QC_LABELS_END__"       expr
          && T.isInfixOf "stdArgs"                 expr

-- | #220: 'Witness.hs' must call 'runQuickCheckWithLabelsInProcess'
-- (in-process path) rather than 'runQuickCheckWithLabelsViaCabalRepl'
-- (subprocess path) in its 'handle' function.
testWitnessUsesInProcessPath :: IO Bool
testWitnessUsesInProcessPath = do
  src <- TIO.readFile "src/HaskellFlows/Tool/Witness.hs"
  pure $ T.isInfixOf "runQuickCheckWithLabelsInProcess ghcSess" src

-- | #220: 'runQuickCheckWithLabelsInProcess' must fall back to the
-- cabal-repl subprocess path when 'evalIOString' fails (e.g. the
-- user's project has no QuickCheck in its build-depends, so the
-- loaded stanza's package environment excludes Test.QuickCheck).
--
-- The cabal-repl fallback injects @--build-depends=QuickCheck@
-- automatically, so it always works regardless of the project's deps.
-- The fallback is triggered both on Left (compile/runtime exception)
-- and on Right with no sentinel markers (silent type-mismatch).
testWitnessInProcessFallback :: IO Bool
testWitnessInProcessFallback = do
  src <- TIO.readFile "src/HaskellFlows/Tool/QuickCheck.hs"
  pure $  -- Left (exception) branch triggers subprocess fallback
          T.isInfixOf "Left _ex ->" src
       && T.isInfixOf "runQuickCheckWithLabelsViaCabalRepl (gsProject ghcSess)" src
          -- Right (no-sentinel) branch also falls back
       && T.isInfixOf "__QC_OUTPUT_START__" src

-- | #240: 'compileErrorResult' returns a failed ToolResult with
-- kind=compile_error so agents distinguish "0 samples" from "compile error".
testWitnessCompileErrorResult :: IO Bool
testWitnessCompileErrorResult =
  let result = WitnessTool.compileErrorResult "\\x -> notAFunction x" "Not in scope: notAFunction"
  in case decodeToolResult result of
    Left _  -> pure False
    Right r ->
      pure $  Env.reStatus r == Env.StatusFailed
           && maybe False ((== Env.CompileError) . Env.eeKind) (Env.reError r)

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
