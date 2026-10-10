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
  , (.&&.)
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
import qualified HaskellFlows.Tool.Gate as Gate
import qualified HaskellFlows.Tool.CreateProject as CreateProject
import qualified HaskellFlows.Tool.Move as MoveTool
import qualified HaskellFlows.Tool.DepsExplain as DepsExplain
import qualified HaskellFlows.Tool.PropertyAudit as PropertyAuditTool
import qualified HaskellFlows.Tool.QuickCheck as QcTool
import qualified HaskellFlows.Tool.QuickCheckExport as QcExport
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
import HaskellFlows.Tool.Lint (parseHlintJson)
import qualified HaskellFlows.Tool.Lint as LintTool
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
import Control.Monad (replicateM, unless, when)
import Text.Read (readMaybe)
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
import HaskellFlows.Ghc.IdeSession
  ( EvalError (..)
  , EvalErrorClass (..)
  , classifyEvalError
  , evalErrorKind
  )
import HaskellFlows.Parser.Cabal
  ( fieldSplit
  , listFieldOf
  , projectModuleFilesFromCabal
  , splitStanzas
  , stanzaHeaderOf
  )
import HaskellFlows.Tool.IdeBacked (moduleKeyOf)
import HaskellFlows.Util.Process (capOutput)
import qualified HaskellFlows.Tool.Bootstrap as BootstrapTool
import qualified HaskellFlows.Tool.Browse as BrowseTool
import qualified HaskellFlows.Tool.Complete as CompleteTool
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
import HaskellFlows.Mcp.Transport (deliverOnce)
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

import Spec.Harness (quickTest, test)
import Spec.Scratch (scratchTests)
import Spec.AddImportUnit
import Spec.AddModulesHandle
import Spec.ApplyExports
import Spec.Arbitrary
import Spec.ArbitraryUnit
import Spec.Batch
import Spec.Bootstrap
import Spec.BudgetGate
import Spec.Config
import Spec.Complete
import Spec.CreateProject
import Spec.Deps
import Spec.DepsExplainLab
import Spec.DepsFormat
import Spec.DepsUnit
import Spec.Descriptors
import Spec.Discover
import Spec.DogfoodHint
import Spec.Envelope
import Spec.Extract
import Spec.FinalMisc
import Spec.FixWarningUnit
import Spec.Format
import Spec.GateUnit
import Spec.Goto
import Spec.Guidance
import Spec.HaddockUnit
import Spec.HandleApplyRemove
import Spec.HoleParse
import Spec.InfoAdvanced
import Spec.InfoHoogle
import Spec.Lint
import Spec.ModuleNameUnit
import Spec.MoveUnit
import Spec.NextStepFull
import Spec.NextStepLoad
import Spec.NextStepUnit
import Spec.ParseError
import Spec.PermissiveJSON
import Spec.PlanUnit
import Spec.Progress
import Spec.PropertyAuditUnit
import Spec.Protocol
import Spec.QcExport
import Spec.RefactorTool
import Spec.RemoveModulesUnit
import Spec.Rename
import Spec.Sanitize
import Spec.Schema
import Spec.ServerUnit
import Spec.Store
import Spec.SuggestAdvanced
import Spec.SuggestLaws
import Spec.SuggestSig
import Spec.SwitchProjectUnit
import Spec.TaxonomyUnit
import Spec.Toolchain
  ( testToolchainStatusBackcompatFields
  , testToolchainStatusEnvelopeShape
  , testToolchainStatusFailedIncludesInventory
  , testToolchainWarmupEnvelopeShape
  , testToolchainWarmupPartialWarnings
  )
import Spec.PartialFunctions
import Spec.TraversalGuards
import Spec.ValidateCabal
import Spec.SurfaceHarness
import Spec.WorkflowState
import Spec.WorkflowTool
import Spec.DispatchUnit
  ( testMkToolEnvFields
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
import Spec.ArgCheckUnit
  ( testSchemaPropertyNames
  , testUnknownArgKeys
  , testDidYouMeanBaseDir
  , testUnknownArgsWarningFires
  , testUnknownArgsWarningSilentWhenClean
  )

-- | Number of full suite passes to run. @HASKELL_FLOWS_TEST_REPEAT=N@
-- (N >= 1) runs the whole suite N times and fails if ANY pass has a
-- failure — a cheap flakiness hunt that surfaces non-deterministic
-- tests (the class that hid the #288 runArgv lazy-I/O race). Default 1.
testRepeatCount :: IO Int
testRepeatCount = do
  mv <- lookupEnv "HASKELL_FLOWS_TEST_REPEAT"
  pure $ case mv >>= readMaybe of
    Just n | n >= 1 -> n
    _              -> 1

-- Regression tests for the repeat-runner env parsing (Phase 1). Each
-- sets-then-unsets HASKELL_FLOWS_TEST_REPEAT so it never leaks to a
-- sibling test; the main loop reads the count once at startup so these
-- mutations can't change the in-flight repeat count.
testRepeatCountDefaultsToOne :: IO Bool
testRepeatCountDefaultsToOne = do
  unsetEnv "HASKELL_FLOWS_TEST_REPEAT"
  (== 1) <$> testRepeatCount

testRepeatCountReadsValid :: IO Bool
testRepeatCountReadsValid = do
  setEnv "HASKELL_FLOWS_TEST_REPEAT" "5"
  n <- testRepeatCount
  unsetEnv "HASKELL_FLOWS_TEST_REPEAT"
  pure (n == 5)

testRepeatCountClampsZero :: IO Bool
testRepeatCountClampsZero = do
  setEnv "HASKELL_FLOWS_TEST_REPEAT" "0"
  n <- testRepeatCount
  unsetEnv "HASKELL_FLOWS_TEST_REPEAT"
  pure (n == 1)

testRepeatCountFallsBackOnGarbage :: IO Bool
testRepeatCountFallsBackOnGarbage = do
  setEnv "HASKELL_FLOWS_TEST_REPEAT" "not-a-number"
  n <- testRepeatCount
  unsetEnv "HASKELL_FLOWS_TEST_REPEAT"
  pure (n == 1)

main :: IO ()
main = do
  repeats <- testRepeatCount
  outcomes <- replicateM repeats $ do
    ok <- runAllTests
    when (repeats > 1) $
      putStrLn ("=== suite pass: " <> (if ok then "PASS" else "FAIL") <> " ===")
    pure ok
  if and outcomes then exitSuccess else exitFailure

-- | One full pass of the suite. Extracted from 'main' so it can be run
-- N times under HASKELL_FLOWS_TEST_REPEAT; returns True iff every test
-- in the pass passed.
runAllTests :: IO Bool
runAllTests = do
  results <-
    sequence $
      [ test "parseGhcErrors extracts header"    testParseHeader
      , test "code tools: all 5 registered"        testCodeToolsRegistered
      , test "#261: pathToModule derives module from path" testArbitraryPathToModule
      , test "#261: renderArbitraryModule emits Wno-orphans module" testArbitraryModuleRender
      -- Issue #214 — no-args reload uses library stanza, not test-suite stanza
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
      , quickTest "prop_chooseStoreModule_nonIdent_uses_hint" prop_chooseStoreModule_nonIdent_uses_hint
      , quickTest "prop_chooseStoreModule_ident_no_info_uses_hint" prop_chooseStoreModule_ident_no_info_uses_hint
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
      , test "#CI-coverage: renderResult status=failed still includes inventory"
                                                   testToolchainStatusFailedIncludesInventory
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
      , test "Envelope #90 Phase D: legacy 'success' field dropped"
                                                   testEnvelopeLegacySuccessDropped
      , test "Envelope #90 Phase D: legacy 'error_kind' field dropped"
                                                   testEnvelopeLegacyErrorKindDropped
      -- W6.8: the ghcide route (routeIde) serves browse/complete/goto/
      -- hole/info; their resolution contracts moved to the e2e suite.
      -- The unit surface keeps the pure payload layers they share.
      , test "#145: ghc_complete zero hits + qualified prefix → remediation hint"
                                                   testCompleteQualifiedRemediation
      , test "#225: ghc_complete qualified remediation names module and suggests bare prefix"
                                                   testCompleteQualifiedRemediation225
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
      , test "parseTypedHoles: bare-name fits (GHC 9.12, ghcide render)"
                                                                 testBareNameFitsGhc912
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
      , test "parseTypeParams extracts one tyvar"   testTypeParamsOne
      , test "parseTypeParams extracts two tyvars"  testTypeParamsTwo
      , test "parseTypeParams empty for monotype"   testTypeParamsNone
      , test "renderTemplate wraps polymorphic T a" testTemplatePolymorphic
      , test "renderTemplate multi-param context"   testTemplateMultiParam
      , test "server wraps runTool in timeout"      testServerOuterTimeout
      , test "ghc_eval exposes Control.Concurrent"  testEvalContextHasControlConcurrent
      , test "ghc_eval enforces inner per-call budget" testEvalInnerTimeoutBudget
      , test "classify: mid-text `No instance for Arbitrary' (EXC-wrapped)"
                                                   testClassifyMissingArbitraryMidText
      , test "classify: mid-text `No instance for Show (IO ())'" 
                                                   testClassifyShowIoMidText
      , test "classify: EXC-wrapped scope error is ECScope"
                                                   testClassifyScopeWrappedInExc
      , test "classify: timeout budget is ECTimeout" testClassifyTimeoutText
      , test "classify: evalErrorKind total mapping" testEvalErrorKindMapping
      , quickTest "prop_fieldSplit_roundtrip"            prop_fieldSplit_roundtrip
      , quickTest "prop_listFieldOf_format_invariant"    prop_listFieldOf_format_invariant
      , quickTest "prop_projectModuleFiles_dotpaths"     prop_projectModuleFiles_dotpaths
      , quickTest "prop_splitStanzas_header_order"       prop_splitStanzas_header_order
      , quickTest "prop_moduleKeyOf_dot_roundtrip"       prop_moduleKeyOf_dot_roundtrip
      , test "moduleKeyOf: absolute == relative; later marker wins"
                                                   testModuleKeyOfAbsRel
      , quickTest "prop_capOutput_trunc_iff_cut"         prop_capOutput_trunc_iff_cut
      , test "load paths derive interactive imports from source" testLoadAutoImports
      , test "Deferred pass writes to MCP-private build dir"      testDeferredIsolatedOutputs
      , test "ghc_deps add: idempotent no-op returns unchanged"  testDepsAddIdempotent
      , test "ghc_switch_project: empty dir -> create_project"   testSwitchProjectEmptyDir
      , test "ghc_add_modules: accepts stanza param"            testAddModulesStanzaParam
      , test "ghc_quickcheck: widens scope via :m +"            testQuickCheckScopeWidening
      , test "ghc_quickcheck: runner uses :{ do :} not bare <-"  testQuickCheckRunnerDoBrace
      , test "initialize emits instructions field"  testInitializeEmitsInstructions
      , test "instructions mention key tools+flows" testInstructionsMentionCore
      , test "nextStep: create_project -> deps"     testNextStepCreateProject
      , test "#266 xsession: empty ledger reads empty" testSessionLedgerEmpty
      -- Phase 5: cross-tool nextStep arms
      , test "suggest: functor fmap two laws"      testSuggestFunctorFmap
      , test "suggest: evaluator preservation"     testSuggestEvaluatorPreservation
      , test "suggest: constant-folding soundness" testSuggestConstFoldingSoundness
      , test "suggest: evaluator needs sibling"    testSuggestEvaluatorNoSibling
      , test "gate: tool registered in inventory"  testGateRegistered
      , test "gate: all-skip parses + passes"      testGateAllSkip
      , test "#138: gate all-skip returns refused/validation" testGateAllSkipRefused
      , test "#138: gate summary avoids empty-verbs malform" testGateSummaryNoEmptyVerbs
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
      , test "B-1: schemaPropertyNames extracts declared keys"    testSchemaPropertyNames
      , test "B-1: unknownArgKeys detects base_dir as unknown"   testUnknownArgKeys
      , test "B-1: didYouMean suggests path for pathh"           testDidYouMeanBaseDir
      , test "B-1: unknownArgsWarning fires on bogus param"      testUnknownArgsWarningFires
      , test "B-1: unknownArgsWarning silent on clean call"      testUnknownArgsWarningSilentWhenClean
      , test "add_import: missing hoogle returns success=false (#53)" testAddImportMissingHoogle
      , test "#146: addImportToSession rejects invalid import gracefully" testAddImportToSessionInvalid
      , test "#146: addImportToSession accepts valid base import"       testAddImportToSessionValid
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
      , test "B-5: extractRedundantNames pulls GHC-38856 names" testExtractRedundantNames38856
      , test "B-5: parseImportList splits a simple import" testParseImportListSimple
      , test "B-5: parseImportList declines complex export forms" testParseImportListComplexDeclines
      , test "B-5: planFor38856 partial → rewrite import list" testPlanFor38856Partial
      , test "B-5: planFor38856 all-redundant → drop line" testPlanFor38856DropsWhenEmpty
      , test "B-5: planFor38856 no message → advise only" testPlanFor38856NoMessageAdvises
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
      , test "workflow-state: initial empty"       testWorkflowStateInitial
      , test "workflow-state: renderHelp thresholds" testWorkflowStateHelp
      , test "#263: discover returns at most 5"         testDiscoverAtMostFive
      , test "#263: discover ranks phase-relevant"      testDiscoverPhaseRelevance
      , test "#264: plan matches module-with-qc template" testPlanMatchesModuleQc
      , test "#264: plan low-confidence lists alternatives" testPlanLowConfidenceListsAlternatives
      , test "#284: plan scaffolds all named modules"  testPlanMultiModule
      , test "#284: plan caps confidence on complex goal" testPlanComplexGoalCapped
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
      , test "nextStep: every tool covered or whitelisted" testNextStepFullCoverage
      , test "harness: tools/list golden freeze (#268 companion)" testToolsListGolden
      , test "harness: nextStep sweep — no unregistered refs"   testNextStepSweep
      , test "harness: no raw string emitters in src/"          testNoRawNextStepStrings
      , test "harness: action specs round-trip"                 testActionSpecsTotal
      , test "staleness: wired into server (static)"  testStalenessWired
      , test "#280: identity differs -> stale"        testStalenessIdentityDiffers
      , test "#280: identity matches -> fresh"         testStalenessIdentityMatches
      , test "#265: progress notification has spec shape" testProgressNotificationShape
      , test "#265: progressToken extracted from _meta"   testProgressTokenPresent
      , test "#265: no _meta -> no progress token"        testProgressTokenAbsent
      , test "#265: collecting sink receives events"      testProgressCollectingSink
      , test "#265: no subscription -> noop sink"         testProgressNoSubscriptionNoop
      , test "workflow: phase pre-scaffold"           testPhasePreScaffold
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
      , test "remove_modules: tool registered"        testRemoveModulesRegistered
      , test "remove_modules: strips exposed entry"   testRemoveModulesStripsCabal
      , test "remove_modules: idempotent no-op"       testRemoveModulesIdempotent
      , test "remove_modules: preserves other fields" testRemoveModulesPreservesFields
      , test "#157: remove_modules strips other-modules entry"    testRemoveModulesOtherModules
      , test "#157: remove_modules other-modules idempotent"      testRemoveModulesOtherModulesIdempotent
      , test "#157: remove_modules finds both sections"           testRemoveModulesBothSections
      , test "#248: remove_modules not_found for non-existent module" testRemoveModulesNotFoundField
      , test "gate: runStep catches exceptions"       testGateRunStepCatchesExceptions
      , test "gate: cabalStep uses bracket + partial safe" testGateCabalStepBracket
      , test "bootstrap: tool registered"             testBootstrapRegistered
      , test "bootstrap: preview returns dynamic content" testBootstrapPreview
      , test "#193: bootstrap default (no write arg) writes to disk" testBootstrapDefaultWrite
      , test "bootstrap: write persists to disk"      testBootstrapWrite
      , test "bootstrap: pathForHost is closed enum"  testBootstrapPathEnum
      , test "#179: bootstrap write=true nextStep says rules written" testBootstrapWriteNextStep
      , test "#179: bootstrap preview nextStep says re-run with write=true" testBootstrapPreviewNextStep
      , test "release: workflow file exists + well-formed" testReleaseWorkflow
      , test "ghc-api: GhcSession boots + exprType roundtrip" testGhcSessionBoots
      , test "ghc-api: HscEnv persists across withGhcSession calls" testGhcSessionPersists
      , test "ghc-api: bootstrapProject captures cabal flags for library" testCabalBootstrapLibrary
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
      , test "ghc-api: absolutizePathArg single-token shapes (#43)"
                                                                 testAbsolutizePathArgSingleToken
      , test "ghc-api: absolutizePathArg eq-form (#43)"           testAbsolutizePathArgEqForm
      , test "ghc-api: absolutizeStanzaFlags two-token pairs (#43)"
                                                                 testAbsolutizeStanzaFlagsTwoToken
      , test "ghc-api: absolutizeStanzaFlags idempotent (#43)"    testAbsolutizeStanzaFlagsIdempotent
      , test "ghc-api: absolutizeStanzaFlags preserves order (#43)"
                                                                 testAbsolutizeStanzaFlagsPreservesOrder
      -- Issue #132 — not_in_scope classification
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
      -- Issue #95 Phase D · nextStep quality gates
      -- Issue #95 Phase C · golden dispatch snapshot
      -- Issue #94 Phase A · tool taxonomy invariants
      , test "#94A: tool count ≤ 50 (surface-bloat cap)"              testToolCountWithinCap
      , test "#94A: every ToolName has a category"                     testEveryToolHasCategory
      , test "#268: TOOL_TAXONOMY.md lists every registered tool"      testTaxonomyDocListsAllTools
      -- Issue #99 Phase B · per-tool version surface
      , test "#99B: every ToolName has a non-empty version"           testEveryToolHasVersion
      -- Issue #94 Phase B · action-discriminated 'modules' primitive
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
      -- Issue #195 — type-sig skip + nextStep routing
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
      , test "#205: compileFailResult dry_run=true propagates to result field"  testRefactorCompileFailDryRunTrue
      , test "#205: extractFreeVarNames picks up not-in-scope variables"        testExtractFreeVarNames
      , test "#205: extractFreeVarNames empty when no not-in-scope errors"      testExtractFreeVarNamesEmpty
      , test "#205: compileFailResult adds note for free-variable errors"       testRefactorFreeVarNote
      , test "#201: extractQcOutputAt slices indexed sentinel output"          testExtractQcOutputAt
      -- Issue #200 — regression_pct precision
      -- Issue #135 — summariseMeasurementErrors truncation
      -- Issue #108 — typed-hole reclassification in check_module + refactor
      -- Issue #188 — check_module uses loadSpecificFileForTarget
      -- Issue #109 — .cabal comment-stripping in check_project
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
      -- Issue #114 — ghc_imports dedup via nubBy importKey
      , test "#114: nubBy dedup removes duplicate module keys"             testImportsNubByDeduplication
      -- Issue #119 — DX paper-cuts batch
      , test "#119: Env.GateFailure exists in enum + wire form"            testGateFailureKindExists
      , test "#119: removeDep unchangedResult has no verb field"           testUnchangedResultNoVerb
      -- Issue #110 — ghc_load outside hs-source-dirs validation
      , test "#110: Env.OutsideSourceDirs exists in enum + wire form"     testOutsideSourceDirsKindExists
      -- Issue #166 — ghc_load must not pick up unregistered src/ files
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
      -- Issue #129 — ghc_check_project deadline-based timeout
      , test "#250: renderRunLine uses module name not path"  testRenderRunLineUsesModuleName
      , test "#244: findCommonStanzaWithPkg finds stanza containing pkg" testDepsCommonStanzaPkgFound
      , test "#244: findCommonStanzaWithPkg returns Nothing when pkg absent" testDepsCommonStanzaPkgAbsent
      , test "#244: findCommonStanzaWithPkg returns Nothing when no common stanzas" testDepsCommonStanzaNoCommon
      , test "#244: unchangedResult' emits hint field when mHint=Just" testDepsUnchangedResultHintField
      , test "#243: suggest route splices evalContextExtras into its queries"   testSuggestCallsAugmentContext
      , test "#242: add_import bypasses Hoogle for module-path names (source check)" testAddImportBypassesHoogle
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
      -- Phase 1 — HASKELL_FLOWS_TEST_REPEAT repeat-runner env parsing
      , test "repeat-runner: defaults to 1 when env unset"          testRepeatCountDefaultsToOne
      , test "repeat-runner: reads a valid positive int"           testRepeatCountReadsValid
      , test "repeat-runner: clamps 0 to 1"                        testRepeatCountClampsZero
      , test "repeat-runner: falls back to 1 on non-numeric"       testRepeatCountFallsBackOnGarbage
      ]
      ++ scratchTests
      ++ [ test "F2: deliverOnce — first delivery wins, second is a no-op" testDeliverOnceFirstWins
         , test "F2: deliverOnce runs the winner's action" testDeliverOnceRunsWinnerAction
         ]
  pure (and results)

-- ---------------------------------------------------------------------------
-- F1 — ghcide backend (HaskellFlows.Ghc.IdeSession)
-- ---------------------------------------------------------------------------

-- The eval runner surfaces compile failures as exceptions whose
-- rendered text buries the diagnostic behind the runner's "EXC:"
-- prefix and GHC's location header (GHC User's Guide, "Error
-- messages": every diagnostic renders as
-- <file>:<line>:<col>: error: [GHC-xxxxx] followed by the body).
-- 'classifyEvalError' must therefore find its markers ANYWHERE in
-- the message. The regression these tests pin classified such texts
-- as ECException, which stalled the anchor chain (Sandbox escape
-- 1/3) and hid the honest missing_instance taxonomy
-- (Missing Arbitrary 0/2).

testClassifyMissingArbitraryMidText :: IO Bool
testClassifyMissingArbitraryMidText = pure $
  evClass (classifyEvalError excArbitrary) == ECMissingInstance "arbitrary"
  where
    excArbitrary = T.pack $
      "EXC: <interactive>:1:35: error: [GHC-39999]\n\
      \    * No instance for `Arbitrary Foo'\n\
      \        arising from a use of `quickCheckWithResult'"

testClassifyShowIoMidText :: IO Bool
testClassifyShowIoMidText = pure $
  evClass (classifyEvalError excShowIo) == ECIoWrapper
  where
    excShowIo = T.pack $
      "EXC: <interactive>:1:41: error: [GHC-39999]\n\
      \    * No instance for `Show (IO ())'\n\
      \        arising from a use of `print'"

testClassifyScopeWrappedInExc :: IO Bool
testClassifyScopeWrappedInExc = pure $
  evClass (classifyEvalError
    "EXC: <interactive>:1:1: error: Variable not in scope: foo")
    == ECScope

testClassifyTimeoutText :: IO Bool
testClassifyTimeoutText = pure $
  evClass (classifyEvalError "timeout after 30s") == ECTimeout

testEvalErrorKindMapping :: IO Bool
testEvalErrorKindMapping = pure $ and
  [ evalErrorKind (EvalError ECTimeout "") == Env.InnerTimeout
  , evalErrorKind (EvalError (ECMissingInstance "arbitrary") "")
      == Env.MissingInstance
  , evalErrorKind (EvalError ECCompile "") == Env.CompileError
  , evalErrorKind (EvalError ECException "") == Env.CompileError
  , evalErrorKind (EvalError ECIoWrapper "") == Env.CompileError
  ]

-- W5.2 — HaskellFlows.Parser.Cabal: the pure .cabal seam extracted
-- from IdeSession. Laws pin the two renderings cabal itself
-- accepts (inline comma-joined vs one-per-line block) and the
-- module->path mapping the anchor chain depends on.

-- | Capitalized Haskell identifier segment ("Foo", "Bar9").
arbIdent :: QC.Gen Text
arbIdent = do
  c <- QC.elements ['A'..'Z']
  rest <- QC.listOf (QC.elements (['a'..'z'] ++ ['0'..'9'] ++ "_"))
  pure (T.pack (c : rest))

-- | Dot-joined module name ("Foo", "Expr.Sub").
arbModule :: QC.Gen Text
arbModule = do
  segs <- QC.listOf1 arbIdent
  pure (T.intercalate "." segs)

instance QC.Arbitrary Text where
  arbitrary = T.pack <$> QC.arbitrary

prop_fieldSplit_roundtrip :: Text -> Text -> Property
prop_fieldSplit_roundtrip k v =
  not (T.any (== ':') k)
    && k == T.strip k
    && not (T.null (T.strip v))
    && v == T.strip v ==>
    fieldSplit (k <> ": " <> v) === (k, v)

-- | Both list renderings the Cabal grammar accepts must yield the
-- same items — the parser cannot prefer a style.
prop_listFieldOf_format_invariant :: Property
prop_listFieldOf_format_invariant =
  QC.forAll (QC.listOf1 arbModule) $ \items ->
    listFieldOf "exposed-modules" (inline items) === items
      .&&. listFieldOf "exposed-modules" (block items) === items
  where
    inline is = ["exposed-modules: " <> T.intercalate ", " is]
    block is = case is of
      []       -> []
      (i0 : rest) -> ("exposed-modules: " <> i0) : [ "          " <> i | i <- rest ]

prop_projectModuleFiles_dotpaths :: Property
prop_projectModuleFiles_dotpaths =
  QC.forAll (QC.listOf1 arbModule) $ \mods ->
    projectModuleFilesFromCabal (cabalOf mods)
      === [ "src/" <> T.unpack (T.replace "." "/" m) <> ".hs" | m <- mods ]
  where
    cabalOf ms = T.unlines
      [ "name: prop-cabal"
      , "library"
      , "    exposed-modules: " <> T.intercalate ", " ms
      ]

-- | 'splitStanzas' keeps every recognized header, in order, and
-- nothing else decides stanza identity.
prop_splitStanzas_header_order :: Property
prop_splitStanzas_header_order =
  QC.forAll (QC.listOf arbLine) $ \ls ->
    map fst (splitStanzas ls) === [ h | Just h <- map stanzaHeaderOf ls ]
  where
    arbLine = QC.oneof
      [ pure "library"
      , ("test-suite " <>) <$> arbIdent
      , (\i -> "    field-" <> i <> ": v") <$> arbIdent
      , pure "    indented body line"
      ]

-- | #74 law: dots in the module name become path separators and
-- back — the store key round-trips.
prop_moduleKeyOf_dot_roundtrip :: Property
prop_moduleKeyOf_dot_roundtrip =
  QC.forAll arbModule $ \m ->
    moduleKeyOf ("src/" <> T.replace "." "/" m <> ".hs") === m

testModuleKeyOfAbsRel :: IO Bool
testModuleKeyOfAbsRel = pure $ and
  [ moduleKeyOf "src/Foo.hs" == "Foo"
  , moduleKeyOf "/root/x/src/Foo.hs" == "Foo"
  , moduleKeyOf "test/Spec.hs" == "Spec"
  , moduleKeyOf "src/Expr/S.hs" == "Expr.S"
    -- the LATER marker wins: src/test/ nests under test/
  , moduleKeyOf "src/test/Foo.hs" == "Foo"
    -- no marker at all: the whole path (minus extension) is the key
  , moduleKeyOf "weird/Foo.hs" == "weird.Foo"
    -- markers are DIRECTORY names: a segment ending in "src"/"test"
    -- is not one (substring matching here broke the dot round-trip)
  , moduleKeyOf "src/Foo_src/Bar.hs" == "Foo_src.Bar"
  , moduleKeyOf "Qc_test/Foo.hs" == "Qc_test.Foo"
  ]

-- | W5.3 — the single capping law: 'truncated' is True iff the
-- output was actually cut, and the kept prefix is the cap-long head.
prop_capOutput_trunc_iff_cut :: Int -> Text -> Property
prop_capOutput_trunc_iff_cut cap t =
  cap >= 0 ==>
    let (capped, wasTr) = capOutput cap t
    in (wasTr === (T.length t > cap))
         .&&. (capped === T.take cap t)

-- ---------------------------------------------------------------------------
-- F2 — concurrent transport (HaskellFlows.Mcp.Transport)
-- ---------------------------------------------------------------------------

testDeliverOnceFirstWins :: IO Bool
testDeliverOnceFirstWins = do
  gate <- newMVar ()
  r1 <- deliverOnce gate (pure ())
  r2 <- deliverOnce gate (pure ())
  pure (r1 && not r2)

testDeliverOnceRunsWinnerAction :: IO Bool
testDeliverOnceRunsWinnerAction = do
  gate <- newMVar ()
  ref <- newIORef False
  _ <- deliverOnce gate (writeIORef ref True)
  _ <- deliverOnce gate (writeIORef ref False)
  readIORef ref
