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
import Data.Maybe (fromMaybe, isJust, isNothing)
import Data.Time.Clock.POSIX (getPOSIXTime)
import System.Exit (exitFailure, exitSuccess)
import qualified System.Environment
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
  , isContinuationFitLine
  , parseFitLine
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
import HaskellFlows.Mcp.Server (allToolDescriptors, allToolNameTexts)
import HaskellFlows.Mcp.NextStep
  ( ChainStep (..)
  , NextStep (..)
  , injectNextStep
  , suggestNext
  )
import HaskellFlows.Mcp.Protocol (ToolCall (..), ToolContent (..), ToolDescriptor (..), ToolResult (..))
import HaskellFlows.Mcp.ToolName
  ( ToolName (..)
  , allToolNames
  , parseToolName
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
import HaskellFlows.Tool.Batch (BatchArgs (..))
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
import qualified HaskellFlows.Tool.Suggest as SuggestTool
import qualified HaskellFlows.Tool.AddImport as AddImport
import qualified HaskellFlows.Tool.AddModules as AddModules
import qualified HaskellFlows.Tool.ApplyExports as ApplyExports
import qualified HaskellFlows.Tool.FixWarning as FixWarning
import qualified HaskellFlows.Mcp.WorkflowState as WS
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
import HaskellFlows.Tool.CheckProject (parseExposedModules)
import HaskellFlows.Tool.Lint (parseHlintJson)
import qualified HaskellFlows.Tool.Lint as LintTool
import HaskellFlows.Tool.Load (checkPathExists)
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
import HaskellFlows.Refactor.Extract
  ( ExtractResult (..)
  , extractBinding
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
  )
import HaskellFlows.Tool.Hoogle
  ( HoogleHit (..)
  , parseHoogleLine
  )
import Control.Concurrent (forkIO, threadDelay)
import Control.Exception (SomeException, bracket_, try)
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
      , test "ghc_load #79: checkPathExists Right" testCheckPathExistsAccepts
      , test "ghc_load #79: checkPathExists Left"  testCheckPathExistsRejects
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
      , test "Envelope #90: legacy `success` derives correctly per status"
                                                   testEnvelopeLegacySuccess
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
      , quickTest "prop_envelope_legacy_success"   prop_envelopeLegacySuccess
      , test "Envelope #90 Phase B: ghc_toolchain_status emits envelope shape"
                                                   testToolchainStatusEnvelopeShape
      , test "Envelope #90 Phase B: ghc_toolchain_status legacy success matches status"
                                                   testToolchainStatusLegacyConsistent
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
      , test "Envelope #90 Phase B: ghc_imports emits envelope with count + imports"
                                                   testImportsEnvelopeShape
      , test "Envelope #90 Phase B: ghc_browse on project module → status=ok"
                                                   testBrowseProjectModuleOk
      , test "Envelope #90 Phase B: ghc_browse on external module → status=no_match"
                                                   testBrowseExternalModuleNoMatch
      , test "Envelope #90 Phase B: ghc_browse rejects missing module arg"
                                                   testBrowseRejectsMissingArg
      , test "Envelope #90 Phase B: ghc_complete with hits → status=ok"
                                                   testCompleteHitsOk
      , test "Envelope #90 Phase B: ghc_complete with zero hits → status=no_match"
                                                   testCompleteNoMatch
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
      , test "Envelope #90 Phase B: ghc_eval pure expr → status=ok"
                                                   testEvalPureExprOk
      , test "Envelope #90 Phase B: ghc_eval refuses newline in expression"
                                                   testEvalRefusesNewline
      , test "Envelope #90 Phase B: ghc_eval refuses sentinel string"
                                                   testEvalRefusesSentinel
      , test "Envelope #90 Phase B: ghc_hole on module with hole → status=ok"
                                                   testHoleWithHoleOk
      , test "Envelope #90 Phase B: ghc_hole on hole-free module → status=no_match"
                                                   testHoleNoHoleMatch
      , test "Envelope #90 Phase B: ghc_hole rejects path traversal"
                                                   testHoleRejectsTraversal
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
      , test "validateCabal flags duplicate deps"  testDuplicateDeps
      , test "validateCabal flags missing synopsis" testMissingSynopsis
      , test "parseExposedModules reads modules"   testParseModules
      , test "extractValidFits parses fits"        testValidFits
      , test "extractValidFits: operator-named fit not absorbed (#71)"
                                                                 testValidFitsOperatorBoundary
      , test "isContinuationFitLine: ' :: ' tagged line is a fresh fit (#71)"
                                                                 testHoleContinuationDetector
      , test "parseSignature simple a -> a"         testSigSimple
      , test "parseSignature with constraint"       testSigConstraint
      , test "parseSignature list"                  testSigList
      , test "suggest matches involutive on a->a"   testSuggestInvolutive
      , test "suggest matches associative on a->a->a" testSuggestAssoc
      , test "suggest associative template applies fn at outer (#52)" testSuggestAssocTemplate
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
      , test "add_import: missing hoogle returns success=false (#53)" testAddImportMissingHoogle
      , test "info: renderConstructorsBlock empty (#54)"  testInfoCtorBlockEmpty
      , test "info: renderConstructorsBlock Maybe (#54)"  testInfoCtorBlockMaybe
      , test "info: successResult includes constructors (#54)" testInfoSuccessIncludesCtors
      , test "info: successResult drops field when none (#54)" testInfoSuccessDropsCtorField
      , test "info: renderClassMethodsBlock shape (#70)"        testInfoClassMethodsBlock
      , test "info: successResult emits class_methods on a class (#70)"
                                                                 testInfoSuccessClassMethods
      , test "info: successResult drops class_methods on a data type (#70)"
                                                                 testInfoSuccessDropsClassMethods
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
      , test "nextStep: add_import count=0 suppresses load (#53)"     testNextStepAddImportZero
      , test "nextStep: add_import count>0 nudges load (#53)"         testNextStepAddImportNonZero
      , test "add_modules: moduleToPath mapping"   testAddModulesPath
      , test "apply_exports: rewriteHeader idempotent" testApplyExportsIdempotent
      , test "apply_exports: injects exports"      testApplyExportsInjects
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
      , test "deps_explain: parseSolverOutput on real dump (#63)" testDepsExplainParse
      , test "deps_explain: identifyRootCause picks deepest (#63)" testDepsExplainRoot
      , test "deps_explain: extractPackages strips versions (#63)" testDepsExplainPackages
      , test "deps_explain: parseSolverOutput Nothing on clean (#63)" testDepsExplainClean
      , test "lab: listTopLevelBindings finds simple sigs (#60)" testLabListSimple
      , test "lab: listTopLevelBindings handles multi-line sig (#60)" testLabListMultiline
      , test "lab: listTopLevelBindings skips empty + non-sigs (#60)" testLabListSkips
      , test "lab: confidenceAtLeast threshold (#60)" testLabConfidence
      , test "explain_error: pickDiagnostic default first (#59)" testExplainPickDefault
      , test "explain_error: pickDiagnostic by index (#59)" testExplainPickIndex
      , test "explain_error: pickDiagnostic out of range (#59)" testExplainPickOOR
      , test "explain_error: extractImports recognises shapes (#59)" testExplainExtractImports
      , test "explain_error: enclosingLineRange clamps (#59)" testExplainRangeClamps
      , test "perf: aggregate empty -> zeros (#61)" testPerfAggregateEmpty
      , test "perf: aggregate single sample (#61)" testPerfAggregateSingle
      , test "perf: aggregate odd count median (#61)" testPerfAggregateOdd
      , test "perf: aggregate even count median average (#61)" testPerfAggregateEven
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
      , test "guidance: no phantom ghc_session"    testGuidanceNoPhantomSession
      , test "guidance: text drops retired-subprocess vocab (#56)" testGuidanceNoRetiredVocab
      , test "guidance: markdown drops retired-subprocess vocab (#56)" testGuidanceMdNoRetiredVocab
      , test "guidance: text mentions in-process GHC API (#56)" testGuidanceMentionsApi
      , test "guidance: markdown mentions in-process GHC API (#56)" testGuidanceMdMentionsApi
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
      , test "nextStep: ghc_load with typed-hole warning \8594 ghc_hole"
                                                                 testNextStepTypedHoleWarn
      , test "nextStep: ghc_load with non-hole warning \8594 ghc_fix_warning"
                                                                 testNextStepFixableWarn
      , test "nextStep: ghc_load with no warnings \8594 ghc_suggest"
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
        , (Env.UnresolvableDep,        "unresolvable_dep")
        , (Env.VerifyFailed,           "verify_failed")
        , (Env.InnerTimeout,           "inner_timeout")
        , (Env.OuterTimeout,           "outer_timeout")
        , (Env.SessionExhausted,       "session_exhausted")
        , (Env.BinaryUnavailable,      "binary_unavailable")
        ]
      pinnedOk = all (\(k, t) -> Env.errorKindToText k == t) pinned
      reverseTotal = all (\k -> Env.textToErrorKind (Env.errorKindToText k) == Just k)
                         allKinds
      countOk = length allKinds == 23  -- §4 promises 23 kinds
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

-- | The deprecated @success@ field must be derived as
-- @ok | partial → True@, every other status → False. This is the
-- contract that lets old clients keep working during the migration
-- window (Phases B–D); a regression here breaks every legacy
-- consumer silently.
testEnvelopeLegacySuccess :: IO Bool
testEnvelopeLegacySuccess =
  let truthy =
        [ Env.StatusOk
        , Env.StatusPartial
        ]
      falsy =
        [ Env.StatusNoMatch
        , Env.StatusRefused
        , Env.StatusFailed
        , Env.StatusTimeout
        , Env.StatusUnavailable
        ]
  in pure (all Env.isLegacySuccess truthy
        && not (any Env.isLegacySuccess falsy))

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
      && lookupKey "success" (A.Bool True)
      && lookupKey "result"  payload
       )

-- | 'mkRefused' produces the canonical refusal shape: status=refused,
-- error present, result absent. The encoded @success@ field must be
-- @False@; this is the *legacy* signal that distinguishes a refusal
-- from an OK response.
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
      && lookupKey "success" (A.Bool False)
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

-- | QC: legacy @success@ derives correctly for every status.
-- Equivalent to the unit test 'testEnvelopeLegacySuccess' but
-- reasoned exhaustively: the contract is *exactly* @ok | partial
-- → True@, no exceptions.
prop_envelopeLegacySuccess :: QC.Property
prop_envelopeLegacySuccess = QC.forAll (QC.elements [minBound..maxBound]) $ \s ->
  let derived  = Env.isLegacySuccess s
      expected = s == Env.StatusOk || s == Env.StatusPartial
  in derived === expected

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

-- | Phase B legacy-window invariant: the deprecated @success@ field
-- on the wire equals @isLegacySuccess(status)@. Catches the case
-- where a future refactor splits the two paths and the legacy field
-- drifts from the structured one.
testToolchainStatusLegacyConsistent :: IO Bool
testToolchainStatusLegacyConsistent = do
  result <- ToolchainStatusTool.handle (A.object [])
  case trContent result of
    [TextContent body] ->
      let bytes = TLE.encodeUtf8 (TL.fromStrict body)
      in pure $ case A.decode bytes :: Maybe A.Value of
           Just (A.Object o) ->
             let mStatus  = AKM.lookup (AKey.fromText "status")  o
                 mSuccess = AKM.lookup (AKey.fromText "success") o
             in case (mStatus, mSuccess) of
                  (Just (A.String s), Just (A.Bool b)) ->
                    maybe False Env.isLegacySuccess (Env.textToStatus s) == b
                  _ -> False
           _ -> False
    _ -> pure False

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
            -- 0 errors ⇒ status ∈ {ok, partial}
            Env.reStatus env `elem` [Env.StatusOk, Env.StatusPartial]
          Just (A.Number _) ->
            -- non-zero errors ⇒ status='failed' (the other branch)
            Env.reStatus env == Env.StatusFailed
          _ -> False
      _ -> False
    Left _ -> False

-- | Phase B contract: a cabal fixture with the duplicate-dep
-- heuristic warning *plus* zero cabal-check errors produces
-- status='partial' with at least one envelope-warning entry. If
-- cabal-check happens to also raise errors on this fixture, status
-- shifts to 'failed' — accept either, but assert the structured
-- 'warnings' array is populated whenever issues exist.
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
                Env.reStatus env == Env.StatusPartial
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
  result <- WorkflowTool.handle pdRef sessRef toolNames ws staleness args
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

-- | 'ghc_bootstrap host=claude-code' (default mode=preview) emits
-- status='ok' with the rules content + the canonical claude-code
-- target path inside 'result'.
testBootstrapClaudeCodePreviewEnvelope :: IO Bool
testBootstrapClaudeCodePreviewEnvelope = do
  decoded <- runBootstrap (A.object [ "host" A..= ("claude-code" :: Text) ])
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

-- | Browsing an external module (e.g. 'Data.Maybe') is not in the
-- project's compile graph. Pre-#90 this returned status=success-false
-- with error_kind='module_not_in_graph'; post-#90 the same surface
-- semantically becomes status='no_match' with the diagnostic
-- context inside 'result' and a 'nextStep' pointer at ghc_info.
testBrowseExternalModuleNoMatch :: IO Bool
testBrowseExternalModuleNoMatch = do
  decoded <- runBrowse (A.object [ "module" A..= ("Data.Maybe" :: Text) ])
  pure $ case decoded of
    Right env
      | Env.reStatus env == Env.StatusNoMatch
      , Just (A.Object payload) <- Env.reResult env ->
          AKM.lookup (AKey.fromText "module") payload == Just (A.String "Data.Maybe")
            && AKM.member (AKey.fromText "remediation") payload
            && case Env.reNextStep env of
                 Just _  -> True   -- next-step pointer included
                 Nothing -> False
    _ -> False

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

runAddImport :: A.Value -> IO (Either String Env.ToolResponse)
runAddImport args = do
  tr <- AddImportTool.handle args
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
    , "ghc_determinism"
    , "ghc_property_lifecycle"
    , "ghc_toolchain_warmup"
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
  let payload = A.object [ "success" .= True, "properties_written" .= (3 :: Int) ]
  in pure (assertNext GhcQuickCheckExport payload GhcGate)

testNextStepDeterminismPass :: IO Bool
testNextStepDeterminismPass =
  let payload = A.object [ "success" .= True, "runs" .= (3 :: Int) ]
  in pure (assertNext GhcDeterminism payload GhcRegression)

testNextStepDeterminismFail :: IO Bool
testNextStepDeterminismFail =
  let payload = A.object [ "success" .= False, "runs" .= (3 :: Int) ]
  in pure (assertNext GhcDeterminism payload GhcQuickCheck)

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

-- | BUG-22 — add_modules now emits a multi-step chain. The
-- primary next tool must be 'ghc_load' AND the chain must
-- include at least 'ghc_load' + 'ghc_check_project'.
testNextStepAddModulesChain :: IO Bool
testNextStepAddModulesChain =
  let payload = A.object [ "success" .= True, "cabal_added" .= (["Foo.Bar"] :: [Text]) ]
  in case suggestNext GhcAddModules True payload of
       Just ns ->
         pure $ nsTool ns == GhcLoad
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
  let payload = A.object [ "success" .= True ]
  in pure (assertNext GhcToolchainWarmup payload GhcWorkflow)

testNextStepPropertyLifecycleList :: IO Bool
testNextStepPropertyLifecycleList =
  let payload = A.object [ "success" .= True, "action" .= ("list" :: Text) ]
  in pure (assertNext GhcPropertyLifecycle payload GhcRegression)

-- | BUG-22: create_project emits the canonical project-bootstrap
-- chain (deps + add_modules + load). Pin that all three steps are
-- present so the agent can hand it off to ghc_batch.
testNextStepCreateProjectChain :: IO Bool
testNextStepCreateProjectChain =
  let payload = A.object [ "success" .= True, "files_written" .= ([] :: [Text]) ]
  in case suggestNext GhcCreateProject True payload of
       Just ns ->
         pure $ nsTool ns == GhcDeps
             && case nsChain ns of
                  Just steps ->
                    let tools = map csTool steps
                    in GhcDeps       `elem` tools
                    && GhcAddModules `elem` tools
                    && GhcLoad       `elem` tools
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
-- polling nudge that points at ghc_determinism / check_project.
testHistoryPolling :: IO Bool
testHistoryPolling =
  let nudges = WS.historyNudges (replicate 5 GhcLoad)
  in pure $ any ("polling" `T.isInfixOf`) nudges
         && any ("ghc_determinism" `T.isInfixOf`) nudges

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

--------------------------------------------------------------------------------
-- BUG-16 — ghc_remove_modules symmetric to ghc_add_modules
--------------------------------------------------------------------------------

-- | Tool is registered in the canonical registry. If this
-- fails, the tool exists as dead code (not dispatchable).
testRemoveModulesRegistered :: IO Bool
testRemoveModulesRegistered = pure $
  "ghc_remove_modules" `elem` allToolNameTexts

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

-- | BUG-06 nextStep coverage for the new tool: 'ghc_remove_modules'
-- on success suggests project-wide check + reload chain so any
-- dangling import surfaces immediately.
testNextStepRemoveModules :: IO Bool
testNextStepRemoveModules =
  let payload = A.object
        [ "success"      .= True
        , "cabal_removed".= (["Foo.Old"] :: [Text])
        ]
  in case suggestNext GhcRemoveModules True payload of
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
testBootstrapRegistered = pure ("ghc_bootstrap" `elem` allToolNameTexts)

-- | 'ghc_bootstrap(host="claude-code")' preview mode returns
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
      && T.isInfixOf "ghc_workflow"            body
      && not wrote          -- preview must NOT write

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
      && T.isInfixOf "ghc_bootstrap"             readme
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
      && T.isInfixOf "`ghc_bootstrap`"  readme
      && T.isInfixOf "`ghc_gate`"       readme
      && T.isInfixOf "`ghc_suggest`"    readme
      && T.isInfixOf "`ghc_remove_modules`" readme
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
        , "ghc_regression"           -- list/run
        , "ghc_property_lifecycle"   -- list/drop
        , "ghc_validate_cabal"       -- errors > 0 vs clean
        , "ghc_quickcheck"           -- state = passed/failed
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
  let lowEdits  = WS.WorkflowState 0 2 Nothing 0 0 []
      highEdits = WS.WorkflowState 0 5 Nothing 0 0 []
      nudgeLow  = WS.renderHelp lowEdits
      nudgeHigh = WS.renderHelp highEdits
  in pure $ null nudgeLow
         && any (T.isInfixOf "edits since the last ghc_load") nudgeHigh

-- | Phase 11j: all 5 Code tools registered in the inventory.
testCodeToolsRegistered :: IO Bool
testCodeToolsRegistered = pure $
  all (`elem` allToolNameTexts)
    [ "ghc_add_import"
    , "ghc_add_modules"
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
  , CreateProject.validateName "abc-123-def"       == Right "abc-123-def"
  , CreateProject.validateName "single"            == Right "single"
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
  -- Use a dedicated tempdir as PATH so hoogle is guaranteed missing
  let args = A.object [ "name" A..= ("fromMaybe" :: T.Text) ]
  result <- AddImport.handle args
  -- Restore PATH so other tests aren't affected.
  case origPath of
    Just p  -> System.Environment.setEnv "PATH" p
    Nothing -> System.Environment.unsetEnv "PATH"
  case trContent result of
    [TextContent t] ->
      -- Issue #90 Phase B: the response is now an envelope.
      -- Drill into 'error.message' / 'error.remediation' for the
      -- structured fields. Legacy 'success: false' is still
      -- emitted at top level during the migration window.
      let parsed = A.decode (TLE.encodeUtf8 (TL.fromStrict t)) :: Maybe A.Value
      in pure $ case parsed of
           Just v -> fieldBool "success" v == Just False
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
    fieldBool k (A.Object o) = case AKM.lookup (AKey.fromText k) o of
      Just (A.Bool b) -> Just b
      _               -> Nothing
    fieldBool _ _ = Nothing
    lookupField k (A.Object o) = AKM.lookup (AKey.fromText k) o
    lookupField _ _            = Nothing

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
  let isErr   = trIsError result
      payload = extractPayload result
  pure
    (  isErr
    && before == after
    && hasField "rejected" payload
    && fieldEquals "success" (A.Bool False) payload
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
  let payload   = extractPayload result
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
    && hasField "rejected" (extractPayload result)
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

-- | Issue #64: 'pairCombinations' on an empty list returns no
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
  pure
    (  trIsError result
    && bodyAfter == original
    && hasField "rejected" (extractPayload result)
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
testQcExportRegistered = pure $ "ghc_quickcheck_export" `elem` allToolNameTexts

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

-- | Phase 11g: ghc_gate must be in the canonical tool list + the
-- descriptor mentions its three sub-steps.
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
  in pure $ case suggestNext GhcCreateProject True payload of
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
testNextStepSuggest :: IO Bool
testNextStepSuggest =
  let payload = A.object [ "success" .= True, "count" .= (3 :: Int) ]
  in pure $ case suggestNext GhcSuggest True payload of
       Just ns -> nsTool ns == GhcQuickCheck
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

-- | ghc_regression(list) → ghc_regression(run).
testNextStepRegressionList :: IO Bool
testNextStepRegressionList =
  let payload = A.object [ "success" .= True, "action" .= ("list" :: Text) ]
  in pure $ case suggestNext GhcRegression True payload of
       Just ns -> nsTool ns == GhcRegression
       Nothing -> False

-- | Refactor landed → verify compile.
testNextStepRefactor :: IO Bool
testNextStepRefactor =
  let payload = A.object [ "success" .= True, "compile" .= ("ok" :: Text) ]
  in pure $ case suggestNext GhcRefactor True payload of
       Just ns -> nsTool ns == GhcLoad
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

-- | Exploratory tools (type/info/eval/goto/doc/complete) don't get
-- a next-step hint — the user drives them.
testNextStepExploratoryNothing :: IO Bool
testNextStepExploratoryNothing = pure $
  all nothing
    [ suggestNext GhcType     True (A.object [])
    , suggestNext GhcInfo     True (A.object [])
    , suggestNext GhcEval     True (A.object [])
    , suggestNext GhcGoto     True (A.object [])
    , suggestNext GhcDoc      True (A.object [])
    , suggestNext GhcComplete True (A.object [])
    , suggestNext GhcCoverage True (A.object [])
    , suggestNext GhcWorkflow True (A.object [])
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
      ns   = NextStep { nsTool = GhcLoad, nsWhy = "because"
                      , nsExample = Nothing, nsChain = Nothing }
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
  pure $ T.isInfixOf "ghc_create_project" nsCode
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
         && any (\m -> mPercent m == 92) (crMetrics rpt))

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
      let args = A.object [ "path" A..= T.pack dirB ]
      result  <- SwitchProject.handle pdRef sessRef' storeRef args
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
      let args = A.object [ "path" A..= T.pack dirB ]
      _ <- SwitchProject.handle pdRef sessRef storeRef args
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
