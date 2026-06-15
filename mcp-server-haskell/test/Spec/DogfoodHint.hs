-- | Unit tests for dogfood-hint firing, tool-description template
-- compliance, SwitchProject empty-dir guard, PathBootstrap helpers,
-- AddModules JSON-array form, and cabal cross-stanza checks.
--
-- Extracted from the Spec.hs monolith (#271) via the function-export shape.
module Spec.DogfoodHint
  ( testWithDogfoodHintFiresWhenSelf
  , testWithDogfoodHintNotFiresWhenNotSelf
  , testWithDogfoodHintNotFiresOnReadTool
  , testDescriptionsMeetTemplate
  , testSwitchAcceptsEmpty
  , testPathBootstrapAbsolute
  , testPathBootstrapExisting
  , testPathBootstrapIdempotent
  , testAddModulesArrayForm
  , testAddModulesStringFallback
  , testCabalStanzaDupCheck
  , testCabalCrossStanzaOk
  , testCabalHsSourceDirsIgnored
  ) where

import qualified Data.Aeson as A
import qualified Data.Text as T
import qualified Data.Text.IO as TIO
import System.Directory (createDirectoryIfMissing, getHomeDirectory, getTemporaryDirectory, removePathForcibly)
import qualified System.Directory
import qualified System.FilePath
import System.FilePath ((</>))

import HaskellFlows.Ghc.CabalBootstrap (bootstrapProject)
import qualified HaskellFlows.Mcp.Envelope as Env
import qualified HaskellFlows.Mcp.PathBootstrap
import HaskellFlows.Mcp.Protocol (ToolDescriptor (..))
import HaskellFlows.Mcp.Server (allToolDescriptors, allToolNameTexts)
import HaskellFlows.Mcp.SelfProject (detectSelfProject)
import HaskellFlows.Types (mkProjectDir)
import HaskellFlows.Ghc.ApiSession (startGhcSession, killGhcSession)
import qualified HaskellFlows.Tool.AddModules as AddModules
import qualified HaskellFlows.Tool.SwitchProject as SwitchProject

import Spec.Helpers (withTempProject)
import Data.Aeson ((.=))
import Data.Maybe (isNothing)
import Data.Text (Text)
import Data.Time.Clock.POSIX (getPOSIXTime)
import Control.Monad (unless)
import qualified HaskellFlows.Mcp.NextStep
import HaskellFlows.Mcp.NextStep
import HaskellFlows.Mcp.ToolName (ToolName (..))
import qualified HaskellFlows.Mcp.SelfProject as SelfProject
import HaskellFlows.Tool.SwitchProject (validateSwitchTarget)
import qualified HaskellFlows.Types
import qualified HaskellFlows.Tool.ValidateCabal as VC

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
