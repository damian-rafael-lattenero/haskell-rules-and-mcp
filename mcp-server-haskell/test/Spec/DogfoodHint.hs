-- | Unit tests for dogfood-hint firing, tool-description template
-- compliance, SwitchProject empty-dir guard, PathBootstrap helpers,
-- AddModules JSON-array form, and cabal cross-stanza checks.
--
-- Extracted from the Spec.hs monolith (#271) via the function-export shape.
module Spec.DogfoodHint
  ( testDescriptionsMeetTemplate
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
