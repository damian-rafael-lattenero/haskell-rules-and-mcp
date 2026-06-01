-- | Unit tests for Tool.Bootstrap (project-level), docs/release workflow
-- checks, nextStep full coverage, WorkflowState tracking, and session
-- activity counters.
--
-- Extracted from the Spec.hs monolith (#271) via the function-export shape.
module Spec.WorkflowState
  ( testBootstrapRegistered
  , testBootstrapPreview
  , testBootstrapDefaultWrite
  , testBootstrapWrite
  , testBootstrapPathEnum
  , testBootstrapWriteNextStep
  , testBootstrapPreviewNextStep
  , testDocsMainReadme
  , testDocsHaskellReadme
  , testReleaseWorkflow
  , testNextStepFullCoverage
  , testWorkflowStateInitial
  , testWorkflowStateTracks
  , testWorkflowStateHelp
  , testSessionActivityEverCalled
  , testSessionActivityErrorStreak
  , testSessionActivityUnusedCount
  , testArbitraryPathToModule
  , testArbitraryModuleRender
  ) where

import qualified Data.Aeson as A
import Data.Aeson ((.=))
import qualified Data.Aeson.Key as AKey
import qualified Data.Aeson.KeyMap as AKM
import qualified Data.Set as Set
import qualified Data.Text as T
import qualified Data.Text.IO as TIO
import Data.Text (Text)
import Data.Time.Clock.POSIX (posixSecondsToUTCTime)
import System.Directory (doesFileExist, getTemporaryDirectory)
import System.FilePath ((</>))

import HaskellFlows.Mcp.NextStep
import qualified HaskellFlows.Mcp.NextStep as NextStep
import HaskellFlows.Mcp.Server (allToolDescriptors, allToolNameTexts)
import HaskellFlows.Mcp.ToolName (ToolName (..), allToolNames, toolNameText)
import qualified HaskellFlows.Mcp.WorkflowState as WS
import qualified HaskellFlows.Mcp.Guidance as Guidance
import HaskellFlows.Mcp.Protocol (ToolContent (..), ToolResult (..))
import HaskellFlows.Tool.Arbitrary (pathToModule, renderArbitraryModule)
import qualified HaskellFlows.Tool.Bootstrap as Bootstrap
import qualified HaskellFlows.Tool.SwitchProject as SwitchProject
import HaskellFlows.Types (ProjectDir, unProjectDir)

import Spec.Helpers (withTempProject)

unProjectDirRaw :: ProjectDir -> FilePath
unProjectDirRaw = unProjectDir

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

-- | #261: pathToModule derives the module name from a target path,
-- dropping a leading conventional source dir.
testArbitraryPathToModule :: IO Bool
testArbitraryPathToModule = pure $
  pathToModule "src/Expr/Syntax/Arbitrary.hs" == "Expr.Syntax.Arbitrary"
    && pathToModule "test/Foo.hs" == "Foo"

-- | #261: renderArbitraryModule emits a -Wno-orphans module importing
-- QuickCheck + the type's defining module.
testArbitraryModuleRender :: IO Bool
testArbitraryModuleRender =
  let out = renderArbitraryModule "Expr.Syntax.Arbitrary" (Just "Expr.Syntax")
              "instance Arbitrary Expr where arbitrary = pure undefined"
  in pure $ T.isInfixOf "{-# OPTIONS_GHC -Wno-orphans #-}" out
         && T.isInfixOf "module Expr.Syntax.Arbitrary where" out
         && T.isInfixOf "import Test.QuickCheck" out
         && T.isInfixOf "import Expr.Syntax" out
