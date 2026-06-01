-- | Unit tests for nextStep routing (#95) and Staleness detection (#280).
-- Tests the post-call hints emitted after Gate, QcExport, Determinism,
-- AddImport, Modules, and other tools.
--
-- Extracted from the Spec.hs monolith (#271) via the function-export shape.
module Spec.NextStepUnit
  ( testNextStepGatePass
  , testNextStepGateFail
  , testNextStepQcExport
  , testNextStepDeterminismPass
  , testNextStepDeterminismFail
  , testClampRunsCapsHigh
  , testClampRunsFloorsLow
  , testClampRunsPassThrough
  , testNextStepAddImport
  , testNextStepAddModulesChain
  , testNextStepApplyExports
  , testNextStepFixWarning
  , testNextStepBrowse
  , testNextStepToolchainWarmup
  , testNextStepPropertyLifecycleList
  , testNextStepCreateProjectChain
  , testStalenessWired
  , testStalenessIdentityDiffers
  , testStalenessIdentityMatches
  ) where

import qualified Data.Aeson as A
import Data.Aeson (Value, object, (.=))
import qualified Data.Aeson.Key as AKey
import Data.Maybe (isNothing)
import qualified Data.Aeson.KeyMap as AKM
import Data.Text (Text)
import qualified Data.Text as T
import qualified Data.Text.IO as TIO

import HaskellFlows.Mcp.NextStep
import qualified HaskellFlows.Mcp.NextStep as NextStep
import HaskellFlows.Mcp.Staleness (StalenessReport (..), binaryIdentityStale)
import HaskellFlows.Mcp.ToolName (ToolName (..))
import qualified HaskellFlows.Tool.Determinism as DeterminismTool

import Spec.Helpers (withTempProject)

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

-- #281: an absurd 'runs' value (e.g. mistaking it for maxSuccess) used to
-- spawn that many cabal-repl subprocesses and crash the MCP. 'clampRuns' now
-- caps the count.
testClampRunsCapsHigh :: IO Bool
testClampRunsCapsHigh =
  pure (DeterminismTool.clampRuns 2000 == DeterminismTool.maxRuns
          && DeterminismTool.maxRuns <= 20)

testClampRunsFloorsLow :: IO Bool
testClampRunsFloorsLow =
  pure (DeterminismTool.clampRuns 0 == 1
          && DeterminismTool.clampRuns (-5) == 1)

testClampRunsPassThrough :: IO Bool
testClampRunsPassThrough =
  pure (DeterminismTool.clampRuns 3 == 3
          && DeterminismTool.clampRuns DeterminismTool.maxRuns
               == DeterminismTool.maxRuns)

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

-- | #280: when the running binary's real path differs from the installed
-- canonical target, the process is on a stale subprocess — flag it (returning
-- the installed path) rather than the old mtime-only false-negative.
testStalenessIdentityDiffers :: IO Bool
testStalenessIdentityDiffers =
  pure ( binaryIdentityStale "/h/.local/bin/x" "/h/.cabal/store/NEW/x"
           == Just "/h/.cabal/store/NEW/x" )

-- | #280: when the running binary IS the installed target, it's fresh.
testStalenessIdentityMatches :: IO Bool
testStalenessIdentityMatches =
  pure (isNothing (binaryIdentityStale "/h/.cabal/store/A/x" "/h/.cabal/store/A/x"))
