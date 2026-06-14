-- | Unit tests for 'Tool.Workflow' (#90 Phase B): status/help envelope
-- shape and unknown-action rejection.
--
-- Extracted from the Spec.hs monolith (#271) via the function-export shape.
module Spec.WorkflowTool
  ( testWorkflowStatusEnvelope
  , testWorkflowStatusHasScratchpad
  , testWorkflowHelpEnvelope
  , testWorkflowRejectsUnknownAction
  ) where

import qualified Data.Aeson as A
import qualified Data.Aeson.Key as AKey
import qualified Data.Aeson.KeyMap as AKM
import qualified Data.Text as T
import qualified Data.Text.Lazy as TL
import qualified Data.Text.Lazy.Encoding as TLE
import Data.IORef (newIORef)
import Control.Concurrent.MVar (newMVar)
import System.Directory (createDirectoryIfMissing, getTemporaryDirectory, removePathForcibly)
import System.FilePath ((</>))

import qualified HaskellFlows.Mcp.Envelope as Env
import HaskellFlows.Mcp.Protocol (ToolContent (..), ToolResult (..))
import HaskellFlows.Mcp.Staleness (StalenessReport (..))
import HaskellFlows.Types (mkProjectDir)
import HaskellFlows.Ghc.ApiSession (startGhcSession, killGhcSession)
import qualified HaskellFlows.Mcp.WorkflowState as WS
import qualified HaskellFlows.Data.Scratchpad as SP
import qualified HaskellFlows.Tool.Workflow as WorkflowTool

import Spec.Helpers (decodeToolResult)

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
  result <- WorkflowTool.runHandle pdRef sessRef toolNames ws staleness False scratchRef args
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
  decoded <- runWorkflow (A.object [ "action" A..= ("status" :: T.Text) ])
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
  decoded <- runWorkflow (A.object [ "action" A..= ("status" :: T.Text) ])
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
  decoded <- runWorkflow (A.object [ "action" A..= ("help" :: T.Text) ])
  pure $ case decoded of
    Right env
      | Env.reStatus env == Env.StatusOk
      , Just (A.Object payload) <- Env.reResult env ->
          AKM.lookup (AKey.fromText "view") payload == Just (A.String "help")
            && AKM.member (AKey.fromText "phaseHint") payload
            && AKM.member (AKey.fromText "steps") payload
    _ -> False

-- | An unknown action lands as status='failed' with
-- error.kind='validation' (the value was structurally a valid
-- string but outside the action enum).
testWorkflowRejectsUnknownAction :: IO Bool
testWorkflowRejectsUnknownAction = do
  decoded <- runWorkflow (A.object [ "action" A..= ("teleport" :: T.Text) ])
  pure $ case decoded of
    Right env
      | Env.reStatus env == Env.StatusFailed
      , Just err <- Env.reError env ->
          Env.eeKind err == Env.Validation
    _ -> False
