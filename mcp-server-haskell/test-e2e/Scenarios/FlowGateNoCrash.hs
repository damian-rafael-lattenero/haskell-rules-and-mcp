-- | Flow: 'ghc_gate' returns a structured failure when cabal
-- output is noisy (#75). Pre-#75, a project whose test/Spec.hs
-- failed to compile produced a tall stderr stream from cabal —
-- enough to fill the OS pipe buffer — and the gate's pre-#75
-- pipe drainer deadlocked. The MCP transport interpreted the
-- ensuing silence as a dead subprocess and emitted
-- 'MCP error -32000: Connection closed'.
--
-- Post-#75, 'cabalStep' delegates to
-- 'readCreateProcessWithExitCode' which strict-drains both
-- streams. The gate returns a normal report with
-- @cabal_test.status = "fail"@ and the tool response reaches
-- the agent intact.
--
-- This scenario is the canonical regression: green for the
-- post-fix shape, red for any future regression that
-- reintroduces the lazy-pipe deadlock.
module Scenarios.FlowGateNoCrash
  ( runFlow
  ) where

import Data.Aeson (Value (..), object, (.=))
import qualified Data.Aeson.Key as Key
import qualified Data.Aeson.KeyMap as KeyMap
import Data.Text (Text)
import qualified Data.Text as T
import qualified Data.Text.IO as TIO
import System.FilePath ((</>))

import E2E.Assert
  ( Check (..)
  , checkPure
  , liveCheck
  , stepFooter
  , stepHeader
  )
import qualified E2E.Client as Client
import HaskellFlows.Mcp.ToolName (ToolName (..))

runFlow :: Client.McpClient -> FilePath -> IO [Check]
runFlow c projectDir = do
  -- Step 1 — scaffold a fresh project. The default test/Spec.hs
  -- compiles cleanly; we'll deliberately break it next.
  _ <- Client.callTool c GhcCreateProject
         (object [ "name" .= ("gate-crash-demo" :: Text) ])

  -- Step 2 — overwrite test/Spec.hs with a body that fails to
  -- compile. cabal will emit a tall error stream which is
  -- exactly the deadlock-trigger the pre-#75 pipe drainer
  -- couldn't survive.
  TIO.writeFile (projectDir </> "test" </> "Spec.hs")
    "module Main where\nmain :: IO ()\nmain = totallyUndefinedSymbol"

  t0 <- stepHeader 1 "ghc_gate on broken cabal_test → structured failure (#75)"
  rGate <- Client.callTool c GhcGate
             (object [ "skip_cabal_build" .= True ])
  -- The gate must return success: false (cabal_test failed).
  -- Crucially it must NOT have crashed the connection — the
  -- response body is what we're inspecting.
  let okShape =
           fieldBool "success" rGate == Just False
        && hasField "steps"        rGate
        && hasField "totalDurationSec" rGate
  cShape <- liveCheck $ checkPure
    "gate returns structured failure (no transport crash)"
    okShape
    ("Got: " <> truncRender rGate)
  stepFooter 1 t0

  -- Step 3 — every subsequent tool call must succeed. If the
  -- gate had crashed the subprocess, this 'workflow status'
  -- would either fail outright or come back with a reset
  -- projectDir. Both are reported in the e2e log.
  t1 <- stepHeader 2 "MCP transport survives cabal_test failure (#75)"
  rWf <- Client.callTool c GhcWorkflow
           (object [ "action" .= ("status" :: Text) ])
  let okSurvive =
           hasField "phase" rWf
        && hasField "toolsActive" rWf
  cSurvive <- liveCheck $ checkPure
    "subsequent ghc_workflow(status) succeeds"
    okSurvive
    ("Got: " <> truncRender rWf)
  stepFooter 2 t1

  pure [cShape, cSurvive]

--------------------------------------------------------------------------------
-- helpers
--------------------------------------------------------------------------------

fieldBool :: Text -> Value -> Maybe Bool
fieldBool k v = case lookupField k v of
  Just (Bool b) -> Just b
  _             -> Nothing

hasField :: Text -> Value -> Bool
hasField k v = case lookupField k v of
  Just _  -> True
  Nothing -> False

lookupField :: Text -> Value -> Maybe Value
lookupField k (Object o) = KeyMap.lookup (Key.fromText k) o
lookupField _ _          = Nothing

truncRender :: Value -> Text
truncRender v =
  let raw = T.pack (show v)
      cap = 600
  in if T.length raw > cap then T.take cap raw <> "…(truncated)" else raw
