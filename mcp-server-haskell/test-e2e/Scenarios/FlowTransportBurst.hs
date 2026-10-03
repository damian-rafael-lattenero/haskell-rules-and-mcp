-- | Flow: transport burst-drain (subprocess).
--
-- The ONLY scenario besides the smoke that talks to the real
-- binary over pipes. Regression test for the drain-on-EOF race:
-- a client writes every request and closes stdin immediately;
-- in-flight workers must still deliver every response before
-- the process exits cleanly.
--
-- The tool mix (three evals + type + check + property) forces
-- the eval-lock serialization that raced session restarts
-- during F3 (silent NO-RESP drops, rc=0 exit mid-worker).
module Scenarios.FlowTransportBurst
  ( runFlow
  ) where

import Data.Aeson (object, (.=))
import Data.Text (Text)
import qualified Data.Text as T

import qualified E2E.Assert as Assert
import qualified E2E.Client as Client
import qualified E2E.Fixture as Fixture
import qualified E2E.Smoke as Smoke
import HaskellFlows.Mcp.ToolName (ToolName (GhcCheck, GhcProperty))

runFlow :: Client.McpClient -> FilePath -> IO [Assert.Check]
runFlow c projectDir = do
  t0 <- Assert.stepHeader 1 "burst 6 tool calls + EOF (subprocess)"
  Fixture.copyBaselineInto projectDir
  -- Warm the project (solver + library build + session boot) via the
  -- in-process client first: a COLD burst makes 6 concurrent workers
  -- each pay the full cabal-resolve cost and the drain outlives any
  -- sane timeout — that measures build time, not the transport.
  _ <- Client.callTool c GhcCheck
         (object [ "action" .= ("load" :: Text)
                 , "module_path" .= ("src/Placeholder.hs" :: Text) ])
  _ <- Client.callTool c GhcProperty (object
         [ "action" .= ("check" :: Text)
         , "property" .= ("\\x -> length (show x) >= 1" :: Text)
         , "module" .= ("src/Placeholder.hs" :: Text) ])
  binary <- Client.findMcpBinaryPath
  r <- Smoke.runBurstDrain binary projectDir
  let logT = T.pack ("burst log: " <> Smoke.brLog r)
  c1 <- Assert.liveCheck (Assert.Check
          { Assert.cName   = "burst · every request id answered (drain-on-EOF)"
          , Assert.cOk     = Smoke.brAllAnswered r
          , Assert.cDetail = logT
          })
  c2 <- Assert.liveCheck (Assert.Check
          { Assert.cName   = "burst · clean exit 0 (drain, not crash)"
          , Assert.cOk     = Smoke.brCleanExit r
          , Assert.cDetail = logT
          })
  c3 <- Assert.liveCheck (Assert.Check
          { Assert.cName   = "burst · response count matches request count"
          , Assert.cOk     = Smoke.brTotalResponse r == Smoke.brExpected r
          , Assert.cDetail = logT
          })
  Assert.stepFooter 1 t0
  pure [c1, c2, c3]
