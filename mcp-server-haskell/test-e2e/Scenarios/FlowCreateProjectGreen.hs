-- | Flow: 'ghc_create_project' produces a green-by-default
-- scaffold (#69).
--
-- Pre-#69, the very first 'ghc_validate_cabal' call after a
-- fresh scaffold returned 1 cabal-check warning aggregating 3
-- missing fields ('category', 'maintainer', 'description').
-- Every new MCP user saw a yellow signal in their first 30
-- seconds, even though they had done nothing wrong.
--
-- Post-#69 the scaffold inserts sensible stubs for all three
-- (Development / you@example.com / TODO:). 'ghc_validate_cabal'
-- now reports 0 warnings; the agent's first gate-call is green.
module Scenarios.FlowCreateProjectGreen
  ( runFlow
  ) where

import Data.Aeson (Value (..), object, (.=))
import qualified Data.Aeson.Key as Key
import qualified Data.Aeson.KeyMap as KeyMap
import Data.Text (Text)
import qualified Data.Text as T

import E2E.Assert
  ( Check (..)
  , checkPure
  , liveCheck
  , stepFooter
  , stepHeader
  )
import qualified E2E.Client as Client
import E2E.Envelope (statusOk, fieldInt)
import HaskellFlows.Mcp.ToolName (ToolName (..))

runFlow :: Client.McpClient -> FilePath -> IO [Check]
runFlow c _projectDir = do
  -- Step 1 — scaffold a fresh project. Nothing else, no edits;
  -- the gate must come back green from the very first call.
  _ <- Client.callTool c GhcCreateProject
         (object [ "name" .= ("green-scaffold-demo" :: Text) ])

  t0 <- stepHeader 1 "ghc_validate_cabal on fresh scaffold returns 0 warnings (#69)"
  rVal <- Client.callTool c GhcValidateCabal (object [])
  let warnings = fieldInt "warnings" rVal
      errors   = fieldInt "errors"   rVal
      ok =  statusOk rVal == Just True
         && warnings == Just 0
         && errors   == Just 0
  cVal <- liveCheck $ checkPure
    "post-scaffold validate_cabal: warnings=0, errors=0"
    ok
    ("Got: warnings=" <> T.pack (show warnings)
       <> " errors=" <> T.pack (show errors)
       <> " | " <> truncRender rVal)
  stepFooter 1 t0

  pure [cVal]

--------------------------------------------------------------------------------
-- helpers
--------------------------------------------------------------------------------

truncRender :: Value -> Text
truncRender v =
  let raw = T.pack (show v)
      cap = 600
  in if T.length raw > cap then T.take cap raw <> "…(truncated)" else raw
