-- | Flow: 'ghc_browse' returns an actionable nextStep when the
-- requested module isn't in the project's compile graph (#72).
--
-- 'ghc_imports' lists 'Prelude' as an active import of any
-- session, but 'ghc_browse(module="Prelude")' fails because the
-- browse path only enumerates modules from the project's own
-- module graph. Pre-#72 the agent saw a dead-end string error.
-- Post-#72 the response carries:
--
--   * error_kind = "module_not_in_graph"
--   * remediation = how to fall back
--   * nextStep    = pointer at 'ghc_info' for per-name lookup
--
-- We assert the structured shape on a known-base module
-- ('Prelude') and check that 'ghc_imports' indeed lists the
-- same module — the discrepancy that triggered the issue.
module Scenarios.FlowBrowseInteractive
  ( runFlow
  ) where

import Data.Aeson (Value (..), object, (.=))
import qualified Data.Aeson.Key as Key
import qualified Data.Aeson.KeyMap as KeyMap
import Data.Text (Text)
import qualified Data.Text as T
import qualified Data.Vector as V

import E2E.Assert
  ( Check (..)
  , checkPure
  , liveCheck
  , stepFooter
  , stepHeader
  )
import qualified E2E.Client as Client
import E2E.Envelope (statusOk, fieldText, lookupField)
import HaskellFlows.Mcp.ToolName (ToolName (..))

runFlow :: Client.McpClient -> FilePath -> IO [Check]
runFlow c _projectDir = do
  -- Step 1 — bootstrap a project so the GhcSession is alive.
  _ <- Client.callTool c GhcProject
         (object [ "action" .= ("create" :: Text), "name" .= ("browse-interactive-demo" :: Text) ])
  _ <- Client.callTool c GhcLoad
         (object [ "module_path" .= ("src/BrowseInteractiveDemo.hs" :: Text) ])

  -- Step 2 — browse Prelude. Pre-#72 returns a dead-end error;
  -- post-#72 returns a structured failure with nextStep.
  t0 <- stepHeader 1 "ghc_browse(Prelude) returns structured nextStep (#72)"
  rBr <- Client.callTool c GhcBrowse
           (object [ "module" .= ("Prelude" :: Text) ])
  -- Issue #90: Browse no-match emits status='no_match' (not
-- failed/refused) without an error envelope — the diagnostic
-- context lives in 'result' and the agent steers via
-- 'nextStep'. Check status + the remediation/nextStep payload.
  let okShape =
           statusOk rBr == Just False
        -- 'success=false' covers both 'no_match' and 'failed'
        -- via the synthesised projection in lookupField.
        && (maybe False (T.isInfixOf "ghc_info") (fieldText "remediation" rBr)
              || maybe False (T.isInfixOf "hoogle_search") (fieldText "remediation" rBr))
        -- nextStep must point at ghc_info specifically.
        && nextStepTool rBr == Just "ghc_info"
  cShape <- liveCheck $ checkPure
    "browse Prelude → status=no_match with remediation + nextStep=ghc_info"
    okShape
    ("Got: " <> truncRender rBr)
  stepFooter 1 t0

  -- Step 3 — sanity: 'ghc_imports' DOES list Prelude. The point
  -- of the issue was that the two tools disagree silently.
  t1 <- stepHeader 2 "ghc_imports lists Prelude (the inconsistency surface) (#72)"
  rImp <- Client.callTool c GhcImports (object [])
  let imports = case lookupField "imports" rImp of
        Just (Array a) ->
          [ s | String s <- V.toList a ]
        _ -> []
      hasPrelude = any (T.isInfixOf "Prelude") imports
  cImp <- liveCheck $ checkPure
    "ghc_imports advertises Prelude — discrepancy surface preserved"
    hasPrelude
    ("Got imports: " <> T.intercalate ", " imports)
  stepFooter 2 t1

  pure [cShape, cImp]

--------------------------------------------------------------------------------
-- helpers
--------------------------------------------------------------------------------

nextStepTool :: Value -> Maybe Text
nextStepTool v = case lookupField "nextStep" v of
  Just (Object o) -> case KeyMap.lookup (Key.fromText "tool") o of
    Just (String s) -> Just s
    _               -> Nothing
  _ -> Nothing

truncRender :: Value -> Text
truncRender v =
  let raw = T.pack (show v)
      cap = 600
  in if T.length raw > cap then T.take cap raw <> "…(truncated)" else raw
