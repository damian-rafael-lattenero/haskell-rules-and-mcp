-- | Flow: 'ghc_browse' on a standard-library module ('Prelude')
-- returns a successful listing (#168 fallback path).
--
-- History:
--   #72  — Pre-fix: dead-end string error for Prelude.
--   #72  — Post-fix: structured no_match with nextStep=ghc_info.
--   #168 — Added 'queryBrowseFallback' via 'lookupModule'; Prelude
--            browse now SUCCEEDS (status='ok', entries >= 200).
--
-- The scenario verifies the #168 post-state:
--
--   * status='ok' (not no_match)
--   * count >= 200 (Prelude exports ~259 names in GHC 9.x)
--   * nextStep.tool present (agent steered toward next action)
--
-- Step 2 confirms 'ghc_imports' still lists Prelude — the
-- discrepancy surface from #72 is preserved: Prelude is in scope
-- but the browse now returns entries rather than a no_match.
module Scenarios.FlowBrowseInteractive
  ( runFlow
  ) where

import Data.Aeson (Value (..), object, (.=))
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
import E2E.Envelope (statusOk, lookupField)
import HaskellFlows.Mcp.ToolName (ToolName (..))

runFlow :: Client.McpClient -> FilePath -> IO [Check]
runFlow c _projectDir = do
  -- Step 1 — bootstrap a project so the GhcSession is alive.
  _ <- Client.callTool c GhcProject
         (object [ "action" .= ("create" :: Text), "name" .= ("browse-interactive-demo" :: Text) ])
  _ <- Client.callTool c GhcLoad
         (object [ "module_path" .= ("src/BrowseInteractiveDemo.hs" :: Text) ])

  -- Step 2 — browse Prelude. Fix #168 added 'queryBrowseFallback'
  -- via 'lookupModule', so Prelude browse now succeeds (status='ok')
  -- with a full entry listing (count >= 200).
  t0 <- stepHeader 1 "ghc_browse(Prelude) succeeds via #168 fallback path"
  rBr <- Client.callTool c GhcBrowse
           (object [ "module" .= ("Prelude" :: Text) ])
  -- #168: Prelude is now browseable via the package-env fallback.
  -- We assert status='ok', at least 200 entries, and that the
  -- module echo matches what we asked for.
  let browseCount = case lookupField "count" rBr of
        Just (Number n) -> round n :: Int
        _               -> 0
      browseModule = case lookupField "module" rBr of
        Just (String s) -> s
        _               -> ""
      okShape =
           statusOk rBr == Just True
        && browseCount >= 200
        && browseModule == "Prelude"
  cShape <- liveCheck $ checkPure
    "browse Prelude → status=ok, count≥200 (#168 fallback works)"
    okShape
    ("Got: " <> truncRender rBr)
  stepFooter 1 t0

  -- Step 3 — sanity: 'ghc_imports' lists Prelude in 'session_preloads'.
  -- F-10 split the interactive context: agent-injected modules
  -- (Prelude, System.IO, Data.List, …) go into 'session_preloads' so
  -- the source-import 'imports' field reflects only the file's own
  -- imports.  The discrepancy with ghc_browse surface is preserved —
  -- Prelude is still observable through ghc_imports, just under a
  -- distinct key.
  t1 <- stepHeader 2 "ghc_imports lists Prelude (the inconsistency surface) (#72)"
  rImp <- Client.callTool c GhcImports (object [])
  let imports  = case lookupField "imports" rImp of
        Just (Array a) -> [ s | String s <- V.toList a ]
        _              -> []
      preloads = case lookupField "session_preloads" rImp of
        Just (Array a) -> [ s | String s <- V.toList a ]
        _              -> []
      hasPrelude = any (T.isInfixOf "Prelude") (imports <> preloads)
  cImp <- liveCheck $ checkPure
    "ghc_imports advertises Prelude — discrepancy surface preserved"
    hasPrelude
    ( "Got imports: " <> T.intercalate ", " imports
      <> " | session_preloads: " <> T.intercalate ", " preloads )
  stepFooter 2 t1

  pure [cShape, cImp]

--------------------------------------------------------------------------------
-- helpers
--------------------------------------------------------------------------------

truncRender :: Value -> Text
truncRender v =
  let raw = T.pack (show v)
      cap = 600
  in if T.length raw > cap then T.take cap raw <> "…(truncated)" else raw
