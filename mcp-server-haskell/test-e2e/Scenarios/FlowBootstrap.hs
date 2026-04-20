-- | Flow: @ghci_bootstrap@ host-rules self-install.
--
-- Exercises the portability tool that BUG-10 introduced:
-- preview vs write, three host targets, path-validation via
-- @mkModulePath@.
--
-- Host enum:
--   "claude-code" → .claude/rules/haskell-flows-mcp.md
--   "cursor"      → .cursor/rules/haskell-flows-mcp.md
--   "generic"     → no write; content returned for paste
module Scenarios.FlowBootstrap
  ( runFlow
  ) where

import Data.Aeson (Value (..), object, (.=))
import Data.Text (Text)
import qualified Data.Text as T
import System.Directory (doesFileExist)
import System.FilePath ((</>))

import E2E.Assert
  ( Check (..)
  , checkJsonField
  , checkJsonFieldMatches
  , checkPure
  , liveCheck
  , stepFooter
  , stepHeader
  )
import qualified E2E.Client as Client

runFlow :: Client.McpClient -> FilePath -> IO [Check]
runFlow c projectDir = do
  ----------------------------------------------------------------
  -- (1) claude-code preview — returns markdown, no file written
  ----------------------------------------------------------------
  t0 <- stepHeader 1 "bootstrap(claude-code) preview"
  r1 <- Client.callTool c "ghci_bootstrap"
          (object [ "host" .= ("claude-code" :: Text) ])
  c1 <- liveCheck $ checkJsonField "claude-code preview success" r1 "success" (Bool True)
  c2 <- liveCheck $ checkJsonField "claude-code preview mode=preview"
                      r1 "mode" (String "preview")
  c3 <- liveCheck $ checkJsonFieldMatches
          "claude-code preview carries markdown content"
          r1 "content" (containsText "haskell-flows")
          "'content' must carry the rendered workflowRulesMarkdown"
  let cPath = projectDir </> ".claude" </> "rules" </> "haskell-flows-mcp.md"
  exists1 <- doesFileExist cPath
  c4 <- liveCheck $ checkPure
          "claude-code preview · no file written yet"
          (not exists1)
          "preview mode must NOT touch the disk"
  stepFooter 1 t0

  ----------------------------------------------------------------
  -- (2) claude-code write=true — persists the file
  ----------------------------------------------------------------
  t1 <- stepHeader 2 "bootstrap(claude-code) write=true"
  r2 <- Client.callTool c "ghci_bootstrap" (object
    [ "host"  .= ("claude-code" :: Text)
    , "write" .= True
    ])
  c5 <- liveCheck $ checkJsonField "claude-code write success" r2 "success" (Bool True)
  c6 <- liveCheck $ checkJsonField "claude-code mode=written"
                      r2 "mode" (String "written")
  exists2 <- doesFileExist cPath
  c7 <- liveCheck $ checkPure
          "claude-code write · file lands under .claude/rules/"
          exists2
          "expected the MD file at projectDir/.claude/rules/haskell-flows-mcp.md"
  stepFooter 2 t1

  ----------------------------------------------------------------
  -- (3) cursor write=true — different canonical path
  ----------------------------------------------------------------
  t2 <- stepHeader 3 "bootstrap(cursor) write=true"
  r3 <- Client.callTool c "ghci_bootstrap" (object
    [ "host"  .= ("cursor" :: Text)
    , "write" .= True
    ])
  c8 <- liveCheck $ checkJsonField "cursor write success" r3 "success" (Bool True)
  let cursorPath = projectDir </> ".cursor" </> "rules" </> "haskell-flows-mcp.md"
  cursorExists <- doesFileExist cursorPath
  c9 <- liveCheck $ checkPure
          "cursor write · file lands under .cursor/rules/"
          cursorExists
          "cursor host targets .cursor/rules/haskell-flows-mcp.md"
  stepFooter 3 t2

  ----------------------------------------------------------------
  -- (4) generic — never writes, always returns content
  ----------------------------------------------------------------
  t3 <- stepHeader 4 "bootstrap(generic) (never writes)"
  r4 <- Client.callTool c "ghci_bootstrap" (object
    [ "host"  .= ("generic" :: Text)
    , "write" .= True   -- even with write=true, generic never writes
    ])
  c10 <- liveCheck $ checkJsonField "generic success" r4 "success" (Bool True)
  c11 <- liveCheck $ checkJsonFieldMatches
          "generic · content carries the markdown"
          r4 "content" (containsText "haskell-flows")
          "generic host returns the body for the agent to paste"
  stepFooter 4 t3

  pure [c1, c2, c3, c4, c5, c6, c7, c8, c9, c10, c11]

--------------------------------------------------------------------------------
-- helpers
--------------------------------------------------------------------------------

containsText :: Text -> Value -> Bool
containsText needle (String s) = needle `T.isInfixOf` s
containsText _      _          = False
