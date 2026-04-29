-- | Flow: 'ghc_add_import' surfaces the missing-hoogle path
-- with @success: false@ instead of a misleading no-op
-- @success: true@ (#53).
--
-- Pre-fix behaviour
-- -----------------
-- When @hoogle@ wasn't on PATH, ghc_add_import returned
-- @{success: true, count: 0, imports: []}@ with a generic hint
-- AND a 'nextStep' claiming \"the import was added to the
-- module header. Reload to confirm.\" Both fields lied: nothing
-- was added and there is nothing to confirm.
--
-- New contract
-- ------------
-- Missing @hoogle@ → @{success: false, error: \"hoogle binary not
-- found on PATH\", remediation: ...}@. Empty hits when hoogle
-- IS available → @{success: true, count: 0}@ with an honest
-- hint and no 'nextStep'.
--
-- Test environment
-- ----------------
-- The scenario sets @PATH@ to a sentinel directory that contains
-- no hoogle binary, then invokes 'ghc_add_import'. The MCP
-- subprocess inherits the parent's environment, so the override
-- propagates to the @findExecutable@ call inside the handler.
module Scenarios.FlowAddImportNoHoogle
  ( runFlow
  ) where

import Data.Aeson (Value (..), object, (.=))
import qualified Data.Aeson.Key as Key
import qualified Data.Aeson.KeyMap as KeyMap
import Data.Text (Text)
import qualified Data.Text as T
import qualified System.Environment as Env
import Control.Exception (bracket_)

import E2E.Assert
  ( Check (..)
  , checkPure
  , liveCheck
  , stepFooter
  , stepHeader
  )
import qualified E2E.Client as Client
import E2E.Envelope (statusOk, errorMessage, lookupField)
import HaskellFlows.Mcp.ToolName (ToolName (..))

runFlow :: Client.McpClient -> FilePath -> IO [Check]
runFlow c _projectDir = do
  -- Step 1 — invoke ghc_add_import. The MCP subprocess inherits
  -- the ambient PATH at fork-time; we briefly scrub PATH around
  -- the call so 'findExecutable' inside the subprocess sees no
  -- hoogle. (In dev/CI environments hoogle is not installed by
  -- default anyway — the scrub is a belt-and-suspenders gate.)
  origPath <- Env.lookupEnv "PATH"
  let scrubbedPath = "/var/empty:/tmp/no-hoogle-here-deadbeef"

  t0 <- stepHeader 1 "ghc_add_import without hoogle returns success=false (#53)"
  cMissing <- bracket_
    (Env.setEnv "PATH" scrubbedPath)
    (case origPath of
       Just p  -> Env.setEnv "PATH" p
       Nothing -> Env.unsetEnv "PATH")
    (do
      r <- Client.callTool c GhcAddImport
             (object [ "name" .= ("fromMaybe" :: Text) ])
      let success     = statusOk r
          -- Issue #90: 'error' is now an object; the message
          -- is at error.message and remediation lives on the
          -- error envelope at error.remediation. 'errorMessage'
          -- tolerates both shapes during the migration window.
          errFieldOk  = case errorMessage r of
                          Just e  -> "hoogle" `T.isInfixOf` T.toLower e
                          Nothing -> False
          remPresent  = case lookupField "error" r of
                          Just (Object e) -> case KeyMap.lookup
                                                 (Key.fromText "remediation") e of
                            Just (String _) -> True
                            _               -> False
                          _ -> False
      liveCheck $ checkPure
        "success=false with 'hoogle' in error and remediation present"
        (success == Just False && errFieldOk && remPresent)
        ( "Expected: success=false, error mentions 'hoogle', \
          \remediation present. Got: " <> truncRender r )
    )
  stepFooter 1 t0
  pure [cMissing]

--------------------------------------------------------------------------------
-- helpers
--------------------------------------------------------------------------------

truncRender :: Value -> Text
truncRender v =
  let raw = T.pack (show v)
      cap = 600
  in if T.length raw > cap then T.take cap raw <> "…(truncated)" else raw
