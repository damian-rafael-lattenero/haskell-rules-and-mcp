-- | Flow: ghc_load on a nonexistent module_path (issue #79).
--
-- Real-user shape — an agent typos a path or asks to load a module
-- that hasn't been created yet. Pre-fix, the tool returned
-- 'success: true' with the project's library-wide warnings,
-- because 'targetForPath' silently fell back to 'TargetLibrary'
-- for any path that did not match @test/@ / @app/@ / @bench/@. The
-- caller could not distinguish "loaded what I asked for" from
-- "didn't find your file, gave you the lib".
--
-- This scenario locks in the post-fix contract: the handler must
-- short-circuit with 'success: false' and an 'error' field that
-- mentions both \"does not exist\" and the offending path, BEFORE
-- touching the GHCi session.
module Scenarios.FlowLoadNonexistent
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
import E2E.Envelope (statusOk, errorMessage)
import HaskellFlows.Mcp.ToolName (ToolName (..))

runFlow :: Client.McpClient -> FilePath -> IO [Check]
runFlow c _projectDir = do
  ----------------------------------------------------------------
  -- setup: scaffold a tiny project so GHCi has something coherent
  -- to refuse against. Without this, every error path looks the
  -- same as a session-boot failure.
  ----------------------------------------------------------------
  t0 <- stepHeader 1 "scaffold project"
  _ <- Client.callTool c GhcCreateProject
         (object [ "name" .= ("loadnonexistent-demo" :: Text) ])
  stepFooter 1 t0

  ----------------------------------------------------------------
  -- (1) ghc_load on a path that is syntactically valid + within
  -- the project tree but DOES NOT exist on disk.
  --
  -- Pre-#79: returned success=true with library-wide warnings.
  -- Post-fix: success=false, error mentions "does not exist".
  ----------------------------------------------------------------
  t1 <- stepHeader 2 "ghc_load nonexistent module_path"
  r1 <- Client.callTool c GhcLoad
          (object [ "module_path" .= ("src/DoesNotExist.hs" :: Text) ])
  -- Issue #90: post-envelope, 'error' is an object; the message
-- text is at error.message. Use 'errorMessage' from E2E.Envelope.
  let succ1   = statusOk r1
      errMsg  = errorMessage r1
      refused = succ1 == Just False
      mentions s = case errMsg of
        Just t  -> T.isInfixOf s t
        Nothing -> False
      pathInErr  = mentions (T.pack "DoesNotExist.hs")
      reasonInErr = mentions (T.pack "does not exist")
  cMiss <- liveCheck $ checkPure
    "ghc_load #79 · nonexistent path · success=false + 'does not exist' + path"
    (refused && pathInErr && reasonInErr)
    ("The handler must reject a nonexistent module_path explicitly. \
     \Pre-fix, the tool fell back to TargetLibrary and returned \
     \success=true with the library's pre-existing warnings — a \
     \silent success the caller had no way to detect. Raw: "
     <> truncRender r1)
  stepFooter 2 t1

  ----------------------------------------------------------------
  -- (2) liveness: the failed call must not have damaged the
  -- session. A follow-up no-arg ghc_load (which boots the
  -- library by default) should still work.
  ----------------------------------------------------------------
  t2 <- stepHeader 3 "session survives · ghc_load (no args) still loads"
  r2 <- Client.callTool c GhcLoad (object [])
  let alive = statusOk r2 == Just True
  cAlive <- liveCheck $ checkPure
    "ghc_load #79 · session alive after rejected nonexistent path"
    alive
    ("If this fails, the path-existence check has a side effect on \
     \the GHCi session. The fix is supposed to short-circuit BEFORE \
     \touching the session — a follow-up no-arg load must still \
     \succeed. Raw: " <> truncRender r2)
  stepFooter 3 t2

  pure [cMiss, cAlive]

--------------------------------------------------------------------------------
-- helpers (mirrored from FlowGracefulMiss to keep scenarios self-contained)
--------------------------------------------------------------------------------

truncRender :: Value -> Text
truncRender v =
  let raw = T.pack (show v)
      cap = 400
  in if T.length raw > cap then T.take cap raw <> "…(truncated)" else raw
