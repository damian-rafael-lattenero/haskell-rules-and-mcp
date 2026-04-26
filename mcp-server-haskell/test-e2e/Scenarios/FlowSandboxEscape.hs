-- | Flow: /documents/ the arbitrary-code-execution contract of
-- 'ghc_eval' — this scenario does not find a product bug, it
-- codifies the threat model so every future reader understands
-- the trust boundary.
--
-- Contract
-- --------
-- 'ghc_eval' evaluates an arbitrary Haskell expression in the
-- in-process GHC API session. That session is a full Haskell runtime
-- — it can:
--
--   * read and write any file the MCP process has permission for
--   * open network sockets
--   * exec subprocesses
--   * allocate unbounded memory (subject to the RTS heap limit)
--
-- In other words, any client that calls 'ghc_eval' inherits the
-- MCP process's ambient authority. There is no sandbox, no
-- whitelist of allowed modules, no seccomp layer. The only input
-- guards are:
--
--   * 'sanitizeExpression' (newlines, sentinel, size cap — see
--     'FlowInjectionGuard' and 'FlowOversizedInput')
--   * the JSON-RPC transport's own size bounds
--
-- This is an intentional design choice: the MCP's job is to give
-- an LLM a honest interpreter, and interpreters evaluate code.
-- Clients untrusted enough to need a sandbox should be running
-- the MCP itself in a container, VM, or firecracker — not
-- asking the tool layer to enforce it.
--
-- What this scenario asserts
-- --------------------------
-- We PROVE the contract by observation: fire a 'ghc_eval' that
-- writes a file inside the temp project dir, then one that reads
-- it back. Both succeed. A reviewer reading this test learns the
-- threat model without having to dig through source comments.
--
-- We deliberately do NOT try to escape to "/etc/passwd" or any
-- path outside the temp dir — that would be a real filesystem
-- access from the test process, and e2e tests are not the place
-- to touch the host's config files. The tempdir write + read is
-- sufficient to prove the capability.
module Scenarios.FlowSandboxEscape
  ( runFlow
  ) where

import Data.Aeson (Value (..), object, (.=))
import qualified Data.Aeson.Key as Key
import qualified Data.Aeson.KeyMap as KeyMap
import Data.Text (Text)
import qualified Data.Text as T
import Control.Monad (when)
import System.Directory (doesFileExist, removeFile)
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
  _ <- Client.callTool c GhcCreateProject
         (object [ "name" .= ("sandbox-demo" :: Text) ])

  let canary = projectDir </> "sandbox-canary.txt"
      -- Put the value literally in the expression; ghc_eval is
      -- a Haskell evaluator, so writeFile from IO works directly.
      writeExpr = "System.IO.writeFile \"" <> T.pack canary
                  <> "\" \"sandbox-escape-evidence\""
      readExpr  = "System.IO.readFile \""  <> T.pack canary <> "\""

  -- 1. Write. This is the canonical "I can touch the filesystem"
  -- capability.
  t0 <- stepHeader 1 "write · ghc_eval can writeFile inside projectDir"
  w <- Client.callTool c GhcEval
         (object [ "expression" .= writeExpr ])
  onDisk <- doesFileExist canary
  cWrite <- liveCheck $ checkPure
    "ghc_eval wrote a file · arbitrary IO is allowed by design"
    (fieldBool "success" w == Just True && onDisk)
    ("ghc_eval should execute arbitrary IO. If this fails, either \
     \writeFile threw (success=false) or the file wasn't on disk \
     \afterwards. Raw: " <> truncRender w)
  stepFooter 1 t0

  -- 2. Read. Closes the loop: what we wrote is what we read. If
  -- these differ the session is doing something weird (caching,
  -- chroot, whatever); not expected but worth catching.
  t1 <- stepHeader 2 "read · ghc_eval readFile returns what we wrote"
  r <- Client.callTool c GhcEval
         (object [ "expression" .= readExpr ])
  let roundtripped =
        fieldBool "success" r == Just True
        && case lookupField "output" r of
             Just (String s) -> "sandbox-escape-evidence" `T.isInfixOf` s
             _               -> False
  cRead <- liveCheck $ checkPure
    "readFile recovers what writeFile wrote"
    roundtripped
    ("If this fails, ghc_eval is either silently sandboxing or \
     \the file was truncated between write and read. Raw: "
      <> truncRender r)
  stepFooter 2 t1

  -- 3. Contract assertion. The point of this scenario is that the
  -- contract IS 'arbitrary code execution'. An agent that needs
  -- sandboxing must layer it below the MCP (container, VM, jail).
  -- We pin the contract in the test message so a reader knows
  -- this is intentional.
  let cContract = checkPure
        "contract · ghc_eval IS RCE-by-design; no MCP-level sandbox"
        True  -- always passes; the message is the assertion
        ""

  -- Cleanup (best-effort — the tempdir would be scrubbed anyway).
  _ <- tryRemove canary

  pure [cWrite, cRead, cContract]

  where
    tryRemove p = do
      ex <- doesFileExist p
      when ex (removeFile p)

--------------------------------------------------------------------------------
-- helpers
--------------------------------------------------------------------------------

fieldBool :: Text -> Value -> Maybe Bool
fieldBool k v = case lookupField k v of
  Just (Bool b) -> Just b
  _             -> Nothing

lookupField :: Text -> Value -> Maybe Value
lookupField k (Object o) = KeyMap.lookup (Key.fromText k) o
lookupField _ _          = Nothing

truncRender :: Value -> Text
truncRender v =
  let raw = T.pack (show v)
      cap = 400
  in if T.length raw > cap then T.take cap raw <> "…(truncated)" else raw
