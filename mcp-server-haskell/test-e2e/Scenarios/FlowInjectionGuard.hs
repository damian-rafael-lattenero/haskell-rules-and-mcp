-- | Flow: hostile inputs — the bug-finding oracle for the @sanitize@
-- layer that stands between JSON-RPC arguments and the live GHCi.
--
-- The @.cabal@ file states, as a design invariant:
--
--   * Path traversal impossible by construction (ModulePath smart ctor)
--   * GHCi child process is never invoked via shell; argv-form only
--
-- Those are claims — this scenario is the test that makes them
-- honest. We fire three hostile inputs at /separate/ tools and
-- assert the MCP rejects them structurally (success=false with a
-- helpful error), instead of:
--
--   (a) passing the string straight through to GHCi, where an
--       embedded newline splits one call into two commands and
--       desyncs the sentinel framing;
--   (b) forwarding the sentinel literal to GHCi, where a matching
--       occurrence would /prematurely/ end a command;
--   (c) letting a @..@ segment escape the validated project root —
--       the ModulePath smart constructor guarantee.
--
-- Each hostile input is the minimal shape the defender must handle.
-- If any of these slip past, the corresponding layer regressed.
module Scenarios.FlowInjectionGuard
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
import HaskellFlows.Mcp.ToolName (ToolName (..))

runFlow :: Client.McpClient -> FilePath -> IO [Check]
runFlow c _projectDir = do
  _ <- Client.callTool c GhcCreateProject
         (object [ "name" .= ("injguard-demo" :: Text) ])

  ----------------------------------------------------------------
  -- (1) Newline injection via ghc_eval.
  --
  -- A newline in an eval expression would split one GHCi command
  -- into two and desync the <<<GHCi-DONE-...>>> sentinel. The
  -- sanitizer must refuse the input before it reaches the session.
  ----------------------------------------------------------------
  t0 <- stepHeader 1 "newline injection · ghc_eval must refuse a \\n-laden expr"
  r1 <- Client.callTool c GhcEval
          (object [ "expression" .= ("1 + 1\n:quit" :: Text) ])
  let r1Ok = errorShaped r1
  c1 <- liveCheck $ checkPure
    "newline injection refused · success=false + explanatory error"
    r1Ok
    ("ghc_eval MUST sanitize expressions before dispatch. If the \
     \newline reached GHCi, the ':quit' after it would kill the \
     \session and subsequent tools would fail with \
     \SessionExhausted. Raw: " <> truncRender r1)
  stepFooter 1 t0

  ----------------------------------------------------------------
  -- (2) Sentinel poisoning via ghc_eval.
  --
  -- The framing sentinel is a shared secret between the session
  -- driver and the GHCi child. If a user expression contains a
  -- copy of it, the driver must refuse — otherwise a crafted input
  -- could make GHCi emit "done" prematurely and confuse every
  -- subsequent read.
  ----------------------------------------------------------------
  t1 <- stepHeader 2 "sentinel poisoning · ghc_eval must refuse the framing string"
  r2 <- Client.callTool c GhcEval
          (object [ "expression"
                  .= ("\"<<<GHCi-DONE-7f3a2b>>>\"" :: Text) ])
  let r2Ok = errorShaped r2
  c2 <- liveCheck $ checkPure
    "sentinel poisoning refused · sanitizer rejects the framing literal"
    r2Ok
    ("An expression that contains the literal sentinel string, even \
     \inside a Haskell string, must be refused. Allowing it means a \
     \caller can desync the framing protocol. Raw: " <> truncRender r2)
  stepFooter 2 t1

  ----------------------------------------------------------------
  -- (3) Path traversal via ghc_load.
  --
  -- The ModulePath smart constructor normalises + validates so no
  -- '..' segment survives. Arrive here through the JSON-RPC
  -- transport + handleRequest + dispatchTool path to confirm the
  -- guarantee holds for EVERY caller, not just direct-constructor
  -- callers inside the Haskell code.
  ----------------------------------------------------------------
  t2 <- stepHeader 3 "path traversal · ghc_load must refuse '../..' escapes"
  r3 <- Client.callTool c GhcLoad
          (object [ "module_path" .= ("../../etc/passwd" :: Text) ])
  let r3Ok = errorShaped r3
  c3 <- liveCheck $ checkPure
    "path traversal refused · load rejects paths that escape the root"
    r3Ok
    ("The .cabal stanza documents 'Path traversal impossible by \
     \construction (ModulePath smart constructor)'. This assert \
     \keeps that claim honest at the transport boundary. Raw: "
     <> truncRender r3)
  stepFooter 3 t2

  ----------------------------------------------------------------
  -- (4) Path traversal via ghc_lint (issue #81 / CWE-22).
  --
  -- Same invariant as (3) but targeting ghc_lint, which has its
  -- own 'resolveTarget' guard. The earlier release used a literal
  -- string-prefix check that accepted '../..' (the literal
  -- "<root>/../.." starts with "<root>/") and silently launched
  -- hlint on the project's parent directory — only the eventual
  -- 60 s timeout stopped it. The fixed gate must refuse the input
  -- before any subprocess spawns. Substring 'lint_relative_traversal'
  -- in this scenario name is the e2e-only filter handle.
  ----------------------------------------------------------------
  t3 <- stepHeader 4
          "lint_relative_traversal · ghc_lint must refuse '../..'"
  r4 <- Client.callTool c GhcLint
          (object [ "path" .= ("../.." :: Text) ])
  let r4Ok = errorShaped r4
  c4 <- liveCheck $ checkPure
    "ghc_lint refused traversal · escape error before subprocess spawns"
    r4Ok
    ("ghc_lint must reject any 'path' or 'module_path' whose normalised \
     \form contains '..' segments or escapes the project root. The \
     \previous gate did a literal string-prefix check that accepted \
     \'../..' and ran hlint over the parent directory (#81, CWE-22). \
     \Raw: " <> truncRender r4)
  stepFooter 4 t3

  pure [c1, c2, c3, c4]

--------------------------------------------------------------------------------
-- helpers
--------------------------------------------------------------------------------

-- | The MCP rejects hostile input with a success=false payload and
-- some explanatory field ('error', 'errors', or a textual tail).
-- Accept any of those shapes as "structurally refused" — the oracle
-- is "no silent pass-through", not a specific error format.
errorShaped :: Value -> Bool
errorShaped v =
  let success = lookupField "success" v
      explanatory = any ($ v)
        [ hasField "error"
        , hasField "errors"
        , hasField "reason"
        , hasField "hint"
        ]
  in success == Just (Bool False) && explanatory

hasField :: Text -> Value -> Bool
hasField k (Object o) = KeyMap.member (Key.fromText k) o
hasField _ _          = False

lookupField :: Text -> Value -> Maybe Value
lookupField k (Object o) = KeyMap.lookup (Key.fromText k) o
lookupField _ _          = Nothing

truncRender :: Value -> Text
truncRender v =
  let raw = T.pack (show v)
      cap = 400
  in if T.length raw > cap then T.take cap raw <> "…(truncated)" else raw
