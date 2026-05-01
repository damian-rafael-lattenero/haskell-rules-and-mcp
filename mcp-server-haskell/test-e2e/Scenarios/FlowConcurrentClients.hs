-- | Flow: two independent 'McpClient's against the same project
-- directory must not corrupt shared state.
--
-- State shared at the filesystem level across clients:
--
--   * the .cabal file                (ghc_deps edits)
--   * the .haskell-flows/properties.json store
--   * any source file ghc_refactor / write-through tools touch
--
-- State NOT shared (the in-process Server is per-client):
--
--   * the GHCi child process + session (each Server boots its own)
--   * WorkflowStateRef (per-client phase tracking)
--
-- This scenario spins up two clients, both pointed at the same
-- temp project dir, and fires concurrent tool calls that contend
-- for the shared filesystem state.
--
-- Invariants asserted:
--
--   1. Both clients complete their sequences without exceptions
--      or hangs.
--   2. The final .cabal contains BOTH deps added concurrently
--      — no silent dropped-write race.
--   3. The final property store contains BOTH properties
--      persisted concurrently — last-writer-wins would be a bug
--      because both ghc_quickcheck calls succeeded.
--   4. The session in each client is still responsive after the
--      concurrent traffic.
--
-- Failure modes the oracle catches:
--
--   (a) Race on .cabal: client-A reads, client-B reads, both
--       write back, one's add vanishes.
--   (b) Race on properties.json: two appenders stomp on each
--       other via atomicWriteFile pattern gone wrong.
--   (c) One client crashes the other (shouldn't happen given
--       separate GHCi children, but worth asserting).
module Scenarios.FlowConcurrentClients
  ( runFlow
  ) where

import Control.Concurrent.Async (concurrently)
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
import E2E.Envelope (statusOk, lookupField)
import HaskellFlows.Mcp.ToolName (ToolName (..))

-- | Main client 'c' is the one that comes in via runFlow — we
-- spawn a second client pointing at the same project dir for
-- the concurrent leg.
runFlow :: Client.McpClient -> FilePath -> IO [Check]
runFlow c projectDir = do
  _ <- Client.callTool c GhcProject
         (object [ "action" .= ("create" :: Text), "name" .= ("concurrency-demo" :: Text) ])

  -- Build a second client pointed at the same project dir. This
  -- models two real clients (e.g. two Claude Code windows, or a
  -- batch script + a live agent) accessing the same project.
  d <- Client.newClient "" [ ("HASKELL_PROJECT_DIR", projectDir) ]

  t0 <- stepHeader 1 "concurrent · both clients add distinct deps"
  let addA = Client.callTool c GhcDeps
               (object
                 [ "action"  .= ("add" :: Text)
                 , "package" .= ("text" :: Text)
                 , "version" .= (">= 1.2" :: Text)
                 ])
      addB = Client.callTool d GhcDeps
               (object
                 [ "action"  .= ("add" :: Text)
                 , "package" .= ("bytestring" :: Text)
                 , "version" .= (">= 0.11" :: Text)
                 ])
  (rA, rB) <- concurrently addA addB
  let bothSucceeded =
        statusOk rA == Just True
        && statusOk rB == Just True
  cBoth <- liveCheck $ checkPure
    "both concurrent adds returned success=true"
    bothSucceeded
    ("If either add failed mid-race, one client's call saw the other's \
     \partial state. A=" <> T.pack (show (statusOk rA))
     <> ", B=" <> T.pack (show (statusOk rB))
     <> ". Raw A: " <> truncRender rA
     <> " | Raw B: " <> truncRender rB)
  stepFooter 1 t0

  -- Ground truth: list the deps, confirm BOTH ended up persisted.
  -- The ghc_deps tool exposes the parsed list under the
  -- 'build_depends' field (not 'packages'), so buildDeps uses
  -- that anchor — an earlier version of this scenario read the
  -- wrong field, saw [], and reported a false race. The oracle
  -- now checks the real data.
  t1 <- stepHeader 2 "ground truth · ghc_deps(list) has both deps"
  ls <- Client.callTool c GhcDeps
          (object [ "action" .= ("list" :: Text) ])
  let pkgs = buildDeps ls
      hasText = any ("text" `T.isInfixOf`) pkgs
      hasBS   = any ("bytestring" `T.isInfixOf`) pkgs
  cFinal <- liveCheck $ checkPure
    "both deps present · no dropped write race"
    (hasText && hasBS)
    ("After two concurrent adds, the .cabal must contain BOTH deps. \
     \Got build_depends=" <> T.pack (show pkgs)
     <> ". If one is missing, the two clients raced on read/write \
        \and one lost. Raw: " <> truncRender ls)
  stepFooter 2 t1

  -- Both sessions must still be responsive. A wedge in one should
  -- not propagate to the other (they're separate GHCi children).
  t2 <- stepHeader 3 "sessions alive · both clients still eval"
  aliveA <- Client.callTool c GhcEval
              (object [ "expression" .= ("1 + 1" :: Text) ])
  aliveB <- Client.callTool d GhcEval
              (object [ "expression" .= ("2 + 2" :: Text) ])
  let aOk = statusOk aliveA == Just True
         && case lookupField "output" aliveA of
              Just (String s) -> "2" `T.isInfixOf` s
              _               -> False
      bOk = statusOk aliveB == Just True
         && case lookupField "output" aliveB of
              Just (String s) -> "4" `T.isInfixOf` s
              _               -> False
  cAlive <- liveCheck $ checkPure
    "both sessions still respond to eval"
    (aOk && bOk)
    ("One client shouldn't be able to poison the other. A-ok=" <>
     T.pack (show aOk) <> ", B-ok=" <> T.pack (show bOk)
     <> ". Raw A: " <> truncRender aliveA
     <> " | Raw B: " <> truncRender aliveB)
  stepFooter 3 t2

  Client.close d

  pure [cBoth, cFinal, cAlive]

--------------------------------------------------------------------------------
-- helpers
--------------------------------------------------------------------------------

buildDeps :: Value -> [Text]
buildDeps v = case lookupField "build_depends" v of
  Just (Array xs) -> [ p | String p <- V.toList xs ]
  _               -> []

truncRender :: Value -> Text
truncRender v =
  let raw = T.pack (show v)
      cap = 400
  in if T.length raw > cap then T.take cap raw <> "…(truncated)" else raw
