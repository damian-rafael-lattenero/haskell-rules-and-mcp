-- | Flow: the per-command inner timeout is real, not documented fiction.
--
-- The README + CLAUDE.md claim:
--
--   "executeNoLock honours its timeoutMicros via registerDelay."
--   "Server.runTool wraps each handler in a 10-min outer timeout"
--
-- This scenario is the test that makes the FIRST claim honest. We fire
-- a deliberately slow 'ghc_eval' (a 60-second 'threadDelay') and
-- assert three things:
--
--   (a) The call RETURNED in less than 45 s — i.e. the inner 30 s
--       'execute' budget tripped, didn't wait for the user's 60 s.
--   (b) The response was structured-failed (success=false), NOT a
--       silent timeout that looks indistinguishable from success.
--   (c) The session recovered: the very next 'ghc_eval' succeeded,
--       proving 'Server.evictSession' replaces the dead session.
--
-- Failure modes the oracle catches:
--
--   * 'executeNoLock'\'s 'registerDelay' stops being wired up, or the
--     STM retry stops responding to the delay TVar — the call would
--     block the full 60 s (or much longer if the expr were unbounded).
--   * 'SessionExhausted' is swallowed in 'runTool' instead of evicting
--     the session — the recovery call would inherit a wedged session.
--   * The 10-min outer ceiling fires first because the inner budget
--     is broken — test still fails (elapsed 600 s) but the signal
--     comes from outer, not inner.
--
-- Cost: ~30 s wall (bounded by the inner timeout itself).
module Scenarios.FlowTimeoutEnforcement
  ( runFlow
  ) where

import Data.Aeson (Value (..), object, (.=))
import qualified Data.Aeson.Key as Key
import qualified Data.Aeson.KeyMap as KeyMap
import Data.Text (Text)
import qualified Data.Text as T
import Data.Time.Clock.POSIX (getPOSIXTime)

import E2E.Assert
  ( Check (..)
  , checkPure
  , liveCheck
  , stepFooter
  , stepHeader
  )
import qualified E2E.Client as Client

runFlow :: Client.McpClient -> FilePath -> IO [Check]
runFlow c _pd = do
  _ <- Client.callTool c "ghc_create_project"
         (object [ "name" .= ("timeout-demo" :: Text) ])

  -- 1. Pre-flight: the session must be responsive before we poke it.
  t0 <- stepHeader 1 "pre-flight · ghc_eval(1+1) on a fresh session"
  pre <- Client.callTool c "ghc_eval"
           (object [ "expression" .= ("1 + 1" :: Text) ])
  cPre <- liveCheck $ checkPure
    "pre-flight · session responds to 1+1"
    (fieldBool "success" pre == Just True)
    ("setup failed; no point diagnosing the timeout. Raw: "
      <> truncRender pre)
  stepFooter 1 t0

  -- 2. The slow call. threadDelay is chosen over a pure infinite loop
  -- because (a) it doesn't burn CPU (nicer on CI shared runners), and
  -- (b) GHC can't constant-fold it away (some pure bottoms get detected
  -- as <<loop>> and come back early, which would mask a broken timer).
  t1 <- stepHeader 2 "slow eval · threadDelay 60 s must abort in < 45 s"
  startedAt <- getPOSIXTime
  slow <- Client.callTool c "ghc_eval"
            (object [ "expression"
                    .= ("Control.Concurrent.threadDelay 60000000 :: IO ()"
                        :: Text) ])
  endedAt <- getPOSIXTime
  let elapsedMs = round ((realToFrac (endedAt - startedAt) :: Double)
                         * 1000) :: Int
      abortedStructurally = fieldBool "success" slow == Just False
      returnedInTime      = elapsedMs < 45_000

  cTime <- liveCheck $ checkPure
    ("aborted within inner budget · elapsed=" <> T.pack (show elapsedMs)
     <> " ms, success=false")
    (abortedStructurally && returnedInTime)
    ("Expected: ghc_eval returns in < 45 s with success=false. \
     \Got: elapsed=" <> T.pack (show elapsedMs) <> " ms, success="
     <> T.pack (show (fieldBool "success" slow))
     <> ". If elapsed ≥ 60 s, 'executeNoLock' timeoutMicros is not \
        \being honoured. If success=true, a slow call is being \
        \reported as successful. Raw: " <> truncRender slow)
  stepFooter 2 t1

  -- 2b. Structured-error-shape oracle. Pre-fix the server emitted a
  -- plain "Tool threw an exception: SessionExhausted" string in the
  -- content block (no JSON envelope, no discriminator), making it
  -- impossible for clients to tell a timeout apart from a rename
  -- failure at the schema level. Post-fix runTool emits a proper
  -- {success:false, error:…, error_kind:…} object. This check locks
  -- the contract: if someone ever refactors runTool back to plain
  -- text, this scenario fails loudly.
  t1b <- stepHeader 3 "error-shape · runTool emits structured JSON on timeout"
  let errKind  = fieldText "error_kind" slow
      shapeOk  = errKind == Just "session_exhausted"
              || errKind == Just "timeout"
  cShape <- liveCheck $ checkPure
    "error_kind tagged session_exhausted | timeout"
    shapeOk
    ("Expected error_kind ∈ {session_exhausted, timeout}. Got: "
     <> T.pack (show errKind)
     <> ". If this is Nothing, runTool regressed to emitting plain \
        \text instead of JSON for caught exceptions. Raw: "
     <> truncRender slow)
  stepFooter 3 t1b

  -- 3. Recovery. Must work, and must work promptly — the previous
  -- failure should have triggered 'evictSession' so this call boots
  -- a fresh GHCi child.
  t2 <- stepHeader 4 "recovery · next ghc_eval(2+3) must succeed"
  recStart <- getPOSIXTime
  recov <- Client.callTool c "ghc_eval"
             (object [ "expression" .= ("2 + 3" :: Text) ])
  recEnd <- getPOSIXTime
  let recMs        = round ((realToFrac (recEnd - recStart) :: Double)
                            * 1000) :: Int
      recSucceeded = fieldBool "success" recov == Just True
                  && case lookupField "output" recov of
                       Just (String s) -> "5" `T.isInfixOf` s
                       _               -> False
      recPrompt    = recMs < 30_000  -- recovery must not itself stall
  cRec <- liveCheck $ checkPure
    ("alive after timeout · elapsed=" <> T.pack (show recMs) <> " ms")
    (recSucceeded && recPrompt)
    ("After the inner timeout, the server should evict the session and \
     \respawn for the next call. Got: elapsed=" <> T.pack (show recMs)
     <> " ms, success=" <> T.pack (show (fieldBool "success" recov))
     <> ". Raw: " <> truncRender recov)
  stepFooter 4 t2

  pure [cPre, cTime, cShape, cRec]

--------------------------------------------------------------------------------
-- helpers (mirror FlowSessionRobustness.hs so the two scenarios read
-- the same way — no upstream sharing yet to avoid churning the harness)
--------------------------------------------------------------------------------

fieldBool :: Text -> Value -> Maybe Bool
fieldBool k v = case lookupField k v of
  Just (Bool b) -> Just b
  _             -> Nothing

fieldText :: Text -> Value -> Maybe Text
fieldText k v = case lookupField k v of
  Just (String s) -> Just s
  _               -> Nothing

lookupField :: Text -> Value -> Maybe Value
lookupField k (Object o) = KeyMap.lookup (Key.fromText k) o
lookupField _ _          = Nothing

truncRender :: Value -> Text
truncRender v =
  let raw = T.pack (show v)
      cap = 400
  in if T.length raw > cap then T.take cap raw <> "…(truncated)" else raw
