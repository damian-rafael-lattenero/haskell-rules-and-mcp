-- | Flow: the GHCi child loses its stdout; the MCP notices
-- (Dead status flip) and recovers (evictSession + fresh boot).
--
-- Distinction from 'FlowSessionRobustness':
--
--   FlowSessionRobustness fires *user-space* exceptions (undefined,
--   'error', 'div 0') — those are thrown inside GHCi and caught by
--   GHCi's own top-level handler. The GHCi runtime stays up; the
--   session is still alive at the OS level.
--
--   FlowGhciSigkill breaks the *framing channel itself* by closing
--   stdout inside the child. 'drainHandle' sees EOF on the merged
--   stdout reader, flips 'sStatus' to 'Dead', and every STM-retry
--   in 'executeNoLock' wakes via the status TVar and throws
--   'SessionExhausted'. This is the exact code path that fires
--   when the GHCi child dies for any real-world reason (OOM kill,
--   panic, segfault in foreign code) — just triggered surgically
--   instead of by a race.
--
-- NOTE on the kill vector — this scenario has eaten two false
-- positives already, which is why the comment is this long:
--
--   v1: used 'System.Exit.exitWith (ExitFailure 42)'. GHCi's
--       top-level catches 'ExitException' and just reprints the
--       prompt — the child kept running. The whole scenario passed
--       in 1 ms without ever triggering the Dead path. Caught
--       only because a later "respawn oracle" step (see 5 below)
--       asserted the planted sentinel was out of scope post-kill.
--
--   v2: used 'System.Posix.Process.exitImmediately (ExitFailure 1)'.
--       Single-line 'ghci_eval' cannot bring that module into
--       scope, the reference failed to resolve, GHCi reported a
--       scope error (success=false), the scenario's "killShapedAsError"
--       oracle passed for the WRONG reason (the error was a scope
--       error, not a process-death signal). The sentinel survived,
--       and the respawn oracle caught it again.
--
--   v3 (this version): closes the child's stdout handle directly.
--       'System.IO' and its members are always in scope in a fresh
--       GHCi — no import required. Closing stdout is benign to
--       GHCi's runtime (it doesn't throw), but it EOF-terminates
--       the reader thread in the MCP, which is the actual signal
--       we care about. The 'sStatus' flip to 'Dead' is now
--       guaranteed.
--
-- The invariant tested here:
--
--   /When the GHCi child terminates abnormally at the OS level, the
--    next tool call must succeed by respawning on a fresh GHCi —
--    not hang waiting for a sentinel that cannot arrive, and not
--    silently reuse the original dead session./
--
-- Failure modes the oracle catches:
--
--   (a) The 'Dead' status flip stops happening — 'drainHandle' EOF
--       handling regresses and 'executeNoLock' STM retries forever.
--   (b) 'runTool' catches 'SessionExhausted' but forgets to
--       'evictSession' — the MVar still holds the corpse; the next
--       call also fails.
--   (c) 'getOrStartSession' fails to boot a fresh child after evict.
--   (d) The 'sentinel_before_death' binding remains in scope after
--       recovery — meaning no respawn actually happened and the
--       test would pass vacuously (the bug that bit the first
--       iteration of this scenario — see module header).
module Scenarios.FlowGhciSigkill
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
  _ <- Client.callTool c "ghci_create_project"
         (object [ "name" .= ("sigkill-demo" :: Text) ])

  -- 1. Pre-flight.
  t0 <- stepHeader 1 "pre-flight · ghci_eval(1+1) on a fresh session"
  pre <- Client.callTool c "ghci_eval"
           (object [ "expression" .= ("1 + 1" :: Text) ])
  cPre <- liveCheck $ checkPure
    "pre-flight · session responds to 1+1"
    (fieldBool "success" pre == Just True)
    ("pre-flight failed before we got to the SIGKILL part. Raw: "
      <> truncRender pre)
  stepFooter 1 t0

  -- 2. Plant a sentinel binding in the LIVE session. If the session
  -- survives the "kill", this binding will still be visible in step
  -- 4 below and the oracle will rightly fail the test. If the
  -- session truly dies and is respawned, the binding vanishes.
  t1 <- stepHeader 2 "plant sentinel · let sentinel_before_death = 77"
  _ <- Client.callTool c "ghci_eval"
         (object [ "expression" .= ("let sentinel_before_death = 77 :: Int"
                                    :: Text) ])
  sanity <- Client.callTool c "ghci_eval"
              (object [ "expression" .= ("sentinel_before_death" :: Text) ])
  let plantOk = fieldBool "success" sanity == Just True
             && case lookupField "output" sanity of
                  Just (String s) -> "77" `T.isInfixOf` s
                  _               -> False
  cPlant <- liveCheck $ checkPure
    "sanity · sentinel binding is live before the kill"
    plantOk
    ("If the binding isn't visible pre-kill, the later absence check \
     \after recovery proves nothing. Raw: " <> truncRender sanity)
  stepFooter 2 t1

  -- 3. Break the framing channel: close the child's stdout handle.
  -- GHCi does not throw; the child stays alive at the OS level, but
  -- the MCP's reader thread hits EOF on stdout and flips the status
  -- to Dead. That is the production code path that fires whenever
  -- the child dies for real (OOM, segfault, signal). See the
  -- module header for the two kill vectors this replaced.
  --
  -- The MCP response shape on this call is intentionally loose:
  --   * any non-hanging return is acceptable,
  --   * structured success=false is preferred (and is what the fix
  --     for BUG-A guarantees).
  t2 <- stepHeader 3
          "kill · hClose stdout — EOFs the reader, flips sStatus to Dead"
  killStart <- getPOSIXTime
  kill <- Client.callTool c "ghci_eval"
            (object [ "expression"
                    .= ("System.IO.hClose System.IO.stdout" :: Text) ])
  killEnd <- getPOSIXTime
  let killMs = round ((realToFrac (killEnd - killStart) :: Double)
                      * 1000) :: Int
      killReturned = killMs < 45_000
      killShapedAsError =
        -- Either: structured failure (fix for BUG-A landed), OR
        -- a legacy exception shape (pre-fix). Both are "not-success".
        fieldBool "success" kill == Just False
        || case kill of
             Object o -> KeyMap.member (Key.fromText "isError") o
             _        -> False

  cKill <- liveCheck $ checkPure
    ("kill call returned non-success · elapsed=" <> T.pack (show killMs) <> " ms")
    (killReturned && killShapedAsError)
    ("Expected: exitImmediately to return within the inner budget as a \
     \structured failure. Got: elapsed=" <> T.pack (show killMs)
     <> " ms, success=" <> T.pack (show (fieldBool "success" kill))
     <> ". If elapsed ≥ 45 s, the Dead-status flip didn't happen; if \
        \success=true, the MCP mis-classified a dead-session call. \
        \Raw: " <> truncRender kill)
  stepFooter 3 t2

  -- 4. Recovery. Must succeed promptly on a FRESH GHCi.
  t3 <- stepHeader 4 "recovery · next ghci_eval(2+3) on a FRESH session"
  recStart <- getPOSIXTime
  recov <- Client.callTool c "ghci_eval"
             (object [ "expression" .= ("2 + 3" :: Text) ])
  recEnd <- getPOSIXTime
  let recMs        = round ((realToFrac (recEnd - recStart) :: Double)
                            * 1000) :: Int
      recSucceeded = fieldBool "success" recov == Just True
                  && case lookupField "output" recov of
                       Just (String s) -> "5" `T.isInfixOf` s
                       _               -> False
  cRec <- liveCheck $ checkPure
    ("alive after child death · elapsed=" <> T.pack (show recMs) <> " ms")
    (recSucceeded && recMs < 60_000)
    ("After the child's _exit, the next call must land on a fresh \
     \GHCi session. Got: elapsed=" <> T.pack (show recMs)
     <> " ms, success=" <> T.pack (show (fieldBool "success" recov))
     <> ". If this hangs, evictSession was not called on \
        \SessionExhausted. Raw: " <> truncRender recov)
  stepFooter 4 t3

  -- 5. The oracle that caught the v1 tautology: if the session was
  -- truly respawned, 'sentinel_before_death' is NOT in scope — the
  -- eval must fail with an Out-of-scope error. If it succeeds and
  -- returns 77, the "kill" didn't actually kill and this whole
  -- scenario is proving nothing.
  t4 <- stepHeader 5 "respawn oracle · sentinel_before_death MUST be out-of-scope"
  ghost <- Client.callTool c "ghci_eval"
             (object [ "expression" .= ("sentinel_before_death" :: Text) ])
  let ghostSuccess = fieldBool "success" ghost == Just True
      ghostOutput = case lookupField "output" ghost of
                      Just (String s) -> s
                      _               -> T.empty
      ghostNotInScope =
        -- Either GHCi reports "Variable not in scope", OR the tool
        -- reports success=false with a compile error. Anything with
        -- output containing the literal "77" means the session never
        -- respawned — that's the tautology we're guarding against.
        not (fieldBool "success" ghost == Just True
             && "77" `T.isInfixOf` ghostOutput)
  cGhost <- liveCheck $ checkPure
    "respawn real · sentinel_before_death no longer resolves to 77"
    ghostNotInScope
    ("The sentinel planted BEFORE the 'kill' is still live AFTER the \
     \'recovery'. This means the 'kill' did not actually kill the \
     \child — the test is asserting nothing about the respawn path. \
     \Got success=" <> T.pack (show ghostSuccess)
     <> ", output=" <> T.pack (show ghostOutput)
     <> ". Raw: " <> truncRender ghost)
  stepFooter 5 t4

  pure [cPre, cPlant, cKill, cRec, cGhost]

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
