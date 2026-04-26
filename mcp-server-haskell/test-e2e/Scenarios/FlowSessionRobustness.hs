-- | Flow: the GHCi session survives user-space exceptions.
--
-- Many real expressions throw at runtime: @undefined@, @1 \`div\` 0@,
-- @head []@, @error "boom"@. A caller might evaluate any of these
-- either intentionally (debugging) or accidentally (typo in a
-- debug-print). The session MUST survive — the next call has to
-- respond promptly, not with @SessionExhausted@ or a 10 s timeout.
--
-- The invariant tested here:
--
--   /After any thrown Haskell exception in a user expression, the
--    very next tool call must succeed within the normal latency
--    envelope./
--
-- Failure modes the oracle catches:
--
--   (a) The session dies (writeTVar status Dead) on a user throw —
--       subsequent tools return SessionExhausted, and the dev has
--       to manually restart.
--   (b) The session stalls — the sentinel framing desyncs because
--       an exception trace included bytes that looked like a sentinel
--       fragment. The next call blocks.
--   (c) The session gives a partial response — the bytes from the
--       panic leak into the output field of the next unrelated call.
module Scenarios.FlowSessionRobustness
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

-- | Expressions we expect GHCi to reject at RUNTIME (not at compile
-- time). Each pairs (label, expression).
hostileEvals :: [(Text, Text)]
hostileEvals =
  [ ("undefined"            , "undefined :: Int")
  , ("divide by zero"       , "(1 :: Int) `div` 0")
  , ("head of empty list"   , "head ([] :: [Int])")
  , ("error call"           , "error \"boom\" :: Int")
  , ("partial pattern match", "let Just x = Nothing :: Maybe Int in x")
  ]

runFlow :: Client.McpClient -> FilePath -> IO [Check]
runFlow c _pd = do
  -- Minimal scaffold so the GHCi session comes up with a package.
  _ <- Client.callTool c GhcCreateProject
         (object [ "name" .= ("sessrob-demo" :: Text) ])

  -- Pre-flight: session must answer a trivial eval before we start
  -- throwing things at it. Catches unrelated setup failures.
  t0 <- stepHeader 1 "pre-flight · ghc_eval(1+1) on a fresh session"
  preflight <- Client.callTool c GhcEval
                 (object [ "expression" .= ("1 + 1" :: Text) ])
  let preOk = fieldBool "success" preflight == Just True
  cPre <- liveCheck $ checkPure
    "pre-flight · session responds to a trivial eval"
    preOk
    ("If the session cannot answer 1+1 we can't diagnose anything \
     \downstream. Raw: " <> truncRender preflight)
  stepFooter 1 t0

  -- For every hostile expression: fire it, then fire a sentinel
  -- eval. Both outcomes matter:
  --   * hostile call itself returned (no hang, no exception bubbling
  --     past the transport).
  --   * follow-up succeeded (session alive).
  checks <- mapM (oneHostileCycle c) hostileEvals

  pure (cPre : concat checks)

-- | One round trip: hostile eval, then a pilot 'ghc_eval "1+1"' to
-- prove the session survived.
oneHostileCycle
  :: Client.McpClient
  -> (Text, Text)
  -> IO [Check]
oneHostileCycle c (label, expr) = do
  t <- stepHeader 2 ("hostile cycle · " <> label)

  rHostile <- Client.callTool c GhcEval
                (object [ "expression" .= expr ])
  let hostileReturned =
        -- Either outcome is fine — we only care that the CALL
        -- came back with SOME structured response. A hanging call
        -- would have blown the outer timeout.
        case rHostile of
          Object _ -> True
          _        -> False
  cReturn <- liveCheck $ checkPure
    ("hostile · '" <> label <> "' returns a structured response (no hang)")
    hostileReturned
    ("ghc_eval on a user-throwing expression must come back with \
     \a payload. Any other shape (null, non-object) means the \
     \transport swallowed it. Raw: " <> truncRender rHostile)

  -- The critical one: prove the session is still alive.
  rPilot <- Client.callTool c GhcEval
              (object [ "expression" .= ("2 + 3" :: Text) ])
  let pilotOk = fieldBool "success" rPilot == Just True
             && case lookupField "output" rPilot of
                  Just (String s) -> "5" `T.isInfixOf` s
                  _ -> False
  cAlive <- liveCheck $ checkPure
    ("alive after '" <> label <> "' · ghc_eval(2+3) returns 5")
    pilotOk
    ("After '" <> label <> "' the session must still evaluate 2+3 to \
     \5 cleanly. If this fails, the user throw took the session with \
     \it OR bled bytes into the next output. Raw: " <> truncRender rPilot)

  stepFooter 2 t
  pure [cReturn, cAlive]

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
