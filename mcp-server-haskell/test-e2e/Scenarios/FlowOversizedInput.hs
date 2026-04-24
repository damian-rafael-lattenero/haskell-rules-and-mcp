-- | Flow: an oversized expression must be rejected at the MCP boundary,
-- not forwarded to the in-process GHC API where it consumes parser memory.
--
-- Motivation
-- ----------
-- 'HaskellFlows.Ghc.Sanitize.sanitizeExpression' enforces three
-- invariants today:
--
--   * non-empty after 'T.strip'         (avoids prompt-loop)
--   * no newlines or \\r                 (avoids framing split)
--   * no literal sentinel in the input  (avoids premature framing)
--
-- What it does NOT enforce is a /size/ cap. 'maxEvalBytes = 64 KiB'
-- bounds the OUTPUT that the MCP hands back to its client — but a
-- 1 MB input expression flies straight through sanitizeExpression,
-- lands in 'compileExpr' which must parse + type-check + link it in
-- the server's own HscEnv, and consumes memory roughly linear in the
-- input size for every tool that routes through sanitizeExpression
-- (ghci_eval, ghci_type, ghci_info, ghci_complete, ghci_doc,
-- ghci_goto, ghci_arbitrary, ghci_quickcheck, ghci_suggest).
--
-- Threat model
-- ------------
-- This is a CWE-400 (Uncontrolled Resource Consumption / DoS) vector.
-- An LLM host with a buggy prompt can accidentally send a huge
-- expression (imagine a REPL that forgot to truncate a table dump
-- before passing it as an argument). The MCP server is a shared
-- process; if its heap is blown by one pathological client, every
-- other client's session dies with it.
--
-- Contract asserted here
-- ----------------------
--   * ghci_eval with a 256 KiB expression returns structurally
--     (success=false, error mentions size / length / oversize).
--   * The call returns promptly (under 2 s) — because the MCP
--     rejects at the boundary BEFORE writing to the child pipe.
--   * The session remains alive: the very next ghci_eval with a
--     small expression succeeds. The oversized input must not
--     have poisoned the framing state.
--
-- Failure modes the oracle catches
-- --------------------------------
--   (a) No size cap in 'sanitizeExpression' → the 256 KiB string
--       reaches the child, the parse takes multiple seconds, and
--       eventually GHCi returns a parse error. Elapsed would be
--       long and the error shape would NOT mention size — both
--       signals.
--   (b) A cap is present but 'truncates instead of refusing' →
--       returning success=true with a truncated expression would
--       silently change user intent and fail this oracle because
--       we assert success=false.
--   (c) The size check fires after the input has already been
--       written to the child's stdin (wrong order of checks) →
--       the framing state may be left in a bad shape; the
--       follow-up ghci_eval would fail.
module Scenarios.FlowOversizedInput
  ( runFlow
  ) where

import Data.Aeson (Value (..), object, (.=))
import qualified Data.Aeson.Key as Key
import qualified Data.Aeson.KeyMap as KeyMap
import Data.Char (toLower)
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

-- | 256 KiB of 'A' wrapped in a Haskell String literal. 4× the
-- proposed 64 KiB cap so the check is unambiguous on both sides.
oversizedExpression :: Text
oversizedExpression =
  "length \"" <> T.replicate (256 * 1024) "A" <> "\""

runFlow :: Client.McpClient -> FilePath -> IO [Check]
runFlow c _pd = do
  _ <- Client.callTool c "ghci_create_project"
         (object [ "name" .= ("oversize-demo" :: Text) ])

  -- 1. Pre-flight so an unrelated session-up error doesn't look
  -- like an oversize-reject failure.
  t0 <- stepHeader 1 "pre-flight · ghci_eval(1+1) on a fresh session"
  pre <- Client.callTool c "ghci_eval"
           (object [ "expression" .= ("1 + 1" :: Text) ])
  cPre <- liveCheck $ checkPure
    "pre-flight · session responds to 1+1"
    (fieldBool "success" pre == Just True)
    ("pre-flight failed. Raw: " <> truncRender pre)
  stepFooter 1 t0

  -- 2. The oversized eval. If 'sanitizeExpression' has a size cap,
  -- the server answers in well under a second (no write to child).
  -- If not, GHCi gets the 256 KiB string, spends seconds parsing it,
  -- and eventually returns success=true with length=256 * 1024 —
  -- every check in this step fails, pointing at the missing cap.
  t1 <- stepHeader 2
          "oversize · 256 KiB expression must be refused at the boundary"
  bigStart <- getPOSIXTime
  big <- Client.callTool c "ghci_eval"
           (object [ "expression" .= oversizedExpression ])
  bigEnd <- getPOSIXTime
  let bigMs       = round ((realToFrac (bigEnd - bigStart) :: Double)
                           * 1000) :: Int
      wasRejected = fieldBool "success" big == Just False
      returnedFast = bigMs < 2_000
      errText      = case fieldText "error" big of
                        Just t  -> T.map toLower t
                        Nothing -> T.empty
      mentionsSize =
        any (`T.isInfixOf` errText)
          [ "size", "length", "too large", "oversize", "bytes", "cap" ]
  cReject <- liveCheck $ checkPure
    ("rejected at boundary · elapsed=" <> T.pack (show bigMs) <> " ms")
    (wasRejected && returnedFast)
    ("Expected: success=false in < 2 s. Got: elapsed="
      <> T.pack (show bigMs) <> " ms, success="
      <> T.pack (show (fieldBool "success" big))
      <> ". A slow return with success=true means the 256 KiB string \
         \reached GHCi — 'sanitizeExpression' is missing its size cap. \
         \Raw: " <> truncRender big)
  cMsg <- liveCheck $ checkPure
    "error message mentions size / length / oversize"
    mentionsSize
    ("The rejection message should name the failure mode so callers \
     \can act on it. Got error=" <> T.pack (show (fieldText "error" big))
     <> ". If this fails but cReject passes, the refusal is happening \
        \for some OTHER reason — the cap is still missing. Raw: "
      <> truncRender big)
  stepFooter 2 t1

  -- 3. Session must still be alive. A boundary-rejected input should
  -- not have touched the child's stdin — the framing state must be
  -- pristine.
  t2 <- stepHeader 3 "alive · session still responds after the reject"
  post <- Client.callTool c "ghci_eval"
            (object [ "expression" .= ("2 + 3" :: Text) ])
  let aliveOk = fieldBool "success" post == Just True
             && case lookupField "output" post of
                  Just (String s) -> "5" `T.isInfixOf` s
                  _               -> False
  cAlive <- liveCheck $ checkPure
    "session alive · next ghci_eval(2+3) returns 5"
    aliveOk
    ("If the oversized input was partially written to the child, the \
     \next call's sentinel framing could desync. Raw: "
      <> truncRender post)
  stepFooter 3 t2

  pure [cPre, cReject, cMsg, cAlive]

--------------------------------------------------------------------------------
-- helpers
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
