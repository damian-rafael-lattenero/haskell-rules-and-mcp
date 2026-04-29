-- | Envelope-aware oracle helpers (issue #90 Phase D step 2).
--
-- The pre-#90 wire format put @success :: Bool@ and
-- @error_kind :: Text@ at the top level. Every scenario therefore
-- carried a local copy of @fieldBool@ / @fieldText@ that read
-- those fields directly. After the #90 envelope migration the
-- canonical shape is:
--
-- @
--   { "status"   : "ok"|"partial"|"no_match"|"refused"|"failed"|
--                  "timeout"|"unavailable"
--   , "result"?  : <tool-specific payload>
--   , "error"?   : { "kind", "message", "field"?, "cause"? }
--   , "warnings"?: [...]
--   , "nextStep"?: { "tool", "why", ... }
--   , "meta"?    : { "took_ms"?, ... }
--   }
-- @
--
-- Phase D step 2 closes out #90 by:
--
--   1. Promoting these helpers to a single shared module
--      (this file), so each scenario stops re-defining its own
--      copy of @fieldBool@ / @fieldText@ / @lookupField@.
--   2. Reading the canonical envelope discriminators
--      ('statusOk' on @status@, 'errorKind' on @error.kind@)
--      instead of the legacy duplicates ('success', 'error_kind').
--   3. Letting 'HaskellFlows.Mcp.Envelope' drop the legacy
--      top-level fields once every consumer is on the new
--      helpers.
--
-- Generic 'fieldBool' / 'fieldText' / 'fieldInt' accessors stay
-- here too — many scenarios read tool-specific payload fields
-- ('applied', 'dry_run', 'module', etc.) that aren't envelope
-- discriminators and don't change shape across the migration.
module E2E.Envelope
  ( -- * Envelope discriminators
    statusOk
  , statusIs
  , errorKind
    -- * Generic field accessors
  , fieldBool
  , fieldText
  , fieldInt
  , lookupField
    -- * Result drilling
  , resultPayload
  ) where

import Data.Aeson (Value (..))
import qualified Data.Aeson.Key as Key
import qualified Data.Aeson.KeyMap as KeyMap
import Data.Maybe (fromMaybe)
import Data.Text (Text)

-- | @True@ iff the envelope's @status@ field is @"ok"@.
--
-- This is the post-#90 replacement for the pre-envelope
-- @fieldBool "success" v == Just True@ check. Every other
-- terminal status — 'partial', 'no_match', 'refused', 'failed',
-- 'timeout', 'unavailable' — returns @Just False@ (so the
-- caller's @== Just True@ check stays correct).
--
-- Returns 'Nothing' iff there is no @status@ field at all (the
-- response is malformed or hasn't been migrated to the envelope
-- shape yet — should never happen post-#90 Phase C).
statusOk :: Value -> Maybe Bool
statusOk = fmap (== "ok") . statusField

-- | Specific-status check. Useful when an oracle wants to assert
-- @status == "no_match"@ or @status == "refused"@ rather than
-- just \"not ok\".
statusIs :: Text -> Value -> Bool
statusIs s v = statusField v == Just s

-- | The closed @ErrorKind@ string from the envelope's @error.kind@
-- nested field — the post-#90 replacement for the pre-envelope
-- @fieldText "error_kind" v@ read.
--
-- Returns 'Nothing' on responses that have no @error@ object
-- (i.e. successful responses) or whose @error@ object lacks a
-- @kind@ field.
errorKind :: Value -> Maybe Text
errorKind v = case lookupField "error" v of
  Just (Object o) -> case KeyMap.lookup (Key.fromText "kind") o of
    Just (String k) -> Just k
    _               -> Nothing
  _ -> Nothing

-- | Internal: pull the @status@ field as a plain 'Text'. Every
-- post-#90 envelope must carry one of the seven canonical status
-- strings; this returns 'Nothing' iff the field is missing or
-- non-string.
statusField :: Value -> Maybe Text
statusField v = case lookupField "status" v of
  Just (String s) -> Just s
  _               -> Nothing

--------------------------------------------------------------------------------
-- generic field accessors
--------------------------------------------------------------------------------

-- | Read a top-level boolean field. Used for tool-specific flags
-- like @applied@, @dry_run@, @no_change@ — these aren't envelope
-- discriminators and don't change shape across #90.
fieldBool :: Text -> Value -> Maybe Bool
fieldBool k v = case lookupField k v of
  Just (Bool b) -> Just b
  _             -> Nothing

-- | Read a top-level string field. Used for fields like
-- @module@, @path@, @symbol@ — non-discriminator payload.
fieldText :: Text -> Value -> Maybe Text
fieldText k v = case lookupField k v of
  Just (String s) -> Just s
  _               -> Nothing

-- | Read a top-level integer field. Used for counts like
-- @passed@, @failed@, @count@. Truncates a non-integer
-- 'Scientific' to its floor — fine for the integer fields the
-- envelope and tool payloads emit (counts, durations in ms,
-- exit codes).
fieldInt :: Text -> Value -> Maybe Int
fieldInt k v = case lookupField k v of
  Just (Number n) -> Just (fromInteger (truncate (toRational n :: Rational)))
  _               -> Nothing

-- | Look up an arbitrary field. Returns the raw 'Value' so the
-- caller can branch on type or recurse into nested objects.
lookupField :: Text -> Value -> Maybe Value
lookupField k (Object o) = KeyMap.lookup (Key.fromText k) o
lookupField _ _          = Nothing

--------------------------------------------------------------------------------
-- envelope drilling
--------------------------------------------------------------------------------

-- | Drill through the envelope to its inner @result@ payload.
-- Most tool-specific fields ('module', 'count', 'applied', etc.)
-- live there post-#90; the envelope wraps them.
--
-- Falls back to the input 'Value' itself when there is no
-- @result@ field, so this is safe to call on either an envelope
-- or a bare payload — useful for incremental migration.
resultPayload :: Value -> Value
resultPayload v = fromMaybe v (lookupField "result" v)
