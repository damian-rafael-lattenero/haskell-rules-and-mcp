-- | Issue #92 Phase A — JSON Schema builder helpers for action-discriminated
-- tools.
--
-- Background: 'tools/list' is the canonical truth-source for /\"what
-- arguments does this tool accept?\"/, but pre-#92 tools whose
-- per-action required fields differ (e.g. 'ghc_refactor''s
-- @rename_local@ vs @extract_binding@) hand-wrote a single flat
-- @required@ list that lied about the runtime contract. A host that
-- followed the schema sent /plausible-by-spec/ requests the runtime
-- rejected — the dogfood pass surfaced this on the very first
-- 'ghc_refactor' attempt.
--
-- This module exposes a small builder API so the schema mirrors the
-- runtime: one 'SchemaBranch' per discriminant variant, each with
-- its own self-contained @properties@ + @required@ set, all wrapped
-- in a top-level @oneOf@ that hosts can render as a tabbed UI.
--
-- Phase A scope (this file): the helper module + unit tests.
-- Phases B–E (issue #92 §5) cover migrating individual tools, the
-- CI lint gate, and the contributor docs.
module HaskellFlows.Mcp.Schema
  ( -- * Field-shape builders
    stringField
  , integerField
  , booleanField
  , arrayField
  , constString
  , typedField
    -- * Branch-discriminated schemas
  , SchemaBranch (..)
  , discriminatedSchema
  , flatObjectSchema
  ) where

import Data.Aeson (Value (..), object, (.=))
import qualified Data.Aeson.Key as Key
import Data.Text (Text)

--------------------------------------------------------------------------------
-- Field-shape builders
--
-- Each helper returns a Draft-7 JSON Schema fragment for a single
-- field type. They're not strictly needed (a caller could write the
-- 'object' literal inline) but having them centralised lets the
-- description format evolve without grepping every 'Tool/*.hs'.
--------------------------------------------------------------------------------

-- | A required-or-optional string field with a one-line description.
stringField :: Text -> Value
stringField = typedField "string"

-- | A required-or-optional integer field.
integerField :: Text -> Value
integerField = typedField "integer"

-- | A required-or-optional boolean field.
booleanField :: Text -> Value
booleanField = typedField "boolean"

-- | A required-or-optional array field. The @items@ shape is left
-- unconstrained — callers that want stricter typing should write
-- the schema fragment inline.
arrayField :: Text -> Value
arrayField = typedField "array"

-- | A field whose value MUST equal a specific string constant. Used
-- to encode the discriminant in 'oneOf' branches: the @\"action\"@
-- field of the @rename_local@ branch is @const \"rename_local\"@.
constString :: Text -> Value
constString v =
  object
    [ "type"  .= ("string" :: Text)
    , "const" .= v
    ]

-- | Generic single-type field with a description. Re-exported so
-- callers can build less common types (\"number\", \"object\", ...)
-- without falling back to inline objects.
typedField :: Text -> Text -> Value
typedField ty desc =
  object
    [ "type"        .= ty
    , "description" .= desc
    ]

--------------------------------------------------------------------------------
-- Branch-discriminated schemas
--------------------------------------------------------------------------------

-- | One arm of a discriminated tool's input schema. Each arm
-- corresponds to a single value of the discriminant field
-- (e.g. @\"rename_local\"@, @\"extract_binding\"@) and carries the
-- properties + required set that arm specifically demands.
--
-- Invariant maintained by 'discriminatedSchema': the discriminant
-- field is always present in 'sbProperties' (with @const = sbDiscriminantValue@)
-- AND always in 'sbRequired'. Callers don't need to repeat it in
-- either list — the helper splices it in. This is the single
-- guard against the bug class the issue describes ('I forgot to
-- list the discriminant in the branch's required set').
data SchemaBranch = SchemaBranch
  { sbDiscriminantValue :: !Text
    -- ^ The literal string the discriminant field must equal in
    --   this branch (e.g. @\"rename_local\"@).
  , sbDescription       :: !Text
    -- ^ One-line description shown in the host's UI.
  , sbProperties        :: ![(Text, Value)]
    -- ^ Field name → JSON-Schema fragment. The discriminant field
    --   itself MUST NOT appear here — 'discriminatedSchema' adds it.
  , sbRequired          :: ![Text]
    -- ^ Required field names. The discriminant field MUST NOT
    --   appear here — 'discriminatedSchema' adds it.
  }
  deriving stock (Eq, Show)

-- | Build a top-level @oneOf@-discriminated schema for an
-- action-style tool.
--
-- The published shape is:
--
-- > { "type": "object",
-- >   "oneOf": [
-- >     { "type": "object",
-- >       "description": "<branch desc>",
-- >       "properties": { "<discrim>": { "const": "<value>" }, ... },
-- >       "required":   ["<discrim>", ...],
-- >       "additionalProperties": false }
-- >   , ...
-- >   ]
-- > }
--
-- The @additionalProperties: false@ makes branch-mismatch surface
-- with a clean validator error in hosts that respect Draft-7;
-- well-behaved clients render only the fields the chosen branch
-- declares.
discriminatedSchema
  :: Text             -- ^ Discriminant field name (\"action\" / \"host\" / ...)
  -> [SchemaBranch]
  -> Value
discriminatedSchema discrim branches =
  object
    [ "type"  .= ("object" :: Text)
    , "oneOf" .= map (renderBranch discrim) branches
    ]

renderBranch :: Text -> SchemaBranch -> Value
renderBranch discrim sb =
  let allProperties =
        (discrim, constString (sbDiscriminantValue sb)) : sbProperties sb
      allRequired =
        discrim : sbRequired sb
  in object
       [ "type"                 .= ("object" :: Text)
       , "description"          .= sbDescription sb
       , "properties"           .= object [ Key.fromText k .= v | (k, v) <- allProperties ]
       , "required"             .= allRequired
       , "additionalProperties" .= False
       ]

-- | A flat object schema for tools without action discrimination.
-- Equivalent to the inline pattern most tools use today; offered
-- here so the migration path can stay consistent (every tool
-- builds its schema through this module rather than open-coding it).
flatObjectSchema
  :: [(Text, Value)]   -- ^ properties
  -> [Text]            -- ^ required
  -> Value
flatObjectSchema props req =
  object
    [ "type"                 .= ("object" :: Text)
    , "properties"           .= object [ Key.fromText k .= v | (k, v) <- props ]
    , "required"             .= req
    , "additionalProperties" .= False
    ]
