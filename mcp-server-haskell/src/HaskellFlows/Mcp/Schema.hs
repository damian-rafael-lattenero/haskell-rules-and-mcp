-- | Issue #92 Phase A — JSON Schema builder helpers for
-- action-discriminated tools.
--
-- Background: 'tools/list' is the canonical truth-source for /\"what
-- arguments does this tool accept?\"/, but pre-#92 tools whose
-- per-action required fields differ (e.g. 'ghc_refactor''s
-- @rename_local@ vs @extract_binding@) hand-wrote a single flat
-- @required@ list that lied about the runtime contract.
--
-- The original Phase A design wrapped each variant in a top-level
-- Draft-7 @oneOf@. That shape is valid JSON Schema but the Claude
-- API rejects @input_schema@ objects that carry @oneOf@ / @allOf@
-- / @anyOf@ at the top level — registering the MCP fails the whole
-- session with HTTP 400. We therefore emit a /flat/ schema: every
-- branch's properties are merged into a single @properties@ map,
-- and the discriminant becomes a plain @enum@ field. Per-action
-- required-field enforcement lives entirely in the runtime
-- 'FromJSON' parsers (which already enforce it).
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
-- Note on what gets published vs. what stays informative:
-- 'discriminatedSchema' merges every branch's 'sbProperties' into
-- the published flat schema and emits the union of
-- 'sbDiscriminantValue' as the discriminant @enum@. 'sbDescription'
-- and 'sbRequired' are NOT serialised to the schema (the Claude
-- API forbids the top-level @oneOf@ shape that would have carried
-- them). They remain in the data type for documentation purposes
-- and as the source of truth the runtime 'FromJSON' parsers cross-
-- reference when enforcing per-action required-field semantics.
data SchemaBranch = SchemaBranch
  { sbDiscriminantValue :: !Text
    -- ^ The literal string the discriminant field must equal in
    --   this branch (e.g. @\"rename_local\"@). Published as one
    --   element of the discriminant's @enum@.
  , sbDescription       :: !Text
    -- ^ One-line description for the branch. Informational;
    --   not serialised to the published schema.
  , sbProperties        :: ![(Text, Value)]
    -- ^ Field name → JSON-Schema fragment. The discriminant field
    --   itself MUST NOT appear here — 'discriminatedSchema' adds it.
    --   Merged across all branches into the flat published schema.
  , sbRequired          :: ![Text]
    -- ^ Required field names for this branch. Informational;
    --   not serialised to the published schema (the runtime
    --   'FromJSON' parser is the source of truth for per-action
    --   required-field enforcement).
  }
  deriving stock (Eq, Show)

-- | Build a flat schema for an action-discriminated tool.
--
-- The published shape is:
--
-- > { "type": "object",
-- >   "properties": {
-- >     "<discrim>": { "type": "string", "enum": ["v1","v2",...],
-- >                    "description": "..." },
-- >     "<field-from-any-branch>": { ... },
-- >     ...
-- >   },
-- >   "required": ["<discrim>"],
-- >   "additionalProperties": false
-- > }
--
-- All properties from every branch are merged into a single map
-- (left-biased: the first occurrence of a name wins). Only the
-- discriminant is advertised as required at the schema level;
-- per-action required-field enforcement is handled by the
-- runtime 'FromJSON' parsers (so the schema and runtime stay in
-- sync without needing top-level @oneOf@, which the Claude API
-- rejects).
discriminatedSchema
  :: Text             -- ^ Discriminant field name (\"action\" / \"host\" / ...)
  -> [SchemaBranch]
  -> Value
discriminatedSchema discrim branches =
  let mergedProps    = nubByKey (concatMap sbProperties branches)
      enumValues     = map sbDiscriminantValue branches
      discrimField   = object
        [ "type"        .= ("string" :: Text)
        , "enum"        .= enumValues
        , "description" .= ("Action to perform — exactly one of the listed values." :: Text)
        ]
      allProperties  = (discrim, discrimField) : mergedProps
  in object
       [ "type"                 .= ("object" :: Text)
       , "properties"           .= object [ Key.fromText k .= v | (k, v) <- allProperties ]
       , "required"             .= ([discrim] :: [Text])
       , "additionalProperties" .= False
       ]

-- | Left-biased dedup of an association list by key.
nubByKey :: Eq k => [(k, v)] -> [(k, v)]
nubByKey = go []
  where
    go acc [] = reverse acc
    go acc ((k, v) : rest)
      | any ((== k) . fst) acc = go acc rest
      | otherwise              = go ((k, v) : acc) rest

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
