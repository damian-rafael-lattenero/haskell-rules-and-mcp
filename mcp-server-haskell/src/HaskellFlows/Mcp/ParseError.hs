-- | Issue #85 — friendly formatting for Aeson 'parseEither' errors at
-- the tool boundary.
--
-- The default 'parseEither parseJSON' error is the raw Aeson FromJSON
-- parser output, e.g.:
--
-- > Error in $: key "expression" not found
-- > Error in $.line: parsing Int failed, expected Number, but encountered String
--
-- That string makes its way verbatim into MCP responses today, with
-- two concrete pain points for an LLM agent reading the response:
--
--   * @\"Error in \$\"@ is JSON-pointer syntax for the document root —
--     it reads like there's a literal dollar sign in the input.
--   * The shape is one big string; an agent that wants to programmatically
--     distinguish \"missing required arg\" from \"argument has the wrong
--     type\" has to substring-match on Aeson internals.
--
-- This module rewrites both shapes into something the agent can route on:
--
--   * 'kind' — discriminator: @missing_arg@ / @type_mismatch@ /
--     @validation@ — surfaced via 'Env.eeKind' on the error envelope.
--   * 'field' — the name of the offending JSON key when the parser pinned
--     it (extracted from @key \"X\"@ or @\$.X@ / @\$['X']@) — surfaced via
--     'Env.eeField'.
--   * 'message' — a friendly natural-language rewrite (\"Required argument
--     'expression' is missing\") — the human-facing summary.
--   * 'eeCause' — preserves the raw Aeson string so a debugger can still
--     see exactly what the parser said.
--
-- The pattern matchers are deliberately narrow — anything they don't
-- recognise falls through to a passthrough that keeps the original
-- string. Wider rewrites would risk masking unfamiliar parse shapes
-- behind generic prose.
module HaskellFlows.Mcp.ParseError
  ( -- * Friendly tool-result helper
    formatParseError
    -- * Pure interpretation (exposed for unit tests)
  , InterpretedParseError (..)
  , interpretParseError
  ) where

import Data.Text (Text)
import qualified Data.Text as T

import qualified HaskellFlows.Mcp.Envelope as Env
import HaskellFlows.Mcp.Protocol (ToolResult)

-- | Structured view of a parse error after we've tried to recognise
-- one of the canonical Aeson shapes. The 'ipMessage' is the
-- human-friendly rewrite; 'ipKind' is the discriminator; 'ipField'
-- is the offending key when we could pin it.
data InterpretedParseError = InterpretedParseError
  { ipKind     :: !Env.ErrorKind
    -- ^ Either 'Env.MissingArg', 'Env.TypeMismatch', or 'Env.Validation'
    --   for the fall-through case.
  , ipField    :: !(Maybe Text)
    -- ^ Offending JSON field, when we could pin it.
  , ipMessage  :: !Text
    -- ^ Friendly rewrite suitable for the envelope's @error.message@.
  , ipRaw      :: !Text
    -- ^ The original Aeson string verbatim (for 'Env.eeCause').
  }
  deriving stock (Eq, Show)

-- | Parse the Aeson error string into an 'InterpretedParseError'.
--
-- Recognised shapes:
--
--   * @Error in \$: key \"X\" not found@      — missing required key
--   * @Error in \$.X: <reason>@                — error on field X
--   * @Error in \$['X']: <reason>@             — bracket-quoted field
--   * any string containing @parsing T failed@ — type mismatch
--   * any string containing @expected … encountered@ — type mismatch
--
-- Unrecognised shapes pass through with @ipKind = Validation@ and
-- the original message — we'd rather surface the raw Aeson text than
-- silently coerce it into the wrong category.
interpretParseError :: String -> InterpretedParseError
interpretParseError raw =
  let rawT = T.pack raw
  in case extractMissingKey rawT of
       Just fld -> InterpretedParseError
         { ipKind    = Env.MissingArg
         , ipField   = Just fld
         , ipMessage =
             "Required argument '" <> fld <> "' is missing."
         , ipRaw     = rawT
         }
       Nothing  -> case extractTypedFieldReason rawT of
         Just (fld, reason) -> InterpretedParseError
           { ipKind    = Env.TypeMismatch
           , ipField   = Just fld
           , ipMessage =
               "Argument '" <> fld
                 <> "' has the wrong type: " <> reason
           , ipRaw     = rawT
           }
         Nothing
           | hasTypeMismatchSignal rawT -> InterpretedParseError
              { ipKind    = Env.TypeMismatch
              , ipField   = Nothing
              , ipMessage =
                  "Argument has the wrong type. " <> rawT
              , ipRaw     = rawT
              }
           | otherwise -> InterpretedParseError
              { ipKind    = Env.Validation
              , ipField   = Nothing
              , ipMessage = "Invalid arguments: " <> rawT
              , ipRaw     = rawT
              }

-- | Convenience wrapper: build a failed 'ToolResult' from a parse
-- error string. The drop-in replacement for the duplicated
-- @parseErrorResult@ helpers each tool used to carry locally.
formatParseError :: String -> ToolResult
formatParseError raw =
  let ip      = interpretParseError raw
      envErr  = (Env.mkErrorEnvelope (ipKind ip) (ipMessage ip))
                  { Env.eeField = ipField ip
                  , Env.eeCause = Just (ipRaw ip)
                  }
  in Env.toolResponseToResult (Env.mkFailed envErr)

--------------------------------------------------------------------------------
-- internal pattern matchers
--------------------------------------------------------------------------------

-- | Extract the field from @Error in $: key \"X\" not found@.
extractMissingKey :: Text -> Maybe Text
extractMissingKey t = do
  rest <- T.stripPrefix "Error in $: key \"" t
  let (fld, after) = T.breakOn "\"" rest
  rest2 <- T.stripPrefix "\"" after
  if "not found" `T.isInfixOf` rest2 && not (T.null fld)
    then Just fld
    else Nothing

-- | Extract @(field, reason)@ from @Error in \$.X: <reason>@ or
-- @Error in \$['X']: <reason>@. The field name is unwrapped from
-- the bracket form.
extractTypedFieldReason :: Text -> Maybe (Text, Text)
extractTypedFieldReason t = case T.stripPrefix "Error in $" t of
  Nothing  -> Nothing
  Just rest -> case T.uncons rest of
    Just ('.', dotted) -> dotPath dotted
    Just ('[', bracketed) -> bracketPath bracketed
    _ -> Nothing
  where
    dotPath dotted =
      let (fld, after) = T.breakOn ":" dotted
      in case T.stripPrefix ":" after of
           Just reason | not (T.null fld) ->
             Just (T.strip fld, T.strip reason)
           _ -> Nothing

    bracketPath bracketed = do
      rest <- T.stripPrefix "'" bracketed
      let (fld, after) = T.breakOn "'" rest
      rest2 <- T.stripPrefix "']:" after
      if T.null fld then Nothing else Just (fld, T.strip rest2)

-- | Heuristic: any of the canonical Aeson type-mismatch phrases
-- present in the string.
hasTypeMismatchSignal :: Text -> Bool
hasTypeMismatchSignal t =
  any (`T.isInfixOf` t)
    [ "expected"          -- common to "expected X, but encountered Y"
    , "parsing"           -- "parsing Int failed"
    ]
