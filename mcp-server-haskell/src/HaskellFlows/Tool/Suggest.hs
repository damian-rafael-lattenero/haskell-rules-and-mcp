-- | @ghci_suggest@ — emit candidate QuickCheck laws for a function
-- based on its type signature.
--
-- Flow:
--
-- 1. Query GHCi for the function's type via @:t@.
-- 2. Parse the type structurally via 'HaskellFlows.Parser.TypeSignature'.
-- 3. Run every rule in 'HaskellFlows.Suggest.Rules.allRules' against
--    the parsed signature.
-- 4. Return the matching suggestions with @law@, @property@
--    (ready-to-run), @rationale@, @confidence@, @category@.
--
-- Innovation vs TS port:
--
-- * Rule engine is composable — rules are values in a list, not
--   hard-coded @if-else@ branches. Extending is a one-line append
--   in 'Suggest.Rules.allRules'.
-- * Each suggestion carries a @rationale@ and @confidence@ — the
--   agent can weight which to try first.
-- * Optional @category@ filter lets the caller scope to only
--   algebraic / list / monoid laws when that's what they need.
module HaskellFlows.Tool.Suggest
  ( descriptor
  , handle
  , SuggestArgs (..)
  , outOfScopeResult
  ) where

import Data.Aeson
import Data.Aeson.Types (parseEither)
import Data.Text (Text)
import qualified Data.Text as T
import qualified Data.Text.Lazy as TL
import qualified Data.Text.Lazy.Encoding as TLE

import HaskellFlows.Ghci.Session
import HaskellFlows.Mcp.Protocol
import HaskellFlows.Parser.Type (parseTypeOutput, ParsedType (..), isOutOfScope)
import HaskellFlows.Parser.TypeSignature (parseSignature)
import HaskellFlows.Suggest.Rules
  ( Confidence (..)
  , Suggestion (..)
  , applyRules
  )

descriptor :: ToolDescriptor
descriptor =
  ToolDescriptor
    { tdName        = "ghci_suggest"
    , tdDescription =
        "Given a function name, propose QuickCheck properties that "
          <> "the function's type signature suggests. Each suggestion "
          <> "includes a ready-to-run property expression, a rationale, "
          <> "and a confidence score. Feed the property straight into "
          <> "ghci_quickcheck."
    , tdInputSchema =
        object
          [ "type"       .= ("object" :: Text)
          , "properties" .= object
              [ "function_name" .= object
                  [ "type"        .= ("string" :: Text)
                  , "description" .=
                      ("Name of the function to analyse. Example: "
                       <> "\"reverse\", \"Prelude.map\"." :: Text)
                  ]
              , "category" .= object
                  [ "type"        .= ("string" :: Text)
                  , "description" .=
                      ("Optional category filter: \"algebraic\", \"list\", "
                       <> "\"monoid\", \"predicate\". Omit to return all "
                       <> "matching laws." :: Text)
                  ]
              ]
          , "required"             .= ["function_name" :: Text]
          , "additionalProperties" .= False
          ]
    }

data SuggestArgs = SuggestArgs
  { saFunctionName :: !Text
  , saCategory     :: !(Maybe Text)
  }
  deriving stock (Show)

instance FromJSON SuggestArgs where
  parseJSON = withObject "SuggestArgs" $ \o -> do
    fn <- o .:  "function_name"
    c  <- o .:? "category"
    pure SuggestArgs { saFunctionName = fn, saCategory = c }

handle :: Session -> Value -> IO ToolResult
handle sess rawArgs = case parseEither parseJSON rawArgs of
  Left parseError ->
    pure (errorResult (T.pack ("Invalid arguments: " <> parseError)))
  Right args -> case sanitizeExpression (saFunctionName args) of
    Left e -> pure (errorResult (formatCommandError e))
    Right safe -> do
      typeRes <- typeOf sess safe
      case typeRes of
        Left cmdErr ->
          pure (errorResult (formatCommandError cmdErr))
        Right gr
          -- BUG-15: surface GHC's "not in scope" diagnostic as a
          -- structured, actionable hint instead of the raw GHC
          -- error message. The most common cause is that the
          -- caller asked for laws on a function whose module
          -- hasn't been loaded in this GHCi session — the fix is
          -- to @ghci_load@ the module first, not to rephrase the
          -- function name.
          | isOutOfScope (grOutput gr) ->
              pure (outOfScopeResult safe (grOutput gr))
          | not (grSuccess gr) ->
              pure (errorResult (grOutput gr))
          | otherwise -> do
              let mParsedType = parseTypeOutput (grOutput gr)
              case mParsedType of
                Nothing -> pure (errorResult
                  ( "Could not parse ':t " <> safe <> "' output. "
                 <> "Raw:\n" <> grOutput gr ))
                Just pt -> case parseSignature (ptType pt) of
                  Nothing -> pure (errorResult
                    ( "Could not parse signature: " <> ptType pt ))
                  Just sig -> do
                    let matches = applyRules safe sig
                        filtered = case saCategory args of
                          Nothing -> matches
                          Just c  -> filter ((c ==) . sCategory) matches
                    pure (successResult safe (ptType pt) filtered)

--------------------------------------------------------------------------------
-- response shaping
--------------------------------------------------------------------------------

successResult :: Text -> Text -> [Suggestion] -> ToolResult
successResult fn sig suggestions =
  let payload =
        object
          [ "success"     .= True
          , "function"    .= fn
          , "signature"   .= sig
          , "count"       .= length suggestions
          , "suggestions" .= map renderSuggestion suggestions
          , "hint"        .= hintFor suggestions
          ]
  in ToolResult
       { trContent = [ TextContent (encodeUtf8Text payload) ]
       , trIsError = False
       }

renderSuggestion :: Suggestion -> Value
renderSuggestion s =
  object
    [ "law"        .= sLaw s
    , "property"   .= sProperty s
    , "rationale"  .= sRationale s
    , "confidence" .= renderConfidence (sConfidence s)
    , "category"   .= sCategory s
    ]

renderConfidence :: Confidence -> Text
renderConfidence = \case
  High   -> "high"
  Medium -> "medium"
  Low    -> "low"

hintFor :: [Suggestion] -> Text
hintFor [] =
  "No laws matched the signature. Common reasons: function is effectful \
  \(IO / monadic), arity > 2, or return type is polymorphic in a way rules \
  \don't pattern-match on yet."
hintFor xs =
  "Try the highest-confidence suggestion first via ghci_quickcheck. "
  <> "Total: " <> T.pack (show (length xs))
  <> " candidate law(s). High-confidence: "
  <> T.pack (show (length (filter ((High ==) . sConfidence) xs)))
  <> "."

errorResult :: Text -> ToolResult
errorResult msg =
  ToolResult
    { trContent = [ TextContent (encodeUtf8Text (object
        [ "success" .= False
        , "error"   .= msg
        ]))
      ]
    , trIsError = True
    }

-- | BUG-15: a structured response for the "function not in scope"
-- case. The raw GHC error (@<interactive>:1:1: error: [GHC-88464]
-- Variable not in scope: foo@) is passed back as @ghcError@ for
-- drill-in, but the top-level keys speak the agent's language:
-- @reason@ names the class of failure and @hint@ tells the agent
-- exactly which tool to call next. 'success: false' is preserved
-- so the MCP treats this as an error for metrics, but the
-- @nextStep@ push in @Server.runTool@ still runs — steering the
-- agent straight at @ghci_load@ instead of letting it guess.
outOfScopeResult :: Text -> Text -> ToolResult
outOfScopeResult fn ghcOutput =
  ToolResult
    { trContent = [ TextContent (encodeUtf8Text (object
        [ "success"  .= False
        , "reason"   .= ("function_not_in_scope" :: Text)
        , "function" .= fn
        , "hint"     .=
            ( "`" <> fn <> "` is not in scope in the current GHCi \
              \session. Load the module that defines it first via \
              \ghci_load(module_path=\"<path>\"), or pass a fully \
              \qualified name (e.g. \"Data.List.sort\") if the \
              \definition lives in an already-loaded module under \
              \a qualified import."
              :: Text )
        , "ghcError" .= ghcOutput
        ]))
      ]
    , trIsError = True
    }

formatCommandError :: CommandError -> Text
formatCommandError = \case
  ContainsNewline  -> "function_name must be a single line (no newline characters)"
  ContainsSentinel -> "function_name contains the internal framing sentinel and was rejected"
  EmptyInput       -> "function_name is empty"

encodeUtf8Text :: Value -> Text
encodeUtf8Text = TL.toStrict . TLE.decodeUtf8 . encode
