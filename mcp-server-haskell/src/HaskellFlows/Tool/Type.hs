-- | @ghci_type@ — Phase 2 tool.
--
-- Mirrors @mcp-server/src/tools/type-check.ts@: accepts a Haskell
-- expression, asks GHCi for its type via @:t@, returns a parsed
-- @{ expression, type }@ JSON payload.
--
-- Boundary safety: the expression argument is routed through
-- 'sanitizeExpression', so a newline or the framing sentinel can't reach
-- GHCi and desync our single-sentinel protocol.
module HaskellFlows.Tool.Type
  ( descriptor
  , handle
  , TypeArgs (..)
  ) where

import Data.Aeson
import Data.Aeson.Types (parseEither)
import Data.Text (Text)
import qualified Data.Text as T
import qualified Data.Text.Lazy as TL
import qualified Data.Text.Lazy.Encoding as TLE

import HaskellFlows.Ghci.Session
import HaskellFlows.Mcp.Protocol
import HaskellFlows.Parser.Type

-- | The schema surfaced to clients via @tools/list@.
descriptor :: ToolDescriptor
descriptor =
  ToolDescriptor
    { tdName        = "ghci_type"
    , tdDescription =
        "Get the type of a Haskell expression using GHCi's :t command. "
          <> "Use this to verify types of subexpressions before composing them, "
          <> "or to understand what type a function expects/returns."
    , tdInputSchema =
        object
          [ "type"       .= ("object" :: Text)
          , "properties" .= object
              [ "expression" .= object
                  [ "type"        .= ("string" :: Text)
                  , "description" .=
                      ("The Haskell expression to type-check. Examples: \
                       \\"map (+1)\", \"foldr\", \"Just . show\"" :: Text)
                  ]
              ]
          , "required"             .= ["expression" :: Text]
          , "additionalProperties" .= False
          ]
    }

newtype TypeArgs = TypeArgs
  { taExpression :: Text
  }
  deriving stock (Show)

instance FromJSON TypeArgs where
  parseJSON = withObject "TypeArgs" $ \o ->
    TypeArgs <$> o .: "expression"

-- | Handle a @tools/call@ for @ghci_type@.
handle :: Session -> Value -> IO ToolResult
handle sess rawArgs = case parseEither parseJSON rawArgs of
  Left parseError ->
    pure (errorResult (T.pack ("Invalid arguments: " <> parseError)))
  Right (TypeArgs expr) -> do
    res <- typeOf sess expr
    case res of
      Left cmdErr ->
        pure (errorResult (formatCommandError cmdErr))
      Right gr
        | not (grSuccess gr) ->
            pure (errorResult (grOutput gr))
        | isOutOfScope (grOutput gr) ->
            pure (errorResult (grOutput gr))
        | otherwise ->
            pure (successResult expr (grOutput gr))

--------------------------------------------------------------------------------
-- response shaping
--------------------------------------------------------------------------------

successResult :: Text -> Text -> ToolResult
successResult originalExpr raw =
  let payload = case parseTypeOutput raw of
        Just pt ->
          object
            [ "success"    .= True
            , "expression" .= ptExpression pt
            , "type"       .= ptType pt
            ]
        Nothing ->
          -- Parser failed — surface raw so the agent can still read it.
          object
            [ "success"    .= True
            , "expression" .= originalExpr
            , "raw"        .= raw
            ]
  in ToolResult
       { trContent = [ TextContent (encodeUtf8Text payload) ]
       , trIsError = False
       }

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

formatCommandError :: CommandError -> Text
formatCommandError = \case
  ContainsNewline ->
    "expression must be a single line (no newline characters allowed)"
  ContainsSentinel ->
    "expression contains the internal framing sentinel and was rejected"
  EmptyInput ->
    "expression is empty"
  InputTooLarge sz cap ->
    "expression is too large (" <> T.pack (show sz) <> " chars, cap is "
      <> T.pack (show cap) <> ")"

-- | UTF-8-safe JSON → Text. Fixes the Phase-1 TODO in Tool.Load where
-- @T.pack . show . encode@ mis-rendered non-ASCII output.
encodeUtf8Text :: Value -> Text
encodeUtf8Text = TL.toStrict . TLE.decodeUtf8 . encode
