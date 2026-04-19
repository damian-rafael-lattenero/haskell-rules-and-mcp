-- | @ghci_info@ — Phase 3 tool.
--
-- Mirrors @mcp-server/src/tools/type-info.ts@: given a Haskell name,
-- issues @:i@ via the persistent GHCi and returns a structured summary —
-- the 'InfoKind' + extracted instances — rather than a free-form blob.
--
-- Boundary safety mirrors 'HaskellFlows.Tool.Type': the name is routed
-- through 'sanitizeExpression' before hitting GHCi so a newline or the
-- framing sentinel can't desync the response protocol.
module HaskellFlows.Tool.Info
  ( descriptor
  , handle
  , InfoArgs (..)
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
    { tdName        = "ghci_info"
    , tdDescription =
        "Get detailed information about a Haskell name (function, type, typeclass) "
          <> "using GHCi's :i command. Shows the definition, kind, instances, and "
          <> "where it's defined."
    , tdInputSchema =
        object
          [ "type"       .= ("object" :: Text)
          , "properties" .= object
              [ "name" .= object
                  [ "type"        .= ("string" :: Text)
                  , "description" .=
                      ("The name to look up. Examples: \"Functor\", \
                       \\"Map.Map\", \"Maybe\", \"(++)\"" :: Text)
                  ]
              ]
          , "required"             .= ["name" :: Text]
          , "additionalProperties" .= False
          ]
    }

newtype InfoArgs = InfoArgs
  { iaName :: Text
  }
  deriving stock (Show)

instance FromJSON InfoArgs where
  parseJSON = withObject "InfoArgs" $ \o ->
    InfoArgs <$> o .: "name"

-- | Handle a @tools/call@ for @ghci_info@.
handle :: Session -> Value -> IO ToolResult
handle sess rawArgs = case parseEither parseJSON rawArgs of
  Left parseError ->
    pure (errorResult (T.pack ("Invalid arguments: " <> parseError)))
  Right (InfoArgs nm) -> do
    res <- infoOf sess nm
    case res of
      Left cmdErr ->
        pure (errorResult (formatCommandError cmdErr))
      Right gr
        | not (grSuccess gr)         -> pure (errorResult (grOutput gr))
        | isOutOfScope (grOutput gr) -> pure (errorResult (grOutput gr))
        | otherwise                  -> pure (successResult (grOutput gr))

--------------------------------------------------------------------------------
-- response shaping
--------------------------------------------------------------------------------

successResult :: Text -> ToolResult
successResult raw =
  let parsed = parseInfoOutput raw
      payload =
        object
          [ "success"    .= True
          , "name"       .= piName parsed
          , "kind"       .= kindToText (piKind parsed)
          , "definition" .= piDefinition parsed
          , "instances"  .= piInstances parsed
          ]
  in ToolResult
       { trContent = [ TextContent (encodeUtf8Text payload) ]
       , trIsError = False
       }

kindToText :: InfoKind -> Text
kindToText = \case
  IkClass       -> "class"
  IkData        -> "data"
  IkNewtype     -> "newtype"
  IkTypeSynonym -> "type-synonym"
  IkFunction    -> "function"
  IkUnknown     -> "unknown"

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
  ContainsNewline  -> "name must be a single line (no newline characters)"
  ContainsSentinel -> "name contains the internal framing sentinel and was rejected"
  EmptyInput       -> "name is empty"

encodeUtf8Text :: Value -> Text
encodeUtf8Text = TL.toStrict . TLE.decodeUtf8 . encode
