-- | @ghc_property_lifecycle@ — inspect + prune the persisted
-- property store. Lean: list every stored property with its pass
-- count and last-updated timestamp, so an agent can reason about
-- staleness or prune properties tied to removed functions.
module HaskellFlows.Tool.PropertyLifecycle
  ( descriptor
  , handle
  ) where

import Data.Aeson
import Data.Text (Text)
import qualified Data.Text.Lazy as TL
import qualified Data.Text.Lazy.Encoding as TLE

import HaskellFlows.Data.PropertyStore (Store, StoredProperty (..), loadAll)
import HaskellFlows.Mcp.Protocol

descriptor :: ToolDescriptor
descriptor =
  ToolDescriptor
    { tdName        = "ghc_property_lifecycle"
    , tdDescription =
        "Inspect the persisted property store. Returns one entry per "
          <> "stored property with its expression, module, cumulative "
          <> "pass count, and last-updated POSIX time. Use to identify "
          <> "properties worth pruning."
    , tdInputSchema =
        object
          [ "type"       .= ("object" :: Text)
          , "properties" .= object []
          , "additionalProperties" .= False
          ]
    }

handle :: Store -> Value -> IO ToolResult
handle store _rawArgs = do
  props <- loadAll store
  let payload = object
        [ "success"    .= True
        , "count"      .= length props
        , "properties" .= map render props
        ]
  pure ToolResult
         { trContent = [ TextContent (encodeUtf8Text payload) ]
         , trIsError = False
         }
  where
    render p = object
      [ "expression" .= spExpression p
      , "module"     .= spModule p
      , "passed"     .= spPassed p
      , "updated"    .= spUpdated p
      ]

encodeUtf8Text :: Value -> Text
encodeUtf8Text = TL.toStrict . TLE.decodeUtf8 . encode
