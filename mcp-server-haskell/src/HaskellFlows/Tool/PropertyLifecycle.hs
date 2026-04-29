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

import HaskellFlows.Data.PropertyStore (Store, StoredProperty (..), loadAll)
import qualified HaskellFlows.Mcp.Envelope as Env
import HaskellFlows.Mcp.Protocol
import HaskellFlows.Mcp.ToolName (ToolName (..), toolNameText)

descriptor :: ToolDescriptor
descriptor =
  ToolDescriptor
    { tdName        = toolNameText GhcPropertyLifecycle
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

-- | Issue #90 Phase C: pure introspection of the property store.
-- Always status='ok' (the operation has no failure modes — an
-- empty store still returns count=0 with an empty list under
-- 'result.properties').
handle :: Store -> Value -> IO ToolResult
handle store _rawArgs = do
  props <- loadAll store
  let payload = object
        [ "count"      .= length props
        , "properties" .= map render props
        ]
  pure (Env.toolResponseToResult (Env.mkOk payload))
  where
    render p = object
      [ "expression" .= spExpression p
      , "module"     .= spModule p
      , "passed"     .= spPassed p
      , "updated"    .= spUpdated p
      ]
