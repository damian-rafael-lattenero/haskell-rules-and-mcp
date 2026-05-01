-- | @ghc_property_lifecycle@ — inspect + prune the persisted
-- property store. Lean: list every stored property with its pass
-- count and last-updated timestamp, so an agent can reason about
-- staleness or prune properties tied to removed functions.
module HaskellFlows.Tool.PropertyLifecycle
  ( handle
  ) where

import Data.Aeson

import HaskellFlows.Data.PropertyStore (Store, StoredProperty (..), loadAll)
import qualified HaskellFlows.Mcp.Envelope as Env
import HaskellFlows.Mcp.Protocol

-- | #94 Phase C step 6: this module's @descriptor@ was retired
-- when the four legacy property-store tools were merged into
-- 'HaskellFlows.Tool.PropertyStore'. 'handle' is no longer
-- reachable through the wire — the consolidated
-- @ghc_property_store(action=\"list\")@ branch dispatches to
-- 'Regression.handle' instead (its @list@ path emits the @action@
-- field that downstream NextStep payload-probes expect). 'handle'
-- below is kept exported because some existing unit tests
-- exercise the introspection helper directly.

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
