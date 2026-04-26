-- | Closed enumeration of every MCP @resources/*@ URI our server exposes.
--
-- The MCP spec lets a server publish a fixed catalogue of resources via
-- @resources/list@; clients fetch them by exact URI string via
-- @resources/read@. Today we ship one resource (the agent workflow rules
-- markdown), but the contract — "Resources.hs advertises the URI,
-- Server.hs renders the body, Spec.hs pins the inventory" — already
-- forces three string literals to stay in lockstep. A typo in any of
-- the three silently degrades the resource to "unknown URI".
--
-- Reifying the URIs as a sum type:
--
--   * Server.hs's @renderResource@ becomes exhaustive — adding a new
--     resource without a renderer is a GHC warning.
--   * Spec.hs's regression test pins the constructor list, not a list
--     of literal strings copy-pasted from the spec.
--   * Adding a new resource is a single-place change: add a constructor
--     here, extend 'resourceUriText' / 'parseResourceUri', then GHC
--     points at every call site that needs an update.
--
-- Wire format is preserved exactly via 'resourceUriText' / 'parseResourceUri'.
module HaskellFlows.Mcp.ResourceUri
  ( ResourceUri (..)
  , resourceUriText
  , parseResourceUri
  , allResourceUris
  , allResourceUriTexts
  ) where

import qualified Data.Map.Strict as Map
import Data.Text (Text)

-- | Every URI advertised via @resources/list@. Add a constructor here
-- when shipping a new resource.
data ResourceUri
  = WorkflowRules
  -- ^ @haskell-flows://rules/workflow@ — agent-facing markdown
  -- rendered from the live tool registry.
  deriving stock (Eq, Ord, Show, Enum, Bounded)

-- | Render to the canonical wire string. Sole source of truth for the
-- URI literal — every other site routes through here or 'parseResourceUri'.
resourceUriText :: ResourceUri -> Text
resourceUriText = \case
  WorkflowRules -> "haskell-flows://rules/workflow"

-- | Inverse of 'resourceUriText'. Returns 'Nothing' for any wire string
-- outside the closed catalogue so 'Server.dispatch' can short-circuit
-- to @invalidParamsErr@ instead of leaking a half-handled request.
parseResourceUri :: Text -> Maybe ResourceUri
parseResourceUri = flip Map.lookup reverseMap
  where
    reverseMap = Map.fromList [ (resourceUriText u, u) | u <- allResourceUris ]

-- | Every URI in declaration order. Used by the regression test that
-- pins the resource catalogue.
allResourceUris :: [ResourceUri]
allResourceUris = [minBound .. maxBound]

-- | Convenience: 'allResourceUris' rendered as 'Text'. Tests asserting
-- on the wire-format inventory take this directly.
allResourceUriTexts :: [Text]
allResourceUriTexts = map resourceUriText allResourceUris
