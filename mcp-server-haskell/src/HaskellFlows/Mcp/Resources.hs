-- | MCP @resources@ surface — first-class spec primitive the
-- client (Claude Desktop, Cursor, VS Code) exposes as browseable
-- documentation attached to the server.
--
-- We advertise one resource: the canonical agent-facing rules
-- document (@haskell-flows://rules/workflow@). The body is
-- rendered dynamically by 'HaskellFlows.Mcp.Guidance.workflowRulesMarkdown'
-- at @resources/read@ dispatch time — kept out of this module to
-- break a cyclic import (Resources does not see the tool registry).
--
-- Security: this module only advertises URIs. Body rendering is
-- pure text generation from compile-time data structures. No
-- filesystem read, no network fetch, no untrusted input.
module HaskellFlows.Mcp.Resources
  ( ResourceDescriptor (..)
  , allResources
  , knownResourceUris
  ) where

import Data.Aeson
import Data.Text (Text)

data ResourceDescriptor = ResourceDescriptor
  { rdUri         :: !Text
  , rdName        :: !Text
  , rdDescription :: !Text
  , rdMimeType    :: !Text
  }
  deriving stock (Eq, Show)

instance ToJSON ResourceDescriptor where
  toJSON rd = object
    [ "uri"         .= rdUri rd
    , "name"        .= rdName rd
    , "description" .= rdDescription rd
    , "mimeType"    .= rdMimeType rd
    ]

-- | Complete resource catalogue surfaced via @resources/list@.
-- Add a row here if you ship a new resource; render it in
-- 'HaskellFlows.Mcp.Server'\'s @resources/read@ dispatch.
allResources :: [ResourceDescriptor]
allResources =
  [ ResourceDescriptor
      { rdUri         = "haskell-flows://rules/workflow"
      , rdName        = "Haskell-flows agent workflow rules"
      , rdDescription =
          "Canonical rules describing every registered tool, the \
          \situation->tool table, session invariants, and the \
          \dogfood-fix-in-place flow. Body is rendered dynamically \
          \from the live tool registry — always in sync with the \
          \MCP's real tool count."
      , rdMimeType    = "text/markdown"
      }
  ]

-- | Convenience list of advertised URIs, used by regression tests
-- that pin the resource inventory.
knownResourceUris :: [Text]
knownResourceUris = map rdUri allResources
