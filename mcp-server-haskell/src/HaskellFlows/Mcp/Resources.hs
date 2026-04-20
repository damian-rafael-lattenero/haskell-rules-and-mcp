-- | MCP @resources@ surface — first-class spec primitive the
-- client (Claude Desktop, Cursor, VS Code) exposes as browseable
-- documentation attached to the server.
--
-- We ship one resource today: the canonical agent-facing rules
-- document (`use-haskell-flows-mcp.md`) — same text the session
-- @initialize.instructions@ carries, but addressable so clients
-- that want to render it in a side-panel can do so on demand.
--
-- Security: resources served here are STATIC TEXT baked into the
-- binary. There is no filesystem read, no network fetch, no
-- untrusted input path. An agent cannot convince the MCP to serve
-- an arbitrary file.
module HaskellFlows.Mcp.Resources
  ( ResourceDescriptor (..)
  , allResources
  , readResource
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
allResources :: [ResourceDescriptor]
allResources =
  [ ResourceDescriptor
      { rdUri         = "haskell-flows://rules/workflow"
      , rdName        = "Haskell-flows agent workflow rules"
      , rdDescription =
          "Canonical rules file describing the 25 tools, situation->\
          \tool table, invariants, and dogfood-fix-in-place flow. \
          \Mirrors the text surfaced via initialize.instructions."
      , rdMimeType    = "text/markdown"
      }
  ]

-- | Dispatch @resources/read@ — match on URI + return the embedded
-- text. Unknown URIs return 'Nothing' so the caller can emit a
-- structured error.
readResource :: Text -> Maybe Text
readResource uri = case uri of
  "haskell-flows://rules/workflow" -> Just workflowRulesContent
  _                                -> Nothing

-- | Embedded rules text — kept compact; the detailed inventory
-- already lives in tool descriptors + initialize.instructions.
workflowRulesContent :: Text
workflowRulesContent =
  "# haskell-flows — agent workflow rules (resource form)\n\
  \\n\
  \You are connected to the `haskell-flows` MCP. Use it for ALL\n\
  \Haskell work. Do not shell out to cabal/ghc/ghci/hlint.\n\
  \\n\
  \## Start of session\n\
  \1. ghci_workflow(action=\"status\")\n\
  \2. ghci_toolchain_status()\n\
  \\n\
  \## Situation -> tool\n\
  \- new data T           -> ghci_arbitrary(type_name=\"T\")\n\
  \- hole/stub            -> ghci_hole(module_path=...)\n\
  \- want properties      -> ghci_suggest(function_name=\"f\")\n\
  \- check a law          -> ghci_quickcheck(property=..., module_path=...)\n\
  \- rename local         -> ghci_refactor(action=\"rename_local\", ...)\n\
  \- add dep              -> ghci_deps(action=\"add\", package=..., stanza=...)\n\
  \- ready to push        -> ghci_gate()  (regression + cabal test + cabal build)\n\
  \- materialize test/    -> ghci_quickcheck_export()\n\
  \- not in scope         -> ghci_add_import(name=\"X\")\n\
  \- register new module  -> ghci_add_modules(modules=[...])\n\
  \- what next            -> ghci_workflow(action=\"help\")\n\
  \\n\
  \## Per-response push\n\
  \Every successful tool call carries a `nextStep` field with\n\
  \`{tool, why, example?}` — take it as an anchor; ignore when it\n\
  \doesn't fit your intent.\n\
  \\n\
  \## Dogfood-fix-in-place flow\n\
  \Tool misbehaves → Edit mcp-server-haskell/src/... + add a regression\n\
  \test in test/Spec.hs → scripts/ci-local.sh --fast → commit + push.\n\
  \Keep working with the stale binary; CI validates.\n\
  \\n\
  \## Liveness guarantees\n\
  \SessionStatus = Alive | Overflowed | Dead. GHCi death flips to\n\
  \Dead via drainHandle EOF detection; every executeNoLock wakes\n\
  \via STM registerDelay. Server.runTool wraps each tool in a\n\
  \10-minute outer timeout. No tool hangs indefinitely.\n"
