-- | @ghc_project@ — action-discriminated primitive that subsumes the
-- four legacy project-lifecycle tools:
--
--   * @action: \"create\"@   — 'HaskellFlows.Tool.CreateProject'
--   * @action: \"switch\"@   — 'HaskellFlows.Tool.SwitchProject'
--   * @action: \"validate\"@ — 'HaskellFlows.Tool.ValidateCabal'
--   * @action: \"bootstrap\"@— 'HaskellFlows.Tool.Bootstrap'
--
-- Issue #94 Phase C step 5: the four per-verb tools are retired
-- outright and replaced by this single action-discriminated
-- primitive. This collapses four wire surfaces to one and avoids
-- the @ghc_create_project@ vs @ghc_switch_project@ vs
-- @ghc_validate_cabal@ vs @ghc_bootstrap@ name-grid agents had to
-- memorise.
--
-- This module exports only the schema 'descriptor' — dispatch lives
-- in 'HaskellFlows.Mcp.Server' because each underlying handler has
-- a different signature with respect to server state ('IORef
-- ProjectDir', 'MVar (Maybe GhcSession)', the property 'Store',
-- @[ToolDescriptor]@). Bundling that into a single
-- 'ProjectDir -> Value -> IO ToolResult' wrapper would have meant
-- exposing all of those handles from this module, which is a
-- cleaner-looking layering violation than the explicit per-arm
-- routing in 'Server.dispatchByName'.
--
-- Schema is per-action @oneOf@-discriminated (issue #92): each
-- action declares its own required-field set. A host that respects
-- Draft-7 'oneOf' will reject mismatched payloads at validation
-- time before the runtime sees them.
module HaskellFlows.Tool.Project
  ( descriptor
  ) where

import Data.Aeson
import Data.Text (Text)

import qualified HaskellFlows.Mcp.Schema as Schema
import HaskellFlows.Mcp.Protocol
import HaskellFlows.Mcp.ToolName (ToolName (..), toolNameText)

descriptor :: ToolDescriptor
descriptor =
  ToolDescriptor
    { tdName        = toolNameText GhcProject
    , tdDescription =
        "Project lifecycle: scaffold (action='create'), switch the \
        \active root (action='switch'), validate the .cabal \
        \(action='validate'), or self-install host rules \
        \(action='bootstrap'). #94 Phase C step 5 successor to \
        \ghc_create_project + ghc_switch_project + \
        \ghc_validate_cabal + ghc_bootstrap; the four legacy tools \
        \have been removed in the same commit."
    , tdInputSchema = schema
    }

schema :: Value
schema = Schema.discriminatedSchema "action"
  [ Schema.SchemaBranch
      { Schema.sbDiscriminantValue = "create"
      , Schema.sbDescription       =
          "Scaffold a minimal cabal project (library + test-suite) in \
          \the current project directory. Creates <name>.cabal, \
          \cabal.project, src/<Module>.hs, and test/Spec.hs."
      , Schema.sbProperties        =
          [ ("name", Schema.stringField
              "Package name (Hackage shape: letters + digits + hyphen, \
              \must start with a letter).")
          , ("module", Schema.stringField
              "Top-level module name. Default: derived from the package \
              \name by PascalCase-ing the segments.")
          , ("overwrite", Schema.booleanField
              "If true, overwrite existing scaffolded files. Default: \
              \false — fails if any target already exists.")
          ]
      , Schema.sbRequired          = ["name"]
      }
  , Schema.SchemaBranch
      { Schema.sbDiscriminantValue = "switch"
      , Schema.sbDescription       =
          "Repoint the MCP at a different cabal project without \
          \restarting the host. The new path must be absolute, must \
          \exist, and must contain at least one .cabal file (or be \
          \empty — empty dir is allowed because action='create' is \
          \the canonical follow-up). Tears down the in-process \
          \GhcSession (if any) and re-opens the property store \
          \against the new path."
      , Schema.sbProperties        =
          [ ("path", Schema.stringField
              "Absolute path to the target cabal project directory. \
              \Example: \"/Users/me/projects/new-app\".")
          ]
      , Schema.sbRequired          = ["path"]
      }
  , Schema.SchemaBranch
      { Schema.sbDiscriminantValue = "validate"
      , Schema.sbDescription       =
          "Validate the project's .cabal file. Runs `cabal check` + \
          \common-issue heuristics (duplicate deps, missing \
          \default-language, phantom exposed-modules). Returns \
          \structured per-issue output with severity tags."
      , Schema.sbProperties        = []
      , Schema.sbRequired          = []
      }
  , Schema.SchemaBranch
      { Schema.sbDiscriminantValue = "bootstrap"
      , Schema.sbDescription       =
          "Self-install host-specific guidance files from content \
          \baked into the MCP binary. Hosts: 'claude-code' (writes \
          \.claude/rules/haskell-flows-mcp.md), 'cursor' \
          \(.cursor/rules/haskell-flows-mcp.md), or 'generic' \
          \(returns the text without writing). Dry-run by default; \
          \pass write=true to actually write the file. Content is \
          \dynamically derived from the live tool registry — never \
          \stale vs the running binary."
      , Schema.sbProperties        =
          [ ("host", hostField)
          , ("write", Schema.booleanField
              "If true, write the file to disk under the project dir. \
              \If false (default), return the content so the agent \
              \can preview before committing.")
          ]
      , Schema.sbRequired          = ["host"]
      }
  ]
  where
    hostField :: Value
    hostField = object
      [ "type"        .= ("string" :: Text)
      , "enum"        .= (["claude-code", "cursor", "generic"] :: [Text])
      , "description" .= ("Target host convention." :: Text)
      ]
