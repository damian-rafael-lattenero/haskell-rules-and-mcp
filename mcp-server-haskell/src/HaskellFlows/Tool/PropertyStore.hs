-- | @ghc_property_store@ — action-discriminated primitive that
-- subsumes the four legacy property-store tools:
--
--   * @action: \"list\"@   — 'HaskellFlows.Tool.Regression' (action=list)
--   * @action: \"run\"@    — 'HaskellFlows.Tool.Regression' (action=run)
--   * @action: \"export\"@ — 'HaskellFlows.Tool.QuickCheckExport'
--   * @action: \"audit\"@  — 'HaskellFlows.Tool.PropertyAudit'
--
-- Issue #94 Phase C step 6: the four per-verb tools are retired
-- outright and replaced by this single action-discriminated
-- primitive. This collapses four wire surfaces to one and aligns
-- with the previous mergers' pattern.
--
-- (Note: 'HaskellFlows.Tool.PropertyLifecycle' had the same
-- shape as @action=list@ on the legacy 'ghc_regression'; the
-- consolidated @list@ branch routes to 'Regression.handle' with
-- @action=list@ so the wire shape (including the @action@ field)
-- is byte-identical to the legacy 'ghc_regression(action=list)'
-- caller. 'PropertyLifecycle.handle' is no longer reachable through
-- this surface but is kept exported because some unit tests still
-- exercise it directly.)
--
-- This module exports only the schema 'descriptor' — dispatch lives
-- in 'HaskellFlows.Mcp.Server.dispatchPropertyStore' because each
-- underlying handler has a different signature with respect to
-- server state ('Store', 'GhcSession', 'ProjectDir').
--
-- Schema is per-action @oneOf@-discriminated (issue #92): each
-- action declares its own required-field set (which, for these
-- four, is empty — 'action' is the only field).
module HaskellFlows.Tool.PropertyStore
  ( descriptor
  ) where

import Data.Aeson

import qualified HaskellFlows.Mcp.Schema as Schema
import HaskellFlows.Mcp.Protocol
import HaskellFlows.Mcp.ToolName (ToolName (..), toolNameText)

descriptor :: ToolDescriptor
descriptor =
  ToolDescriptor
    { tdName        = toolNameText GhcPropertyStore
    , tdDescription =
        "Inspect, replay, export, or audit the persisted property \
        \store. Actions: 'list' (one entry per stored property with \
        \pass count and last-updated time), 'run' (replay every \
        \stored property as a regression suite), 'export' (materialise \
        \test/Spec.hs from the live store), 'audit' (pairwise \
        \contradiction detector across stored laws). #94 Phase C \
        \step 6 successor to ghc_property_lifecycle + ghc_regression \
        \+ ghc_quickcheck_export + ghc_property_audit; the four \
        \legacy tools have been removed in the same commit."
    , tdInputSchema = schema
    }

schema :: Value
schema = Schema.discriminatedSchema "action"
  [ Schema.SchemaBranch
      { Schema.sbDiscriminantValue = "list"
      , Schema.sbDescription       =
          "Inspect every stored property — returns count + entries \
          \with expression, module, cumulative pass count, and \
          \last-updated POSIX time."
      , Schema.sbProperties        = []
      , Schema.sbRequired          = []
      }
  , Schema.SchemaBranch
      { Schema.sbDiscriminantValue = "run"
      , Schema.sbDescription       =
          "Replay every persisted QuickCheck property as a regression \
          \suite. Per-property pass/fail under 'replays', total \
          \regression count under 'regressions'."
      , Schema.sbProperties        = []
      , Schema.sbRequired          = []
      }
  , Schema.SchemaBranch
      { Schema.sbDiscriminantValue = "export"
      , Schema.sbDescription       =
          "Materialise test/Spec.hs from the persisted store. The \
          \emitted file is exactly what 'cabal test' will replay in \
          \CI; use this to seed a project's regression net."
      , Schema.sbProperties        = []
      , Schema.sbRequired          = []
      }
  , Schema.SchemaBranch
      { Schema.sbDiscriminantValue = "audit"
      , Schema.sbDescription       =
          "Pairwise contradiction probe across the persisted property \
          \set. Reports any pair of laws that disagree on a shared \
          \counter-example so the agent can prune or refine the \
          \weaker law."
      , Schema.sbProperties        = []
      , Schema.sbRequired          = []
      }
  ]
