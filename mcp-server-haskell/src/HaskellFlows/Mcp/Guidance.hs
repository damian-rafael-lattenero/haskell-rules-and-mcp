-- | Canonical agent-facing guidance. Single source of truth for
-- what the LLM reads at session start and what the
-- @haskell-flows://rules/workflow@ resource serves.
--
-- Before this module there were four divergent surfaces:
--   * @Server.sessionInstructions@ (hand-edited string)
--   * @Resources.workflowRulesContent@ (hand-edited markdown)
--   * @.claude/rules/use-haskell-flows-mcp.md@ (user-side copy)
--   * @README.md@ situation-tool table
--
-- Every new Phase added a tool and one of the four copies forgot
-- to sync. The BUG-05 fix is to derive both rendered surfaces
-- from a single data-driven source:
--
--   * 'situationTable' — hand-curated list of (situation, tool,
--     example). Lives here so adding a tool with its usage row is
--     one edit in one file.
--   * 'sessionInstructionsText' — plain text for
--     @InitializeResult.instructions@ (MCP spec: free-form string).
--   * 'workflowRulesMarkdown' — markdown for the
--     @haskell-flows://rules/workflow@ resource (MCP spec:
--     @resources/read@).
--
-- Both take @[ToolDescriptor]@ so the tool count + tool-name list
-- always match the live registry — no way for the text to claim
-- "25 tools" while the registry ships 36.
module HaskellFlows.Mcp.Guidance
  ( SituationRow (..)
  , situationTable
  , sessionInstructionsText
  , workflowRulesMarkdown
  ) where

import Data.Text (Text)
import qualified Data.Text as T

import HaskellFlows.Mcp.Protocol (ToolDescriptor (..))

-- | One row in the situation-to-tool mapping. 'srExample' is a
-- one-line argument hint the agent can paraphrase into a real
-- call; kept short so the rendered table stays grep-able.
data SituationRow = SituationRow
  { srSituation :: !Text
  , srTool      :: !Text
  , srExample   :: !Text
  }
  deriving stock (Eq, Show)

-- | Canonical situation-to-tool table. Add a row here when you
-- land a new tool — both the plain-text instructions and the
-- markdown resource pick it up automatically.
situationTable :: [SituationRow]
situationTable =
  [ SituationRow "new data T declared"
                 "ghc_arbitrary"
                 "type_name=\"T\""
  , SituationRow "typed hole or empty stub"
                 "ghc_hole"
                 "module_path=\"src/X.hs\""
  , SituationRow "want QuickCheck laws from a signature"
                 "ghc_suggest"
                 "function_name=\"f\""
  , SituationRow "check a property"
                 "ghc_quickcheck"
                 "property=\"\\\\x -> ...\", module=\"src/X.hs\""
  , SituationRow "check property stability"
                 "ghc_determinism"
                 "property=\"...\", runs=5"
  , SituationRow "replay persisted properties"
                 "ghc_regression"
                 "action=\"run\""
  , SituationRow "materialize test/Spec.hs"
                 "ghc_quickcheck_export"
                 "(no args)"
  , SituationRow "property store lifecycle (list/drop)"
                 "ghc_property_lifecycle"
                 "action=\"list\""
  , SituationRow "rename a local binding"
                 "ghc_refactor"
                 "action=\"rename_local\", scope_line_start=, scope_line_end="
  , SituationRow "add a dependency"
                 "ghc_deps"
                 "action=\"add\", package=\"X\", stanza=\"library\"|\"test-suite\""
  , SituationRow "register new modules"
                 "ghc_add_modules"
                 "modules=[\"Foo.Bar\"]"
  , SituationRow "de-register modules"
                 "ghc_remove_modules"
                 "modules=[\"Foo.Old\"], delete_files=false"
  , SituationRow "add a missing import"
                 "ghc_add_import"
                 "name=\"Data.Map\""
  , SituationRow "apply a module export list"
                 "ghc_apply_exports"
                 "module_path=\"src/X.hs\", exports=[\"foo\"]"
  , SituationRow "list live imports in GHCi"
                 "ghc_imports"
                 "(no args)"
  , SituationRow "browse a module"
                 "ghc_browse"
                 "module=\"Foo.Bar\""
  , SituationRow "fix a GHC warning"
                 "ghc_fix_warning"
                 "module_path=\"src/X.hs\""
  , SituationRow "coverage report"
                 "ghc_coverage"
                 "(no args, 8 HPC metrics)"
  , SituationRow "lint (matches CI)"
                 "ghc_lint"
                 "path=\"src/\""
  , SituationRow "format source"
                 "ghc_format"
                 "module_path=\"src/X.hs\", write=true"
  , SituationRow "module gate"
                 "ghc_check_module"
                 "module_path=\"src/X.hs\""
  , SituationRow "project-wide gate"
                 "ghc_check_project"
                 "(no args)"
  , SituationRow "pre-push finalizer"
                 "ghc_gate"
                 "(regression + cabal test + cabal build)"
  , SituationRow "scaffold a new project"
                 "ghc_create_project"
                 "name=\"my-pkg\""
  , SituationRow "validate .cabal"
                 "ghc_validate_cabal"
                 "(no args)"
  , SituationRow "toolchain gates (cabal/ghc/hlint)"
                 "ghc_toolchain_status"
                 "(no args)"
  , SituationRow "toolchain warmup (probe optional bins)"
                 "ghc_toolchain_warmup"
                 "(no args)"
  , SituationRow "batch N tool calls"
                 "ghc_batch"
                 "actions=[{tool,args},...]"
  , SituationRow "install host rules (no repo clone)"
                 "ghc_bootstrap"
                 "host=\"claude-code\"|\"cursor\"|\"generic\", write=false"
  , SituationRow "what should I do next"
                 "ghc_workflow"
                 "action=\"help\""
  ]

--------------------------------------------------------------------------------
-- plain text: initialize.instructions
--------------------------------------------------------------------------------

-- | Build the plain-text instructions block the MCP carries in
-- @InitializeResult.instructions@. Dynamic in the tool count +
-- the exact tool name list so it cannot drift from the live
-- registry.
sessionInstructionsText :: [ToolDescriptor] -> Text
sessionInstructionsText descriptors =
  let nTools = length descriptors
      names  = T.intercalate ", " (map tdName descriptors)
  in T.unlines $
      [ "haskell-flows MCP — " <> tshow nTools
          <> " tools for Haskell development."
      , "Use this MCP for ALL Haskell work; do not shell out to"
      , "cabal/ghc/ghci/hlint/fourmolu/ormolu/hoogle/hls directly."
      , ""
      , "Start-of-session handshake:"
      , "  1. ghc_workflow(action=\"status\")     — confirm alive + "
          <> tshow nTools <> " tools"
      , "  2. ghc_toolchain_status()              — cabal/ghc/hlint gates"
      , "  3. ghc_workflow(action=\"help\")       — state-aware nudges"
      , ""
      , "Situation -> tool (canonical shortlist):"
      ]
      <> map renderRow situationTable
      <>
      [ ""
      , "Every successful tool response carries a `nextStep` field:"
      , "  { tool, why, example?, chain? }"
      , "Follow it when the flow fits what you intended; ignore when"
      , "it does not. The optional `chain` is a multi-step plan — you"
      , "can batch the suggested steps via ghc_batch(actions=chain)."
      , ""
      , "Invariants (do NOT bypass):"
      , "  * Never edit .cabal by hand for deps — use ghc_deps"
      , "    (post-edit parse invariant rejects corrupted files)."
      , "  * Never sed/awk across .hs files — use ghc_refactor"
      , "    (snapshot-and-compile-verify rolls back on failure)."
      , "  * Never shell out to cabal/ghc/ghci/hlint — every external"
      , "    tool has a dedicated MCP wrapper with structured output,"
      , "    timeouts, and DoS caps."
      , ""
      , "Liveness guarantees (post-Phase 11c):"
      , "  * SessionStatus = Alive | Overflowed | Dead."
      , "  * GHCi death wakes every STM-blocked executeNoLock via a"
      , "    status-TVar write."
      , "  * executeNoLock honours its timeoutMicros via registerDelay."
      , "  * Server.runTool wraps each handler in a 10-min outer"
      , "    timeout (defence-in-depth). Any hang > 10 min is a"
      , "    real regression — report it."
      , ""
      , "Dogfood-fix-in-place flow:"
      , "  If a tool returns a wrong result or hangs, edit"
      , "  mcp-server-haskell/src/HaskellFlows/, add a regression"
      , "  test in test/Spec.hs, run scripts/ci-local.sh --fast,"
      , "  commit+push. Keep working with the stale running binary"
      , "  — the fix lands on the next natural reinstall."
      , ""
      , "Full tool inventory (" <> tshow nTools <> "):"
      , "  " <> names
      ]
  where
    renderRow r =
      "  " <> padR 40 (srSituation r) <> " -> "
            <> srTool r <> "(" <> srExample r <> ")"
    padR n t = t <> T.replicate (max 0 (n - T.length t)) " "

--------------------------------------------------------------------------------
-- markdown: resources/read
--------------------------------------------------------------------------------

-- | Markdown variant served via @resources/read@ for the
-- @haskell-flows://rules/workflow@ URI. Same underlying data as
-- 'sessionInstructionsText'; clients that render the resource in
-- a side-panel get the pretty version.
workflowRulesMarkdown :: [ToolDescriptor] -> Text
workflowRulesMarkdown descriptors =
  let nTools = length descriptors
      names  = map tdName descriptors
  in T.unlines $
      [ "# haskell-flows — agent workflow rules"
      , ""
      , "You are connected to the `haskell-flows` MCP ("
          <> tshow nTools <> " tools). Use it for ALL Haskell work;"
      , "do not shell out to cabal/ghc/ghci/hlint directly."
      , ""
      , "## Session handshake"
      , ""
      , "1. `ghc_workflow(action=\"status\")` — confirm alive + "
          <> tshow nTools <> " tools"
      , "2. `ghc_toolchain_status()` — external-binary gates"
      , "3. `ghc_workflow(action=\"help\")` — state-aware nudges"
      , ""
      , "## Situation → tool"
      , ""
      , "| Situation | Tool | Example |"
      , "|---|---|---|"
      ]
      <> map rowMd situationTable
      <>
      [ ""
      , "## Per-response push"
      , ""
      , "Every successful tool call carries a `nextStep` field:"
      , ""
      , "```json"
      , "{ \"success\": true,"
      , "  \"nextStep\": {"
      , "    \"tool\":  \"<next tool>\","
      , "    \"why\":   \"<one-line rationale>\","
      , "    \"example\": { \"<arg>\": \"<value>\" },"
      , "    \"chain\": [ { \"tool\": \"...\", \"args\": {...} }, ... ]"
      , "  }"
      , "}"
      , "```"
      , ""
      , "The optional `chain` lets you batch multi-step plans via"
      , "`ghc_batch(actions=chain)` in a single round-trip."
      , ""
      , "## Invariants"
      , ""
      , "- **Never** edit `.cabal` by hand for deps — use `ghc_deps`."
      , "- **Never** sed/awk `.hs` files — use `ghc_refactor`."
      , "- **Never** shell out to `cabal`/`ghc`/`ghci`/`hlint`."
      , ""
      , "## Liveness guarantees"
      , ""
      , "- `SessionStatus = Alive | Overflowed | Dead`."
      , "- GHCi death wakes every STM-blocked `executeNoLock` via a"
      , "  status-TVar write."
      , "- `executeNoLock` honours `timeoutMicros` via `registerDelay`."
      , "- `Server.runTool` wraps each handler in a 10-min outer timeout."
      , ""
      , "## Full tool inventory (" <> tshow nTools <> ")"
      , ""
      ]
      <> map (\n -> "- `" <> n <> "`") names
      <>
      [ ""
      , "## Dogfood-fix-in-place"
      , ""
      , "Tool misbehaves → edit `mcp-server-haskell/src/...` → add a"
      , "regression test in `test/Spec.hs` → `scripts/ci-local.sh"
      , "--fast` → commit+push. Keep dogfooding with the stale"
      , "running binary; CI validates, the fix lands on the next"
      , "natural reinstall."
      ]
  where
    rowMd r =
      "| " <> srSituation r
      <> " | `" <> srTool r <> "`"
      <> " | `" <> srExample r <> "` |"

tshow :: Int -> Text
tshow = T.pack . show
