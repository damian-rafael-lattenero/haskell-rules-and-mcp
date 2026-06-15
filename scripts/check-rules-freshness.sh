#!/usr/bin/env bash
# check-rules-freshness.sh — gate the .claude/rules file against drift.
#
# What this catches that a manual review misses:
#
#   * Hardcoded tool-count literals (the "25 tools" → "35 tools" mode
#     of bit-rot the rules file went through before PR-1).
#   * Retired wire names recommended outside their "Replaces retired
#     X" / "retired" annotation context — i.e. any line that asks the
#     agent to call a tool that no longer exists.
#
# What this does NOT catch:
#
#   * Subtle wording bugs, factually wrong decision-matrix rows,
#     stale section ordering. Those need human review or behavioural
#     tests (the dogfood-2026-XX-XX-comprehensive.md replay).
#
# Exits 0 on clean rules, non-zero on any drift hit. Invoked from
# scripts/ci-local.sh after the hlint step.

set -euo pipefail

# Resolve repo root from this script's location so `cd <repo> && bash
# scripts/check-rules-freshness.sh` and direct invocation both work.
SCRIPT_DIR="$(cd "$(dirname "$0")" && pwd)"
REPO_ROOT="$(dirname "$SCRIPT_DIR")"
RULES="$REPO_ROOT/.claude/rules/use-haskell-flows-mcp.md"
RULES2="$REPO_ROOT/.claude/rules/haskell-flows-mcp.md"

if [ ! -f "$RULES" ]; then
  echo "ERROR: rules file not found at $RULES" >&2
  exit 2
fi
if [ ! -f "$RULES2" ]; then
  echo "ERROR: rules file not found at $RULES2" >&2
  exit 2
fi

fail_count=0

red()    { printf '\033[1;31m%s\033[0m\n' "$*"; }
warn()   { printf '\033[1;33m%s\033[0m\n' "$*"; }
ok()     { printf '\033[1;32m%s\033[0m\n' "$*"; }

# ----------------------------------------------------------------------
# Check 1: hardcoded tool-count literals.
#
# The rules file delegates inventory truth to docs/TOOL_TAXONOMY.md
# (which is CI-enforced). Any "<NN> tools" literal in the rules file
# is by definition a drift hazard — the day a tool lands or retires,
# the count ages and the rules contradict the canonical source.
# ----------------------------------------------------------------------
count_hits=$(grep -nE '\b(2[0-9]|3[0-9]|4[0-9])\s+tools\b' "$RULES" || true)
if [ -n "$count_hits" ]; then
  red "DRIFT: hardcoded tool-count literal in rules file."
  echo "$count_hits"
  echo "  → delete the literal; reference docs/TOOL_TAXONOMY.md instead."
  fail_count=$((fail_count + 1))
fi

# ----------------------------------------------------------------------
# Check 2: retired wire names recommended outside annotation context.
#
# Retired tool names (Phase B/C consolidation in #94) are deliberately
# allowed to appear in lines that ALSO contain the word "retired" or
# "Replaces" — those are informational citations explaining what the
# new tool subsumed. ANY OTHER occurrence asks the agent to call a
# tool that no longer exists, so it must fail the gate.
# ----------------------------------------------------------------------
RETIRED_NAMES=(
  ghc_validate_cabal
  ghc_create_project
  ghc_switch_project
  ghc_bootstrap
  ghc_property_lifecycle
  ghc_regression
  ghc_quickcheck_export
  ghc_property_audit
  ghc_toolchain_status
  ghc_toolchain_warmup
  ghc_determinism
  ghc_move
  ghc_deps_explain
)

for name in "${RETIRED_NAMES[@]}"; do
  # Lines that mention the retired name BUT do not also contain
  # "retired" / "Replaces" — those are the offenders.
  bad_lines=$(grep -nE "\b$name\b" "$RULES" \
                | grep -vE 'retired|Replaces' || true)
  if [ -n "$bad_lines" ]; then
    red "DRIFT: rules file recommends retired wire name '$name'"
    echo "$bad_lines"
    echo "  → either remove the line or annotate with 'Replaces retired ...'"
    fail_count=$((fail_count + 1))
  fi
done

# ----------------------------------------------------------------------
# Check 3: retired-name + self-consistency checks on haskell-flows-mcp.md.
#
# Unlike use-haskell-flows-mcp.md, this briefing file is ALLOWED to carry
# a "(NN tools)" count literal — it is a quick-reference for the agent.
# The count-literal check does NOT apply here.  Instead we verify:
#   a) No retired wire names are recommended (same as Check 2).
#   b) The count in the header "(NN tools)" matches the number of "- `...`"
#      lines in the "## Full tool inventory" section.
# ----------------------------------------------------------------------

# 3a: retired wire names in the briefing file.
for name in "${RETIRED_NAMES[@]}"; do
  bad_lines2=$(grep -nE "\b$name\b" "$RULES2" \
                 | grep -vE 'retired|Replaces' || true)
  if [ -n "$bad_lines2" ]; then
    red "DRIFT: haskell-flows-mcp.md recommends retired wire name '$name'"
    echo "$bad_lines2"
    echo "  → either remove the line or annotate with 'Replaces retired ...'"
    fail_count=$((fail_count + 1))
  fi
done

# 3b: header count vs actual inventory count.
header_count=$(grep -oE '\([0-9]+\s+tools\)' "$RULES2" | grep -oE '[0-9]+' | head -1 || true)
# Count "- `toolname`" lines inside the inventory section (between
# "## Full tool inventory" and the next "## " section header).
inventory_count=$(sed -n '/^## Full tool inventory/,/^## [A-Z]/p' "$RULES2" \
                    | grep -cE "^- \`" || true)
if [ -n "$header_count" ] && [ "$header_count" != "$inventory_count" ]; then
  red "DRIFT: haskell-flows-mcp.md header says ($header_count tools) but inventory has $inventory_count entries."
  echo "  → update the header count or add/remove the missing tool entries."
  fail_count=$((fail_count + 1))
fi

# ----------------------------------------------------------------------
# Summary
# ----------------------------------------------------------------------
if [ "$fail_count" -gt 0 ]; then
  red "rules-freshness check FAILED ($fail_count issue(s))."
  exit 1
fi

ok "rules-freshness check passed."
