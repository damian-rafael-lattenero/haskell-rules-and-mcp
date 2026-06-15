#!/usr/bin/env bash
# coverage-gate.sh — ratchet gate on unit-suite HPC coverage.
#
# WHAT: runs the unit suite with coverage, extracts the `expressions
# used` and `top-level declarations used` percentages from `hpc report`,
# and compares them against the committed floor in
# mcp-server-haskell/coverage-baseline.txt.
#
#   * measured < floor  → FAIL (coverage regressed).
#   * measured > floor  → pass + a hint to RAISE the floor (the ratchet
#                         only moves up, and only by an explicit commit).
#   * measured == floor → pass.
#
# WHY hpc report and not `cabal test --enable-coverage` stdout: cabal
# only writes the HTML report; the textual percentage summary comes from
# `hpc report` against the .tix + mix dirs (the same mechanism
# src/HaskellFlows/Tool/Coverage.hs uses). The two mix dirs (library +
# test-suite Main) must BOTH be passed or hpc can't resolve every .mix.
#
# Usage:
#   scripts/coverage-gate.sh            # run the gate
#   scripts/coverage-gate.sh --measure  # print measured %, skip the gate
#
# Run on demand and in the CI `coverage` job; wired into
# scripts/ci-local.sh --full.

set -euo pipefail

export PATH="$HOME/.ghcup/bin:$HOME/.cabal/bin:$PATH"

SCRIPT_DIR="$(cd "$(dirname "$0")" && pwd)"
REPO_ROOT="$(dirname "$SCRIPT_DIR")"
MCP_PKG="$REPO_ROOT/mcp-server-haskell"
BASELINE="$MCP_PKG/coverage-baseline.txt"

MEASURE_ONLY=false
case "${1:-}" in
  --measure) MEASURE_ONLY=true ;;
  '') : ;;
  *) echo "unknown flag: $1 (use --measure or no args)"; exit 2 ;;
esac

red()  { printf '\033[1;31m%s\033[0m\n' "$*"; }
warn() { printf '\033[1;33m%s\033[0m\n' "$*"; }
ok()   { printf '\033[1;32m%s\033[0m\n' "$*"; }
say()  { printf '\033[1;36m%s\033[0m\n' "$*"; }

cd "$MCP_PKG"

# ----------------------------------------------------------------------
# 1. run the unit suite with coverage (produces .tix + mix dirs).
# ----------------------------------------------------------------------
say "Running unit suite with --enable-coverage (this recompiles with -fhpc)…"
cabal test haskell-flows-mcp-test --enable-coverage --test-show-details=direct

# ----------------------------------------------------------------------
# 2. locate the .tix and every mix dir (library + test Main).
# ----------------------------------------------------------------------
TIX="$(find dist-newstyle -name 'haskell-flows-mcp-test.tix' | head -1)"
if [ -z "$TIX" ]; then
  red "coverage-gate: no .tix file found under dist-newstyle/ — did coverage build run?"
  exit 1
fi

declare -a HPCDIRS
while IFS= read -r d; do
  HPCDIRS+=("--hpcdir=$d")
done < <(find dist-newstyle -type d -path '*extra-compilation-artifacts/hpc/vanilla/mix')

if [ "${#HPCDIRS[@]}" -eq 0 ]; then
  red "coverage-gate: no hpc mix dirs found — cannot run hpc report."
  exit 1
fi

# ----------------------------------------------------------------------
# 3. run hpc report and extract the two percentages.
# ----------------------------------------------------------------------
REPORT="$(hpc report "${HPCDIRS[@]}" "$TIX")"
echo "$REPORT"

# `NN% expressions used (a/b)` / `NN% top-level declarations used (a/b)`.
# Take the leading integer percent on the matching line.
extract_pct() {
  echo "$REPORT" | grep -E "$1" | grep -oE '[0-9]+%' | head -1 | tr -d '%'
}
EXPR_PCT="$(extract_pct 'expressions used')"
TOP_PCT="$(extract_pct 'top-level declarations used')"

if [ -z "$EXPR_PCT" ] || [ -z "$TOP_PCT" ]; then
  red "coverage-gate: could not parse percentages from hpc report output."
  exit 1
fi

say "Measured: expressions=${EXPR_PCT}%  top_level=${TOP_PCT}%"

if [ "$MEASURE_ONLY" = true ]; then
  exit 0
fi

# ----------------------------------------------------------------------
# 4. read the baseline floor.
# ----------------------------------------------------------------------
if [ ! -f "$BASELINE" ]; then
  red "coverage-gate: baseline not found at $BASELINE"
  exit 2
fi
EXPR_FLOOR="$(grep -E '^expressions=' "$BASELINE" | cut -d= -f2 | tr -dc '0-9')"
TOP_FLOOR="$(grep -E '^top_level=' "$BASELINE" | cut -d= -f2 | tr -dc '0-9')"

if [ -z "$EXPR_FLOOR" ] || [ -z "$TOP_FLOOR" ]; then
  red "coverage-gate: malformed baseline (need 'expressions=' and 'top_level=' lines)."
  exit 2
fi
say "Floor:    expressions=${EXPR_FLOOR}%  top_level=${TOP_FLOOR}%"

# ----------------------------------------------------------------------
# 5. compare. Fail on regression; hint on improvement.
# ----------------------------------------------------------------------
fail=0
if [ "$EXPR_PCT" -lt "$EXPR_FLOOR" ]; then
  red "REGRESSION: expressions ${EXPR_PCT}% < floor ${EXPR_FLOOR}%."
  fail=1
fi
if [ "$TOP_PCT" -lt "$TOP_FLOOR" ]; then
  red "REGRESSION: top-level ${TOP_PCT}% < floor ${TOP_FLOOR}%."
  fail=1
fi

if [ "$fail" -eq 1 ]; then
  red "coverage-gate FAILED — coverage dropped below the ratchet floor."
  echo "  → add tests to restore coverage, or (only if the drop is"
  echo "    intentional, e.g. dead code removed) lower the floor in"
  echo "    $BASELINE with a justification in the commit message."
  exit 1
fi

if [ "$EXPR_PCT" -gt "$EXPR_FLOOR" ] || [ "$TOP_PCT" -gt "$TOP_FLOOR" ]; then
  warn "Coverage IMPROVED over the floor — ratchet it up:"
  warn "  edit $BASELINE → expressions=${EXPR_PCT}  top_level=${TOP_PCT}"
  warn "  (a 1-point cushion below these is fine to absorb jitter)."
fi

ok "coverage-gate passed (expressions ${EXPR_PCT}% ≥ ${EXPR_FLOOR}%, top-level ${TOP_PCT}% ≥ ${TOP_FLOOR}%)."
