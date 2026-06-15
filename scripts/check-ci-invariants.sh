#!/usr/bin/env bash
# check-ci-invariants.sh — guard the GitHub workflow against the
# test-failure-masking regression.
#
# THE BUG THIS CATCHES (2026-06-15):
#   `cabal test … --keep-going` returns exit 0 even when a test-suite
#   exits non-zero. A workflow that runs tests with --keep-going can
#   therefore report a GREEN check while tests are actually RED. This
#   was observed empirically on cabal 3.14 and removed from both
#   scripts/ci-local.sh and .github/workflows/haskell-ci.yml.
#
#   Build steps (`cabal build … --keep-going`) are FINE and desirable —
#   surfacing multiple compile errors at once saves rerun cycles. It is
#   ONLY the test exit code that must stay honest.
#
# This script fails if any line in the workflow that invokes `cabal test`
# also carries `--keep-going`. Exits 0 clean, non-zero on violation.
# Invoked from scripts/ci-local.sh and the CI `invariants` job.

set -euo pipefail

SCRIPT_DIR="$(cd "$(dirname "$0")" && pwd)"
REPO_ROOT="$(dirname "$SCRIPT_DIR")"
WORKFLOW="$REPO_ROOT/.github/workflows/haskell-ci.yml"

red()  { printf '\033[1;31m%s\033[0m\n' "$*"; }
ok()   { printf '\033[1;32m%s\033[0m\n' "$*"; }

if [ ! -f "$WORKFLOW" ]; then
  red "ERROR: workflow not found at $WORKFLOW"
  exit 2
fi

fail_count=0

# ----------------------------------------------------------------------
# Check 1: no `cabal test` line may carry `--keep-going`.
#
# Match lines that mention `cabal test` AND `--keep-going` on the same
# line. The fix runs each suite as `cabal test <suite>` with no
# keep-going, so a violation means someone reintroduced the masking.
# ----------------------------------------------------------------------
bad_lines=$(grep -nE 'cabal test' "$WORKFLOW" | grep -- '--keep-going' || true)
if [ -n "$bad_lines" ]; then
  red "DRIFT: 'cabal test' with --keep-going in haskell-ci.yml — masks test failures."
  echo "$bad_lines"
  echo "  → drop --keep-going from the test step; run each suite explicitly and"
  echo "    track per-suite exit codes (see the 'Run tests (unit + e2e)' step)."
  fail_count=$((fail_count + 1))
fi

if [ "$fail_count" -gt 0 ]; then
  red "ci-invariants check FAILED ($fail_count issue(s))."
  exit 1
fi

ok "ci-invariants check passed."
