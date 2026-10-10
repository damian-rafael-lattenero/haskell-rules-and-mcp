#!/usr/bin/env bash
# ar-run — the arithmetic-expr dogfood (W3.2), same guarantees as hm-run:
#
#   1. dogfood-local-check  — ar sources compile warning-free and the
#                            2 properties pass, 300 tests each
#   2. probe.sh (filtered)  — server binary rebuilt + freshness-law
#                            checked + one e2e scenario on the wire
#   3. ar-loop.py           — the MCP-only run under hangwatch, with
#                            the scaled E4 verdict
#
# Usage: scripts/dogfood/ar-run.sh [e2e-filter]  (default: write_module flow)
set -euo pipefail

HERE="$(cd "$(dirname "$0")" && pwd)"      # .../scripts/dogfood
SERVER="$(cd "$HERE/../.." && pwd)"        # mcp-server-haskell
REPO="$(cd "$SERVER/.." && pwd)"
FILTER="${1:-Flow: Whole-module authorship}"

FIXTURE="$REPO/playground/ar-dogfood"
SPOOL="$REPO/playground/ar-spool"

echo "══ 1/3 · local source check (compile + properties) ══"
"$HERE/dogfood-local-check.sh" "$HERE/ar-loop.py"

echo "══ 2/3 · fresh server binary (probe, filtered ≈ '$FILTER') ══"
"$SERVER/scripts/probe.sh" "$FILTER"

BIN="$(cd "$SERVER" && cabal list-bin haskell-flows-mcp)"
echo "   server binary: $BIN"

echo "══ 3/3 · MCP-only dogfood under hangwatch ══"
python3 "$SERVER/scripts/hangwatch.py" ar-dogfood \
  "$HERE/ar-loop.py" "$BIN" "$FIXTURE" "$SPOOL" \
  STALL_S=180 TOTAL_S=900
