#!/usr/bin/env bash
# hm-run — the HM dogfood (W3.1) end to end, guaranteed-fresh.
#
#   1. dogfood-local-check — sources compile warning-free and
#                             the 3 properties pass, 300 tests each
#   2. probe.sh (filtered)  — server binary rebuilt + freshness-law
#                             checked + one e2e scenario on the wire
#   3. hm-loop.py           — the MCP-only run under hangwatch, with
#                             the E4 verdict (calls/breaks/zombies/
#                             props/cold_start)
#
# Every block must be green; the script stops at the first that isn't.
# Usage: scripts/dogfood/hm-run.sh [e2e-filter]  (default: write_module flow)
set -euo pipefail

HERE="$(cd "$(dirname "$0")" && pwd)"      # .../scripts/dogfood
SERVER="$(cd "$HERE/../.." && pwd)"        # mcp-server-haskell
REPO="$(cd "$SERVER/.." && pwd)"
FILTER="${1:-Flow: Whole-module authorship}"

FIXTURE="$REPO/playground/hm-dogfood"
SPOOL="$REPO/playground/hm-spool"

echo "══ 1/3 · local source check (compile + properties) ══"
"$HERE/dogfood-local-check.sh"

echo "══ 2/3 · fresh server binary (probe, filtered ≈ '$FILTER') ══"
"$SERVER/scripts/probe.sh" "$FILTER"

BIN="$(cd "$SERVER" && cabal list-bin haskell-flows-mcp)"
echo "   server binary: $BIN"

echo "══ 3/3 · MCP-only dogfood under hangwatch ══"
python3 "$SERVER/scripts/hangwatch.py" hm-dogfood \
  "$HERE/hm-loop.py" "$BIN" "$FIXTURE" "$SPOOL" \
  STALL_S=180 TOTAL_S=900
