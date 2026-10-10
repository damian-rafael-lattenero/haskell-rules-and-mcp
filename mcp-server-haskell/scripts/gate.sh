#!/usr/bin/env zsh
# Tiered quality gate — NO commit without T1 green.
#   gate.sh t0   → build only                       (~3 min)
#   gate.sh t1   → build + unit + Dogfood canary    (~6 min)
#   gate.sh t2   → t1 + full e2e (hangwatch-guarded)(~16 min)
set -e
cd "$(dirname "$0")/.."
. "$HOME/.ghcup/env"

TIER="${1:-t1}"
STALL="${STALL_S:-180}"
TOTAL="${TOTAL_S:-2400}"
WATCHDOG="scripts/hangwatch.py"

say() { printf "\n══ %s ══\n" "$1"; }

say "T0 · build"
if ! cabal build exe:haskell-flows-mcp test:haskell-flows-mcp-test test:haskell-flows-mcp-e2e > /tmp/gate-build.log 2>&1; then
  grep -E " error|Error" -A4 /tmp/gate-build.log | head -40
  echo "T0 FAIL (build) — log: /tmp/gate-build.log"
  exit 1
fi
BIN_E2E=$(cabal list-bin haskell-flows-mcp-e2e)
BIN_UT=$(cabal list-bin haskell-flows-mcp-test)
echo "T0 OK"

if [ "$TIER" = "t0" ]; then exit 0; fi

say "T1a · unit suite"
"$BIN_UT" > /tmp/gate-unit.log 2>&1
PASS=$(grep -cE '^PASS' /tmp/gate-unit.log || true); FAIL=$(grep -cE 'FAIL' /tmp/gate-unit.log || true)
echo "unit: $PASS PASS / $FAIL FAIL"
[ "$FAIL" -eq 0 ] || { echo "T1 FAIL (unit)"; tail -20 /tmp/gate-unit.log; exit 1; }

say "T1b · Dogfood canary (deadlock/lifecycle detector)"
OUT=$(python3 "$WATCHDOG" canary "$BIN_E2E" HASKELL_FLOWS_E2E_ONLY="Dogfood" STALL_S="$STALL" TOTAL_S=300)
echo "$OUT"
echo "$OUT" | grep -q "VEREDICTO: DONE" || { echo "T1 FAIL (canary hang/crash)"; exit 1; }
echo "$OUT" | grep -qE "40 / 40" || { echo "T1 FAIL (canary < 40/40)"; exit 1; }
echo "T1 OK"

if [ "$TIER" = "t1" ]; then exit 0; fi

say "T2 · full e2e"
OUT=$(python3 "$WATCHDOG" full "$BIN_E2E" STALL_S="$STALL" TOTAL_S="$TOTAL")
echo "$OUT"
echo "$OUT" | grep -q "VEREDICTO: DONE" || exit 1
echo "T2 OK (burn-down completo — 0 rojos desde 2026-10)"
