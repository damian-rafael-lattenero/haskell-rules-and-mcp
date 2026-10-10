#!/usr/bin/env bash
# agent-score — mechanical E4 score of a real-agent session.
#
# Sources of truth (no vibes):
#   * the server's tool_call_start/tool_call_end events (stderr tee)
#   * on-disk artifacts: .haskell-flows/properties.json, the scaffold
#   * process table: leftover servers from THIS run
#
# An agent getting a clean error envelope and recovering is NOT a
# protocol break — that is the honest-error loop working. Breaks are
# unknown-tool calls / raw protocol errors in the opencode output.
#
# Usage: agent-score.sh <server-log> <fixture-dir> [opencode-output-log]
#                            [baseline-dist-newstyle-servers]
# The zombie count subtracts the baseline captured BEFORE the session:
# pre-existing debris from other runs must not taint this verdict.
set -euo pipefail

LOG="$1"; FIXTURE="$2"; OC_OUT="${3:-}"
BASELINE="${4:-0}"

calls=$(grep -c '"event":"tool_call_start"' "$LOG" || true)
ok_ends=$(grep -c '"tool_call_end","level":"info"' "$LOG" || true)
err_ends=$(grep -c '"tool_call_end","level":"error"' "$LOG" || true)

echo "── tools used (from tool_call_start events)"
grep '"event":"tool_call_start"' "$LOG" \
  | sed -E 's/.*"tool":"([^"]+)".*/   \1/' | sort | uniq -c

props=0
if [ -f "$FIXTURE/.haskell-flows/properties.json" ]; then
  props=$(python3 -c "import json;print(len(json.load(open('$FIXTURE/.haskell-flows/properties.json'))))")
fi

# zombie check: this run's server runs the dist-newstyle binary (the
# shim's argv), which is distinguishable from any other haskell-flows
# server on the box by its path. Baseline (pre-run count) subtracted.
alive=$({ pgrep -f "x/haskell-flows-mcp/build/haskell-flows-mcp/haskell-flows-mcp$" \
          || true; } | wc -l | tr -d ' ')
zombies=$(( alive > BASELINE ? alive - BASELINE : 0 ))

breaks=0
if [ -n "$OC_OUT" ] && [ -f "$OC_OUT" ]; then
  breaks=$(grep -ci "tool not found\|method not found\|unknown tool" "$OC_OUT" || true)
fi

gate_green=no
if grep '"tool_call_end".*"tool":"ghc_check"' "$LOG" | grep -q '"status":"ok"'; then
  gate_green=yes
fi

cat <<EOF

== E4 (real-agent session) ==
  mcp_calls:        $calls
  calls_ok_ended:   $ok_ends  (clean-error recoveries: $err_ends)
  protocol_breaks:  $breaks   (unknown-tool / raw errors in agent output)
  props_persisted:  $props
  gate_green:       $gate_green
  zombies:          $zombies
EOF

verdict=GREEN
[ "$calls" -le 25 ]      || verdict=RED
[ "$breaks" -eq 0 ]      || verdict=RED
[ "$props" -ge 1 ]       || verdict=RED
[ "$zombies" -eq 0 ]     || verdict=RED
[ "$gate_green" = yes ]  || verdict=RED
echo "E4 VERDICT: $verdict"
[ "$verdict" = GREEN ]
