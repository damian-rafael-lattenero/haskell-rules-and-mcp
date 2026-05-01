#!/usr/bin/env bash
# bench-mcp.sh — run the haskell-flows-mcp latency benchmark (#96).
#
# Usage:
#   scripts/bench-mcp.sh           # informational; exits 0 always
#   scripts/bench-mcp.sh --gate    # Phase C gate: exits 1 on p95 breach
#                                  # (also forwarded as HFLOWS_BENCH_GATE=1)
#   scripts/bench-mcp.sh --full    # future: full matrix (Phase D nightly)
#
# Phase A: prints the budget table and exits 0.
# Phase B: measures actual latencies against benchmarks/Reference/.
# Phase C: --gate flag turns the run into a hard gate (exit 1 on
#          sustained p95 breach).  CI uses the env-var form
#          (HFLOWS_BENCH_GATE=1) to keep the workflow YAML
#          declarative — see .github/workflows/haskell-ci.yml.
set -euo pipefail

REPO_ROOT="$(cd "$(dirname "$0")/.." && pwd)"
MCP_PKG="$REPO_ROOT/mcp-server-haskell"

GATE_ARG=""
case "${1:-}" in
  --gate)
    GATE_ARG="--gate"
    ;;
  --full)
    echo "Note: --full is reserved for Phase D (nightly full-matrix)."
    echo "Current run is the same fast-subset Phase B benchmarks."
    ;;
  '') : ;;
  *) echo "unknown flag: $1"; exit 2 ;;
esac

echo "=== haskell-flows-mcp bench ==="
echo "Reference project: $MCP_PKG/benchmarks/Reference/"
if [[ -n "$GATE_ARG" ]]; then
  echo "Mode: --gate (exits 1 on sustained p95 budget breach)"
fi
echo ""

cd "$MCP_PKG"
# `--` separates cabal's args from the executable's args; the bench
# binary parses GATE_ARG internally (see benchmarks/Main.hs).
exec cabal run haskell-flows-mcp-bench --offline -- $GATE_ARG
