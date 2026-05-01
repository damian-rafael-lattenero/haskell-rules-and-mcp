#!/usr/bin/env bash
# bench-mcp.sh — run the haskell-flows-mcp latency benchmark (#96).
#
# Usage:
#   scripts/bench-mcp.sh          # Phase A: print budget table
#   scripts/bench-mcp.sh --fast   # future: fast-subset only (Phase C)
#   scripts/bench-mcp.sh --full   # future: full matrix (Phase D nightly)
#
# Phase A: prints the budget table and exits 0.
# Phase B: will measure actual latencies against benchmarks/Reference/.
# Phase C: will gate on budget breaches (fail with exit 1 on p95 breach).
set -euo pipefail

REPO_ROOT="$(cd "$(dirname "$0")/.." && pwd)"
MCP_PKG="$REPO_ROOT/mcp-server-haskell"

echo "=== haskell-flows-mcp bench ==="
echo "Reference project: $MCP_PKG/benchmarks/Reference/"
echo ""

cd "$MCP_PKG"
cabal run haskell-flows-mcp-bench --offline 2>&1
