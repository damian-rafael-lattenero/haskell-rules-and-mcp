#!/usr/bin/env bash
# agent-server-shim — run haskell-flows-mcp teeing its stderr to a log,
# so a REAL agent session (opencode) can still be scored mechanically:
# the server emits one tool_call_start/tool_call_end JSON line per MCP
# call on stderr.
#
# Usage:  agent-server-shim.sh <binary>
# Log:    $HF_AGENT_LOG  (default /tmp/agent-dogfood-server.log)
set -euo pipefail
LOG="${HF_AGENT_LOG:-/tmp/agent-dogfood-server.log}"
: > "$LOG"
exec "$1" 2> >(tee -a "$LOG" >&2)
