#!/usr/bin/env bash
# agent-run — W3.3: a REAL agent session (opencode) driving one
# haskell-flows-mcp server, scored mechanically for E4.
#
#   1. build the server + freshness law (src/ + app/ only — the agent
#      run exercises no test suites)
#   2. reset the fixture; write a scoped opencode.json there (nearest
#      config wins over the repo root's, which points at an older
#      installed binary). The server runs through agent-server-shim.sh
#      so its stderr (tool_call events) lands in a scoreable log.
#   3. `opencode run` the HM-lite task under hangwatch.
#   4. agent-score.sh prints the E4 table from events + artifacts.
#
# Usage: scripts/dogfood/agent-run.sh
set -euo pipefail

HERE="$(cd "$(dirname "$0")" && pwd)"      # .../scripts/dogfood
SERVER="$(cd "$HERE/../.." && pwd)"        # mcp-server-haskell
REPO="$(cd "$SERVER/.." && pwd)"
FIXTURE="$REPO/playground/agent-dogfood"
LOG="${HF_AGENT_LOG:-/tmp/agent-dogfood-server.log}"

echo "══ 1/4 · fresh server binary ══"
(cd "$SERVER" && cabal build exe:haskell-flows-mcp > /tmp/agent-build.log 2>&1) \
  || { tail -20 /tmp/agent-build.log; exit 1; }
BIN="$(cd "$SERVER" && cabal list-bin haskell-flows-mcp)"
newest_src=$(find "$SERVER/src" "$SERVER/app" -type f -name '*.hs' \
             -exec stat -f '%m' {} + | sort -n | tail -1)
bin_epoch=$(stat -f '%m' "$BIN")
if [ "$bin_epoch" -lt "$newest_src" ]; then
  echo "FRESH FAIL: binary older than newest src (cabal cache lying?)"
  echo "  rm -rf $SERVER/dist-newstyle/build/*/ghc-*/haskell-flows-mcp-*"
  exit 1
fi
echo "   fresh OK · $BIN"

echo "══ 2/4 · fixture + scoped opencode config ══"
rm -rf "$FIXTURE"; mkdir -p "$FIXTURE"
cat > "$FIXTURE/opencode.json" <<EOF
{
  "\$schema": "https://opencode.ai/config.json",
  "permission": { "edit": "allow", "bash": "allow", "webfetch": "allow" },
  "mcp": {
    "haskell-flows": {
      "type": "local",
      "command": ["$HERE/agent-server-shim.sh", "$BIN"],
      "cwd": "$FIXTURE",
      "environment": {
        "HASKELL_PROJECT_DIR": "$FIXTURE",
        "PATH": "/Users/lattenero/.ghcup/bin:/Users/lattenero/.local/bin:/usr/local/bin:/usr/bin:/bin"
      },
      "enabled": true,
      "timeout": 120000
    }
  }
}
EOF
: > "$LOG"

TASK='Trabajá ÚNICAMENTE con las tools MCP del servidor "haskell-flows" (no escribas código Haskell con bash ni con edición directa de archivos; todo el código Haskell va por sus tools). Objetivo, en este orden:
1. Creá un proyecto Haskell de nombre agenthm en el directorio exacto '"$FIXTURE"' (ghc_project action=create, con write=true y path explícito).
2. Registrá y escribí el módulo Agenthm.Math con la función triple :: Int -> Int que devuelve n * 3 (usá las actions de ghc_module para registrar y para escribir el módulo completo).
3. Verificá el proyecto completo en verde (ghc_check action=project).
4. Persistí una propiedad QuickCheck que afirme que triple n == n * 3 (ghc_property action=check).
5. Listá las propiedades persistidas (ghc_property action=list).
Si una tool responde con error, leé su mensaje: suele decir exactamente el paso que falta. No inventes tools ni actions.'

echo "══ 3/4 · opencode session under hangwatch ══"
# the user's opencode default model id no longer exists on the provider
# (zhipuai/glm-5.3 → zai-coding-plan/glm-5.3); pass it explicitly.
OC_MODEL="${OC_MODEL:-zai-coding-plan/glm-5.3}"
BASELINE=$({ pgrep -f "x/haskell-flows-mcp/build/haskell-flows-mcp/haskell-flows-mcp$" \
             || true; } | wc -l | tr -d ' ')
python3 "$SERVER/scripts/hangwatch.py" agent-dogfood \
  /Users/lattenero/.opencode/bin/opencode run --dir "$FIXTURE" \
    --model "$OC_MODEL" \
    --title "W3.3 haskell-flows dogfood" "$TASK" \
  STALL_S=240 TOTAL_S=1200

echo "══ 4/4 · mechanical E4 score ══"
"$HERE/agent-score.sh" "$LOG" "$FIXTURE" /tmp/hangwatch-agent-dogfood.log "$BASELINE"
