#!/usr/bin/env bash
# install-mcp.sh — rebuild the haskell-flows-mcp binary AND reconcile every
# MCP-client config so a plain Claude restart always launches the fresh build.
#
# ── The bug this script defends against ───────────────────────────────────
# Claude Code reads MCP server definitions from TWO places:
#   1. the project .mcp.json   (repo root)
#   2. the global ~/.claude.json  ("mcpServers" — and per-project "projects[…]")
# The GLOBAL one wins at launch. For a long time the global entry pointed at a
# COPY of the binary in ~/.local/bin (an old install step cp'd it there). That
# copy went stale: `cabal install` refreshed ~/.cabal/bin, but Claude kept
# launching the frozen ~/.local/bin copy — so new tools/actions appeared to be
# "missing" even right after a successful install. Deleting the copy then broke
# startup entirely (config pointed at a path that no longer existed).
#
# ── The fix ───────────────────────────────────────────────────────────────
# Point EVERY config at the cabal-install-managed SYMLINK:
#     ~/.cabal/bin/haskell-flows-mcp
# `cabal install exe:` repoints that symlink atomically into the immutable
# store on every build, so "install + restart" follows the symlink to the
# newest binary with zero per-release config edits. This script:
#   • rebuilds via `cabal install` (updates the symlink), and
#   • reconciles ~/.claude.json + .mcp.json so their command == the symlink,
#     auto-fixing any stale path (with a timestamped backup).
#
# Usage:
#   scripts/install-mcp.sh            # rebuild + install + reconcile configs
#   scripts/install-mcp.sh --reconcile # ONLY fix configs (no rebuild) — run with Claude quit
#   scripts/install-mcp.sh --check    # report binary staleness + config alignment (exit≠0 if off)
#   scripts/install-mcp.sh -h         # this help block
set -euo pipefail

# Same PATH dance as ci-local.sh — non-login shells on macOS don't source
# .zprofile, so cabal/ghc need to be made discoverable here.
export PATH="$HOME/.ghcup/bin:$HOME/.cabal/bin:$PATH"

cd "$(dirname "$0")/.."
REPO_ROOT="$(pwd)"

# The canonical command every config must launch: the cabal-managed symlink.
CANONICAL="$HOME/.cabal/bin/haskell-flows-mcp"
CLAUDE_JSON="$HOME/.claude.json"
PROJECT_MCP="$REPO_ROOT/.mcp.json"

step() { printf '\n\033[1;34m==> %s\033[0m\n' "$*"; }
ok()   { printf '\033[1;32m✓\033[0m %s\n' "$*"; }
warn() { printf '\033[1;33m!\033[0m %s\n' "$*"; }
err()  { printf '\033[1;31m✗\033[0m %s\n' "$*"; }

# Read the haskell-flows command path from a Claude Code config JSON.
# Scans top-level mcpServers AND every projects[…].mcpServers. Prints the
# command (empty if absent / unparseable). Read-only.
config_command() {
  local f="$1"
  [[ -f "$f" ]] || { echo ""; return; }
  python3 - "$f" <<'PY'
import json, sys, pathlib
f = pathlib.Path(sys.argv[1])
try:
    d = json.loads(f.read_text())
except Exception:
    print(""); sys.exit(0)
def find(d):
    s = (d.get("mcpServers") or {}).get("haskell-flows")
    if s and s.get("command"):
        return s["command"]
    for p in (d.get("projects") or {}).values():
        s = (p.get("mcpServers") or {}).get("haskell-flows")
        if s and s.get("command"):
            return s["command"]
    return ""
print(find(d))
PY
}

# Surgically repoint a config's haskell-flows command to $CANONICAL by literal
# string replacement (preserves all formatting; the full binary path is unique
# to this server so the replace can't touch anything else). Backs up first.
fix_config() {
  local f="$1" cur="$2"
  cp "$f" "$f.bak-$(date +%Y%m%d-%H%M%S)"
  python3 - "$f" "$cur" "$CANONICAL" <<'PY'
import sys, pathlib
f, cur, new = sys.argv[1], sys.argv[2], sys.argv[3]
p = pathlib.Path(f)
s = p.read_text()
# Replace the exact quoted command path. Unique → surgical.
p.write_text(s.replace('"' + cur + '"', '"' + new + '"'))
PY
}

# Reconcile one config file. Used by --reconcile and the default install path.
reconcile_one() {
  local label="$1" f="$2"
  if [[ ! -f "$f" ]]; then
    warn "$label: not found ($f) — skipping"
    return 0
  fi
  local cur; cur="$(config_command "$f")"
  if [[ "$cur" == "$CANONICAL" ]]; then
    ok "$label already launches the cabal symlink"
  elif [[ -z "$cur" ]]; then
    warn "$label has no haskell-flows server entry"
  else
    fix_config "$f" "$cur"
    ok "$label: '$cur' → '$CANONICAL'  (backup saved)"
  fi
}

reconcile_configs() {
  step "reconcile MCP client configs → $CANONICAL"
  reconcile_one "~/.claude.json (global, wins at launch)" "$CLAUDE_JSON"
  reconcile_one ".mcp.json (project)" "$PROJECT_MCP"
  # Heads-up if a session is live — Claude Code may rewrite ~/.claude.json on
  # quit with its in-memory copy, clobbering the edit. Re-run with Claude fully
  # quit if a restart still launches a stale binary.
  if pgrep -f 'haskell-flows-mcp' >/dev/null 2>&1; then
    warn "an haskell-flows-mcp process is running — a live Claude session may"
    warn "  overwrite ~/.claude.json on quit. If the restart still launches an"
    warn "  old binary, fully quit Claude and run: scripts/install-mcp.sh --reconcile"
  fi
}

binary_mtime() { stat -f %m "$1" 2>/dev/null || stat -c %Y "$1"; }
newest_src_mtime() {
  find mcp-server-haskell/src mcp-server-haskell/app -name '*.hs' -type f \
      -exec stat -f %m {} \; 2>/dev/null | sort -nr | head -1 \
  || find mcp-server-haskell/src mcp-server-haskell/app -name '*.hs' -type f \
      -exec stat -c %Y {} \; | sort -nr | head -1
}

case "${1:-}" in
  -h|--help)
    sed -n '1,33p' "$0"
    exit 0
    ;;
  --reconcile)
    reconcile_configs
    echo
    ok "configs reconciled. Restart Claude to launch the symlinked binary."
    exit 0
    ;;
  --check)
    rc=0
    # 1) binary freshness
    if [[ ! -x "$CANONICAL" ]]; then
      err "no binary at $CANONICAL — run scripts/install-mcp.sh"
      rc=1
    else
      bm=$(binary_mtime "$CANONICAL"); ns=$(newest_src_mtime)
      if [[ "$bm" -ge "$ns" ]]; then
        ok "binary up-to-date (mtime $bm ≥ newest src $ns)"
      else
        warn "binary is stale by $((ns - bm)) s — run scripts/install-mcp.sh"
        rc=1
      fi
    fi
    # 2) config alignment (the part that actually decides what Claude launches)
    for pair in "global:$CLAUDE_JSON" "project:$PROJECT_MCP"; do
      lbl="${pair%%:*}"; f="${pair#*:}"
      cur="$(config_command "$f")"
      if [[ "$cur" == "$CANONICAL" ]]; then
        ok "$lbl config launches the symlink"
      elif [[ -z "$cur" ]]; then
        warn "$lbl config: no haskell-flows entry"
      else
        err "$lbl config launches '$cur' (≠ symlink) — run scripts/install-mcp.sh --reconcile"
        rc=1
      fi
    done
    exit $rc
    ;;
  '')
    : # default path — rebuild, then reconcile
    ;;
  *)
    echo "unknown flag: $1"; exit 2
    ;;
esac

pushd mcp-server-haskell > /dev/null
step "cabal install exe:haskell-flows-mcp  (updates $CANONICAL)"
cabal install exe:haskell-flows-mcp --overwrite-policy=always
popd > /dev/null

ok "installed: $(stat -f '%Sm  %z bytes' "$CANONICAL" 2>/dev/null \
                 || stat -c '%y  %s bytes' "$CANONICAL")"

# Heads-up if the abandoned ~/.local/bin copy still exists — it's no longer
# launched (configs point at the symlink), but leaving it around invites the
# very drift this script exists to prevent.
if [[ -e "$HOME/.local/bin/haskell-flows-mcp" ]]; then
  warn "a stale copy still exists at ~/.local/bin/haskell-flows-mcp — safe to 'rm' it."
fi

reconcile_configs

cat <<NOTE

==> next step
Fresh binary in place and all configs point at the symlink. The running Claude
session still talks to the OLD subprocess — restart Claude (quit + relaunch) so
it spawns $CANONICAL. To confirm after relaunch:

  ghc_workflow(action="status")     # staleness.stale=false
  ghc_workflow(action="discover")   # must succeed (was "unknown action" on the old binary)
NOTE
