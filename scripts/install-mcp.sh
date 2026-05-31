#!/usr/bin/env bash
# install-mcp.sh — rebuild the haskell-flows-mcp binary the active MCP
# config actually launches, then remind you to relaunch the client.
#
# The active config (.mcp.json at the repo root) sets
#   "command": "~/.cabal/bin/haskell-flows-mcp"
# i.e. the symlink that `cabal install exe:haskell-flows-mcp` updates
# atomically on every build. So a single `cabal install` is all that's
# needed — this script just wraps it with the PATH dance, a staleness
# check, and a restart reminder.
#
# History (why this script shrank): it used to also `cp` the binary to
# ~/.local/bin and `strip` that copy. The config was since moved to
# ~/.cabal/bin precisely because a stale ~/.local/bin copy shadowed the
# fresh build and made the Claude API reject the old schema (see the
# .mcp.json comment). The cp+strip steps then operated on a path nobody
# launches — so they were removed. (~/.cabal/bin is a symlink into the
# immutable cabal store, so it must NOT be stripped in place anyway.)
#
# Usage:
#   scripts/install-mcp.sh          # rebuild + install (the only path the config uses)
#   scripts/install-mcp.sh --check  # print whether the installed binary is older than src
#   scripts/install-mcp.sh -h       # this help block
set -euo pipefail

# Same PATH dance as ci-local.sh — non-login shells on macOS don't source
# .zprofile, so cabal/ghc need to be made discoverable here.
export PATH="$HOME/.ghcup/bin:$HOME/.cabal/bin:$PATH"

cd "$(dirname "$0")/.."

# The path the active .mcp.json launches (cabal-install-managed symlink).
CABAL_BIN="$HOME/.cabal/bin/haskell-flows-mcp"

step() { printf '\n\033[1;34m==> %s\033[0m\n' "$*"; }
ok()   { printf '\033[1;32m✓\033[0m %s\n' "$*"; }
warn() { printf '\033[1;33m!\033[0m %s\n' "$*"; }

case "${1:-}" in
  -h|--help)
    sed -n '1,24p' "$0"
    exit 0
    ;;
  --check)
    if [[ ! -x "$CABAL_BIN" ]]; then
      warn "no binary at $CABAL_BIN — run scripts/install-mcp.sh"
      exit 1
    fi
    binary_mtime=$(stat -f %m "$CABAL_BIN" 2>/dev/null || stat -c %Y "$CABAL_BIN")
    newest_src=$(find mcp-server-haskell/src mcp-server-haskell/app -name '*.hs' -type f \
                   -exec stat -f %m {} \; 2>/dev/null \
                 | sort -nr | head -1)
    if [[ -z "$newest_src" ]]; then
      newest_src=$(find mcp-server-haskell/src mcp-server-haskell/app -name '*.hs' -type f \
                     -exec stat -c %Y {} \; \
                   | sort -nr | head -1)
    fi
    if [[ "$binary_mtime" -ge "$newest_src" ]]; then
      ok "binary up-to-date (mtime $binary_mtime ≥ newest src $newest_src)"
      exit 0
    else
      delta=$((newest_src - binary_mtime))
      warn "binary is stale by $delta s — run scripts/install-mcp.sh"
      exit 1
    fi
    ;;
  '')
    : # default path — fall through to install
    ;;
  *)
    echo "unknown flag: $1"; exit 2
    ;;
esac

pushd mcp-server-haskell > /dev/null
step "cabal install exe:haskell-flows-mcp  (updates $CABAL_BIN)"
cabal install exe:haskell-flows-mcp --overwrite-policy=always
popd > /dev/null

ok "installed: $(stat -f '%Sm  %z bytes' "$CABAL_BIN" 2>/dev/null \
                 || stat -c '%y  %s bytes' "$CABAL_BIN")"

# Heads-up if the abandoned ~/.local/bin copy still exists — it's no
# longer launched, but leaving a stale one around invites confusion.
if [[ -e "$HOME/.local/bin/haskell-flows-mcp" ]]; then
  warn "a stale copy still exists at ~/.local/bin/haskell-flows-mcp — unused by the current .mcp.json; safe to 'rm' it."
fi

cat <<'NOTE'

==> next step
The fresh binary is in place, but the running Claude session is still
talking to the OLD subprocess. Restart Claude (quit + relaunch) so it
spawns the new haskell-flows-mcp. To confirm after relaunch, run:

  ghc_workflow(action="status")
NOTE
