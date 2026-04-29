#!/usr/bin/env bash
# install-mcp.sh — rebuild the haskell-flows-mcp binary and put it where
# the MCP client (Claude Code) expects to find it.
#
# Why this exists: `cabal install exe:haskell-flows-mcp` lands the
# binary in `~/.cabal/bin`, but `.mcp.json` at the repo root points
# at `~/.local/bin/haskell-flows-mcp` (a copy, not a hardlink — the
# original setup put it there to keep cabal's bin dir reorderable).
# Running cabal install alone leaves the running MCP unchanged
# because Claude is still launching the OLD copy. This script closes
# that gap with one command.
#
# Steps:
#   1. cabal install exe:haskell-flows-mcp --overwrite-policy=always
#   2. cp  ~/.cabal/bin/haskell-flows-mcp  ~/.local/bin/haskell-flows-mcp
#   3. print a reminder to restart Claude Code so the next invocation
#      picks up the fresh build.
#
# Usage:
#   scripts/install-mcp.sh            # rebuild + install
#   scripts/install-mcp.sh --check    # just print whether the on-disk
#                                     # binary is older than the source
#   scripts/install-mcp.sh -h         # this help block
set -euo pipefail

# Same PATH dance as ci-local.sh — non-login shells on macOS don't
# source .zprofile, so cabal/ghc/hlint/fourmolu need to be made
# discoverable here.
export PATH="$HOME/.ghcup/bin:$HOME/.cabal/bin:$PATH"

cd "$(dirname "$0")/.."

CABAL_BIN="$HOME/.cabal/bin/haskell-flows-mcp"
LOCAL_BIN="$HOME/.local/bin/haskell-flows-mcp"

step() { printf '\n\033[1;34m==> %s\033[0m\n' "$*"; }
ok()   { printf '\033[1;32m✓\033[0m %s\n' "$*"; }
warn() { printf '\033[1;33m!\033[0m %s\n' "$*"; }

case "${1:-}" in
  -h|--help)
    sed -n '1,28p' "$0"
    exit 0
    ;;
  --check)
    if [[ ! -x "$LOCAL_BIN" ]]; then
      warn "no binary at $LOCAL_BIN — run scripts/install-mcp.sh"
      exit 1
    fi
    binary_mtime=$(stat -f %m "$LOCAL_BIN" 2>/dev/null || stat -c %Y "$LOCAL_BIN")
    newest_src=$(find mcp-server-haskell/src mcp-server-haskell/app -name '*.hs' -type f \
                   -exec stat -f %m {} \; 2>/dev/null \
                 | sort -nr | head -1)
    if [[ -z "$newest_src" ]]; then
      newest_src=$(find mcp-server-haskell/src mcp-server-haskell/app -name '*.hs' -type f \
                     -exec stat -c %Y {} \; \
                   | sort -nr | head -1)
    fi
    if [[ "$binary_mtime" -ge "$newest_src" ]]; then
      ok "binary is up-to-date (mtime $binary_mtime ≥ newest src $newest_src)"
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

step "[1/2] cabal install exe:haskell-flows-mcp"
cabal install exe:haskell-flows-mcp --overwrite-policy=always

popd > /dev/null

step "[2/2] copy → $LOCAL_BIN"
mkdir -p "$(dirname "$LOCAL_BIN")"
cp "$CABAL_BIN" "$LOCAL_BIN"

ok "installed: $(stat -f '%Sm  %z bytes' "$LOCAL_BIN" 2>/dev/null \
                 || stat -c '%y  %s bytes' "$LOCAL_BIN")"

cat <<'NOTE'

==> next step
The fresh binary is in place but the running Claude Code session is
still talking to the OLD subprocess. Restart Claude Code (quit and
relaunch) so it spawns the new haskell-flows-mcp. To confirm after
relaunch, run inside Claude:

  ghc_workflow(action="status")   # 'staleness.stale' should be false
NOTE
