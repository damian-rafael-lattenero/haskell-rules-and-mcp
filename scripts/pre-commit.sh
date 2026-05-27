#!/usr/bin/env bash
# pre-commit.sh — fast pre-commit gate (~5 s).
#
# Runs hlint only — the #1 cause of CI reds and fast enough that it
# adds no meaningful latency to every commit.  The heavier cabal build
# and unit-test run live in pre-push.sh, which triggers once on push.
#
# Usage:
#   scripts/pre-commit.sh           # run the gate manually
#   scripts/pre-commit.sh --install # install as .git/hooks/pre-commit
#   scripts/pre-commit.sh --help
#
# As a git hook the script is invoked with no arguments; it exits 0 on
# success and non-zero to abort the commit.
#
set -euo pipefail

# Both ghcup AND cabal user bin dirs must be on PATH.
# macOS non-login shells (Dock/Finder launches) skip .zprofile.
export PATH="$HOME/.ghcup/bin:$HOME/.cabal/bin:$PATH"

# Resolve repo root regardless of where the script is invoked from
# (works whether run from scripts/ or as a .git/hooks/ hook).
REPO_ROOT="$(git rev-parse --show-toplevel)"
cd "$REPO_ROOT"

# -----------------------------------------------------------------------
# --install: write this script as .git/hooks/pre-commit and exit.
# -----------------------------------------------------------------------
if [[ "${1:-}" == "--install" ]]; then
  HOOK="$REPO_ROOT/.git/hooks/pre-commit"
  cp "$REPO_ROOT/scripts/pre-commit.sh" "$HOOK"
  chmod +x "$HOOK"
  echo "✓ Installed as $HOOK"
  echo "  The hook will run on every 'git commit' in this repo."
  exit 0
fi

if [[ "${1:-}" == "--help" || "${1:-}" == "-h" ]]; then
  grep '^#' "$0" | head -20 | sed 's/^# \?//'
  exit 0
fi

# -----------------------------------------------------------------------
# colours
# -----------------------------------------------------------------------
GREEN='\033[0;32m'; RED='\033[0;31m'; YELLOW='\033[1;33m'; NC='\033[0m'
ok()   { echo -e "${GREEN}✓${NC} $*"; }
fail() { echo -e "${RED}✗${NC} $*"; }
step() { echo -e "${YELLOW}▶${NC} $*"; }

# -----------------------------------------------------------------------
# hlint — recursive, same invocation CI uses
# -----------------------------------------------------------------------
step "hlint mcp-server-haskell/"
if hlint mcp-server-haskell/; then
  ok "hlint — no hints"
else
  fail "hlint found hints — fix them before committing."
  echo ""
  echo "Tip: re-run manually to see the full list:"
  echo "  PATH=\"\$HOME/.ghcup/bin:\$HOME/.cabal/bin:\$PATH\" hlint mcp-server-haskell/"
  exit 1
fi
