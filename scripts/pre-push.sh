#!/usr/bin/env bash
# pre-push.sh — minimal safety net before pushing to master.
#
# Catches the two most common CI failures in ~60-90 s:
#
#   [1] hlint        ~5 s   — #1 cause of CI reds (style hints = hard fail)
#   [2] build        ~30 s  — catches compile errors before they hit GHA
#   [3] unit tests   ~45 s  — all unit assertions in test/Spec.hs
#
# What it intentionally skips:
#   * E2E suite      (~200 s, matrix-sharded in CI, not a pre-push gate)
#   * haddock/sdist  (package-quality CI job, slow, low pre-push value)
#
# Usage:
#   scripts/pre-push.sh           # run the safety net
#   scripts/pre-push.sh --install # install as .git/hooks/pre-push
#   scripts/pre-push.sh --help
#
# As a git hook the script is invoked with no arguments; it exits 0 on
# success and non-zero to abort the push.
#
set -euo pipefail

# Both ghcup AND cabal user bin dirs must be on PATH.
# macOS non-login shells (Dock/Finder launches) skip .zprofile.
export PATH="$HOME/.ghcup/bin:$HOME/.cabal/bin:$PATH"

# Resolve repo root regardless of where the script is invoked from.
REPO_ROOT="$(cd "$(dirname "$0")/.." && pwd)"
cd "$REPO_ROOT"

# -----------------------------------------------------------------------
# --install: write this script as .git/hooks/pre-push and exit.
# -----------------------------------------------------------------------
if [[ "${1:-}" == "--install" ]]; then
  HOOK="$REPO_ROOT/.git/hooks/pre-push"
  cp "$REPO_ROOT/scripts/pre-push.sh" "$HOOK"
  chmod +x "$HOOK"
  echo "✓ Installed as $HOOK"
  echo "  The hook will run on every 'git push' in this repo."
  exit 0
fi

if [[ "${1:-}" == "--help" || "${1:-}" == "-h" ]]; then
  grep '^#' "$0" | head -25 | sed 's/^# \?//'
  exit 0
fi

# -----------------------------------------------------------------------
# helpers
# -----------------------------------------------------------------------
GREEN='\033[0;32m'; RED='\033[0;31m'; YELLOW='\033[1;33m'; NC='\033[0m'
ok()   { echo -e "${GREEN}✓${NC} $*"; }
fail() { echo -e "${RED}✗${NC} $*"; }
step() { echo -e "${YELLOW}▶${NC} $*"; }

FAILED=()

run_step() {
  local label="$1"; shift
  step "$label"
  if "$@"; then
    ok "$label"
  else
    fail "$label"
    FAILED+=("$label")
  fi
}

# -----------------------------------------------------------------------
# [1] hlint — recursive, same path CI uses
# -----------------------------------------------------------------------
run_step "hlint mcp-server-haskell/" \
  bash -c 'hlint mcp-server-haskell/ && echo "No hints"'

# -----------------------------------------------------------------------
# [2] build — compiles library + all test suites (deps reused from store)
# -----------------------------------------------------------------------
run_step "cabal build all" \
  bash -c 'cd mcp-server-haskell && cabal build all --disable-documentation -j 2>&1'

# -----------------------------------------------------------------------
# [3] unit tests — haskell-flows-mcp-test (Spec.hs)
#     --test-show-details=direct so failures are visible inline
# -----------------------------------------------------------------------
run_step "unit tests (haskell-flows-mcp-test)" \
  bash -c 'cd mcp-server-haskell && cabal test haskell-flows-mcp-test \
    --test-show-details=direct \
    --disable-documentation 2>&1'

# -----------------------------------------------------------------------
# summary
# -----------------------------------------------------------------------
echo ""
if [[ ${#FAILED[@]} -eq 0 ]]; then
  ok "All checks passed — safe to push."
  exit 0
else
  fail "Failed checks (${#FAILED[@]}):"
  for f in "${FAILED[@]}"; do
    echo "  • $f"
  done
  echo ""
  echo "Fix the issues above before pushing."
  exit 1
fi
