#!/usr/bin/env bash
# ci-local.sh — replicate the Haskell CI workflow locally, in order, so a
# push doesn't surprise us with a red check for something we could have
# caught at author time.
#
# Why this exists: `ghc_lint` (the MCP tool) runs hlint per-module and
# it's easy to lint only the src/ files you just touched while forgetting
# that CI runs `hlint mcp-server-haskell/` recursively — including test/.
# Happened twice: `parseX ... == Nothing` slipped into the test suite
# both times, caught by CI, rejected by hlint, merged after a second
# push. This script closes that loop: one command, full coverage.
#
# Steps (matching .github/workflows/haskell-ci.yml):
#
#   1. cabal configure --enable-tests --enable-benchmarks --disable-documentation
#   2. cabal freeze
#   3. cabal build all --only-dependencies
#   4. cabal build all
#   5. cabal test all --test-show-details=direct
#   6. cabal haddock all --haddock-all
#   7. cabal check
#   8. cabal sdist all
#   9. hlint mcp-server-haskell/      (same recursive path CI uses)
#
# Run from the repo root. Use `scripts/ci-local.sh --fast` to skip the
# slow stages (haddock + sdist) when you just want the compile+lint
# gate for a quick inner loop.
set -euo pipefail

# Put both the ghcup binary dir AND cabal's user bin dir on PATH so
# the script works from any shell (non-login shells on macOS don't
# source .zprofile). 'cabal install hlint' / 'cabal install fourmolu'
# land binaries under '~/.cabal/bin', not under ghcup, so without
# this the recursive hlint step at [9/9] silently no-ops with
# "No hlint on PATH" — exactly the gap that let a redundant-bracket
# warning slip through to GitHub CI in PR #110.
export PATH="$HOME/.ghcup/bin:$HOME/.cabal/bin:$PATH"

cd "$(dirname "$0")/.."

FAST=false
for arg in "$@"; do
  case "$arg" in
    --fast) FAST=true ;;
    -h|--help)
      sed -n '1,30p' "$0"; exit 0 ;;
    *) echo "unknown flag: $arg"; exit 2 ;;
  esac
done

step() { printf '\n\033[1;34m==> %s\033[0m\n' "$*"; }

pushd mcp-server-haskell > /dev/null

step "[1/9] cabal configure"
cabal configure --enable-tests --enable-benchmarks --disable-documentation

step "[2/9] cabal freeze"
cabal freeze

step "[3/9] cabal build --only-dependencies"
cabal build all --only-dependencies

step "[4/9] cabal build"
cabal build all

step "[5/9] cabal test"
# E2E parallelism opt-in: post-#43 the session layer is concurrent-safe,
# but several scenarios (FlowBootstrap, FlowConcurrentClients, …) still
# contain state-isolation bugs that surface under HASKELL_FLOWS_E2E_PARALLEL>=2.
# Sequential by default keeps CI stable; opt in for the dev inner loop:
#   HASKELL_FLOWS_E2E_PARALLEL=4 scripts/ci-local.sh --fast
# Combine with HASKELL_FLOWS_E2E_SKIP_SLOW=1 for the fastest run.
: "${HASKELL_FLOWS_E2E_PARALLEL:=1}"
export HASKELL_FLOWS_E2E_PARALLEL
printf '   (HASKELL_FLOWS_E2E_PARALLEL=%s)\n' "$HASKELL_FLOWS_E2E_PARALLEL"
cabal test all --test-show-details=direct

if [ "$FAST" = false ]; then
  step "[6/9] cabal haddock"
  cabal haddock all --haddock-all

  step "[7/9] cabal check"
  cabal check

  step "[8/9] cabal sdist"
  cabal sdist all --output-dir /tmp/haskell-flows-mcp-sdist
else
  printf '\n\033[1;33mSkipping haddock + check + sdist (--fast mode)\033[0m\n'
fi

popd > /dev/null

step "[9/9] hlint (recursive, matches CI)"
if command -v hlint > /dev/null; then
  hlint mcp-server-haskell/
else
  echo "No hlint on PATH — install it via 'cabal install hlint' or 'ghcup install hlint'." >&2
  exit 1
fi

printf '\n\033[1;32mAll CI-local gates green\033[0m\n'
