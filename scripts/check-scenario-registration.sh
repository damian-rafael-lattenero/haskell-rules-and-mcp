#!/usr/bin/env bash
# check-scenario-registration.sh — guard against unregistered E2E scenarios.
#
# THE DRIFT THIS CATCHES: docs/testing.md §"Adding a scenario" requires
# THREE manual edits for every new Scenarios/FlowX.hs — (1) the cabal
# other-modules, (2) an `import qualified Scenarios.FlowX as FX` in
# Main.hs, and (3) appending `FX.runFlow` to the scenarios list. Miss
# step 3 and the scenario file still COMPILES (it's in cabal) but never
# runs — the suite is green while a whole scenario is silently skipped.
# A passing build cannot catch that; this script does.
#
# For each test-e2e/Scenarios/Flow*.hs it asserts:
#   A. the module is in the cabal e2e other-modules, and
#   B. it is imported in Main.hs (gives the alias) AND that alias's
#      `.runFlow` is referenced (i.e. it's actually in the scenarios list).
#
# Exits 0 clean, non-zero on drift. Run from scripts/ci-local.sh and the
# CI `invariants` job.

set -euo pipefail

SCRIPT_DIR="$(cd "$(dirname "$0")" && pwd)"
REPO_ROOT="$(dirname "$SCRIPT_DIR")"
E2E="$REPO_ROOT/mcp-server-haskell/test-e2e"
MAIN="$E2E/Main.hs"
CABAL="$REPO_ROOT/mcp-server-haskell/haskell-flows-mcp.cabal"

red() { printf '\033[1;31m%s\033[0m\n' "$*"; }
ok()  { printf '\033[1;32m%s\033[0m\n' "$*"; }

for f in "$MAIN" "$CABAL"; do
  if [ ! -f "$f" ]; then
    red "ERROR: expected file not found: $f"
    exit 2
  fi
done

fail_count=0

for path in "$E2E"/Scenarios/Flow*.hs; do
  [ -e "$path" ] || continue
  base="$(basename "$path" .hs)"          # e.g. FlowFormat
  mod="Scenarios.$base"

  # Only scenarios export runFlow; a Flow* helper without it is skipped.
  if ! grep -qE '(^| )runFlow' "$path"; then
    continue
  fi

  # A. registered in the cabal e2e other-modules.
  if ! grep -qE "(^|[[:space:]])$mod([[:space:]]|$)" "$CABAL"; then
    red "DRIFT: $mod is not listed in the e2e other-modules in haskell-flows-mcp.cabal"
    echo "  → add '$mod' under the haskell-flows-mcp-e2e test-suite other-modules."
    fail_count=$((fail_count + 1))
  fi

  # B. imported in Main.hs (capture the alias) …
  alias="$(grep -E "^import qualified $mod[[:space:]]+as[[:space:]]+" "$MAIN" \
             | sed -E 's/.*as[[:space:]]+([A-Za-z0-9_]+).*/\1/' | head -1)"
  if [ -z "$alias" ]; then
    red "DRIFT: $mod is not imported in test-e2e/Main.hs"
    echo "  → add: import qualified $mod as <Alias>"
    fail_count=$((fail_count + 1))
    continue
  fi

  # … and its alias.runFlow appears in the scenarios list.
  if ! grep -qE "\b$alias\.runFlow\b" "$MAIN"; then
    red "DRIFT: $mod is imported (as $alias) but never added to the scenarios list"
    echo "  → append a ( \"Flow: …\", <isSlow>, $alias.runFlow ) tuple to 'scenarios'."
    fail_count=$((fail_count + 1))
  fi
done

if [ "$fail_count" -gt 0 ]; then
  red "scenario-registration check FAILED ($fail_count issue(s))."
  exit 1
fi

ok "scenario-registration check passed."
