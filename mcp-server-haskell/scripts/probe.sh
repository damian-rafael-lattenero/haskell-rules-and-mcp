#!/usr/bin/env bash
# probe.sh — run tests against a binary you are SURE is fresh.
#
# The stale-binary traps this closes:
#   * cabal reports "Up to date" while the binary is old (filtered
#     pipes, interrupted builds, golden regen from a stale binary)
#   * cabal's build cache does not notice externally-deleted
#     artifacts ("Up to date" with the binary MISSING)
#
# How:
#   1. cabal build (relinks anything whose sources changed)
#   2. FRESHNESS LAW: every binary's mtime must be >= the newest
#      mtime under src/ test/ test-e2e/ + *.cabal + cabal.project.
#      A violation means cabal's cache is lying — fail loudly with
#      the fix, never test the old binary.
#   3. print mtime + sha256 of what will run
#   4. run under hangwatch.py → verdict DONE / EXIT-n / HANG, never
#      a silent timeout
#
# Usage:
#   scripts/probe.sh                 # full e2e (every scenario)
#   scripts/probe.sh "Parse honesty" # only scenarios whose TITLE
#                                    # contains this substring
#   scripts/probe.sh unit            # unit suite only
#   scripts/probe.sh golden          # regen tools-list golden from
#                                    # the JUST-BUILT binary
#
# Exit 0 iff fresh + the run passed.
set -euo pipefail
cd "$(dirname "$0")/.."

WHAT="${1:-}"

say() { printf '\n══ %s ══\n' "$1"; }

# ── 1 · build ──
say "T1 · build"
if ! cabal build exe:haskell-flows-mcp test:haskell-flows-mcp-test \
                 test:haskell-flows-mcp-e2e > /tmp/probe-build.log 2>&1; then
  grep -E " error|Error" -A4 /tmp/probe-build.log | head -40
  echo "PROBE FAIL (build) — full log: /tmp/probe-build.log"
  exit 1
fi

# ── 2+3 · freshness law + proof ──
# Each binary is checked against the sources IT compiles: every target
# links the library (src/), and each suite adds its own directory.
newest_source_epoch() { # DIRS...
  local e max=0 f
  for f in "$@"; do :; done   # noop — iterate below via find
  while IFS= read -r f; do
    e=$(stat -f '%m' "$f")
    [ "$e" -gt "$max" ] && max=$e
  done < <(find "$@" -type f -name '*.hs' 2>/dev/null; \
           ls haskell-flows-mcp.cabal cabal.project 2>/dev/null)
  echo "$max"
}

fresh_check() { # BIN LABEL DIR...
  local bin="$1" label="$2"; shift 2
  local bin_epoch src_epoch
  if [ ! -x "$bin" ]; then
    echo "FRESH FAIL ($label): binary MISSING at $bin"
    echo "  cabal's cache thinks it is built — force a real rebuild:"
    echo "    rm -rf dist-newstyle/build/*/ghc-*/haskell-flows-mcp-*"
    return 1
  fi
  bin_epoch=$(stat -f '%m' "$bin")
  src_epoch=$(newest_source_epoch "$@")
  if [ "$bin_epoch" -lt "$src_epoch" ]; then
    echo "FRESH FAIL ($label): binary is OLDER than the newest source it compiles"
    echo "  binary:        $(date -r "$bin_epoch")"
    echo "  newest source: $(date -r "$src_epoch")"
    echo "  cabal's cache is stale — force a real rebuild:"
    echo "    rm -rf dist-newstyle/build/*/ghc-*/haskell-flows-mcp-*"
    return 1
  fi
  echo "fresh OK · $label · $(shasum -a 256 "$bin" | cut -c1-16) · $(stat -f '%Sm' "$bin")"
}

say "T2 · freshness proof (law: binary mtime >= newest source it compiles)"
BIN_MAIN=$(cabal list-bin haskell-flows-mcp)
BIN_E2E=$(cabal list-bin haskell-flows-mcp-e2e)
BIN_UT=$(cabal list-bin haskell-flows-mcp-test)
fresh_check "$BIN_MAIN" "server" src app
fresh_check "$BIN_E2E"   "e2e"    src test-e2e
fresh_check "$BIN_UT"    "unit"   src test

# ── 4 · run ──
run_under_watch() { # LABEL BIN [ENV=VAL...]
  local label="$1" bin="$2"; shift 2
  python3 scripts/hangwatch.py "$label" "$bin" "$@" STALL_S=180 TOTAL_S=3600
}

case "$WHAT" in
  unit)
    say "T3 · unit suite (fresh binary)"
    run_under_watch probe-unit "$BIN_UT"
    ;;
  golden)
    say "T3 · regen tools-list golden from the JUST-BUILT binary"
    HASKELL_FLOWS_REGEN_GOLDEN=1 "$BIN_UT" > /tmp/probe-golden.log 2>&1
    git diff --stat test/golden/tools-list.json || true
    echo "golden regenerated — review the diff before committing."
    ;;
  "")
    say "T3 · full e2e (every scenario, hangwatch-guarded)"
    run_under_watch probe-e2e-full "$BIN_E2E"
    ;;
  *)
    say "T3 · e2e filtered by title ≈ '$WHAT' (hangwatch-guarded)"
    run_under_watch probe-e2e "$BIN_E2E" "HASKELL_FLOWS_E2E_ONLY=$WHAT"
    ;;
esac

say "PROBE OK"
