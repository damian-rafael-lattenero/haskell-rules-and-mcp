#!/usr/bin/env bash
# ci-local.sh — local mirror of `.github/workflows/haskell-ci.yml`.
#
# Goal: catch every red the GHA workflow would catch, before pushing,
# in the smallest possible wall-clock. Layout matches the post-perf-batch
# CI topology:
#
#   * hlint runs FIRST (gates everything, fast-fail in 30 s on style breaks)
#   * build deps + project in one `cabal build all --keep-going`
#   * test with `--keep-going` so multiple regressions surface in one run
#   * package-quality (haddock + cabal check + sdist) only with --full
#
# Default mode = the old `--fast`: hlint + build + tests, no
# haddock/sdist. The 80% inner-loop case. Use --full for everything.
#
# Modes (mutually exclusive flags ranked by speed):
#
#   --unit                  hlint + build + UNIT tests only        (~45 s)
#   --scenario=<substring>  hlint + build + one E2E scenario       (~60-90 s)
#   (default)               hlint + build + UNIT + E2E             (~3 min)
#   --full                  + haddock + cabal check + sdist        (~5 min)
#
# Cross-cutting flags:
#
#   --parallel              run hlint concurrently with `cabal build`
#                            shaves ~30 s off the critical path on
#                            multi-core machines (default: serial,
#                            safer on RAM-constrained boxes)
#   --no-hlint              skip the hlint step (debugging only)
#   --keep-going            propagate to cabal build + test (default ON;
#                            disable with --strict for fail-fast)
#   --strict                disable --keep-going (fail-fast on first red)
#
# Examples:
#
#   scripts/ci-local.sh                           # fast inner loop
#   scripts/ci-local.sh --unit                    # 45 s post-edit check
#   scripts/ci-local.sh --scenario=Arbitrary      # one e2e scenario
#   scripts/ci-local.sh --full                    # full pre-push gate
#   scripts/ci-local.sh --parallel --full         # full + parallel hlint
#
# Backward compat:
#
#   --fast    alias for default (was meaningful pre-rewrite; kept so
#             muscle-memory invocations don't break)
#
set -euo pipefail

# Put both ghcup AND cabal user bin dirs on PATH so the script works
# from any shell. Without this, `hlint` from `cabal install hlint`
# falls off PATH and the lint step silently no-ops.
export PATH="$HOME/.ghcup/bin:$HOME/.cabal/bin:$PATH"

cd "$(dirname "$0")/.."

# -----------------------------------------------------------------------
# arg parsing
# -----------------------------------------------------------------------
MODE="default"     # default | unit | scenario | full
SCENARIO=""
PARALLEL=false
RUN_HLINT=true
KEEP_GOING=true

for arg in "$@"; do
  case "$arg" in
    --fast)         ;;  # alias for default; kept for backward compat
    --unit)         MODE="unit" ;;
    --scenario=*)   MODE="scenario"; SCENARIO="${arg#--scenario=}" ;;
    --full)         MODE="full" ;;
    --parallel)     PARALLEL=true ;;
    --no-hlint)     RUN_HLINT=false ;;
    --keep-going)   KEEP_GOING=true ;;
    --strict)       KEEP_GOING=false ;;
    -h|--help)      sed -n '1,55p' "$0"; exit 0 ;;
    *) echo "unknown flag: $arg"; echo "  use -h for help"; exit 2 ;;
  esac
done

# -----------------------------------------------------------------------
# UI helpers + timing
# -----------------------------------------------------------------------
declare -a TIMINGS  # "label:seconds" entries for the closing summary

step()  { printf '\n\033[1;34m==> [%s] %s\033[0m\n' "$1" "$2"; }
say()   { printf '\033[1;36m   %s\033[0m\n' "$*"; }
ok()    { printf '\033[1;32m   ✓ %s\033[0m\n' "$*"; }
warn()  { printf '\033[1;33m   ! %s\033[0m\n' "$*"; }
fail()  { printf '\033[1;31m   ✗ %s\033[0m\n' "$*"; }

# Run a labeled step, capture wall-clock, append to TIMINGS.
timed() {
  local label="$1"; shift
  local t0; t0=$(date +%s)
  if "$@"; then
    local dt=$(($(date +%s) - t0))
    TIMINGS+=("$label:$dt")
    ok "$label finished in ${dt}s"
  else
    local rc=$?
    fail "$label failed (rc=$rc)"
    return $rc
  fi
}

# Cabal flags that change with --strict.
cabal_keep_going_flag() {
  if [ "$KEEP_GOING" = true ]; then echo "--keep-going"; else echo ""; fi
}

# -----------------------------------------------------------------------
# 1. hlint — fast-fail gate
# -----------------------------------------------------------------------
hlint_step() {
  if [ "$RUN_HLINT" != true ]; then
    say "skipping hlint (--no-hlint)"
    return 0
  fi
  if ! command -v hlint > /dev/null; then
    fail "hlint not on PATH (install via 'cabal install hlint' or 'ghcup install hlint')"
    return 1
  fi
  hlint mcp-server-haskell/
}

# Used by --parallel: run hlint in background, capture rc, defer error.
HLINT_BG_PID=""
HLINT_BG_LOG=""

hlint_step_bg_start() {
  if [ "$RUN_HLINT" != true ]; then return 0; fi
  HLINT_BG_LOG=$(mktemp)
  ( hlint_step ) > "$HLINT_BG_LOG" 2>&1 &
  HLINT_BG_PID=$!
  say "hlint launched in background (pid=$HLINT_BG_PID)"
}

hlint_step_bg_wait() {
  if [ -z "$HLINT_BG_PID" ]; then return 0; fi
  local t0=$(date +%s)
  if wait "$HLINT_BG_PID"; then
    local dt=$(($(date +%s) - t0))
    TIMINGS+=("hlint(parallel):$dt")
    ok "hlint (parallel) finished — clean"
  else
    fail "hlint (parallel) failed:"
    cat "$HLINT_BG_LOG" >&2
    rm -f "$HLINT_BG_LOG"
    return 1
  fi
  rm -f "$HLINT_BG_LOG"
}

# -----------------------------------------------------------------------
# 2. cabal — configure + freeze + build  (combined; matches new CI)
# -----------------------------------------------------------------------
build_step() {
  pushd mcp-server-haskell > /dev/null

  cabal configure --enable-tests --enable-benchmarks --disable-documentation
  cabal freeze
  cabal build all $(cabal_keep_going_flag)

  popd > /dev/null
}

# -----------------------------------------------------------------------
# 3. fixture pre-warm — populates ~/.cabal/store/ with the E2E closure
# -----------------------------------------------------------------------
fixture_warm_step() {
  pushd mcp-server-haskell/test-e2e/Fixtures/Baseline > /dev/null
  cabal build
  popd > /dev/null
}

# -----------------------------------------------------------------------
# 4. tests — unit / scenario / full e2e
# -----------------------------------------------------------------------
unit_test_step() {
  pushd mcp-server-haskell > /dev/null
  cabal test haskell-flows-mcp-test \
    --test-show-details=direct $(cabal_keep_going_flag)
  popd > /dev/null
}

# Scenario flag passes HASKELL_FLOWS_E2E_ONLY to the e2e binary so
# only matching scenarios run.
scenario_test_step() {
  local sub="$1"
  pushd mcp-server-haskell > /dev/null
  HASKELL_FLOWS_E2E_ONLY="$sub" \
    cabal test haskell-flows-mcp-e2e \
      --test-show-details=direct $(cabal_keep_going_flag)
  popd > /dev/null
}

full_test_step() {
  pushd mcp-server-haskell > /dev/null
  : "${HASKELL_FLOWS_E2E_PARALLEL:=4}"
  export HASKELL_FLOWS_E2E_PARALLEL
  say "(HASKELL_FLOWS_E2E_PARALLEL=$HASKELL_FLOWS_E2E_PARALLEL)"
  cabal test all --test-show-details=direct $(cabal_keep_going_flag)
  popd > /dev/null
}

# -----------------------------------------------------------------------
# 5. package-quality — haddock + check + sdist (only --full)
# -----------------------------------------------------------------------
package_quality_step() {
  pushd mcp-server-haskell > /dev/null
  cabal haddock all --haddock-all
  cabal check
  cabal sdist all --output-dir /tmp/haskell-flows-mcp-sdist
  popd > /dev/null
}

# -----------------------------------------------------------------------
# orchestrate
# -----------------------------------------------------------------------
TOTAL_T0=$(date +%s)

step "0/0" "ci-local.sh — mode=$MODE  parallel=$PARALLEL  keep-going=$KEEP_GOING"
say "PATH includes: \$HOME/.ghcup/bin, \$HOME/.cabal/bin"
say "(use --help for full flag list)"

# PR-5: rules-freshness gate runs FIRST. It's a fast (<1s) grep over a
# single markdown file — failing here is a markdown drift bug, not a
# Haskell regression, so we want to surface it before paying the build
# cost. Calls scripts/check-rules-freshness.sh which is also safe to
# invoke standalone.
step "0/N" "rules-freshness check (.claude/rules/use-haskell-flows-mcp.md)"
timed "rules-freshness" bash scripts/check-rules-freshness.sh

if [ "$PARALLEL" = true ] && [ "$RUN_HLINT" = true ]; then
  step "1/N" "hlint + cabal build (parallel)"
  hlint_step_bg_start
  timed "build" build_step
  hlint_step_bg_wait
else
  if [ "$RUN_HLINT" = true ]; then
    step "1/N" "hlint (recursive, matches CI's gate)"
    timed "hlint" hlint_step
  fi
  step "2/N" "cabal build (deps + project)"
  timed "build" build_step
fi

case "$MODE" in
  unit)
    step "3/N" "unit tests only"
    timed "test:unit" unit_test_step
    ;;

  scenario)
    step "3/N" "fixture pre-warm"
    timed "fixture-warm" fixture_warm_step
    step "4/N" "unit tests"
    timed "test:unit" unit_test_step
    step "5/N" "e2e scenario filter: $SCENARIO"
    timed "test:e2e($SCENARIO)" scenario_test_step "$SCENARIO"
    ;;

  default|full)
    step "3/N" "fixture pre-warm"
    timed "fixture-warm" fixture_warm_step
    step "4/N" "unit + e2e tests (parallel within shard)"
    timed "test:all" full_test_step
    ;;
esac

if [ "$MODE" = "full" ]; then
  step "5/N" "package-quality (haddock + check + sdist)"
  timed "package-quality" package_quality_step
fi

# -----------------------------------------------------------------------
# closing summary
# -----------------------------------------------------------------------
TOTAL_DT=$(($(date +%s) - TOTAL_T0))

printf '\n\033[1;32m=================================================\033[0m\n'
printf '\033[1;32m  All ci-local gates green (mode=%s)\033[0m\n' "$MODE"
printf '\033[1;32m=================================================\033[0m\n\n'
printf '  Per-step timing:\n'
for t in "${TIMINGS[@]}"; do
  printf '    %-25s %4ds\n' "${t%%:*}" "${t##*:}"
done
printf '    %-25s %4ds\n' "TOTAL" "$TOTAL_DT"
printf '\n'
