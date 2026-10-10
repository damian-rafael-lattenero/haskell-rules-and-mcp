#!/usr/bin/env bash
# hm-local-check — fast local loop for the HM dogfood sources, no MCP.
#
# Extracts the inline module sources from hm-loop.py (the single source
# of truth: write_module ships exactly these bytes), compiles them with
# the EXACT flags of the scaffold's `common shared` stanza (see
# CreateProject.hs), and runs the Spec properties (300 tests each via
# withMaxSuccess). Warnings fail the check — the MCP project gate fails
# on warnings, so this loop must too, or we "pass" locally and fail the
# dogfood.
#
# Usage: scripts/dogfood/hm-local-check.sh [outdir]   (default /tmp/hm-local-check)
set -euo pipefail

HERE="$(cd "$(dirname "$0")" && pwd)"
OUT="${1:-/tmp/hm-local-check}"

# mirror of the scaffold's `common shared` stanza
SCAFFOLD_FLAGS=(
  -XGHC2024 -XOverloadedStrings -XDerivingStrategies -XLambdaCase
  -Wall -Wcompat -Widentities -Wincomplete-record-updates
  -Wincomplete-uni-patterns -Wpartial-fields -Wredundant-constraints
  -Wunused-packages
)

echo "── extract sources from hm-loop.py"
rm -rf "$OUT"; mkdir -p "$OUT"
python3 - "$HERE/hm-loop.py" "$OUT" <<'PYEOF'
import importlib.util, os, sys
spec = importlib.util.spec_from_file_location("hmloop", sys.argv[1])
m = importlib.util.module_from_spec(spec)
spec.loader.exec_module(m)          # driver code is under __main__
for rel, _mod, src in m.MODULES:
    p = os.path.join(sys.argv[2], rel)
    os.makedirs(os.path.dirname(p), exist_ok=True)
    with open(p, "w") as f:
        f.write(src)
print(f"   {len(m.MODULES)} modules extracted")
PYEOF

echo "── compile with scaffold flags (any warning fails)"
PKGDB=$(echo ~/.cabal/store/ghc-"$(ghc --numeric-version)"*/package.db)
if [ ! -d "$PKGDB" ]; then
  echo "no cabal store package db for ghc $(ghc --numeric-version) — QuickCheck unavailable"
  exit 1
fi
ghc --make "${SCAFFOLD_FLAGS[@]}" \
    -i"$OUT/src" -package-db "$PKGDB" -package QuickCheck \
    -o "$OUT/spec-run" -outputdir "$OUT/build" \
    "$OUT/test/Spec.hs" \
    > "$OUT/compile.log" 2>&1 || { cat "$OUT/compile.log"; exit 1; }
if grep -qi "warning" "$OUT/compile.log"; then
  echo "WARNINGS (the MCP project gate fails these too):"
  grep -i -B1 -A3 "warning" "$OUT/compile.log" | head -40
  exit 1
fi
echo "   0 warnings"

echo "── run properties (3 × 300 tests)"
"$OUT/spec-run"

echo "hm-local-check: GREEN"
