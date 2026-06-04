#!/bin/sh
# haskell-flows ghc shim — intercepts --interactive only.
# Locate real ghc. 'command -v' searches PATH; 'which' fallback.
REAL_GHC="$(command -v ghc 2>/dev/null)"
if [ -z "$REAL_GHC" ]; then
  REAL_GHC="$(which ghc 2>/dev/null)"
fi
if [ -z "$REAL_GHC" ]; then
  echo 'ghc-shim: real ghc not found on PATH' >&2
  exit 1
fi
for arg in "$@"; do
  if [ "$arg" = "--interactive" ]; then
    printf '%s\0' "$@" > "$HASKELL_FLOWS_SHIM_OUT"
    exit 0
  fi
done
exec "$REAL_GHC" "$@"
