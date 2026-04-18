#!/usr/bin/env bash
#
# publish-release-assets.sh — publish hlint/fourmolu/ormolu/hls to the
# `tools-v1.0` GitHub release referenced by src/tools/auto-download.ts.
#
# RUN MANUALLY by the repo owner. The MCP never invokes this script: it only
# reads the published assets at tool-resolve time (with SHA256 verification).
#
# Prerequisites:
#   - `gh` (GitHub CLI) authenticated with push access to the release repo
#   - `sha256sum` on Linux or `shasum -a 256` on macOS
#   - Source binaries already downloaded or built for each target tuple
#
# Security:
#   - Never upload an asset whose checksum you haven't verified locally.
#   - Never commit API tokens; use `gh auth login` session credentials.
#   - Prefer upstream-signed releases (ndmitchell/hlint, fourmolu/fourmolu,
#     haskell/haskell-language-server) — fetch those, verify, then re-upload.
#
# Usage:
#   ./scripts/publish-release-assets.sh <tool> <target> <binary-path>
# Example:
#   ./scripts/publish-release-assets.sh hlint darwin-arm64 ./downloads/hlint

set -euo pipefail

RELEASE_TAG="${RELEASE_TAG:-tools-v1.0}"
REPO="${REPO:-damian-rafael-lattenero/haskell-rules-and-mcp}"

if [ $# -ne 3 ]; then
  echo "Usage: $0 <tool> <target> <binary-path>" >&2
  echo "  tool   : hlint | fourmolu | ormolu | hls" >&2
  echo "  target : darwin-arm64 | darwin-x64 | linux-arm64 | linux-x64" >&2
  echo "  binary : path to the binary to upload" >&2
  exit 1
fi

TOOL="$1"
TARGET="$2"
BIN="$3"

if [ ! -f "$BIN" ]; then
  echo "ERROR: binary not found at $BIN" >&2
  exit 1
fi

if [ "$TOOL" = "hls" ]; then
  ASSET_NAME="haskell-language-server-wrapper-${TARGET}"
else
  ASSET_NAME="${TOOL}-${TARGET}"
fi

if command -v sha256sum >/dev/null 2>&1; then
  SHA256=$(sha256sum "$BIN" | awk '{print $1}')
else
  SHA256=$(shasum -a 256 "$BIN" | awk '{print $1}')
fi

echo "Tool     : $TOOL"
echo "Target   : $TARGET"
echo "Asset    : $ASSET_NAME"
echo "SHA256   : $SHA256"
echo "Release  : $RELEASE_TAG (repo: $REPO)"
echo

# Upload (idempotent — --clobber overwrites existing asset)
gh release upload "$RELEASE_TAG" "$BIN#$ASSET_NAME" \
  --repo "$REPO" \
  --clobber

echo
echo "✅ Published $ASSET_NAME to release $RELEASE_TAG"
echo
echo "Next step: update src/tools/auto-download.ts GITHUB_RELEASES so the"
echo "  $TARGET entry for $TOOL has:"
echo "    sha256: \"$SHA256\""
echo "  and no 'PENDING_CHECKSUM_*' placeholder remains for this target."
