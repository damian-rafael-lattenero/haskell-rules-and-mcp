#!/usr/bin/env bash
set -euo pipefail

SCRIPT_DIR="$(cd "$(dirname "${BASH_SOURCE[0]}")" && pwd)"
PROJECT_DIR="$(cd "${SCRIPT_DIR}/.." && pwd)"

cd "${PROJECT_DIR}"

echo "Building TypeScript scripts..."
npm run build

os="$(uname -s | tr '[:upper:]' '[:lower:]')"
arch="$(uname -m)"

case "${os}" in
  darwin) platform="darwin" ;;
  linux) platform="linux" ;;
  *) echo "Unsupported OS: ${os}" >&2; exit 1 ;;
esac

case "${arch}" in
  x86_64) target_arch="x64" ;;
  aarch64|arm64) target_arch="arm64" ;;
  *) echo "Unsupported architecture: ${arch}" >&2; exit 1 ;;
esac

target="${platform}-${target_arch}"

echo "Downloading hlint for ${target}..."
npm run tools:download -- hlint "${target}"

echo "Downloading fourmolu for ${target}..."
npm run tools:download -- fourmolu "${target}"

echo "Downloading ormolu for ${target}..."
npm run tools:download -- ormolu "${target}" || echo "ormolu download skipped for ${target}"

echo "Updating manifest checksums..."
npm run tools:update-manifest -- \
  --tool hlint \
  --platform "${platform}" \
  --arch "${target_arch}" \
  --version "3.10" \
  --provenance "https://github.com/ndmitchell/hlint/releases/tag/v3.10"

npm run tools:update-manifest -- \
  --tool fourmolu \
  --platform "${platform}" \
  --arch "${target_arch}" \
  --version "0.19.0.1" \
  --provenance "https://github.com/fourmolu/fourmolu/releases/tag/v0.19.0.1"

if [ -f "vendor-tools/ormolu/${target}/ormolu" ]; then
  npm run tools:update-manifest -- \
    --tool ormolu \
    --platform "${platform}" \
    --arch "${target_arch}" \
    --version "0.7.7.0" \
    --provenance "https://github.com/tweag/ormolu/releases/tag/0.7.7.0"
fi

echo "Validating bundled tools..."
npm run tools:validate
npm run tools:test -- hlint
npm run tools:test -- fourmolu
if [ -f "vendor-tools/ormolu/${target}/ormolu" ]; then
  npm run tools:test -- ormolu
fi

echo "Bundled tools setup complete for ${target}."
