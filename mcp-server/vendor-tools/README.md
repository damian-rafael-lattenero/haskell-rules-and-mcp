# Bundled Tools

This directory stores optional binaries used by the MCP server when host tools are not available.

## Resolution Order

The MCP resolves tooling in this order:

1. Host PATH (`~/.ghcup/bin`, `~/.cabal/bin`, system `PATH`)
2. Bundled binary in `vendor-tools`

## Setup (current platform)

From `mcp-server/`:

```bash
chmod +x scripts/setup-bundled-tools.sh
./scripts/setup-bundled-tools.sh
```

This script:

- Builds TypeScript scripts
- Downloads `hlint`, `fourmolu`, and `ormolu` for the current platform
- Updates `bundled-tools-manifest.json` checksums and metadata
- Runs validation and smoke checks

## Manual Steps

```bash
npm run build
npm run tools:download -- hlint darwin-arm64
npm run tools:update-manifest -- --tool hlint --platform darwin --arch arm64 --version 3.9 --provenance https://github.com/ndmitchell/hlint/releases/tag/v3.9
npm run tools:validate
npm run tools:test -- hlint
```

## Manifest

`bundled-tools-manifest.json` must contain:

- `filename` relative to `vendor-tools/`
- `sha256` checksum matching the binary
- `version`
- `provenance` source URL

If checksum is missing or mismatched, the MCP marks the bundled tool as unavailable.
