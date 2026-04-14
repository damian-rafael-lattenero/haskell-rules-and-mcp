# Bundled Tools (Auto-Download)

This directory stores optional binaries used by the MCP server when host tools are not available.

**✨ Auto-Download:** Binaries are downloaded automatically from GitHub Releases the first time you use them. No manual setup required!

## Resolution Order

The MCP resolves tooling in this order:

1. **Host PATH** (`~/.ghcup/bin`, `~/.cabal/bin`, system `PATH`) - preferred
2. **Cached bundled binary** in `vendor-tools/` - if already downloaded
3. **Auto-download** from GitHub Releases - first time use only
4. **Unavailable** - if none of the above work

## How It Works

**First time you use a tool** (e.g., `ghci_lint`):
1. MCP checks host PATH → not found
2. MCP checks `vendor-tools/` → not found
3. MCP downloads from GitHub Releases → ~5-10 seconds
4. Binary cached in `vendor-tools/` for future use
5. Tool executes normally

**Subsequent uses:**
- Instant - uses cached binary from `vendor-tools/`

## Supported Platforms

Auto-download works on:
- ✅ macOS (arm64, x64)
- ✅ Linux (x64)
- ⚠️ Windows - not yet supported (install manually)

## Manual Installation (Optional)

If you prefer to install tools yourself:

```bash
# Via ghcup (recommended)
ghcup install hls
ghcup install hlint

# Via Homebrew (macOS)
brew install haskell-language-server
brew install hlint
```

The MCP will automatically prefer your manually installed versions.

## For Maintainers: Uploading Binaries

To upload new binaries to GitHub Releases:

```bash
cd mcp-server
chmod +x scripts/upload-to-github-releases.sh
./scripts/upload-to-github-releases.sh tools-v1.0
```

This uploads all binaries from `vendor-tools/` to the specified GitHub Release tag.

## Manifest

`bundled-tools-manifest.json` must contain:

- `filename` relative to `vendor-tools/`
- `sha256` checksum matching the binary
- `version`
- `provenance` source URL

If checksum is missing or mismatched, the MCP marks the bundled tool as unavailable.
