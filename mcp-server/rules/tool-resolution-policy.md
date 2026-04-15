# Tool Resolution Policy

This document defines the canonical behavior for optional Haskell tooling:

- `hlint`
- `fourmolu`
- `ormolu`
- `haskell-language-server-wrapper` (`hls`)

The policy must be transparent to users and consistent across macOS, Linux, and Windows.

## Core Rule

For every optional tool, the MCP must resolve in this exact order:

1. **Host tool** (preferred)
2. **Bundled tool** (fallback)
3. **Unavailable** (degraded behavior, not a crash)

In short:

- user has tool -> use it
- user does not have tool but MCP provides bundled -> use bundled
- neither exists -> disable that capability and continue safely

## Required Response Contract

Tool handlers that depend on optional binaries should return structured metadata:

- `available` (boolean)
- `source` (`host` | `bundled` | `none`)
- `binaryPath` (when available)
- `version` (when available)
- `reason` (when unavailable, e.g. `binary-missing`, `checksum-mismatch`, `entry-missing`)

This keeps behavior observable and debuggable for users and CI.

## Degradation Rules

When tool resolution fails:

- `ghci_lint`:
  - returns unavailable with machine-readable reason
  - guidance marks lint as recommended/non-blocking when unavailable
- `ghci_format`:
  - same as lint
- `ghci_hls`:
  - `available=false` for HLS-specific operations
  - users still use compiler diagnostics via `ghci_load(diagnostics=true)`

Never fabricate success when a required binary is missing.

## Platform Strategy

### Local clone workflow

- Prefer host binaries from standard user toolchains (`ghcup`, `cabal`, `PATH`)
- Provide `vendor-tools` fallback where possible
- Validate with:
  - `npm run tools:validate`
  - `npm run tools:test -- <tool>`

### Web/ephemeral runtime workflow

- Prefer pre-baked runtime images with host binaries already installed
- Keep bundled fallback only for small/portable artifacts
- For oversized binaries, use deterministic bootstrap at runtime + checksum validation

## Artifact Size Policy

Git hosting may reject large binaries (>100 MB hard limit, >50 MB warning threshold).

When full artifacts exceed repository limits:

- keep manifest entries valid
- use lightweight executable shims where needed
- keep strict status reporting so the runtime source remains explicit (`host` vs `bundled`)

## CI Expectations

Policy verification must run on all major OS targets:

- macOS
- Linux
- Windows

CI should verify:

1. `tool-installer` behavior is stable cross-platform
2. optional tool handlers degrade cleanly when binaries are absent
3. bundled manifest and checksum logic remains valid

## Non-Negotiables

- No silent fallback that changes semantics without metadata
- No fake success responses
- No blocking workflow step when tool is optional and unavailable
- No manual user intervention required to choose host vs bundled in normal operation

## Auto-Download Extension (Updated)

The resolution order now includes auto-download capability:

1. **Host tool** (preferred)
2. **Bundled tool** (fallback from vendor-tools/)
3. **Auto-download** (download from GitHub releases on first use)
4. **Unavailable** (degraded behavior)

### Supported Platforms for Auto-Download

| Tool | darwin-arm64 | darwin-x64 | linux-x64 | linux-arm64 | Windows |
|------|--------------|------------|-----------|-------------|---------|
| hlint | ✅ | ✅ | ✅ | ✅ | Manual |
| fourmolu | ✅ | ✅ | ✅ | ✅ | Manual |
| ormolu | ✅ | ✅ | ✅ | ✅ | Manual |
| hls | ✅ | ✅ | ✅ | ✅ | Manual |

Windows users must install tools via ghcup or stack.

### Auto-Download Behavior

- Downloads occur on first use when tool is not in PATH and not bundled
- Binaries are cached in `vendor-tools/` after download
- SHA256 checksums are verified post-download
- Failed downloads degrade gracefully to unavailable state
