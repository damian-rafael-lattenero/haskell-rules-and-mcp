# Operator runbook — publishing toolchain assets to `tools-v1.0`

This runbook walks a maintainer through publishing pre-verified binaries of `hlint`, `fourmolu`, `ormolu`, and `hls` to the GitHub Release `tools-v1.0`, so that the MCP's auto-download path works out-of-the-box for end users.

**Audience:** the repository owner (or a delegate with write access to the releases).
**Time:** ~30 min per platform × 4 tools × N platforms (serial) — parallelizable.
**Security:** this is the single trust boundary for end users. Each asset MUST have a verified SHA256 before upload and the matching checksum MUST land in `src/tools/auto-download.ts` in the same PR.

---

## Why this runbook exists

The MCP's `auto-download.ts` hardcodes URLs to `tools-v1.0` assets. When those assets are missing, the MCP falls back to `basic-lint-rules` (for lint) or returns `unavailable: true` (for format). The user experience is still correct but degraded. Publishing the assets turns the degraded paths into the happy path.

The MCP's fallback-URL mechanism (`fallbackUrl` + `fallbackSha256` in each `ToolRelease` entry) lets you point at a known upstream URL as a secondary source if the primary ever 404s. That field is **optional** — populate it when a suitable single-file upstream binary exists.

---

## Source of truth for binaries

Always prefer **upstream signed releases** over self-built binaries. When upstream distributes a bare binary (not a tarball), grab it directly. When upstream ships a tarball, extract the binary and pin a sha256 on the extracted file.

| Tool       | Upstream releases                                                   | Format on common platforms       |
|------------|---------------------------------------------------------------------|----------------------------------|
| `hlint`    | <https://github.com/ndmitchell/hlint/releases>                      | `.tar.gz` with `hlint` inside    |
| `fourmolu` | <https://github.com/fourmolu/fourmolu/releases>                     | bare binary per platform         |
| `ormolu`   | <https://github.com/tweag/ormolu/releases>                          | `.zip` with `ormolu` inside      |
| `hls`      | <https://github.com/haskell/haskell-language-server/releases>       | tarball; installer wrapper is bare on some targets |

Alternative: use `ghcup` on the publishing host to install the exact version you want, then upload `which hlint` etc. (this is the simplest flow if you already trust your local ghcup installation).

---

## Prerequisites

```bash
brew install gh sha256sum   # or on Linux, use your package manager
gh auth login               # needs write access to the release repo
```

Confirm write access:
```bash
gh release view tools-v1.0 --repo damian-rafael-lattenero/haskell-rules-and-mcp
```

---

## Step-by-step (per tool × platform)

Example: publishing `hlint` v3.10 for `darwin-arm64`.

### 1. Obtain the binary

```bash
# Option A: via ghcup on an arm64 Mac
ghcup install hlint 3.10
cp ~/.ghcup/bin/hlint ./hlint-darwin-arm64

# Option B: from upstream tarball
curl -L -o hlint.tar.gz https://github.com/ndmitchell/hlint/releases/download/v3.10/hlint-3.10-arm64-osx.tar.gz
tar -xzf hlint.tar.gz
cp hlint-3.10/hlint ./hlint-darwin-arm64
rm -rf hlint.tar.gz hlint-3.10
```

### 2. Verify it runs
```bash
chmod +x ./hlint-darwin-arm64
./hlint-darwin-arm64 --version
```

### 3. Compute and record the SHA256
```bash
shasum -a 256 ./hlint-darwin-arm64
# → abc123… hlint-darwin-arm64
```

Save that checksum — you will paste it into `auto-download.ts` in step 5.

### 4. Publish via the helper script
```bash
cd mcp-server
./scripts/publish-release-assets.sh hlint darwin-arm64 ../hlint-darwin-arm64
```

The script:
- Re-computes and prints the SHA256.
- Uploads to `tools-v1.0` with `gh release upload --clobber`.
- Prints the exact snippet to paste into `GITHUB_RELEASES`.

### 5. Pin the checksum in code

Edit `mcp-server/src/tools/auto-download.ts`. Replace the target entry's `sha256: undefined` with the real checksum:

```ts
"darwin-arm64": {
  version: "v3.10",
  url: "https://github.com/damian-rafael-lattenero/haskell-rules-and-mcp/releases/download/tools-v1.0/hlint-darwin-arm64",
  sha256: "abc123...",   // <-- paste here
  binaryName: "hlint",
},
```

Commit with a short message, e.g.:

```
chore(mcp): pin hlint darwin-arm64 sha256 for tools-v1.0
```

### 6. Verify the end-to-end path locally

```bash
cd mcp-server
rm -rf vendor-tools/hlint/darwin-arm64   # force re-download
npm run build
# Start a fresh Claude Code session; call ghci_lint on any module
# Expect: ghci_toolchain_status reports source: "installed" and checksumVerified: true
```

---

## Populating `fallbackUrl`

For added resilience, you can point each release entry at an upstream bare-binary URL. The fallback is tried only if the primary download (or checksum check) fails. Only single-file downloads are supported — no tarball extraction.

Example for `fourmolu` on `darwin-arm64`:

```ts
"darwin-arm64": {
  version: "v0.19.0.1",
  url: "https://github.com/damian-rafael-lattenero/haskell-rules-and-mcp/releases/download/tools-v1.0/fourmolu-darwin-arm64",
  sha256: "...",
  fallbackUrl: "https://github.com/fourmolu/fourmolu/releases/download/v0.19.0.1/fourmolu-0.19.0.1-darwin-x86_64",
  fallbackSha256: "...",
  binaryName: "fourmolu",
},
```

The `fallbackSha256` is **strongly recommended**. If omitted, the fallback succeeds without checksum verification and the response surfaces `checksumState: "missing"` so the caller can still reason about trust, but the supply-chain guarantee weakens.

---

## Reviewer checklist (PR that bumps a version)

- [ ] Every touched `ToolRelease` entry has a non-empty `sha256` matching the asset bytes on the release.
- [ ] Primary URL returns HTTP 200 (use `curl -I <url>`).
- [ ] If `fallbackUrl` is set, it also returns 200 and the `fallbackSha256` matches its bytes.
- [ ] `npm test` + `npm run test:integration` + `npm run test:e2e` all green.
- [ ] Release notes mention the new version.

---

## Never do

- Never commit a version bump without the SHA256 — that leaves end users trusting an unverified download.
- Never upload a binary without running it locally first (`--version` smoke).
- Never reuse a `fallbackUrl` that requires extraction (tarball/zip) without first adding an extraction path to `auto-download.ts`. The current code path assumes single-file downloads.
- Never publish with `--no-verify` / skipping pre-commit hooks — the linter and tests catch common mistakes in `auto-download.ts` edits.
