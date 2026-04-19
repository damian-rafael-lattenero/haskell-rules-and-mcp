# Security policy

## Supported versions

Only the latest `0.x` release receives security updates. This is a
single-maintainer experimental project; there are no backports.

| Version | Supported          |
|---------|--------------------|
| 0.1.x   | :white_check_mark: |
| < 0.1.0 | :x:                |

## Reporting a vulnerability

**Please do not open a public GitHub issue for security-sensitive reports.**

Two acceptable channels:

1. **GitHub private security advisory** (preferred): open one at
   <https://github.com/damian-rafael-lattenero/haskell-rules-and-mcp/security/advisories/new>.
   GitHub routes it directly to the maintainer without making it public.
2. **Email**: send to the maintainer's GitHub-registered email with the
   subject `[haskell-flows SECURITY] <one-line summary>`.

Please include:

- A description of the issue and the observable impact.
- Steps to reproduce (minimal repro is ideal).
- The affected version or commit SHA.
- Your preferred disclosure timeline, if you have one.

Response expectations (best-effort, single maintainer):

| Step                             | Target                |
|----------------------------------|-----------------------|
| Initial acknowledgement          | within 7 days         |
| Severity assessment + triage     | within 14 days        |
| Fix or mitigation plan           | within 30 days        |
| Public advisory + credited fix   | after fix is released |

## Scope

**In scope** — vulnerabilities in:

- The MCP server TypeScript code (`mcp-server/src/`).
- The vendored tool distribution pipeline (`vendor-tools/`,
  `src/tools/auto-download.ts`, `src/tools/tool-installer.ts`,
  `scripts/validate-bundled-tools.ts`).
- SHA256 verification of downloaded binaries.
- Path-traversal guards (`src/helpers/paths.ts`).
- Process lifecycle management (`mcp_reload_code`, `ghci_session`).

**Out of scope** — not security concerns of this project:

- Vulnerabilities in `hlint`, `fourmolu`, `ormolu`, `haskell-language-server`
  themselves. Report those upstream.
- Vulnerabilities in `@modelcontextprotocol/sdk` or Node.js core.
- Vulnerabilities in the Haskell code the MCP generates or tests. The MCP
  runs untrusted Haskell input locally in GHCi, same as any developer
  would — the sandbox is whatever GHCi provides.
- Issues that require physical access to the developer's machine.

## Known trust boundaries

Declared explicitly because the project depends on three external trust
decisions:

1. **Bundled tool binaries** — the trust ordering is:
   - **Preferred, user-driven**: install via `ghcup install <tool>` (the
     `recommendedInstall` field the manifest surfaces for every tool).
     `ghcup` is the canonical Haskell toolchain manager and carries its
     own integrity guarantees.
   - **Upstream direct-binary**: when the project publishes a direct
     executable binary (currently: `fourmolu` on `darwin-arm64`), the MCP
     tries the **upstream URL first** (from `fourmolu/fourmolu` releases),
     verified by SHA256. Only fourmolu meets this bar today because
     upstream ships bare binaries; hlint / ormolu / hls upstream ship
     tarballs or zips that would require extraction infrastructure.
   - **Personal mirror** — for tools without a direct-binary upstream, the
     MCP falls through to a mirror on the maintainer's GitHub release
     (`damian-rafael-lattenero/.../tools-v1.0/`). Every mirror asset has a
     SHA256 pinned in `vendor-tools/bundled-tools-manifest.json`. The
     mirror's binaries were extracted from the canonical upstream
     tarballs; the hashes in the manifest let auditors verify the
     byte-equivalence themselves.
   - `ghci_toolchain_status` surfaces `upstreamReleasesPageUrl` +
     `upstreamRecommendedInstall` on every response so agents and users
     can inspect the chain of trust at runtime.
2. **GHCi itself** is executed as a local subprocess; any untrusted Haskell
   expression you pass through `ghci_eval` is evaluated with the same
   privileges as your shell. This is documented in the tool's description;
   treat `ghci_eval` like you treat `eval` in any scripting language.
3. **`mcp_reload_code`** deliberately terminates the Node process via
   `process.exit(0)`. It is staleness-gated (won't exit into stale code) and
   rate-limited (one call per 10 seconds) so it cannot be weaponized into a
   restart-loop DoS, but a caller holding the MCP connection can
   legitimately trigger a restart — that's the point of the tool.

## Disclosure philosophy

Coordinated disclosure preferred. I will:

- Credit the reporter in the fix commit and in the release notes, unless
  they ask to remain anonymous.
- Publish the advisory and the fix together, not on a schedule that
  rewards embargo length for its own sake.
- Not retaliate against responsible reporting — this project is
  small-enough that we can actually keep that simple.
