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

- The Haskell MCP server source (`mcp-server-haskell/`).
- Boundary validation surfaces:
  - `mkModulePath` — path-traversal guard over every file-path
    argument the MCP accepts.
  - `sanitizeExpression` — rejects newlines + the GHCi framing
    sentinel from any expression sent to `ghc_eval` / `ghc_type` /
    `ghc_info` / `ghc_quickcheck`.
  - `validatePackageName`, `validateVersionConstraint`,
    `parseStanzaSelector` in `HaskellFlows.Tool.Deps` — strict
    identifier checks for every `.cabal` edit.
- GHCi subprocess management:
  - Argv-form spawns (`System.Process.proc "cmd" [args]`), never
    shell strings. No interpolation path for agent input.
  - `maxBufferBytes` (16 MiB) DoS cap in `drainHandle`.
  - `SessionStatus` terminal-state detection (`Dead` on EOF) so a
    crashed GHCi cannot hold the server loop.
- `ghc_refactor` snapshot-and-compile-verify — refactors must be
  atomic from the agent's perspective, rolling back on compile
  failure.
- `ghc_deps` post-edit invariant check — refuses to persist a
  `.cabal` edit when the re-parsed result disagrees with the
  requested verb (prevents silent `success=true` with broken file).

**Out of scope** — not security concerns of this project:

- Vulnerabilities in `hlint`, `fourmolu`, `ormolu`,
  `haskell-language-server`, `hpc`, `cabal`, `ghc`, `hoogle`
  themselves. Report those upstream.
- Vulnerabilities in the Haskell code the MCP generates or tests.
  The MCP runs untrusted Haskell input locally in GHCi, same as any
  developer would — the sandbox is whatever GHCi provides.
- Issues that require physical access to the developer's machine.

## Known trust boundaries

Two external trust decisions are explicit:

1. **Toolchain binaries (`cabal`, `ghc`, `hlint`, `hpc`, `fourmolu`,
   `ormolu`, `hoogle`, `hls`)** are resolved from the user's `$PATH`
   at runtime. The MCP never downloads binaries. The recommended
   install path is [`ghcup`](https://www.haskell.org/ghcup/), the
   canonical Haskell toolchain manager, which carries its own
   integrity guarantees (signed metadata + per-release checksums).
   `ghc_toolchain_status` surfaces the resolved path + version of
   every tool so auditors can inspect the chain of trust at runtime.
2. **GHCi** runs as a local subprocess via `cabal repl
   --build-depends QuickCheck`. Any untrusted Haskell expression you
   pass through `ghc_eval` is evaluated with the same privileges as
   your shell. This is documented in the tool's description; treat
   `ghc_eval` like you treat `eval` in any scripting language.

## Disclosure philosophy

Coordinated disclosure preferred. I will:

- Credit the reporter in the fix commit and in the release notes,
  unless they ask to remain anonymous.
- Publish the advisory and the fix together, not on a schedule that
  rewards embargo length for its own sake.
- Not retaliate against responsible reporting — this project is
  small-enough that we can actually keep that simple.
