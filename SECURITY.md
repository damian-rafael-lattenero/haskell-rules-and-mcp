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

## Path-traversal threat model (#100)

**Issue**: MCP tools accept a `module_path` argument that is a
project-relative file path. Without validation, an agent (or malicious
prompt injection) can pass `../../etc/passwd` to read or overwrite files
outside the project root. This is CWE-22.

**Defence — two layers (as of 2026-04-30)**:

### Layer 1 — Pure lexical guard (`mkModulePath`, always active)

Every tool that accepts `module_path` routes the value through
`HaskellFlows.Types.mkModulePath` before any filesystem access. This
function:

1. Joins the project root with the raw path using
   `System.FilePath.normalise` (no IO, no kernel call).
2. Splits the result into directory segments.
3. Rejects any path containing a `..` segment, or whose normalised
   absolute form does not share a prefix with the project root.

Tools covered: `ghc_apply_exports`, `ghc_check_module`,
`ghc_explain_error`, `ghc_fix_warning`, `ghc_format`, `ghc_hole`,
`ghc_lab`, `ghc_lint` (via `resolveTarget`), `ghc_load`, `ghc_refactor`,
and `ghc_check_project`. All return `status=refused, kind=path_traversal`
when the guard fires, with an `eeField` identifying `"module_path"`.

Property-based regression: `prop_pathGuard_dotdot_always_rejected`
(in `test/PathTraversal.hs`) exhaustively fuzz-tests this guard on
adversarial inputs across seven historical CVE categories.

### Layer 2 — IO canonical check (`canonicalModulePathCheck`, defence-in-depth)

After the lexical guard accepts a path, tools that **write files** call
`HaskellFlows.Types.canonicalModulePathCheck` before touching the disk.
This function:

1. Calls `System.Directory.canonicalizePath` on the resolved file path
   (follows all symlinks to their real destination).
2. Calls `canonicalizePath` on the project root.
3. Verifies the canonical file path is prefixed by the canonical root.

Currently applied at: `ghc_format` (write site, `write=true` mode).

**Why write sites only**: `canonicalizePath` requires the path to exist
on disk (it calls `realpath(3)`). For read-only tools the lexical guard
is sufficient; write tools carry more risk so they use both layers.

**macOS note**: on macOS, `/var/folders/...` canonicalises to
`/private/var/folders/...`. Canonicalising _both_ paths (root and file)
neutralises the asymmetry — the prefix check works correctly on all
platforms.

### Known gap — symlinks in lexical-only tools

The lexical guard does not follow symlinks. A symlink at
`src/escape -> /etc` passes the `..`-free check because the link's
filesystem name contains no `..`. The canonical check (Layer 2) catches
this, but it is currently only applied at write sites.

Implication: a read-only tool (`ghc_hole`, `ghc_load`, etc.) can be
tricked into reading a file outside the project root via a pre-planted
symlink. This is documented as a known limitation rather than a
vulnerability, because:

- The MCP runs with the developer's own filesystem permissions.
- Symlinks are physical files the developer themselves would have had to
  create (or an attacker would need separate write access to create them).
- The canonical check is being progressively rolled out to read sites in
  subsequent phases.

Test canary: `testSymlinkEscapeAcceptedByPureGuard` and
`testCanonicalCheckCatchesSymlink` in `test/PathTraversal.hs` document
this gap and verify that Layer 2 closes it.

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
