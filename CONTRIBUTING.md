# Contributing to `haskell-flows`

Thanks for considering a contribution. This project is a single-maintainer
effort — every external PR is genuinely valuable and will get careful review.

---

## Before you open a PR

1. **Read [CODE_OF_CONDUCT.md](CODE_OF_CONDUCT.md).** Brief and standard.
2. **Open an issue first** for anything larger than a typo or a one-line
   fix. Alignment is cheaper than rework.
3. **Every new tool or behavior needs a test** at the appropriate tier
   (pure-logic tests for parsers/validators, integration tests for GHCi
   interaction). See `mcp-server-haskell/test/Spec.hs` for the existing
   pattern — flat `IO Bool` runners + QuickCheck properties for
   boundary invariants (path traversal, command sanitisation).

---

## Dev setup

```bash
# Prerequisites
#   - GHC 9.12+ and Cabal 3.12+ (via ghcup)
#   - macOS ARM verified; other platforms should work with host tools
#     (hlint, fourmolu/ormolu, hpc) on PATH

cd mcp-server-haskell
cabal build all
cabal test all --test-show-details=direct
```

Before pushing, run the CI mirror from the repo root:

```bash
scripts/ci-local.sh --fast      # build + test + hlint, ~30s
scripts/ci-local.sh              # + haddock + cabal check + sdist, ~5 min
```

`--fast` matches the inner loop; the full run matches the `Haskell CI`
workflow end-to-end.

---

## Test expectations

Every change must leave `cabal test all` green. The test suite has three tiers:

- **Unit tests** (`test/Spec.hs` driver + 80+ domain modules under `test/Spec/`) —
  700+ pure-logic and in-process tests. Every parser, validator, and tool response
  shape has a unit test. Add one for any new boundary or invariant.
- **QuickCheck properties** — `Spec.PathTraversal` fuzzes `mkModulePath`; `Spec.Protocol`
  fuzzes ADT bijections; per-domain modules add property tests for their invariants.
- **E2E scenarios** (`test-e2e/`) — 72 scenarios drive the full JSON-RPC surface
  in-process against a real GHCi session. Add a scenario for any new tool or action.

Your PR should add or adjust tests at the right tier rather than lower overall coverage.

Dogfooding against a fresh scratch project is the highest-signal way to validate a
new tool or a non-trivial behaviour change. See
[`docs/dogfood-2026-04-19.md`](docs/dogfood-2026-04-19.md) and
[`docs/dogfood-2026-05-25-full-audit.md`](docs/dogfood-2026-05-25-full-audit.md)
for the log format (F-## findings, W-## wins as anti-regression markers).

---

## Commit style

- **Conventional commits** encouraged but not enforced.
- **One logical change per commit.** Large PRs are easier to review when
  history is coherent.
- **Commit messages describe WHY**, not just what. The diff already shows
  the what.

---

## AI-assistance policy

This project was iterated with Claude Code and follows the spirit of the
Haskell community's [Compact for Responsible Use of AI Tools](https://discourse.haskell.org/t/a-compact-for-responsible-use-of-ai-tools/13923).
The expectations propagate to contributors:

- **You should be able to produce the code you submit without AI assistance.**
  Using an LLM to accelerate writing is fine; using one to submit code you
  don't understand is not. You are the reviewer of record for your own PR
  before it lands in my inbox.
- **Label AI-authored commits explicitly.** If you used an LLM to generate
  a substantive part of a change, add a `Co-Authored-By:` trailer naming
  the model. Transparency, not concealment.
- **Tests are mandatory** for AI-generated logic. A test is your evidence
  that you verified the output.
- **No vibecoded dependencies.** Don't add a dependency suggested by an
  LLM without reading its source, its issue tracker, and its release
  notes yourself.

Contributions that silently ship LLM-generated code without disclosure will
be reverted and the contributor asked to redo the PR with the trailer.

---

## Code style

- Warnings-as-docs: every `-W*` flag in the `common shared` stanza of
  `haskell-flows-mcp.cabal` is active. A clean build under
  `-Wunused-packages` and friends is the baseline.
- HLint has no hints on `master`. New hints must be fixed or suppressed
  inline with a rationale.
- Commit messages and code comments in English.
- Every boundary validator (path, expression, package name, version
  constraint, stanza selector) returns `Either Text Text` — structured
  errors, never `String` exceptions.

---

## Priority areas where help is welcome

If you want to contribute but don't have a specific idea, these are the
areas I'd most like help with:

- **Cross-platform verification**: the MCP targets `darwin-arm64` primarily.
  Linux (`linux-x64`) and `darwin-x64` need smoke tests against real projects.
  Windows is entirely untested.
- **Nix flake**: `flake.nix` is scaffolded but untested. A working dev shell
  pinned to the exact GHC + cabal + toolchain versions would make onboarding
  reproducible.
- **More law-suggestion engines**: Monoid laws, Applicative laws, Traversable
  laws. Add a `Rule` to `HaskellFlows.Suggest.Rules.allRules` with a unit test
  covering the match shape and a false-positive counter-example.
- **`ghc_scratch` promote end-to-end**: the `action=promote` path uses
  `ghc_refactor`'s snapshot-and-compile-verify under the hood. More E2E
  coverage for the multi-module promote case is welcome.
- **Streaming progress** (#265): `ghc_gate` (which runs `cabal build` + `cabal test`)
  is the longest-running tool. A streaming `progress` channel that surfaces
  `[N of M] Compiling Foo` lines would make long builds observable.

---

## Questions

Open an issue with the `question` label. Response time is usually within
a week — this is a side project, not a day job.
