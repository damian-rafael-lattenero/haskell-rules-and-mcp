# Contributing to `haskell-flows`

Thanks for considering a contribution. This project is a single-maintainer
effort — every external PR is genuinely valuable and will get careful review.

---

## Before you open a PR

1. **Read [CODE_OF_CONDUCT.md](CODE_OF_CONDUCT.md).** Brief and standard.
2. **Open an issue first** for anything larger than a typo or a one-line
   fix. Alignment is cheaper than rework.
3. **Every new tool or behavior needs a test** at the appropriate tier
   (unit for logic, integration for GHCi interaction, e2e for full MCP
   protocol). See `mcp-server/src/__tests__/` for patterns.

---

## Dev setup

```bash
# Prerequisites
#   - Node.js 22+
#   - GHC 9.12+ and Cabal 3.12+ (via ghcup)
#   - macOS ARM verified; other platforms: use host tools (see README)

cd mcp-server
npm install
npm run build      # mandatory before test:e2e
npm run test:all   # unit + integration + e2e
```

A Nix flake is on the [Phase E roadmap](docs/community-launch/); until then
install the Haskell toolchain via `ghcup` as usual.

---

## Test expectations

Every change must leave `npm run test:all` green across three consecutive
runs (determinism is a core property of this project). 1157 tests currently
pass on `master`; your PR should add or adjust tests rather than lower
coverage.

```bash
# Verify determinism locally
for i in 1 2 3; do npm run test:all || exit 1; done
```

For Haskell-side changes in `playground/` examples, run the MCP's own
aggregator:

```text
ghci_workflow(action="gate")
```

which orchestrates regression + `cabal test` + `cabal build` in one call.

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

- TypeScript: let `tsc --strict` catch type errors. No `any` without a
  comment explaining why.
- Commit messages and code comments in English.
- Error envelopes follow the pattern: `{ success, error?, _hint?, _guidance?, _nextStep? }`.
- Tools register via `registerStrictTool` (Zod strict) — never bypass.

---

## Priority areas where help is welcome

If you want to contribute but don't have a specific idea, these are the
areas I'd most like help with:

- **Cross-platform support**: build + ship bundled tool binaries for
  `darwin-x64`, `linux-x64`, `linux-arm64`. The manifest + CI already
  accept them; the bottleneck is a clean build on those platforms.
- **Nix flake** (Phase E on the roadmap): declarative dev shell pinned
  to the tool versions the MCP expects.
- **Upstream-first tool resolution** (Phase D): prefer `ndmitchell/hlint`,
  `fourmolu/fourmolu`, `tweag/ormolu`, `haskell/haskell-language-server`
  releases over the personal mirror.
- **More law-suggestion engines**: Monoid laws, Applicative laws,
  Traversable laws. Single `src/laws/engines/*.ts` module per engine
  plus one unit test file per engine.

---

## Questions

Open an issue with the `question` label. Response time is usually within
a week — this is a side project, not a day job.
