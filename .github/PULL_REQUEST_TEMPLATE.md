<!--
  Thanks for opening a PR! Please take a minute to fill out the sections
  below. They help keep the review cycle short.
-->

## Summary

<!-- One paragraph. Why does this change exist? -->

## Changes

<!-- Bullet list. What did you touch and why? -->

-
-
-

## Testing

<!-- How did you verify? Every change needs evidence. -->

- [ ] `cd mcp-server && npm run typecheck` — passes
- [ ] `npm test` — passes (and the total count went UP, not down)
- [ ] `npm run test:integration` — passes
- [ ] `npm run test:e2e` — passes
- [ ] 3× `npm run test:all` determinism — no flakes introduced
- [ ] If this touches the manifest: `npm run tools:validate:urls` — green
- [ ] If this touches CI: the workflow file validates (YAML, referenced actions exist)

## AI-assistance disclosure

<!-- Per CONTRIBUTING.md and the GHC AI policy. Be honest. -->

- [ ] I reviewed every line I'm submitting and could reproduce it without AI help
- [ ] Commits generated with significant LLM assistance carry a `Co-Authored-By:` trailer
- [ ] Any LLM-generated logic has a corresponding test

## Related issues

<!-- "Closes #123" / "Refs #456" -->

## Follow-ups

<!-- Anything knowingly deferred. -->
