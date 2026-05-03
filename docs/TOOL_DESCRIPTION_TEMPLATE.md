# haskell-flows MCP — Tool description template

> Every `ToolDescriptor.tdDescription` follows this 6-field shape so any
> LLM (Claude / Cursor / Codex) can pick the right tool from schema
> alone, without reading source. The shape is enforced by
> `testDescriptionsMeetTemplate` in `test/Spec.hs` (PR-5).

## The 6 fields

| Field | What | Length budget |
|---|---|---|
| **PURPOSE** | One sentence: imperative verb + object | ≤ 20 words |
| **WHEN** | 1–2 specific situations as a bullet phrase | ≤ 30 words |
| **WHEN NOT** | The closest sibling tool that handles the adjacent case | ≤ 25 words |
| **PREREQUISITES** | What must run first (or "none") | ≤ 15 words |
| **OUTPUT** | Top-level keys the response carries | ≤ 20 words |
| **SEE ALSO** | 1–2 sibling tools | ≤ 10 words |

Total budget: ~120 words per tool. The fields are packed into the
existing `tdDescription :: Text` — no schema change. Order matters:
`PURPOSE` first so a reader who sees only the first sentence still
gets the gist.

## Reference exemplars

The HIGH-tier descriptors that already meet the template (read these
before writing a new one):

- `ghc_lab` — `mcp-server-haskell/src/HaskellFlows/Tool/Lab.hs:75-83`
- `ghc_perf` — `mcp-server-haskell/src/HaskellFlows/Tool/Perf.hs:60-67`
- `ghc_explain_error` — `mcp-server-haskell/src/HaskellFlows/Tool/ExplainError.hs:60-67`
- `ghc_witness` — `mcp-server-haskell/src/HaskellFlows/Tool/Witness.hs:61-68`
- `ghc_modules` — `mcp-server-haskell/src/HaskellFlows/Tool/Modules.hs:45-95`

## Canonical shape (literal example)

```
PURPOSE: <one sentence: imperative verb + object>.
WHEN: <bullet 1>; <bullet 2>.
WHEN NOT: <sibling tool> when <adjacent case>.
PREREQUISITES: <prior tool / state / binary> (or "none").
OUTPUT: <top-level response keys>.
SEE ALSO: <tool_a>, <tool_b>.
```

The `PURPOSE:`, `WHEN:`, etc. labels are mandatory in the literal
text — the description-shape lint greps for them.

## Worked example: `ghc_browse`

```
PURPOSE: List names exported by a loaded module + their types.
WHEN: orienting in an unfamiliar module before touching it; confirming
an export was added.
WHEN NOT: the module isn't in the project graph — use hoogle_search
for upstream packages, ghc_info for a single name.
PREREQUISITES: any prior ghc_load or ghc_check_module pulls the
target into the graph.
OUTPUT: {module, count, entries:[name :: type]}; status='no_match'
when the module isn't compiled.
SEE ALSO: ghc_info, hoogle_search.
```

## Worked example: action-discriminated tools

For tools with `oneOf` actions (`ghc_project`, `ghc_property_store`,
`ghc_modules`, `ghc_toolchain`, `ghc_deps`), keep the top-level shape
and document each action in a single sentence:

```
PURPOSE: <one sentence covering the tool family>.
WHEN: action="X" — <when X>; action="Y" — <when Y>; …
WHEN NOT: <sibling> for <adjacent case>.
PREREQUISITES: <whichever apply>.
OUTPUT: per-action shapes detailed in the JSON schema branches.
SEE ALSO: <tool>.
```

## Anti-patterns to avoid

- **Bare `:t`-like descriptions** ("`:t expr`") with no English. The
  description must work for someone who has never used GHCi.
- **One-line stubs** under 80 chars total. The 200-character minimum
  is enforced by lint (PR-5).
- **No SEE ALSO routing**. Every tool has at least one sibling for an
  adjacent case; surface it so the agent doesn't have to discover the
  routing by trial.
- **Hidden requirements in the param doc only** (e.g. "hoogle binary
  must be on PATH" buried in the `query` field's `description`).
  Promote those to PREREQUISITES.
- **No "WHEN NOT"**. When two tools could fit (`ghc_browse` vs
  `hoogle_search`, `ghc_fix_warning` vs `ghc_explain_error`), the
  WHEN NOT field is the disambiguator — without it the LLM picks
  randomly.

## How to verify

- `cabal test` runs `testDescriptionsMeetTemplate` for every
  registered tool: minimum length, presence of `PURPOSE:`, `WHEN:`,
  `WHEN NOT:`, `SEE ALSO:` markers, and a `ghc_` / `hoogle_`
  cross-reference.
- Manual: read your draft and ask "could a fresh agent that hasn't
  seen this tool pick the right one for the right situation?"
