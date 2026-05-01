# haskell-flows-mcp — Binary Size Investigation

> Issue [#101](https://github.com/damian-rafael-lattenero/haskell-rules-and-mcp/issues/101) Phase A.  Measurement-driven baseline for any
> future cold-start / footprint optimisation.  Numbers in this doc are
> reproducible — re-run the commands in §3 against any newer build to
> confirm the regression / improvement story.

---

## 1. Headline numbers

| Metric | Value | Notes |
|---|---:|---|
| Binary on disk | **199 MB** (208,444,224 B) | `~/.local/bin/haskell-flows-mcp` (arm64, ghc-9.12.2, optimised) |
| `--version` cold-start | **~30–60 ms** | Wall-clock; binary loads, prints, exits before GHC init |
| `tools/list` cold-start | **~70–80 ms** | initialize → tools/list round-trip via stdio |
| Stripped binary | **135 MB** (141,683,352 B) | `strip` removes 64 MB / 32% — debug + symbol info |
| Stripped cold-start | **~30–60 ms** | Indistinguishable from unstripped after the first cache load |

The single biggest takeaway: **binary boot itself is already fast**
(tens of milliseconds, not the multi-second number cited in the issue
preamble).  The 5+ s cold-start that motivated the issue is the cost
of the **first `ghc_load` call** — i.e. GHC API initialisation
(setSessionDynFlags + load) — not the cost of starting the executable.
That changes which lever is worth pulling first.

## 2. Section breakdown

`size` (Mach-O, arm64) reports the static segments:

| Segment | Size | What lives here |
|---|---:|---|
| `__TEXT`        | **110 MB** | All compiled Haskell code: ghc-9.12.2 library (~190 MB statically linked, dead-code-eliminated by the linker), aeson/text/bytestring/containers, the haskell-flows-mcp source itself |
| `__DATA`        | **13 MB**  | Initialised data: top-level constants, RTS support tables |
| `__DATA_CONST`  | **32 KB**  | Read-only data |
| `__LINKEDIT`    | **75 MB**  | Symbol table, debug strings, dynamic linker info — **the entire surface that `strip` reclaims** |
| `__PAGEZERO`    | 4 GB (virt) | Address-space reservation; never resident, no on-disk weight |

The `__LINKEDIT` cost is the surprising one.  At 75 MB it accounts for
38% of the on-disk binary; the linker keeps it because Haskell programs
ship full source-name symbol tables for stack traces, deferred
evaluation diagnostics, and the GHCi `:i`/`:t` machinery the MCP itself
relies on.  After `strip`, `__LINKEDIT` shrinks to 11.5 MB (~85 %
reduction in that segment alone) — and that is where the 64 MB / 32 %
total reduction comes from.

## 3. How to reproduce

```bash
# 1. Headline size + section breakdown
ls -la ~/.local/bin/haskell-flows-mcp
size      ~/.local/bin/haskell-flows-mcp
otool -l  ~/.local/bin/haskell-flows-mcp \
  | awk '/segname/ {seg=$2} /vmsize/ {printf "  %s vmsize=%s\n", seg, $2}'

# 2. Cold-start (no GHC init)
for i in 1 2 3; do /usr/bin/time -p haskell-flows-mcp --version; done

# 3. Cold-start to first tools/list
for i in 1 2 3; do
  start=$(python3 -c 'import time;print(int(time.time()*1000))')
  printf '%s\n' \
    '{"jsonrpc":"2.0","id":1,"method":"initialize","params":{"protocolVersion":"2025-06-18","capabilities":{},"clientInfo":{"name":"bench","version":"0"}}}' \
    '{"jsonrpc":"2.0","method":"notifications/initialized"}' \
    '{"jsonrpc":"2.0","id":2,"method":"tools/list"}' \
    | haskell-flows-mcp >/dev/null 2>&1
  echo "$(( $(python3 -c 'import time;print(int(time.time()*1000))') - start )) ms"
done

# 4. Strip experiment
cp ~/.local/bin/haskell-flows-mcp /tmp/haskell-flows-mcp-stripped
strip /tmp/haskell-flows-mcp-stripped
ls -la /tmp/haskell-flows-mcp-stripped
```

## 4. What the data says about the issue's decision tree

Re-reading [#101](https://github.com/damian-rafael-lattenero/haskell-rules-and-mcp/issues/101) §4 in light of the numbers:

| Question the issue asked | Phase A answer |
|---|---|
| Is the binary really 207 MB? | Yes — within rounding (199 MB after the latest rebuild). |
| Is GHC the dominant contributor to `__TEXT`? | Yes — 110 MB of text segment after dead-code elimination is consistent with ghc-9.12.2 being ~80 % of the linked surface. |
| What's the strip ceiling? | **32 % reduction** (199 → 135 MB).  Larger than the 10–20 % the issue's table guessed at, because the symbol-table-heavy `__LINKEDIT` is bigger here than in a typical C binary. |
| Is binary boot the cold-start bottleneck? | **No.** Boot is 30–80 ms.  The "5–6 s for first `ghc_load`" cited in the issue is GHC API init + first-module compile + first .hi load — orthogonal to executable size. |

## 5. Recommended next steps (Phase B onwards)

The Phase A measurement materially changes the proposed phases:

- **Phase B (cheap optimisations) — ✅ LANDED.**  Delivered in two
  pieces:

  * `cabal.project`: `package haskell-flows-mcp { split-sections: True }`.
    Tells GHC to emit one section per top-level definition so the
    linker can drop unreferenced symbols.  Cross-platform: the right
    linker invocation is chosen automatically per OS.
  * `scripts/install-mcp.sh`: a `[3/3]` step strips the copied binary.
    `--no-strip` is available for the rare case where a developer
    wants the symbol table for `lldb` / `gdb`.

  Both layers preserve binary functionality (`--version` and `--help`
  both work post-strip; the unit + e2e suites still pass).  Combined
  saving: **~64 MB off the 199 MB on-disk** for every developer's
  `~/.local/bin`.

- **Phase C (thin-client / fat-worker split) is no longer well-motivated
  by *cold-start*.**  Binary boot is already 30–80 ms.  The split would
  only help if the GHC API init itself can be deferred / amortised —
  which is a different conversation (essentially: "lazy `runGhc`" or
  "daemonised worker per project root").

  If the goal is to reduce the **first-tool latency** for sessions that
  do exactly one Haskell tool call, the right lever is profiling
  `startGhcSession` + `setSessionDynFlags`, not splitting the binary.
  That is genuine future work but should be filed as its own issue with
  its own measurement baseline.

- **Phase D (decide):** Phase B alone closes the on-disk size complaint
  with high confidence.  Phase C should be deferred until somebody can
  produce a session profile showing the GHC-init cost is the actual
  bottleneck for a real user flow.

## 6. What is *not* measured here (explicit out-of-scope)

- **Resident-set size during a `ghc_check_project`** — Phase A only
  looks at on-disk and boot time.  RSS during real work is harder to
  measure portably and is the right metric for the multi-session
  developer-with-three-terminals case in the issue preamble.
- **Linux numbers.**  All measurements above are macOS arm64.  The
  `__LINKEDIT` story is Mach-O specific; ELF binaries on Linux store
  the equivalent information differently and may show a different
  strip ratio.
- **Per-dependency contribution.**  Without `bloaty` or equivalent
  installed locally, this doc cannot break `__TEXT` down by linked
  package.  Adding `bloaty` to the toolchain probe would let a future
  Phase produce a per-package waterfall.

---

*Reproduce all numbers above against a fresh build before drawing any
new conclusions; the figures in this doc are a point-in-time snapshot
of the current `master` (commit visible in the surrounding git log).*
