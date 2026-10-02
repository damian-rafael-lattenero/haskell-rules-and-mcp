# Propuesta de condensación — haskell-flows MCP (E4, 2026-10-01)

Input: dogfood `docs/dogfood-2026-10-01-opencode-hm.md` (F1–F27) +
autopsia dirigida (root causes verificados en código). Objetivo: que un
LLM cualquiera con este MCP sepa **qué usar en bucle, qué puntualmente,
y cómo dejarse guiar** para ganar confianza en el Haskell generado.

## El bucle de confianza (el corazón que la superficie debe reflejar)

```
escribir ──▶ verificar ──▶ sugerir ley ──▶ correr ley ──▶ persistir
   ▲                                                        │
   └───────────── fix ◀──── counterexample ◀────────────────┘
                         (gate agregado antes de push)
```

Regla de diseño: **1 tool = 1 paso del bucle**. Hoy: 36 tools = 14 usadas
(39%), 5 protocol breaks forzados, 3 vehículos de ejecución divergentes.

## Superficie propuesta: 12 tools

### Core del bucle (8)

| Tool | Absorbe | Mata hallazgos |
|---|---|---|
| `ghc_project` (create/switch/validate) | project, SwitchProject | F5: create SIEMPRE en subdir propio |
| `ghc_module` (add/write/remove) | modules, scratch-promote, refactor-write | F5, F9, F16, F20: `write` recibe el módulo COMPLETO (header+imports+decls), compile-verify atómico, imports incluidos. scratch queda como `write` sin target (borrador tipado) |
| `ghc_edit` (rename/extract/move/patch_imports/exports/fix_warning) | refactor, add_import(file), apply_exports, fix_warning | F10: import de archivo sin hoogle; hoogle solo para *search* |
| `ghc_check` (module/project, `with_lint`, `strict`) | check_module, check_project, lint | F19: warnings ≠ failed (enum separado: ok/warn/error) |
| `ghc_eval` | eval (queda) | — |
| `ghc_suggest` | suggest | F22/F23: **type-check de la property generada antes de emitirla** (barato, in-process); simetría printer/parser |
| `ghc_property` (check/run/arbitrary/list/export/audit) | quickcheck, property_store, arbitrary, PropertyAudit/Lifecycle | F21/F24/F25: arbitrary COMPILA la instance en el store-module (no template paste); properties viven en UN módulo generado por el server (p.ej. `test/Properties.hs` registrado en el test-suite) — el contexto siempre compila porque el server lo posee |
| `ghc_gate` (build/test/regression) | gate (queda — hoy es lo mejor) | — |

### Soporte (4)

| Tool | Absorbe | Nota |
|---|---|---|
| `ghc_deps` | deps | queda (funcionó) |
| `ghc_explain_error` | explain_error | queda (mejor nextStep-target) |
| `ghc_batch` | batch | agregación HONESTA (no_match≠ok; F20) y sin fail-fast ciego (F8) |
| `ghc_session` (status/help/warmup/imports) | workflow, toolchain, ToolchainStatus/Warmup, imports | UN wayfinder; phase-machine como la de workflow pero con steps COHERENTES con la fase (F3) y staleness corregido (F2) |

### Deprecar / esconder tras `action=`

`ghc_hole`, `ghc_complete`, `ghc_browse`, `ghc_doc`, `ghc_goto`,
`ghc_type`, `ghc_info` → **`ghc_inspect`** (action=hole/complete/browse/
doc/goto/type/info). HLS ya cubre esto para humanos; para agentes el
valor marginal era bajo y la confusión alta (7 nombres ≈ 1 concepto:
"mirar el código").
`ghc_lint`, `ghc_format` → flags de `ghc_check` / `ghc_edit.format`.
`ghc_lab`, `ghc_witness`, `ghc_perf`, `ghc_coverage` → `ghc_inspect`
avanzado o deprecate hasta demanda real (uso ~0 en dogfood real).
`ghc_hoogle` → `ghc_inspect.search`; **hoogle nunca gate** de nada.
`ghc_scratch` → disuelto en `ghc_module(write)`.
`ghc_add_import` → `ghc_edit.patch_imports` (archivo) + auto-imports de
`ghc_eval` (sesión).

36 → 12 namespaces, ~60 acciones tipadas. Menos superficie = menos
schemas inconsistentes (F11/F17) y menos tokens de tools/list por request.

## Arreglos estructurales no negociables (sin esto, condensar es maquillaje)

1. **Transport concurrente con timeout real** (F13/F15): reader-thread
   separado del dispatch; per-request deadline que también cubra waits
   no-interruptibles (watchdog que responde error y marca la sesión
   sospechosa). El zombie server es el bug que mata la confianza para
   siempre — un agente no reintenta tras 10 min de silencio.
2. **Un solo vehículo de ejecución** (F24/F25/F26): in-process GHC API
   puro. Eliminar shim-replay + `cabal v2-repl` scripting + sentinels.
   El plan ya existe (`docs/GHC-API-rewrite-plan.md`) — esta auditoría
   agrega la evidencia de urgencia: el pipeline property-first (la razón
   de ser del producto) es el principal víctima de la divergencia.
3. **Cold-start con presupuesto y señal** (F14): warmup explícito en
   `ghc_session(warmup)` que PRE-BUILDA deps con salida de progreso;
   el resto de las tools rechazan con `status: warming` + ETA en vez de
   colgarse 10 min en silencio.
4. **Guidance determinista por estado de sesión** (F3/F18): nextStep
   SIEMPRE derivado del (fase, status) — nunca texto de success en
   failure. Tests de contrato: para cada (tool, status≠ok) debe existir
   un nextStep ≠ success-path.
5. **Honestidad del envelope como invariant testeado** (F20/F27):
   property-based test del propio server: "ninguna respuesta ok sin
   efecto en disco", "post-mortem.error_streak cuenta todos los failed".

## Guidance para «un LLM cualquiera» (el cómo dejarse llevar)

1. `initialize.instructions` mantiene la tabla situación→tool (hoy es
   buena) PERO sobre 12 tools, con el bucle numerado:
   `1 ghc_module.write → 2 ghc_check → 3 ghc_suggest → 4 ghc_property.check → 5 ghc_gate`.
2. Cada nextStep lleva `step: "3/5"` — el agente sabe dónde está del
   bucle sin leer docs.
3. Phase-machine explícita con **tools rechazadas por fase**: si el
   agente llama `ghc_property` en pre-scaffold, el server responde
   `refused` + "primero ghc_project(create)" — corregir en vez de
   ejecutar-y-fallar.
4. `ghc_batch` recibe chains SOLO del server (las que hoy emite) — el
   agente no compone batches a mano (F8/F15 nos enseñaron que los
   batches grandes son el peor caso del transport).

## Qué NO condensar

- El envelope (status/kind/remediation) es el mejor asset — se mantiene.
- `ghc_gate` y `ghc_deps` tal cual (funcionaron impecables).
- La idea scratch→promote (hipótesis verificada antes de tocar fuente):
  sobrevive como `ghc_module(write)` sin target + promote posterior.

## Métrica de éxito de la condensación

Repetir el dogfood HM con la superficie nueva en ≤ 25 llamadas MCP,
0 protocol breaks forzados, 0 zombies, properties persistidas ≥ 3, y
cold-start ≤ 60s con señal de progreso. (Esta sesión: ~50 llamadas,
5 breaks, 2 zombies, 0 persistidas, 592s.)
