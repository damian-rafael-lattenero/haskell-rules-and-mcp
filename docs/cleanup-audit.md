# Auditoría de limpieza — qué se poda, qué se queda (2026-10-02)

Método: cross-referencing completo de imports (src/test/e2e), registry
dispatch, cabal exposed-modules, LOC reales. No opiniones — evidencia.

## Wave 1 — Ejecutada (cero cambio de superficie)

| Borrado | Evidencia |
|---|---|
| `Tool/PropertyLifecycle.hs` (44 LOC) | **0 importers** en todo el repo; el comentario que lo justificaba ("tests still exercise it") era stale — ningún test lo importa |
| `Mcp/ErrorKind.hs` (~70 LOC) | Enum legacy de 3 valores; Envelope tiene el real de 26. Solo lo usaba `Server.toolException` (folded: ahora toma `Env.ErrorKind` directo) + 4 tests del propio enum legacy (borrados) |
| `spike/` + `spike-target/` + exe `spike-ghc-api` | Spike histórico del rewrite YA completado (fases 0-7 landed); 0 refs en scripts/CI; el conocimiento vive en `docs/GHC-API-rewrite-plan.md` |
| `docs/TODO-parallel-e2e.md` | Su sucesor lo declara obsoleto ("queda obsoleto cuando este plan se ejecute") y el plan se ejecutó |

Resultado: build verde, 700+ tests PASS (−4 tests del enum muerto).

## Wave 2 — Propuesta: condensación de superficie 36 → 12 (break v0.3)

Los 36 nombres ya son hoy 25 composites con sub-handlers heredados
(evidencia: ToolName.hs documenta los merges #94). La consolidación
termina el trabajo empezado. Mapa con destino de módulos:

### Se quedan (7, sin cambio de nombre)
`ghc_project` (256+432+259+434 ya fusionados), `ghc_deps` (+fold
DepsExplain 500), `ghc_gate` (483), `ghc_explain_error` (513),
`ghc_batch`, `ghc_eval` (ghcide route), `ghc_suggest` (472)

### Merges (5 tools nuevas absorben 14)
| Nueva | Absorbe | Módulos y LOC |
|---|---|---|
| `ghc_check` | check_module + check_project + lint | CheckModule+CheckProject+Lint (~1.4k) |
| `ghc_property` | quickcheck + property_store + arbitrary | QuickCheck 1042 + PropertyStore 179 + Arbitrary + PropertyAudit 538 + QuickCheckExport 592 + Regression 402 (quedan como actions) |
| `ghc_session` | workflow + toolchain + imports | Workflow 782 + Toolchain 109+235+92 + Imports 129 |
| `ghc_edit` | refactor + add_import + apply_exports + fix_warning + format | Refactor 719 + AddImport 367 + ApplyExports + FixWarning 584 + Format 238 (Move 784 **muere** — move_symbol se depreca) |
| `ghc_module` | modules + scratch-promote | Modules 154 (+AddModules/RemoveModules 494+494 como actions internas ya son); Scratch 933 **se disuelve** en write |

### Delete outright (7 tools, ~2.5k LOC src)
`ghc_lab` (563), `ghc_witness` (508), `ghc_perf` (537 — Bench/ queda
huérfano: revisar), `ghc_coverage` (362 — Parser.Coverage muere con él),
`ghc_hole`, `ghc_type`, `ghc_info`, `ghc_browse`, `ghc_complete`,
`ghc_goto`, `ghc_doc`, `ghc_load`, `hoogle_search` — los inspect van a
`ghc_inspect(action=…)` SOLO type/hole/browse/doc si ghcide los da
gratis; el resto muere (LSP del host ya lo sirve). `ghc_load` muere: en
backend ghcide el concepto "cargar" no existe (shake lo hace reactivo);
en legacy, check ya cubre.

### Trabajo colateral obligatorio (Wave 2b)
- `ToolName` enum + `Registry` (36 → 12 handlers)
- `initialize.instructions` (situación→tool se regenera)
- `NextStep.hs` 1332 LOC: rutas por ToolName — rewrite de la tabla
- e2e: 79 scenarios → reescribir los de tools muertas (~la mitad)
- tests: de 88 archivos Spec/*, mueren los de tools deletadas
  (estimado ~30 archivos)
- README/badges/TOOL_TAXONOMY/flows.md
- Estimación: 2-3 sesiones

## Wave 3 — Infra que muere DESPUÉS (gated por F3, no antes)

| Pieza | Gatillo de muerte |
|---|---|
| `Ghc/CabalBootstrap.hs` + shim | F3: property pipeline sobre ghcide (muere el vehículo cabal-repl) |
| `Parser/QuickCheck.hs`, `Parser/Error.hs` (regex GHC output) | F3/F4: diagnósticos estructurados ghcide |
| `Ghc/ApiSession.hs` (1837) | F4 completo: cuando las 12 tools corran 100% ghcide |
| Deps cabal: `regex-tdfa`, `scientific`(?) | cuando los parsers mueran |

## Decisión pendiente (única)

Wave 2 rompe el wire contract (36→12 sin aliases, según E4). Con base
de usuarios actual ~0, la rotura no cuesta nada hoy y cuesta caro
mañana. Recomendación: **directo en master** (sin branch v0.3), tag
`v0.2.0` antes del break como despedida de la superficie vieja.
