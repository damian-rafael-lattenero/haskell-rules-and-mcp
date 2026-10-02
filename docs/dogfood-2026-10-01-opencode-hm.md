# Dogfood: Hindley-Milner 100% via MCP — sesión opencode (2026-10-01)

Agente: GLM-5.3 vía opencode, guiado exclusivamente por el protocolo MCP
(harness stdio propio: FIFO + daemon detached; cero uso de Write/Read del host
sobre el proyecto, salvo donde se indica «protocol break forzado»).

Proyecto: `playground/hm-opencode` — lexer, parser recursive-descent,
Algorithm W con let-polymorphism, pretty-printer, evaluator. 8 módulos,
~330 líneas generadas. Resultado final: `ghc_gate` verde
(300× roundtrip + 2 propiedades, `cabal test` pass).

## Resumen ejecutivo

El bucle *puede* completarse, pero el pipeline estrella (suggest →
quickcheck → property-store auto-persist) **nunca funcionó end-to-end**.
Las properties que sí corrieron (y encontraron bugs reales: counterexample
`EVar ""`) lo hicieron por `cabal test` vía `ghc_gate`, es decir, por el
camino que cualquier agente sin MCP ya tiene. El valor neto del MCP en esta
sesión quedó en: scaffold + stubs + compile-verify de promotes + gates
agregados + sobre todo **en la honestidad del envelope cuando funciona**.

## Métricas

| Métrica | Valor |
|---|---|
| Llamadas MCP (harness, 3 daemons) | ~50 (47 contadas por el server + restarts) |
| Tools usadas / 36 | **14 (39%)** |
| Tool más usada | `ghc_scratch` (16) |
| Protocol breaks forzados por diseño | **5** (imports a mano ×3, instances Arbitrary a mano, Spec.hs a mano) |
| Bugs del MCP encontrados | 24 numerados (F1–F27, ver abajo) |
| Properties persistidas por el MCP | **0** (las 300 passes fueron vía cabal test) |
| Wall time sesión | ~42 min (incl. 10 min cold-start y 12 min perdidos en batches zombies) |

## Flujo seguido (el prescripto por initialize.instructions)

1. `ghc_workflow(status)` → `ghc_toolchain(status)` → `ghc_workflow(help)`
2. `ghc_project(create)` → `ghc_batch` chain: deps → modules → load
3. Authoría: `ghc_scratch(write)` → `check` → `promote` por módulo
4. `ghc_check_project` / `ghc_check_module` gates
5. `ghc_suggest` → `ghc_arbitrary` → `ghc_quickcheck` (pipeline roto, ver F22–F26)
6. Rescate: properties reales en `test/Spec.hs` + `ghc_gate`
7. `ghc_workflow(post-mortem)`

## Hallazgos

### Críticos (bloquean o matan la confianza)

- **F13/F15 — Transport zombie**: una línea JSON-RPC >~4–8KB mata el thread
  lector del transport. El daemon queda vivo (fifo abierta, writes
  aceptados) pero nunca procesa nada: sin error, sin log, sin exit. Todo
  request posterior se pierde en silencio. Reprodujo 2 veces; threshold
  exacto pendiente (autopsia: `Mcp/Transport.hs`).
- **F14 — Cold-start de 10 minutos**: primer tool-call tras boot con
  proyecto cabal real = 592s (compila deps del proyecto vía subprocess
  cabal, daemon a 0% CPU mientras). Sin señal de progreso: un agente
  concluye «MCP muerto» (nos pasó). Warm: 14ms.
- **F23 — Sugerencia ill-typed con confianza HIGH**: el motor roundtrip
  generó `prettyExpr (parseExpr x) == Right x` (al revés; ni tipa).
  El flujo prescripto es copiar-la-tal-cual a quickcheck → falla.
- **F24 — Wiring auto-infligido**: el contexto de compile de
  `ghc_quickcheck` no resuelve los módulos que el propio `ghc_modules(add)`
  registró (`Hm.Lexer/Hm.Syntax not in other-modules`,
  `Could not find module HmOpencode`).
- **F25 — Contexto single-module**: properties cross-module (Arbitrary en
  Spec, parseExpr en Parser, prettyExpr en Pretty) no compilan bajo ningún
  `module=` posible. El caso de uso canónico del README (evaluador de 5
  módulos) sería igual de imposible.
- **F26 — Claim arquitectural falso**: «no subprocess GHCi», pero existe
  `.haskell-flows/shim/ghc-shim.sh` que intercepta `ghc --interactive`
  (captura argv y exit 0). El modelo es mixto shim+subprocess, no
  in-process puro (ver `docs/GHC-API-rewrite-plan.md`).
- **F20 — Promote fantasma**: `scratch(promote)` de una entry **inexistente**
  devuelve `ok` sin hacer nada (nos pasó con hm-lexer/pretty/eval tras
  batches fail-fast). Mentira piadosa del envelope.

### Mayores (erosionan el bucle)

- **F16 — Deadlock de imports (el hallazgo central de UX)**:
  `add_import` solo inyecta al contexto GHCi y su remediation textual es
  «paste the import line at the top of your .hs file» — violando su propio
  invariant «never edit .hs by hand». Combinado con F9 (promote no splicea
  los `imports` de la entry) ⇒ **no existe camino bendito para crear un
  módulo que necesite imports nuevos**. Forzó 3 de los 5 protocol breaks.
- **F9 — Asimetría check→promote**: `scratch(check)` aplica los imports de
  la entry; `promote` los descarta → check verde, promote rojo.
- **F22 — Sibling-aware asimétrico**: el roundtrip dispara desde el parser
  (`suggest(parseExpr)`) pero el printer (`suggest(prettyExpr)`) recibe 0
  sugerencias con hint genérico.
- **F10 — hoogle como gate duro**: `add_import` exige hoogle indexado
  incluso para imports totalmente cualificados (y sin él: status failed en
  el handshake inicial de una máquina fresh).
- **F5 — `project(create)` desparrama el proyecto** en el project-dir
  raíz (sin subdir): 2 proyectos en el mismo dir colisionan.
- **F8 — batch fail-fast sin detalle**: 1 fallo skippea N acciones
  dependientes sin summary visible del corte.

### Menores / UX

- **F1**: `toolchain(status)` = failed por hlint faltante (fresh machine,
  primera impresión = muro). `partial` existe pero no se usa aquí.
- **F2**: staleness warning autocontradictorio (newerBy negativo, «restart
  to pick up» cuando el binario referido es más viejo; valor congelado).
- **F3**: `workflow(help)`: steps genéricos contradictorios con la fase
  (pre-scaffold sugiere ghc_load).
- **F7+**: rollback atómico de promote funciona (verificado 2 veces).
- **F17**: vocabulario de compile inconsistente: `ok` /
  `ok-with-pre-existing-errors` / `None`.
- **F18**: nextStep de failure-path con texto de success («gate is green,
  run ghc_gate») sobre check_project rojo.
- **F19**: warnings = módulo «failed» (3 warnings de Eval → failed): higiene
  mezclada con corrección; «4 failed» cuando había 2 errores reales.
- **F21**: template de `ghc_arbitrary` no cascada a tipos anidados
  (olvida `Arbitrary Literal`) y es paste-based.
- **F27**: `post-mortem.error_streak = 0` tras múltiples failures del
  session; `passed_properties = 0` pese a 300 passes (el accounting solo
  ve sus propias tools).

## Protocol breaks forzados (todos documentados por F16/F21/F24)

1. Imports de 6 módulos escritos a mano (el tool lo prescribe).
2. `Arbitrary` instances pegadas a mano (template paste-based).
3. `test/Spec.hs` reescrito a mano para el rescate vía gate.
4. Fix de `genIdent` dangling (error genuino del agente; el gate lo atrapó).

## Lo que funcionó bien (para no perderlo)

- Envelope uniforme: `status`/`error.kind`/`remediation` — cuando hay error,
  el remediation es accionable (F6+ nos corrigió `target_module` al vuelo).
- `ghc_scratch(check)`: type-check de hipótesis sin tocar disco: rápido y
  claro. El concepto scratch→promote es bueno; la implementación de imports
  lo mata.
- `ghc_gate`: steps per-gate con command/exit/duration/stderr inline. La UX
  de gate más clara de todo el toolkit.
- Diagnósticos GHC estructurados (código, línea, columna, suggested fix)
  en check_module/promote.
- `ghc_arbitrary` template sized/frequency correcto (salvo cascada F21).

## Inputs para la autopsia (Etapa 3)

1. `Mcp/Transport.hs`: reader line-buffer — confirmar límite y excepción
   silenciosa (F13/F15).
2. `Tool/QuickCheck*` + shim: contexto de compile, por qué falla el
   componente test (F24/F26).
3. `Tool/Suggest*` engines: generación del roundtrip invertido (F23),
   asimetría sibling (F22), regex sobre strings de tipo (known).
4. `Tool/Scratch.hs` + `Tool/Refactor.hs`: promote de entries inexistentes
   (F20), imports descartados (F9).
5. `Mcp/NextStep.hs`: routing failure-path (F18).
6. `Tool/AddImport.hs`: por qué hoogle es gate duro (F10).

## Conclusión de E2

Un LLM con este MCP construye Haskell más lento que sin él (por protocol
breaks y zombies), salvo en dos momentos: el **scaffold+stubs+registry**
y el **gate agregado con salida estructurada**. La promesa diferencial
(property-first loop con auto-persist) es hoy infumable en proyectos
multi-módulo reales. La superficie usada real (14 tools) y el patrón de
dependencias entre hallazgos (todo confluye en: authoría de archivos,
contexto de compile, y transport) son la base de la propuesta de
condensación de la Etapa 4.
