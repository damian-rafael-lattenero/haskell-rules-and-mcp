# Plan: Cierre oficial haskell-flows MCP — v1.0.0

**Objetivo terminal:** declarar la versión oficial 1.0.0 y publicarla. Concretamente: (1) fixes de los 5 hallazgos de dogfood con tests de regresión pineados, (2) nueva verb `ghc_module(action=write_module)` (cierra el gap E4 de autoría), (3) **triple dogfood verificable** (driver crudo HM + segundo dogfood chico + sesión con agente real opencode) con métrica E4 verde documentada, (4) bootstrap del usuario (`--doctor`, `docs/requirements.md`, scaffold con QuickCheck, quickstart en README), (5) README/docs minimalistas y sobrios + limpieza liviana de la raíz, (6) GitHub Actions sobrio (1 job por push, deep-checks semanales, release por tag), (7) tag `v1.0.0` + GitHub Release con binarios verificados (darwin-arm64, linux-x64, sdist, sha256). Encuadre: **cierre estable sin roadmap** — "v1.0.0 — stable, maintenance only". Windows: declarado no-soportado, honestamente.

**Respuesta a la pregunta de Maven:** Haskell tiene Hackage (equivalente a Maven Central, publica *sdists* fuente con `cabal upload`). Para un binario el estándar es tag + GitHub Release; la versión canónica vive en el `.cabal` (`version: 1.0.0.0`). Hackage queda como opción futura, fuera del cierre.

**Requisitos runtime (verificados en código, base del bootstrap):** duros = `ghc` + `cabal` (ghcide embebido resuelve el proyecto vía hie-bios→cabal; sin cabal las tools IDE fallan con cradle error). Por feature con degradación honesta = `hlint` (lint), `fourmolu`/`ormolu` (format), `hoogle` (import resolution). QuickCheck es per-project vía cabal — el scaffold de create hoy NO lo incluye (gap que corrige 1.5). El server ya auto-augmenta PATH (`~/.ghcup/bin`, `~/.cabal/bin`, `~/.local/bin`, homebrew) en `Mcp/PathBootstrap.hs`.

**Disciplina transversal (ya vigente):** ningún commit sin `mcp-server-haskell/scripts/gate.sh t1` verde; T2 antes de pushear; toda corrida e2e/probe bajo `hangwatch` (veredicto DONE/HANG, nunca bare); commits chicos por ítem; hipótesis de causa raíz apoyadas en fuente oficial (ej.: ghcide adjunta los diagnósticos del parser a la regla `GetParsedModule` — base del fix 1.2).

---

## Wave 1 — Aterrizar lo pendiente + fixes de honestidad (server)

**1.0 — Commit del trabajo in-flight.** Limpiar la instrumentación DEBUG de `handleEval` (`src/HaskellFlows/Tool/IdeBacked.hs:800-845`: restaurar `firstRight evalRetry`, sacar logging EVAL-DEBUG — la versión limpia llega en 1.4). Commitear: FlowNestedModuleEval.hs + registro en Main.hs + entrada cabal + `scripts/dogfood/mcpd.py` (repo-portable: spool como argumento). T1 → commit.

**1.1 — ghc_check sin `action` → project.** `routeIde` (`IdeBacked.hs:188-193`): hoy sin action cae al backstop. Cambiar `otherwise` → `handleCheckProject` (las instructions prometen `ghc_check((no args))`). Schema/descripción en `Tool/Check.hs` + golden `test/golden/tools-list.json` + check e2e `ghc_check {}`. T1 → commit.

**1.2 — Ceguera a errores de parseo.** `ideDiagnosticsFor'` (`src/HaskellFlows/Ghc/IdeSession.hs:332-366`) lee solo rule-diags de `TypeCheck` y `GetModSummary`; el parser publica los suyos bajo `GetParsedModule` (no importado). Fix: importarlo, forzar con `use` explícito, concatenar (`tc <> ms <> parsed`). Regresión e2e FlowParseHonesty: módulo parse-broken → `check(action=module)` Y `check(action=project)` fallan (`overall=false`); promote con verify sobre parse-broken hace rollback. T1 → commit.

**1.3 — Sweep de guías/hints con tools retiradas.** Referencias **prescriptivas** → vocabulario de las 12 tools; se conservan las menciones históricas "Wave 2b successor to …". Archivos: `Mcp/Guidance.hs:173-205` + `:238-315` (handshake ghc_workflow/ghc_toolchain → `ghc_session(action=status/help)`; ghc_refactor → ghc_edit; liveness ghc_quickcheck/ghc_property_store → ghc_property); `Tool/Suggest.hs:226-248` (hint "via ghc_load"); `Tool/Goto.hs:229-237`; `Tool/Info.hs:366-374`; `Tool/Workflow.hs` (claves `ghciAlive`→`sessionAlive` :541,624; scratchpad hints :574-584; loadStep/steps :637-667); `Mcp/WorkflowState.hs:238-364`; `Mcp/NextStep.hs:601-721`; `Tool/Deps.hs:148-151,540-552`; `Tool/CreateProject.hs:401-412` (hint que enseñe `path=` + auto-switch #256); `Tool/RemoveModules.hs`; `Tool/AddImport.hs`; `Tool/FixWarning.hs`; `Tool/Project.hs:164-167`; `Tool/Batch.hs:73`. Tests pineados a actualizar EN EL MISMO commit: `test/Spec/SuggestLaws.hs:128`, `WorkflowState.hs:209,313`, `WorkflowTool.hs:66`, `Progress.hs:106,116`, anclajes de Spec.hs. Golden en lockstep. `scripts/check-rules-freshness.sh:70-84`: extender `RETIRED_NAMES` con wave-2b (ghc_workflow, ghc_toolchain, ghc_load, ghc_refactor, ghc_scratch, ghc_quickcheck, ghc_property_store, ghc_check_module, ghc_check_project, ghc_add_import, ghc_modules, ghc_imports, ghc_hole, ghc_type, ghc_info, hoogle_search, ghc_fix_warning…). Cosmético: títulos e2e `Main.hs:179,269`. T1 → commit(s).

**1.4 — eval/suggest: surfacear causa real del ancla.** Reemplazo limpio del DEBUG: fold que recolecta `(anchor, error)` de la cadena; si ninguna ancla tipa, elegir el primer error no-de-scope (p.ej. BadDependency) y adjuntarlo al mensaje agregado. Helper puro unit-testeado. `IdeBacked.hs:800-845` (handleEval) y `:1089-1115` (handleSuggest). T1 → commit.

**1.5 — Scaffold con QuickCheck (bootstrap del property-loop).** `Tool/CreateProject.hs` `cabalFile` ~:355-365: agregar `QuickCheck >= 2.14` al build-depends del test-suite scaffold. Actualizar unit tests que asertan contenido del cabal (`test/Spec/CreateProject.hs`). T1 → commit.

## Wave 2 — `ghc_module(action=write_module)` (la única feature nueva de superficie)

Escribir un módulo completo del proyecto (nuevo o sobrescribir) con la disciplina de promote: snapshot + write + compile-verify + rollback.
- `Mcp/Action.hs:248-257`: verb `write_module` en el enum (`asExtraVerbs`), schema en `Tool/ModuleMgmt.hs` (campos `module`, `code`).
- Handler siguiendo el patrón promote: intercept en `routeIde` (`IdeBacked.hs:205-210`) → escritura con verify vía sesión (misma disciplina de sesión que promote/add).
- Tests: unit (snapshot/rollback puro) + e2e FlowWriteModule: módulo nuevo compila y aparece en check(project); sobrescribir existente; código roto → rollback + failed honesto.
- Golden `tools-list.json` + `docs/TOOL_TAXONOMY.md` (12 tools se mantienen, verb documentado) — ambos CI-enforced. T1 → commit.

## Wave 3 — Triple dogfood (evidencia comprobable, requisito para taggear)

**3.1 Driver HM (el principal).** Fix del driver: `body_of()` en `mcpd.py` strippea el header de módulo completo (export lists multi-línea) — split tras la línea `) where`. Reset `playground/hm-dogfood-2/` + spool; loop completo HM **solo vía MCP**: `--doctor` smoke → create(path=) → add modules → write_module/promote (Syntax/Lexer/Parser/Pretty/Typecheck/Eval) → eval (flat y anidado) → ≥3 propiedades persistidas vía `ghc_property` (verifica el fix 1.5: properties sin paso extra) → `ghc_check(action=project)` verde. Métrica E4 desde metrics.log: **≤25 calls, 0 breaks de protocolo, 0 zombies, ≥3 props persistidas, cold-start ≤60s** (línea base previa ~50/5/2/0/592s).

**3.2 Segundo dogfood chico (arithmetic-expr).** Proyecto pequeño (~10-15 calls): parser de expresiones aritméticas con 1-2 propiedades (round-trip pretty/parse), mismo protocolo solo-MCP. Valida que el loop no es específico del caso HM.

**3.3 Sesión con agente real (opencode).** Configurar el MCP en scope del proyecto dogfood (opencode ya existe en el repo — `opencode.json`), correr una sesión scripted (opencode run) con la tarea HM-lite, capturar el transcript. Scorificar E4 desde el transcript: ¿el agente siguió el handshake de las instructions? ¿llamó solo tools vigentes? ¿persistió properties? Este es el test de verdad de las instructions/hints que arreglamos en 1.3.

**3.4 Documento.** `docs/dogfood-2026-10-<fecha>.md`: tabla de métricas E4 de los tres runs + hallazgos→fixes. T2 → commit + push.

## Wave 4 — Release engineering 1.0.0

1. **Versión**: `haskell-flows-mcp.cabal` → `version: 1.0.0.0` + description honesta (12 tools, motor ghcide, stdio). `app/Main.hs:37-38`: `--version` hoy hardcodea "0.1.0.0" → alinear a 1.0.0.0 (single-source: constante de versión en un módulo). `cabal check` verde.
2. **CHANGELOG.md**: entrada `## [1.0.0]` con el arco real (36→12 condensación E4, ghcide único backend, ApiSession borrado, fixes de dogfood, write_module, --doctor, scaffold QuickCheck, breaking: superficie reducida con mapeo sucesores→TOOL_TAXONOMY). Limpiar `[Unreleased]`.
3. **Un solo flujo de release**: borrar `haskell-ci.yml` (incluye su job release embebido duplicado); `release.yml` queda como único (matrix darwin-arm64/linux-x64 + tar.gz + sha256 + sdist + publish). Naming unificado `haskell-flows-mcp-{darwin-arm64,linux-x64}.tar.gz`; `docs/install.md` y README deben coincidir.
4. **CI sobrio**: `ci.yml` — push/PR a master (path-filter): 1 job ubuntu con el contenedor GHCR existente, GHC más nuevo, sin shards: build + unit + e2e + hlint + `cabal check`, `timeout-minutes: 45`. `weekly.yml` — domingo: matrix 2 GHC + coverage (ratchet) + bench gate + flakiness + package-quality; reemplaza y borra `bench-nightly.yml` + `flakiness-nightly.yml`. Se mantienen `release.yml`, `build-ci-image.yml`, `nix-flake.yml`. Actualizar required checks de branch protection (gh api).
5. Verificar el CI nuevo verde en master antes de seguir.

## Wave 5 — Bootstrap del usuario (primera corrida verificable)

1. **`--doctor` flag** (`app/Main.hs`): reutiliza las probes de `Tool/ToolchainStatus.hs` (gates: cabal/ghc/hlint; opcionales: fourmolu/ormolu/hoogle) e imprime tabla sobria ✓/✗ + remediation de una línea (ghcup/cabal install), exit 0/1. Es la respuesta al "archivo de inicialización": vive dentro del binario, no puede desincronizarse. Unit tests del render.
2. **`docs/requirements.md`** (nuevo, sobrio): tabla de requisitos duros (GHC 9.10–9.12 + cabal vía ghcup, una línea de curl) vs opcionales por feature (hlint→lint, fourmolu→format, hoogle→import resolution), nota QuickCheck per-project (incluido en scaffolds desde 1.5; `ghc_deps add QuickCheck` para proyectos existentes), PATH auto-bootstrap, tabla de env vars (`HASKELL_PROJECT_DIR` + knobs de timeouts), verificación en 2 pasos (`--doctor` → `ghc_session(action="toolchain")`).
3. T1 → commit.

## Wave 6 — README/docs sobrios + limpieza de raíz

1. **README raíz reescrito** (~90 líneas, sin emojis ni marketing): qué es en un párrafo (MCP server for property-first Haskell development, 12 tools sobre ghcide), badges sobrios (CI, release, license), **Status: "v1.0.0 — stable. Maintenance only: bugfixes y compatibilidad de GHC; no feature roadmap"**, tabla de las 12 tools, quickstart de 4 pasos (prereqs ghcup → descargar binario o `cabal install` → `--doctor` → config MCP JSON), loop property-first en 4 pasos, verificación (tiers t0/t1/t2 + conteo e2e), arquitectura en ~6 líneas, plataformas soportadas con "Windows: not supported" explícito, links a docs/ (incluido requirements.md), licencia BSD-3.
2. **`mcp-server-haskell/README.md` reescrito**: espejo operativo (install, wiring, referencia corta de las 12 tools con actions, dev loop con gate.sh, layout sin ApiSession ni cifras stale).
3. **docs/**: crear `docs/history/` y mover lo histórico (GHC-API-rewrite-plan, ghcide-spike-F0, ghcide-backend-F1, bench-cold-start, dogfood-2026-04-19×2, dogfood-2026-05-01, dogfood-2026-05-25, haskell-flows-mcp.pdf). Corregir cifras en `install.md` (12 tools + naming de assets + link a requirements.md), `flows.md` (12), `PERFORMANCE.md` (12), `.claude/rules/haskell-flows-mcp.md` (12). `TOOL_TAXONOMY.md` ya correcto. Actualizar links.
4. **Limpieza liviana de la raíz** (antes de mover/ignorar: grep en CI/tests/scripts para confirmar que nada los referencie): `.gitignore` del scratch de playground (hm-dogfood-2 y derivados regenerables), `spike-ghcide/` → `docs/history/spike-ghcide/`. Quedan en la raíz: mcp-server-haskell/, docs/, scripts/, bench/, playground/ (solo fixtures si algo los usa), archivos de gobernanza, flake.nix (se mantiene).
5. Commits por grupo (READMEs / docs history / cifras / limpieza). T1 → commit.

## Wave 7 — Freeze y publicación

1. `gate.sh t1` + `t2` completo verde bajo hangwatch (100% con los checks nuevos).
2. Push a master; CI sobrio verde.
3. CHANGELOG con fecha final → commit → **tag anotado `v1.0.0`** → push del tag → `release.yml` corre.
4. Verificación de release (comprobable): `gh release view v1.0.0` con assets (2 binarios + sdist + checksums); `curl` + `shasum -a 256 -c` desde /tmp; `./haskell-flows-mcp --version` = 1.0.0.0; `--doctor` del binario descargado; smoke stdio (initialize + tools/list).
5. Reporte final: tabla E4 (3 runs), conteo de tests, links de la release.

## Criterios de éxito (verificables)

- T2 100% verde con nuevos checks de regresión (parse-honesty, default-action, write_module, hints, doctor, scaffold QuickCheck).
- Triple dogfood E4 verde: ≤25 calls / 0 breaks / 0 zombies / ≥3 props persistidas / cold-start ≤60s (driver HM); dogfood chico ídem proporcional; sesión opencode sigue el handshake sin llamar tools retiradas.
- `cabal check` limpio, golden tools-list consistente, `--version` y `--doctor` funcionan en el binario release.
- CI: 1 job por push (~15-20 min), deep semanal, un solo flujo de release.
- Release v1.0.0 con assets descargables y verificables por checksum (repara el antecedente de v0.1.0 con cero assets).
- README/docs sobrios sin nombres retirados en texto prescriptivo ni cifras contradictorias; bootstrap documentado end-to-end (prereqs → doctor → MCP config); raíz del repo limpia.

## Riesgos conocidos

- El fix de parse-errors puede descubrir diagnósticos en escenarios hoy verdes → T2 y reconciliar.
- Golden `tools-list.json` toca cualquier cambio de schema/descripción → lockstep (tests CI-enforced lo exigen).
- Branch protection: required checks cambian con el CI nuevo → actualizar con `gh api` sin dejar el repo bloqueado.
- `ghc_edit(action=import)` depende de hoogle opcional: si un dogfood lo ejercita sin hoogle local, esperar `unavailable` y documentarlo (no instalar hoogle global para no ensuciar).
- La sesión opencode depende del modelo/agente disponible ese día: si no se puede correr, degradar a driver crudo documentando el intento — no bloquea el tag, pero se reporta honestamente.
- Mover archivos trackeados (spike-ghcide, docs históricos) con `git mv` para conservar historia.