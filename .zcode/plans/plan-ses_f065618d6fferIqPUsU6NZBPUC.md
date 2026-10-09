# Plan: haskell-flows MCP 100% funcional — pure-core orgánico

**Objetivo terminal:** e2e 390/390 verde, las 12 tools al 100% sobre ghcide (backend único), `ApiSession.hs` borrado, 2 escenarios hueco creados, y el motor consolidado con ADTs/ncúcleos puros (sin substring-matching, sin JSON-spelunking, sin locks globales). Regla transversal nueva: cada hipótesis de causa raíz se apoya en documentación/fuente oficial (GHC User's Guide, ghcide/hls upstream, cabal docs) antes de implementar.

**Disciplina transversal:** ningún commit sin `scripts/gate.sh t1` verde; T2 antes de pushear; toda corrida e2e/probe bajo hangwatch (veredicto DONE/HANG, nunca bare); commits pequeños por ítem; `KNOWN-RED.md` como ledger vivo.

## Wave 0 — Restaurar la infra de verificación
`gate.sh` hoy no puede correr (hardcodea hangwatch.py en un temp dir vacío).
1. Recrear `hangwatch.py` en `mcp-server-haskell/scripts/` (watchdog: budget TOTAL_S + stall STALL_S sin crecimiento de log → veredicto DONE/HANG/EXIT-n + tail del último paso; kill del process group completo).
2. `WATCHDOG=scripts/hangwatch.py` relativo al repo.
3. Dedupe del canario (corre 2×300s por T1 → 1 vez); T0 falla por exit code de `cabal build` (el grep con `|| true` traga fallos), log en /tmp/gate-build.log.
4. Baseline fresca T0→T1→T2; reconciliar rojos reales vs ledger (header 39 vs ítems 38; familia C header "(9)" vs ítems 14) y corregir KNOWN-RED.md.

## Wave 1 — Aterrizar los fixes in-flight BIEN (2 bloqueantes de la revisión)
1. `findMcpBinaryPath`: función pura `siblingCandidates :: FilePath -> [FilePath]` — subir al dir de paquete (5 niveles, no 4) y bajar `x/haskell-flows-mcp/build/haskell-flows-mcp/haskell-flows-mcp` (+ variante t→x); elegir el primero existente. Property test del layout.
2. Guard #127: enrutar `handleEval` por `Sanitize.sanitizeExpression` (mata segfault RTS por `2^99`, guard de newlines, sentinel duplicado). Mismo borde para `handleType`.
3. `maxEvalBytes` → `maxExpressionBytes` (semántica de input).
4. UTF-8 pre-check extendido a `action=project`.
5. `capOutput :: Int -> Text -> (Text, Bool)` pura (truncated real, hoy hardcodeado False).
6. Label `Main.hs:128` sin `/ doc`; `.zcodeignore` con bench-results bajo lambda-hm realmente ignorado.
7. `ONLY=` por escenario tocado → T1 → commit.

## Wave 2 — Resto familia Envelope/shape
- **ADT `Diag`** (severity/code/range/message) desde `ideDiagnosticsFor'` + `classifyDiag` pura: Found-hole → warning, artifact GHC-58427 "is not loaded" → filtrado (TypedHoles + LoadHoleDiagnostics). Mata KeyMap-spelunking. Property tests: round-trip, todo error lleva código.
- RegressionLoadFailed shape; DiskFull honesto (`failed` o `persisted=false`); RCE contract verificado.

## Wave 3 — Store-lifecycle (10)
1. `probes/store.py` contra fixture (iteración sin suite).
2. **ADT `PropertyKey = ByPath FilePath | ByModule ModuleName`** + `normalizeKey` total (mata la clase #74 GatesProperties/CheckModuleProps).
3. Shapes list/export/audit post-move de renderStored/listResult.
4. Erradicar los 3 locks `unsafePerformIO` (PropertyStore.hs:66, Scratchpad.hs:81, Deps.hs:107) → registro Server; saves atómicos (PropertyStoreRace).

## Wave 4 — Cross-component (14, la profunda) — probe-first
1. `probes/crosscomp.py` (create→add→load→prop, ~30s) — iterar hasta probe verde ANTES de tocar la suite.
2. **ADT `Backend = NotStarted | Warming | Live IdeSession | Draining`** con única puerta `withBackend` (componente descubierto mid-action; deadlock/half-warm irrepresentable).
3. Bytecode-of-target: extender closure `GetLinkable` de `ideInteractiveEnvFor` a la unidad del target (MissingArbitrary).
4. Generalizar warm-and-retry para `.hi` stale (GHC-47808) + dirtying vía `cabalMTimeKeys`.
5. ParityMatrix (6) cae con el fix de familia; verificar aislada Y secuenciada.
6. FP: `AnchorPlan` puro (heurísticas de anclas fuera de IO) + **ADT `EvalError`** total reemplazando `Either Text Text`/substring-matching.

## Wave 5 — Consolidación FP del motor
`EvalError` en ideEvalExprIn/ideTypeOfExprIn/ideInteractiveEnvFor con mapping total a ErrorKind; parser cabal puro extraído (`Parser/Cabal.hs`) + property tests; `capOutput` global (eval + subprocess gate).

## Wave 6 — C2: migrar ~15 verbos legacy y borrar ApiSession
Riesgo ascendente, cada uno con escenario pineado + T1 + commit:
1. `ghc_inspect{info,complete,goto,browse,hole}` (introspección read-only, receta addRdrEnv)
2. `ghc_session(imports)`
3. `ghc_suggest` (Rules.hs ya puro)
4. `ghc_module` scratch {check,show,promote}
5. `ghc_property{arbitrary,audit}`
6. `ghc_edit{rename_local,extract_binding}` (Rename/Extract ya núcleos puros)
7. `ghc_edit{move_symbol,import}`
8. Borrado final: ApiSession.hs, srvGhcSession, teSession, evict/killGhcSession → colapso al ADT Backend. Superficie sigue en 12 tools (golden intacto).

## Wave 7 — Escenarios hueco + cierre
Poison-recovery (excepción mid-flow → próximo boot limpio; pinea Backend ADT) y multi-unit lifecycle (lib+test+exe, module-add mid-sesión; pinea Wave 4). T2 → 390/390; retirar KNOWN-RED.md; arreglar TOOL_TAXONOMY.md (13→12, doc cortado) y header obsoleto de IdeBacked.hs. Push final.

**No hacemos:** cambio de mónada base, effect systems, big-bang rewrite. Si Wave 4 resulta upstream (bug ghcide/GHC): mitigación + escenario documentando el upstream, sin bloquear waves 5-7.