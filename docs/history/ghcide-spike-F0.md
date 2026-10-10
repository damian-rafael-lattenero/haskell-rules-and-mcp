# F0 — Spike ghcide-as-library: VEREDICTO PASS (2026-10-02)

`mcp-server-haskell/spike-ghcide/` — reproducer ejecutable:
`cabal run exe:spike-ghcide -- target` → imprime el gate A/B/C.

## Resultado

```
== [A] embed + diagnostics      PASS   (2 typechecks: 0.91s; IdeCommand boot: ~10ms)
== [B] per-component sessions   PASS   (QuickCheck: test=OK / lib=hidden-package)
== [C] in-process execution     PASS   (pure 1+1→2 en lib; IO sample' QuickCheck en test)
== total spike wall time: 2.2s
```

**Qué prueba B (el corazón del gate):** la sesión del componente
`test:spike-test` resuelve `Test.QuickCheck` y la del componente
`lib:spike-target` NO ("member of the hidden package"). Esto es
exactamente la "cabal-aware package resolution" que el plan de rewrite
original documentó como blocker y que hoy rompe `ghc_quickcheck` en
multi-módulo (F24 del dogfood). ghcide 2.15 + hie-bios 0.21 la entrega.

## Recetas API verificadas (input directo de F1)

1. **Embed sin LSP**: `Development.IDE.Main` con
   `defaultArguments recorder root mempty` y
   `argCommand = Custom (IdeCommand (ideState -> IO ()))`.
   `defaultMain` bloquea; el IdeCommand recibe la IdeState viva.
2. **Recorder**: `makeDefaultStderrRecorder Nothing` +
   `cmapWithPrio (pretty :: Log -> Doc ())` para obtener
   `Recorder (WithPriority Log)`.
3. **Cradle**: hie.yaml EXPLÍCITO (`cradle: cabal:` con path→component).
   Sin hie.yaml el implicit-cradle degradó a direct/base-only con
   diagnósticos de cradle-error vacíos. ⇒ **F1: el scaffold del MCP debe
   generar hie.yaml siempre**.
4. **Sesión para eval/typecheck interactivo**: regla `GhcSessionDeps`
   (NO `GhcSession` — con GhcSession el setContext muere con
   `lookupFinderCache: tried to lookup home module`).
5. **Runner**: `Development.IDE.GHC.Util.evalGhcEnv :: HscEnv -> Ghc b -> IO b`
   + `setContext [IIDecl <target>, IIDecl Prelude]` (Prelude NO está
   implícito en ese contexto) + `compileExpr` + `unsafeCoerce`.
6. **Diagnósticos**: `getDiagnostics :: IdeState -> STM [FileDiagnostic]`
   (¡STM!), `FileDiagnostic` es record (`fdFilePath`, `fdLspDiagnostic`);
   severidad via `_severity == Just DiagnosticSeverity_Error`.
7. **Timeouts**: `System.Timeout` funciona sobre `evalGhcEnv`+compileExpr
   (la ejecución QuickCheck IO corrió bajo el mismo budget de 30s).

## Métricas de arranque (M1, para comparar contra el MCP actual)

| Métrica | MCP actual (ApiSession) | Spike ghcide |
|---|---|---|
| Boot hasta tool usable | 592s (cold, compila deps via subprocess) | ~1s (cradle cached) |
| Typecheck módulo simple | ~300-3000ms | ~450ms c/u |
| Diagnósticos | regex sobre stderr | estructurados (severity/range/código) |
| Componentes | 1 solo (lib) — test imposible | N componentes via hie.yaml |

## Bugs propios atrapados durante el spike (mérito del gate)

- `generate` es `IO`, no pura (B falló hasta notarlo).
- `unGen`/`arbitrary` no re-exportados por `Test.QuickCheck`.
- `setContext` sin Prelude → `Int` fuera de scope.

## Decisión de gate (según plan F0)

A+B+C verdes ⇒ **seguir con F1** (adaptador IdeSession + flag
`HASKELL_FLOWS_BACKEND=ghcide`). El fallback HLS-subprocess NO se
necesita. Nota de alcance: falta medir el cold-start CON compilación de
deps del proyecto (acá el target ya estaba built — el warmup de F2
debe cubrir ese caso con progreso observable).
