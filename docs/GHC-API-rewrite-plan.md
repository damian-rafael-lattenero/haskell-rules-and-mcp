# Plan: migrar el MCP de subprocess `ghci` a GHC API in-process

> Documento self-contained. Leer de principio a fin antes de empezar.
> No requiere memoria de sesiones anteriores.

## Contexto

**Qué es este repo**: [haskell-rules-and-mcp](https://github.com/damian-rafael-lattenero/haskell-rules-and-mcp) es un MCP server en Haskell para desarrollo Haskell con LLMs (Claude Code / Cursor / etc). Expone ~25 tools (ghci_load, ghci_eval, ghci_check_module, ghci_refactor, ghci_deps, ghci_quickcheck, etc.) que envuelven la toolchain Haskell (GHCi, cabal, hlint, hoogle).

**Estado actual (master @ 060caca+)**: 233 checks e2e verdes, 4 bugs reales cazados (A/D/G/H documentados en git log), 11 scenarios adversariales. Suite robusta. Dev loop: 127s con `HASKELL_FLOWS_E2E_SKIP_SLOW=1`; CI: ~200-220s. Intentamos paralelismo — fallido por limitaciones de cabal repl. Deuda documentada en `docs/TODO-parallel-e2e.md` (ese doc queda obsoleto cuando este plan se ejecute).

**Qué vamos a hacer acá**: reemplazar el subprocess `cabal repl` / `ghci` por la **GHC API in-process** (mismo patrón que HLS y ghcid). Eso habilita 7 beneficios UX concretos para usuarios agentes LLM, no solo speed-up de tests.

## Beneficios target (los 7 que justifican el rewrite)

| # | Beneficio | Cuantificación |
|---|---|---|
| 1 | Cold-start de session: 5-8s → <1s | Cada primer call en session nueva |
| 2 | Errores estructurados (SourceError tipado) vs regex-sobre-stdout | Mejor diagnóstico → mejores fixes del LLM |
| 3 | Paralelismo real de tool calls del LLM | N HscEnv en paralelo, sin contención |
| 4 | Reload incremental (solo módulos que cambiaron) | Proyectos medianos-grandes: iteración más rápida |
| 5 | Multi-session support (N sesiones aisladas) | Features futuras: compare branches, etc. |
| 6 | Introspección más precisa (lookupName vs parsear `:i`) | Menos ambigüedad en info tool |
| 7 | Cross-GHC robustness (GHC API estable) | Menos casos especiales en parsers |

**Riesgos aceptados**: ~2000-3000 LOC de rewrite, dependencia directa del paquete `ghc` (ata a versiones específicas de GHC), riesgo de regresiones de comportamiento en 25 tools.

## La arquitectura actual (antes del rewrite)

```
┌─ MCP Server (Haskell proc) ──────────────────┐
│  ┌─ MVar (Maybe Session) ────────────┐      │
│  │  ┌─ Session ────────────────────┐ │      │
│  │  │  ProcessHandle (cabal repl) │ │      │
│  │  │  stdin/stdout/stderr pipes  │ │      │
│  │  │  TVar Text buffer           │ │      │
│  │  │  TVar SessionStatus         │ │      │
│  │  │  TMVar () (lock)            │ │      │
│  │  └─────────────────────────────┘ │      │
│  └──────────────────────────────────┘      │
│           ▲                                  │
│           │ all 25 tools share 1 session    │
│           │                                  │
│  ghci_type → `:t expr` → parse stdout       │
│  ghci_info → `:i name` → parse stdout       │
│  ghci_load → `:l path` → parse diagnostics  │
│  ghci_eval → `expr`    → parse output       │
│  (etc.)                                      │
└──────────────────────────────────────────────┘
           │ sentinel-framed over stdio
           ▼
       [cabal repl → ghci subprocess]
```

**Archivos claves actuales**:
- `mcp-server-haskell/src/HaskellFlows/Ghci/Session.hs` — Session type + startSession + executeNoLock + drainHandle. 480 LOC.
- `mcp-server-haskell/src/HaskellFlows/Ghci/Sentinel.hs` — protocolo de framing (sentinel string).
- `mcp-server-haskell/src/HaskellFlows/Mcp/Server.hs` — tiene `srvSession :: MVar (Maybe Session)` + getOrStartSession + evictSession.
- `mcp-server-haskell/src/HaskellFlows/Tool/*.hs` — 25 tools, cada uno importa Session y llama `execute`, `loadModuleWith`, `typeOf`, `infoOf`, `evaluate`, `runProperty`.
- `mcp-server-haskell/src/HaskellFlows/Parser/*.hs` — parsers de output de ghci (Error, Hole, Type, Coverage, QuickCheck, TypeSignature).

**Lo que vamos a preservar**:
- El `Session` type (opaque) y sus invariants (Alive/Overflowed/Dead)
- La API de tools (JSON schemas + tool names) — **cero breaking change** para el cliente LLM
- El harness de tests e2e (216+ checks)

**Lo que vamos a reemplazar**:
- El productor de Session: `startSession` pasa de "spawn cabal repl" a "boot HscEnv in-process"
- Los parsers que regex-matchean stdout de ghci: pasan a construir JSON a partir de estructuras GHC API

## Arquitectura target (post-rewrite)

```
┌─ MCP Server ─────────────────────────────────────────┐
│  ┌─ MVar (Maybe GhcSession) ─────────────────┐      │
│  │  ┌─ GhcSession ──────────────────────────┐│      │
│  │  │  IORef HscEnv (GHC.API state)        ││      │
│  │  │  IORef (Map ModuleName ModSummary)   ││      │
│  │  │  MVar () (lock — still needed)       ││      │
│  │  │  Optional: Session (fallback ghci    ││      │
│  │  │    subprocess for ghci_quickcheck,   ││      │
│  │  │    ghci_regression, ghci_determinism)││      │
│  │  └──────────────────────────────────────┘│      │
│  └──────────────────────────────────────────┘      │
│                                                      │
│  ghci_type → GHC.exprType → render PprStyle         │
│  ghci_info → GHC.lookupName → structured JSON       │
│  ghci_load → GHC.load → SourceError[] → JSON        │
│  ghci_eval → GHC.compileParsedExpr + run → JSON     │
│  ghci_quickcheck → subprocess ghci (dual path)      │
│  (etc.)                                              │
└──────────────────────────────────────────────────────┘
```

**El dual-path para QuickCheck/Regression/Determinism** es la concesión arquitectónica: QuickCheck necesita runtime execution, randomización, shrinking — todo eso funciona mejor en un subprocess `ghci` que in-process GHC API. Los otros 22 tools migran a full in-process.

## Fases — ejecución multi-sesión

Cada fase es **commiteable independiente** y deja master en estado verde. Las fases son aditivas: la nueva infra convive con la vieja hasta la Fase 7 (cleanup).

### Fase 0 — Spike GHC API (1 sesión, 1-2h)

**Goal**: probar que podemos bootear un `HscEnv` in-process, cargar un módulo, y ejecutar una query típica (`:t expr`).

**Pasos**:
1. Crear branch `claude/ghc-api-spike`.
2. Agregar a `haskell-flows-mcp.cabal` (library stanza):
   ```
   , ghc               >= 9.10 && < 9.13
   , ghc-paths          >= 0.1 && < 0.2
   ```
   `ghc-paths` da el libdir default sin hardcodearlo.
3. Crear `mcp-server-haskell/spike/GhcApiSpike.hs`:
   ```haskell
   module Main where
   import GHC
   import GHC.Paths (libdir)
   import GHC.Driver.Session (DynFlags(..))
   import Control.Monad.IO.Class (liftIO)

   main :: IO ()
   main = runGhc (Just libdir) $ do
     dflags <- getSessionDynFlags
     _ <- setSessionDynFlags dflags
     -- Add a target (a Haskell source file)
     target <- guessTarget "spike-target/src/Demo.hs" Nothing Nothing
     setTargets [target]
     _ <- load LoadAllTargets
     -- Query the type of an expression
     ty <- exprType TM_Inst "map (+1)"
     liftIO $ putStrLn $ "exprType result: " ++ show ty
   ```
4. Crear un target mínimo `spike-target/src/Demo.hs`:
   ```haskell
   module Demo (greet) where
   greet :: String -> String
   greet x = "Hello, " ++ x
   ```
5. Compilar + ejecutar. Verificar que `exprType` devuelve algo razonable.

**Criterio de éxito**: el spike imprime el tipo de `map (+1)` sin crash. Confirma que:
- El paquete `ghc` compila con nuestra toolchain (GHC 9.12.2 + cabal 3.12.1.0)
- `runGhc Nothing libdir` arranca un HscEnv
- `load LoadAllTargets` funciona sobre un módulo simple
- `exprType` devuelve una estructura tipada

**Si falla**:
- Si `ghc` no resuelve: probar pin explícito a la versión del GHC instalado (`ghc == 9.12.2`).
- Si `libdir` no encontrado: `GHC.Paths.libdir` debería dar el path; alternativa es env var `NIX_GHC_LIBDIR` o detectarlo via `ghc --print-libdir`.
- Si `load` falla con "cannot find Demo": `targetContents` argument es `Nothing`, puede requerir pasar el source explícitamente.

**Deliverable**: branch con spike funcionando, screenshot/log del output. No merge.

### Fase 1 — Infraestructura `GhcSession` paralela (1 sesión, 3-4h)

**Goal**: crear el nuevo `GhcSession` type + `startGhcSession` / `killGhcSession` **junto al** `Session` existente. Server sostiene ambos. Ninguna tool migra todavía.

**Archivos a crear**:
- `mcp-server-haskell/src/HaskellFlows/Ghc/ApiSession.hs` — módulo nuevo con:
  ```haskell
  data GhcSession = GhcSession
    { gsEnvRef   :: !(IORef HscEnv)    -- mutable GHC API state
    , gsLibdir   :: !FilePath
    , gsLock     :: !(MVar ())          -- serialise writes to envRef
    , gsProject  :: !ProjectDir
    }

  startGhcSession :: ProjectDir -> IO GhcSession
  killGhcSession  :: GhcSession -> IO ()
  withGhcSession  :: GhcSession -> Ghc a -> IO a
  -- withGhcSession corre un Ghc action bajo el lock, lee el HscEnv,
  -- ejecuta, graba el nuevo HscEnv en el IORef
  ```

**Archivos a modificar**:
- `mcp-server-haskell/src/HaskellFlows/Mcp/Server.hs`:
  ```haskell
  data Server = Server
    { ...
    , srvSession      :: !(MVar (Maybe Session))      -- legacy
    , srvGhcSession   :: !(MVar (Maybe GhcSession))   -- new
    , ...
    }
  ```
  + `getOrStartGhcSession` / `evictGhcSession` análogos a los de Session.
- `mcp-server-haskell/haskell-flows-mcp.cabal`: agregar el módulo nuevo a `exposed-modules`.

**Criterio de éxito**:
- El build es verde
- Los 233 tests e2e siguen pasando (nadie usa `GhcSession` todavía)
- Un unit test nuevo en `test/Spec.hs` demuestra que se puede hacer startGhcSession → exprType → killGhcSession

**Estimación**: 3-4h. El 80% es wiring de boilerplate; la miga real es entender los DynFlags correctos para nuestro target.

### Fase 2 — Migrar tools simples read-only (1 sesión, 4-5h)

> **Finding (post-Fase-1 derisk, master @ e56bb58)**: portar un tool
> "read-only" aislado rompe los scenarios e2e que encadenan
> `ghci_load` → `ghci_type(localBinding)`. `ghci_load` sigue escribiendo
> al `Session` legacy mientras que el tool migrado leería de
> `GhcSession` — dos universos paralelos. **Opciones para resolver**:
>
> 1. **Fusionar Fase 2 + Fase 3** en una sola sesión (~9-11h): migrar
>    `load` + los 6 read-only juntos. Garantiza state compartido en
>    el mismo session.
> 2. **Auto-load on boot** en `startGhcSession`: parsear `.cabal`,
>    extraer `exposed-modules`, correr `setTargets + load` al primer
>    `withGhcSession`. Equivale al init script de `cabal repl`. Suma
>    ~1-2h a Fase 2 y la desacopla de Fase 3.
> 3. **Dual-write de load**: `ghci_load` invoca los dos sessions hasta
>    que Fase 3 migra. Simple pero añade coupling que hay que limpiar.
>
> El test unitario `ghc-api: HscEnv persists across withGhcSession calls`
> (test/Spec.hs) prueba la invariante load-once-query-many, así que
> cualquiera de las 3 opciones es técnicamente factible.

**Goal**: portar los 6 tools más simples (sólo lectura, sin state change) al `GhcSession`. El `Session` legacy sigue existiendo para las otras tools.

**Tools a migrar** (en orden creciente de complejidad):
1. `ghci_type` — `Tool.Type` — hoy parsea output de `:t`. Post: `GHC.exprType` + pretty-print.
2. `ghci_info` — `Tool.Info` — hoy parsea output de `:i`. Post: `GHC.lookupName` + `tyThingToHsDecl` o similar para el decl, `GHC.lookupInstances` para las instancias.
3. `ghci_complete` — `Tool.Complete` — hoy llama `:complete`. Post: `GHC.getNamesInScope` + filtro por prefijo.
4. `ghci_doc` — `Tool.Doc` — hoy parsea `:doc`. Post: `GHC.getDocs` (API disponible desde GHC 8.6+).
5. `ghci_browse` — `Tool.Browse` — hoy llama `:browse`. Post: `GHC.getModuleInfo` + `modInfoExports` + map a HsDecl.
6. `ghci_goto` — `Tool.Goto` — hoy parsea "Defined at" marker. Post: `GHC.lookupName` + `nameSrcSpan` estructurado.

**Patrón de migration por tool**:
1. Mantener el schema JSON del tool (zero breaking change)
2. En el handler, reemplazar la llamada a Session por una a GhcSession
3. Llamar la función de GHC API correspondiente
4. Construir el JSON response desde la estructura GHC
5. Los scenarios existentes deben seguir verdes (nuestros oracles validan el response shape + contenido, no CÓMO se produjo)

**API cheatsheet por tool**:
- `exprType :: TcRnExprMode -> String -> Ghc Type` (usar `TM_Inst` para mostrar como lo haría `:t`)
- `lookupName :: Name -> Ghc (Maybe TyThing)` + `nameSrcSpan :: Name -> SrcSpan`
- `getNamesInScope :: Ghc [Name]`
- `getDocs :: Name -> Ghc (Either GetDocsFailure (Maybe HsDocString, Map Int HsDocString))`
- `getModuleInfo :: Module -> Ghc (Maybe ModuleInfo)` + `modInfoExports :: ModuleInfo -> [Name]`

**Criterio de éxito**:
- Los 6 tools migrados pasan sus scenarios e2e existentes
- Los scenarios `FlowExploratory` (cubre 5/6 de estos) sigue 100% verde
- Benchmark: cold start de uno de estos tools pasa de ~5-8s (cabal repl boot) a ~1s

**Estimación**: 4-5h. La primera tool (type) tarda más porque estás aprendiendo; las otras siguen el mismo patrón.

### Fase 3 — Migrar `load` + `check_module` + `check_project` + `hole` (1 sesión, 5-6h)

**Goal**: portar los 4 tools que escriben al HscEnv (cargan módulos).

**Tools a migrar**:
1. `ghci_load` — `Tool.Load` — hoy llama `:l`. Post: `GHC.setTargets` + `GHC.load`. El output estructurado viene de `GHC.guessTarget` + errores capturados via `logAction` en DynFlags.
2. `ghci_check_module` — `Tool.CheckModule` — hoy hace doble `:l` (strict + deferred). Post: `load` con distintos DynFlags.
3. `ghci_check_project` — `Tool.CheckProject` — hoy itera sobre exposed-modules. Post: un solo `load LoadAllTargets` que compila todo.
4. `ghci_hole` — `Tool.Hole` — hoy usa `-fdefer-typed-holes` + parsea warnings. Post: flag + recolectar diagnostics tipados.

**Clave técnica**: setear un `LogAction` custom en los DynFlags para **capturar los diagnósticos** en una lista en lugar de que vayan a stderr:
```haskell
collectDiagnostics :: IORef [Diagnostic] -> LogAction
collectDiagnostics ref dflags _ sev srcSpan msg =
  modifyIORef' ref (Diagnostic { dSpan = srcSpan, dSeverity = sev, dMsg = msg } :)
```

**Criterio de éxito**:
- Scenarios `FlowTypeBreakage`, `FlowTypedHoles`, `FlowQualityGates` 100% verdes
- Check project pasa de ~6-8s (iterando con ghci) a <1s (un solo load paralelo via `-j`)

> **Status as of master @ a749f35**: Fases 0-3 cerradas + Fase 6
> parcial. Scorecard:
>
> | Tool                 | Phase | Backend                                   |
> |----------------------|-------|-------------------------------------------|
> | ghci_type            | 2     | GHC API (exprType)                         |
> | ghci_info            | 2     | GHC API (parseName + getInfo)              |
> | ghci_complete        | 2     | GHC API (getNamesInScope)                  |
> | ghci_doc             | 2     | GHC API (getDocs)                          |
> | ghci_browse          | 2     | GHC API (getModuleInfo)                    |
> | ghci_goto            | 2     | GHC API (nameSrcSpan)                      |
> | ghci_imports         | 6     | GHC API (getContext)                       |
> | ghci_load            | 3     | Hybrid — legacy load, GhcSession invalidate |
> | ghci_hole            | 3     | Hybrid — legacy load, GhcSession invalidate |
> | ghci_check_module    | 3     | Hybrid — legacy load, GhcSession invalidate |
> | ghci_check_project   | 3     | Hybrid — legacy load, GhcSession invalidate |
> | ghci_refactor        | 6     | Hybrid — legacy verify, GhcSession invalidate |
> | ghci_add_import, ghci_add_modules, ghci_remove_modules, ghci_apply_exports, ghci_create_project, ghci_deps, ghci_fix_warning, ghci_format | — | File-mutation tools: dispatch now invalidates GhcSession cache so Phase-2 reads re-scan on next access |
> | ghci_eval            | 4     | Legacy (in-process HValue/coerce deferred) |
> | ghci_quickcheck      | 5     | Legacy (QC dual-path — intentional)        |
> | ghci_regression      | 5     | Legacy (dual-path)                         |
> | ghci_determinism     | 5     | Legacy (dual-path)                         |
> | ghci_arbitrary       | 6     | Legacy (parses :i output; works fine)      |
> | ghci_suggest         | 6     | Legacy (parses :i output; works fine)      |
>
> **Net**: 7 tools fully in-process, 12 hybrid (legacy authoritative +
> GhcSession cache sync), 6 still pure-legacy. 233/233 e2e green. The
> architecture is ready for true Fase 7 cleanup once Fase 4 (eval) and
> the Arbitrary/Suggest parser-migrations land.

### Fase 4 — Migrar `eval` (1 sesión, 5-8h, el más complejo)

**Goal**: in-process evaluator para `ghci_eval`.

**Complejidad**: evaluar una expresión requiere (a) type-checkearla, (b) compilarla a bytecode, (c) ejecutarla en el runtime linker. GHC API expone esto pero con ceremony:
```haskell
-- Pseudo-code
evalExpr :: GhcSession -> String -> IO (Either Error String)
evalExpr sess expr = withGhcSession sess $ do
  hv <- compileExpr expr          -- HValue
  -- Coerce y print
  result <- liftIO (unsafeCoerce hv :: IO ())  -- or similar
  ...
```

**Problemas conocidos** (mitigations):
- **TH**: `compileExpr` con TH requiere `-fexternal-interpreter`. Agregar a DynFlags default.
- **Print**: `compileExpr` devuelve el valor, pero nosotros queremos ver su `show`. Wrap la expression: `"show (" ++ userExpr ++ ")"`.
- **IO actions**: si el user evaluá `print (1+1)`, la IO corre — pero el output va a `stdout` del MCP server, no vuelve. Solución: redirigir stdout a un `Handle` buffer temporal (`withRedirectedStdout`).

**Criterio de éxito**:
- `FlowSessionRobustness` 100% verde (undefined, div 0, head [], error, pattern match — todos deben atraparse in-process)
- `FlowExprEvaluatorDogfood` 100% verde (exhaustive eval flow)
- Scenario nuevo que verifica que el output de IO se captura correctamente

### Fase 5 — QuickCheck / Regression / Determinism: dual path (1 sesión, 4-5h)

**Goal**: estos 3 tools se quedan usando subprocess `ghci` porque runtime execution + randomización es impráctico in-process. El Server sostiene **ambas** sessions: GhcSession para los 22 tools, Session (legacy) solo para estos 3.

**Patrón**:
```haskell
handleQuickCheck :: Server -> ... -> IO ToolResult
handleQuickCheck srv args = do
  sess <- getOrStartSession srv  -- legacy ghci subprocess
  -- Usa el código actual de runProperty sin cambios
  ...
```

**Optimización**: lazily start el subprocess ghci. La mayoría de sessions del LLM agent **nunca** llaman quickcheck. Si no se llama, no se spawn. El cold-start de 5-8s solo lo paga quien use estos 3 tools.

**Criterio de éxito**:
- `FlowMutation`, `FlowPropertyLifecycle`, `FlowPropertyStoreRace` 100% verdes
- Los 22 tools no-QC son independientes del subprocess ghci (lo pueden levantar o no)

### Fase 6 — Migrar tools restantes (1 sesión, 3-4h)

**Tools aún legacy** al llegar acá: `ghci_arbitrary`, `ghci_suggest`, `ghci_imports`, `ghci_apply_exports`, `ghci_add_import`, `ghci_refactor`, `ghci_fix_warning`, `ghci_add_modules`, `ghci_remove_modules`.

La mayoría son compile-verify (reuse el migrated `load`). Sólo `refactor` tiene complexity extra por el snapshot-restore, pero el "verify" step es compile-only → usa el nuevo load.

**Criterio de éxito**:
- Todos los scenarios verdes
- `srvSession` legacy **solo se spawnea** cuando el LLM llama ghci_quickcheck/regression/determinism

### Fase 7 — Cleanup + paralelismo default (1 sesión, 3-4h)

**Goal**: remover código muerto + habilitar paralelismo verdadero.

**Cambios**:
1. `HaskellFlows.Ghci.Session` → renombrar a `HaskellFlows.Ghci.LegacySession`. Usado solo por 3 tools.
2. Eliminar `sessionCabalArgs`, `withCabalSpawnLock`, `inProcessCabalLock`. No más contention.
3. `Mcp.Server`: `srvSession` se llama `srvLegacySession`; `srvGhcSession` queda primario.
4. `test-e2e/Main.hs`: default `HASKELL_FLOWS_E2E_PARALLEL = min 4 getNumCapabilities`.
5. Borrar `docs/TODO-parallel-e2e.md` (obsoleto — este plan lo reemplazó).
6. Actualizar `docs/testing.md`: sección parallel no es "deferred" más.

**Criterio de éxito**:
- `HASKELL_FLOWS_E2E_PARALLEL=4` corre 10× consecutivo sin flakes (10/10)
- Wall time de CI: <80s
- HLint limpio, CI limpio

### Fase 8 — Features habilitadas por el rewrite (múltiples sesiones, opcional)

Una vez terminada la base, los 7 beneficios se vuelven realizables:
- **Beneficio 2** (errores estructurados): exponer el `Diagnostic` tipado directo en el JSON response de los tools en vez de text parseado. Cambios en parsers internos.
- **Beneficio 3** (tool calls paralelos): exponer en el protocol que N GhcSessions concurrentes son OK; el LLM agent puede paralelizar calls.
- **Beneficio 4** (reload incremental): `GHC.load` ya hace incremental por defecto — solo documentar.
- **Beneficio 5** (multi-session): nuevo tool `ghci_session new/switch/list` para manejar múltiples sessions. Feature UX.
- **Beneficio 6** (introspección precisa): actualizar JSON schemas de `ghci_info`/`ghci_type` para exponer campos estructurados (packageName, moduleName, definedAt, etc.).

Estas fases son opcionales — el refactor base (Fases 0-7) ya da beneficios 1/3/4/7 sin más trabajo.

## Referencias de implementación

- **haskell-language-server** — el modelo canónico:
  - `Development.IDE.Core.Shake` — cómo mantienen un `ShakeSession` con HscEnv
  - `Development.IDE.Core.Compile` — cómo llaman a `load` + capturan diagnostics
  - `Development.IDE.Plugin.Completions.Logic` — ejemplo de usar `getNamesInScope`
- **ghcid** — `Language.Haskell.Ghcid` (más simple; usa ghci subprocess pero con pattern de reuse).
- **doctest** — `Test.DocTest.Internal.Interpreter` — muestra cómo spawn un ghci-in-a-loop.
- **ghc-lib** — si eventualmente queremos desacoplar del GHC instalado del user.

## API cheatsheet esencial

```haskell
-- Boot
runGhc :: Maybe FilePath -> Ghc a -> IO a
getSessionDynFlags :: Ghc DynFlags
setSessionDynFlags :: DynFlags -> Ghc [PreloadUnitId]

-- Targets
guessTarget :: String -> Maybe UnitId -> Maybe Phase -> Ghc Target
setTargets  :: [Target] -> Ghc ()
load        :: LoadHowMuch -> Ghc SuccessFlag

-- Queries
exprType      :: TcRnExprMode -> String -> Ghc Type
lookupName    :: Name -> Ghc (Maybe TyThing)
getNamesInScope :: Ghc [Name]
getModuleInfo :: Module -> Ghc (Maybe ModuleInfo)
getDocs       :: Name -> Ghc (Either GetDocsFailure (Maybe HsDocString, ...))

-- Evaluation
compileExpr   :: String -> Ghc HValue
compileParsedExpr :: LHsExpr GhcPs -> Ghc ForeignHValue

-- Diagnostics
type LogAction = DynFlags -> WarnReason -> Severity -> SrcSpan -> SDoc -> IO ()
-- Set log_action = collectDiagnostics ref dflags
```

## Criterio de aceptación global (todo el plan)

Al finalizar Fase 7:
- [ ] `cabal test haskell-flows-mcp-e2e` verde (≥233 checks)
- [ ] `HASKELL_FLOWS_E2E_PARALLEL=4` 10× consecutivo verde
- [ ] Serial cold start de un tool: <1s (vs 5-8s hoy)
- [ ] `scripts/ci-local.sh --fast` verde
- [ ] HLint limpio
- [ ] CI de GitHub verde en ambos OS (Ubuntu + macOS)
- [ ] `srvGhcSession` es el primary; `srvLegacySession` solo para QC/Regression/Determinism
- [ ] Benchmarks antes/después documentados en el commit de cierre

## Estimación global

| Fase | Tiempo | Deliverable |
|---|---|---|
| 0 — Spike | 1-2h | Branch spike funcionando |
| 1 — Infra GhcSession | 3-4h | Commit 1 |
| 2 — 6 tools simples | 4-5h | Commit 2 |
| 3 — load/check/hole | 5-6h | Commit 3 |
| 4 — eval | 5-8h | Commit 4 |
| 5 — QC dual path | 4-5h | Commit 5 |
| 6 — tools restantes | 3-4h | Commit 6 |
| 7 — cleanup + parallel | 3-4h | Commit 7 |

**Total**: ~30-40h = **5-7 sesiones** de 4-6h cada una. Factible en 1-2 meses part-time.

Cada fase deja master verde y commiteable. Si algo sale mal en la Fase N+1, `git revert` a Fase N.

## Historia relevante (contexto de sesiones anteriores, solo leer si ayuda)

El repo ya tuvo intentos parciales de paralelismo:
- Intentamos hie-bios + subprocess ghci (documentado en `docs/TODO-parallel-e2e.md`, ahora obsoleto). Spike falló — hie-bios 0.14/0.15 tiene parse errors en nuestra combo GHC 9.12 + cabal 3.12. El `.md` queda como histórico; este plan lo reemplaza.
- `withCabalSpawnLock` + init-timeout bumps: parches para paliar contención sin resolver. Se van en Fase 7.
- `HASKELL_FLOWS_E2E_SKIP_SLOW=1`: flag actual para dev loop de 127s. Se mantiene útil incluso post-rewrite (algunos scenarios son inherentemente lentos, ej coverage).
- 4 bugs reales arreglados en sesiones previas (A/D/G/H, ver git log de master). **Preservar ese comportamiento** en la nueva arquitectura es parte del criterio de Fase 2+.

## Pitfalls conocidos

- **Package db**: GHC API necesita saber qué packages están disponibles. Llamar a `initUnits` después de setSessionDynFlags. Para nuestros scaffolds con QuickCheck, asegurar que `-package QuickCheck` está en los flags (o instalarlo al user env una vez).
- **TemplateHaskell en macOS arm64**: `-fexternal-interpreter` es mandatorio. Agregar al default DynFlags.
- **Log spam**: el default LogAction imprime a stderr. Siempre setear uno custom que capture en ref.
- **HscEnv mutation**: `load` y `setTargets` mutan el HscEnv. Siempre leer de `gsEnvRef` al inicio de withGhcSession y escribir al final.
- **Thread safety**: GHC API NO es thread-safe dentro de un HscEnv. Un `MVar ()` alrededor garantiza un-call-at-a-time por session. Múltiples sessions sí pueden correr en paralelo.
