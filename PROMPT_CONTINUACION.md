# Prompt de Continuacion — Haskell Incremental Type-Checking System

Copia y pega esto en una nueva sesion de Claude Code abierta en este proyecto:

---

## El Prompt

```
Estoy trabajando en un sistema para mejorar como se escribe codigo Haskell con Claude Code.
Vengo de varias sesiones de trabajo. Abajo esta el historial completo de lo hecho y lo
que queda pendiente.

### Lo que ya existe y funciona (todo testeado)

#### Parte 1: Claude Rules (.claude/rules/)
3 archivos de reglas que cambian tu comportamiento al escribir Haskell:
- `haskell-development.md` — Protocolo de tipado incremental: NUNCA escribir mas de 1 funcion sin compilar, type signatures primero, typed holes, :t para verificar subexpresiones
- `haskell-errors.md` — Base de conocimiento de ~25 errores comunes de GHC con codigos, explicaciones y fixes
- `haskell-project.md` — Config del proyecto: GHC 9.12, GHC2024, Cabal, build commands

#### Parte 2: MCP Server "haskell-ghci" (mcp-server/)
Un MCP server en TypeScript que mantiene una sesion GHCi PERSISTENTE con auto-reload.

11 tools, TODOS testeados end-to-end como MCP tools:
- `ghci_type` — :t con auto-reload
- `ghci_info` — :i con auto-reload
- `ghci_kind` — :k con auto-reload
- `ghci_eval` — evaluar expresiones puras
- `ghci_load` — cargar/recargar modulos con errores parseados
- `ghci_load(load_all=true)` — lee el .cabal y carga TODOS los modulos de la library en GHCi de una vez con scope completo (:m + *Module). Mas liviano que cabal_build porque es interpretado
- `cabal_build` — compilacion completa con errores parseados
- `hoogle_search` — buscar funciones en Hoogle por tipo o nombre
- `ghci_session` — status/restart de la sesion GHCi
- `ghci_scaffold` — lee el .cabal, detecta modulos sin archivo fuente, crea stubs automaticamente
- `ghci_check_module` — carga un modulo y devuelve resumen estructurado de todos sus exports con tipos (via :browse)

Componentes internos:
- `parsers/cabal-parser.ts` — Parser de .cabal que extrae exposed-modules y hs-source-dirs
- `ghci-session.ts` — Session manager con metodos loadModules(), kill() async, etc.

#### Parte 3: Mini proyecto Haskell — Hindley-Milner Type Inference
Implementado usando el sistema completo (reglas + MCP). 5 modulos en src/HM/:
- `HM.Syntax` — AST (Expr: Var, Lit, App, Lam, Let, If) y tipos (Type: TVar, TCon, TArr, Scheme)
- `HM.Subst` — Substituciones, typeclass Substitutable, free type variables
- `HM.Unify` — Unificacion de Robinson con occurs check
- `HM.Infer` — Algorithm W completo con let-polimorfismo, ExceptT + State para fresh vars
- `HM.Pretty` — Pretty printing de tipos y expresiones

El Main (app/Main.hs) tiene 11 ejemplos de inferencia incluyendo:
- Literales, identidad, const, aplicacion, composicion
- Let-polimorfismo (id usado como Int->Int y Bool->Bool en la misma expresion)
- Deteccion de errores: variable no definida, condicion no-Bool, branches incompatibles

Se puede correr con: export PATH="$HOME/.ghcup/bin:$PATH" && cabal run haskell-rules-and-mcp

### Lo que queda pendiente AHORA MISMO

#### PENDIENTE 1: Verificar el fix del bug de restart (PRIORITARIO)

En la sesion 3 encontramos y arreglamos un bug de race condition en ghci_session restart.
El bug: hacer restart consecutivos a veces daba `Cannot read properties of null (reading 'on')`.

Causa raiz (3 problemas):
1. `kill()` en ghci-session.ts era sincronico — hacia `process.kill("SIGTERM")` y luego
   `this.process = null` sin esperar a que el proceso terminara. El exit handler del viejo
   proceso podia disparar despues y corromper estado.
2. `start()` registraba un exit handler para startup que NUNCA se removia — quedaba junto
   con el de `setupHandlers()`, causando doble emit de "exit".
3. `getSession()` en index.ts registraba un exit handler `ghciSession = null` sin verificar
   que la sesion que murio fuera la misma que la sesion activa — el exit del viejo proceso
   podia nullificar la referencia a la sesion nueva.

Fix aplicado (ya compilado con `npx tsc`, falta reiniciar el MCP server):
- `kill()` ahora es `async` — espera a que el proceso termine antes de resolver
- El exit handler de startup se remueve explicitamente antes de instalar `setupHandlers()`
- Se agrego un flag `settled` para evitar doble reject en la Promise de startup
- `getSession()` ahora guarda la referencia local y el exit handler verifica `ghciSession === session` antes de nullificar

Archivos modificados:
- `mcp-server/src/ghci-session.ts` — kill() async, start() con settled flag y cleanup de handlers
- `mcp-server/src/index.ts` — getSession() con identity check, restart handler con await kill()

**Para verificar**: Reinicia Claude Code (o el MCP server via /mcp) y luego hace 5+ restarts
consecutivos rapidos. Antes del fix, el 2do o 3er restart fallaba. Despues del fix deberian
pasar todos sin error.

#### PENDIENTE 2: Elegir la siguiente tarea

Una vez verificado el fix, elegir que hacer:

OPCION B — EXPANDIR EL HINDLEY-MILNER:
El type inference funciona pero es basico. Mejoras posibles:
1. Agregar fix/letrec para recursion (let rec f = \x -> ... f ... in ...)
2. Agregar pares/tuplas (EPair, TProduct)
3. Agregar type annotations del usuario (EAnn Expr Type) y verificarlas
4. Agregar constructores de datos (ECase con pattern matching)
5. Agregar un parser de texto (usar el MCP + hoogle_search para encontrar funciones utiles)

OPCION C — NUEVO MINI PROYECTO HASKELL:
Implementa algo nuevo usando el sistema completo para seguir validandolo:
- Un parser combinator desde cero
- Un constraint solver (SAT basico)
- Un interprete de lambda calculus con De Bruijn indices
- Un evaluador de un lenguaje con efectos (Free monad)

Usa todos los MCP tools activamente incluyendo los nuevos.

OPCION D — MEJORAR EL MCP:
1. Agregar un tool `ghci_hole_fits` que analice typed holes y devuelva valid fits estructurados
2. Mejorar el error parser para errores multi-linea de GHC 9.12 mas robustamente
3. Crear un test suite automatizado para el MCP server (jest/vitest)
4. Agregar reglas para monadic code (transformers, do-notation patterns)

Decime cual opcion queres hacer (o una combinacion).
```

---

## Historial de lo que se hizo

### Sesion 1: Setup inicial (commit d09f276)
- Creacion de las 3 claude rules (haskell-development, haskell-errors, haskell-project)
- Implementacion del MCP server con 8 tools originales
- Configuracion de .mcp.json

### Sesion 2: Fix + Hindley-Milner + Tools nuevos

**Fix del MCP** (commit 7ccc86d):
- Bug de off-by-one en sentinel sync del GHCi session
- PATH actualizado para macOS ARM (/opt/homebrew/bin)

**Hindley-Milner** (commit 9ee2d39):
- 5 modulos, ~316 lineas de Haskell
- 0 compilaciones fallidas gracias al uso activo del MCP
- Flujo: signatures first con undefined -> ghci_load -> implementar -> ghci_load -> ghci_eval para testear -> siguiente funcion
- Los MCP tools usados: ghci_type (~8 veces), ghci_eval (~15 veces), ghci_load (~10 veces), ghci_info, hoogle_search, cabal_build

**3 Tools nuevos del MCP** (commit ecbb127):
- ghci_scaffold: parser de .cabal + generador de stubs
- ghci_load load_all: carga todos los modulos con scope
- ghci_check_module: resumen estructurado de modulo via :browse
- Testeados parcialmente (node directo, TypeScript compila), falta test end-to-end como MCP tools

### Sesion 3: Testing end-to-end + Fix race condition (sin commitear aun)

**Testing end-to-end de los 3 tools nuevos — todos PASS:**

| Tool | Resultado | Detalle |
|------|-----------|---------|
| `ghci_scaffold` | PASS | Agrego HM.NewModule al .cabal, scaffold creo stub correcto, GHCi lo cargo sin error. Limpiado despues. |
| `ghci_load(load_all=true)` | PASS | Cargo los 6 modulos de la library. Verificado con ghci_eval y ghci_type que ppType, runInfer, inferExpr estaban en scope con tipos correctos. |
| `ghci_check_module` | PASS | HM.Syntax devolvio 6 defs (Name, TVar, Lit, Expr, Type, Scheme). HM.Infer devolvio TypeEnv, runInfer, inferExpr con tipos. Error intencional (Nonexistent type) devolvio error estructurado con linea/columna/codigo GHC-76037. |

**Observacion**: `-fdefer-type-errors` en .ghci hace que type errors no aparezcan como errores
en ghci_check_module (se difieren a runtime). Solo errores de scope/parse se detectan. Esto no
es un bug del tool sino un efecto de la config de GHCi — tenerlo en cuenta.

**Bug encontrado y arreglado: race condition en ghci_session restart**

Sintoma: `Cannot read properties of null (reading 'on')` al hacer restart consecutivos.
El 1er restart funcionaba, el 2do fallaba, el 3ro funcionaba.

Causa raiz (3 problemas en ghci-session.ts e index.ts):
1. `kill()` sincronico que no esperaba al exit del proceso
2. Exit handler de startup que nunca se removia (doble emit)
3. Exit handler en getSession() que podia nullificar la sesion nueva por exit del viejo proceso

Fix aplicado en:
- `mcp-server/src/ghci-session.ts` — kill() async, settled flag, cleanup de startup exit handler
- `mcp-server/src/index.ts` — identity check en exit handler, await kill() en restart

**Estado**: Compilado (`npx tsc` OK), pero NO desplegado todavia. Necesita restart del MCP
server para que tome efecto. NO commiteado aun.

## Notas tecnicas

- Las reglas de `.claude/rules/` se cargan automaticamente en el contexto
- El MCP necesita estar conectado (verificar con ghci_session action=status)
- Si el MCP no esta disponible, rebuild: `cd mcp-server && npx tsc`
- El `.ghci` configura GHCi con `-fdefer-type-errors` y `-fprint-explicit-foralls`
- El auto-reload en ghci_type/ghci_info/ghci_kind hace `:r` antes de cada query
- GHC 9.12.2, GHC2024, Cabal 3.12. Deps: base, containers, array, mtl
- PATH de GHC: export PATH="$HOME/.ghcup/bin:$PATH"
