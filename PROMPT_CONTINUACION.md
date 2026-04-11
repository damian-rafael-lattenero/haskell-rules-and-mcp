# Prompt de Continuacion — Haskell Incremental Type-Checking System

Copia y pega esto en una nueva sesion de Claude Code abierta en este proyecto:

---

## El Prompt

```
Estoy trabajando en un sistema para mejorar como se escribe codigo Haskell con Claude Code.
Vengo de varias sesiones de trabajo. Abajo esta el historial completo de lo hecho y lo
que queda pendiente.

### Lo que ya existe y funciona

#### Parte 1: Claude Rules (.claude/rules/)
4 archivos de reglas que cambian tu comportamiento al escribir Haskell:
- `haskell-development.md` — Protocolo de tipado incremental con 3 niveles (Full/Batch/Write-and-Verify segun complejidad), type signatures primero, typed holes, :t para verificar subexpresiones
- `haskell-errors.md` — Base de conocimiento de ~25 errores comunes de GHC con codigos, explicaciones y fixes
- `haskell-project.md` — Config del proyecto: GHC 9.12, GHC2024, Cabal, build commands
- `haskell-monadic.md` — Patrones monadicos: do-notation, transformers, MTL, anti-patterns, debugging (NUEVO sesion 5)

#### Parte 2: MCP Server "haskell-ghci" (mcp-server/)
Un MCP server en TypeScript que mantiene una sesion GHCi PERSISTENTE con auto-reload.

14 tools (las ultimas 2 son nuevas de sesion 5, necesitan test post-restart):
- `ghci_type` — :t con auto-reload
- `ghci_info` — :i con auto-reload
- `ghci_kind` — :k con auto-reload
- `ghci_eval` — evaluar expresiones puras
- `ghci_load` — cargar/recargar modulos con errores parseados
- `ghci_load(load_all=true)` — lee el .cabal y carga TODOS los modulos en GHCi con scope completo
- `cabal_build` — compilacion completa con errores parseados
- `hoogle_search` — buscar funciones en Hoogle por tipo o nombre
- `ghci_session` — status/restart de la sesion GHCi (kill async, sin race conditions)
- `ghci_scaffold` — lee el .cabal, detecta modulos sin archivo fuente, crea stubs
- `ghci_check_module` — carga un modulo con -fno-defer-type-errors y devuelve resumen estructurado de exports con tipos
- `ghci_batch` — ejecuta multiples comandos GHCi en un solo roundtrip (TESTEADO sesion 5)
- `ghci_hole_fits` — analiza typed holes (_) y devuelve fits estructurados (NUEVO, NECESITA TEST)
- `ghci_diagnostics` — dual-pass check: errores reales + typed holes en un solo call (NUEVO, NECESITA TEST)

Componentes internos:
- `parsers/cabal-parser.ts` — Parser de .cabal que extrae exposed-modules y hs-source-dirs
- `parsers/error-parser.ts` — Parser de errores GHC, regex corregida para [-Wflag] suffixes
- `parsers/type-parser.ts` — Parser de output de :t y :i
- `ghci-session.ts` — Session manager con kill() async, executeBatch(), settled flag

#### Parte 3: Hindley-Milner Type Inference (EXPANDIDO)
9 modulos Haskell implementados usando el sistema completo (reglas + MCP).

src/HM/ — 5 modulos del engine de inferencia:
- `HM.Syntax` — AST expandido:
  - Expr: EVar, ELit, EApp, ELam, ELet, ELetRec, EIf, EPair, EFst, ESnd, EAnn
  - Type: TVar, TCon, TArr, TProd
  - Lit: LInt, LBool
  - Scheme: Forall [TVar] Type
- `HM.Subst` — Substituciones para todos los tipos incluyendo TProd
- `HM.Unify` — Unificacion de Robinson con occurs check, soporta TArr y TProd
- `HM.Infer` — Algorithm W completo con:
  - Let-polimorfismo
  - Letrec (recursive let bindings)
  - Pares (EPair/EFst/ESnd con TProd)
  - Type annotations (EAnn — infer + unify con anotacion)
- `HM.Pretty` — Pretty printing de tipos y expresiones, incluyendo todos los constructores nuevos

src/Parser/ — 4 modulos de parser combinators desde cero:
- `Parser.Core` — Parser type con Functor/Applicative/Monad/Alternative, furthest-failure tracking en <|>, satisfy/char/string/eof, ppParseError
- `Parser.Combinators` — sepBy, between, chainl1/chainr1, option, notFollowedBy
- `Parser.Char` — letter/digit/spaces/lexeme/symbol/natural/integer/identifier/upperIdentifier/reserved/parens
- `Parser.HM` — Parser completo del lenguaje HM:
  - Literales: 42, true, false
  - Variables: identificadores lowercase (excluye keywords)
  - Lambda: \x -> body
  - Aplicacion: juxtaposicion left-associative (f x y = (f x) y)
  - Let: let x = e1 in e2
  - Letrec: let rec f = e1 in e2
  - If: if cond then e1 else e2
  - Pares: (e1, e2), fst e, snd e
  - Annotations: (expr : Type)
  - Tipos: Int, Bool, type vars, a -> b, (a, b)

app/Main.hs — 38 ejemplos (20 manuales + 18 parseados):
- Pipeline completo: texto -> parse -> AST -> infer -> tipo -> pretty print
- Incluye errores de tipo y errores de parseo

Se puede correr con: export PATH="$HOME/.ghcup/bin:$PATH" && cabal run haskell-rules-and-mcp

### Lo que queda pendiente AHORA MISMO

#### PENDIENTE 1: Reiniciar MCP y testear los 2 tools nuevos (PRIORITARIO)

El MCP server tiene 2 tools nuevos compilados (`npx tsc` OK) que necesitan restart para
activarse. Al abrir esta sesion nueva ya deberian estar disponibles.

1. **ghci_hole_fits** — Tool que analiza typed holes y devuelve fits estructurados.
   Para testear:
   - Crear un modulo temporal con typed holes:
     ```haskell
     module HM.TestHole where
     foo :: Int -> String
     foo x = _
     bar :: [Int] -> Int
     bar xs = _ xs
     baz :: (a -> b) -> [a] -> [b]
     baz f xs = _ f xs
     ```
   - Llamar ghci_hole_fits con module_path: "src/HM/TestHole.hs"
   - Verificar que devuelve JSON estructurado con:
     - 3 holes, cada uno con expectedType, relevantBindings, validFits
     - foo: expectedType "String", fits incluyen [] y mempty
     - bar: expectedType "[Int] -> Int", fits incluyen head, last, length
     - baz: expectedType "(a -> b) -> [a] -> [b]", fits incluyen map, fmap
   - Testear con max_fits: 20 para ver mas fits
   - Limpiar el modulo temporal

2. **ghci_diagnostics** — Dual-pass: strict (errores reales) + deferred (typed holes).
   Para testear:

   Test A — Modulo con type error real:
   - Crear modulo con `foo :: Int; foo = True`
   - Llamar ghci_diagnostics
   - Verificar: compiled=false, errors tiene el GHC-83865, holes vacio
   - Limpiar

   Test B — Modulo con typed holes (sin errores):
   - Crear modulo con `foo :: Int -> String; foo x = _`
   - Llamar ghci_diagnostics
   - Verificar: compiled=true, errors vacio, holes tiene 1 entry con fits
   - Limpiar

   Test C — Modulo limpio:
   - Llamar ghci_diagnostics sobre src/HM/Syntax.hs
   - Verificar: compiled=true, errors vacio, holes vacio, summary "No issues"

   Test D — Verificar que la sesion GHCi queda sana despues:
   - Despues de todos los tests, llamar ghci_type con alguna expresion
   - Verificar que -fdefer-type-errors sigue activo

#### PENDIENTE 2: Elegir la siguiente tarea

Una vez verificados los tools, elegir que hacer:

OPCION A — MEJORAR EL PARSER:
1. Agregar multi-arg lambda: \x y z -> body (ahora requiere \x -> \y -> \z -> body)
2. Agregar operadores infijos: 1 + 2, f . g (con precedencia configurable via chainl1)
3. Agregar let con multiples bindings: let x = 1; y = 2 in x + y
4. Mejorar mensajes de error con "did you mean?" para keywords mal escritas
5. Agregar un modo REPL interactivo que lea stdin linea por linea

OPCION B — EXPANDIR EL HINDLEY-MILNER MAS:
1. Agregar constructores de datos y pattern matching (ECase)
2. Agregar listas con syntactic sugar [1, 2, 3]
3. Agregar type classes basicas (Eq, Ord, Show) con instance resolution
4. Agregar mutual recursion (let rec f = ... and g = ... in ...)

OPCION C — NUEVO MINI PROYECTO HASKELL:
Implementa algo nuevo usando el sistema completo para seguir validandolo:
- Un constraint solver (SAT basico)
- Un interprete de lambda calculus con De Bruijn indices
- Un evaluador de un lenguaje con efectos (Free monad)
- Un typechecker bidireccional (mas moderno que Algorithm W)

OPCION E — MEJORAR EL MCP (lo que quedo de D):
1. Crear un test suite automatizado para el MCP server (vitest) — no se pudo en sesion 5 por problemas con npm
2. Agregar un tool `ghci_refactor` que sugiera refactors basados en -Wall warnings
3. Mejorar ghci_hole_fits para que tambien sugiera imports que traerian fits adicionales

Decime cual opcion queres hacer (o una combinacion).
```

---

## Historial completo de lo que se hizo

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
- MCP tools usados: ghci_type (~8 veces), ghci_eval (~15 veces), ghci_load (~10 veces), ghci_info, hoogle_search, cabal_build

**3 Tools nuevos del MCP** (commit ecbb127):
- ghci_scaffold: parser de .cabal + generador de stubs
- ghci_load load_all: carga todos los modulos con scope
- ghci_check_module: resumen estructurado de modulo via :browse

### Sesion 3: Testing end-to-end + Fix race condition

**Testing end-to-end de los 3 tools nuevos — todos PASS:**

| Tool | Resultado | Detalle |
|------|-----------|---------|
| `ghci_scaffold` | PASS | Agrego HM.NewModule al .cabal, scaffold creo stub correcto, GHCi lo cargo sin error. |
| `ghci_load(load_all=true)` | PASS | Cargo los 6 modulos. Verificado scope con ghci_eval y ghci_type. |
| `ghci_check_module` | PASS | HM.Syntax devolvio 6 defs. HM.Infer devolvio tipos. Error intencional dio error estructurado. |

**Bug encontrado y arreglado: race condition en ghci_session restart**
Causa raiz: kill() sincronico, exit handler de startup no removido, identity check faltante.
Fix: kill() async, settled flag, cleanup de handlers, identity check en getSession().

### Sesion 4: Expandir HM + Parser Combinators + Mejoras al sistema (commit 1bb2909)

**Expandir Hindley-Milner (3 features):**

| Feature | Archivos tocados | Tests |
|---------|-----------------|-------|
| ELetRec (recursion) | Syntax, Infer, Pretty | `let rec f = \x -> if true then x else f x in f :: forall t3. t3 -> t3` |
| Pares (EPair/EFst/ESnd/TProd) | Syntax, Subst, Unify, Infer, Pretty | Construccion, proyeccion, swap polimorfico `(t3,t4) -> (t4,t3)` |
| Annotations (EAnn) | Syntax, Infer, Pretty | Conformidad OK + error UnificationFail en mismatch |

**Parser Combinator Library desde cero (4 modulos):**

| Modulo | Lineas | Contenido |
|--------|--------|-----------|
| Parser.Core | ~100 | Parser type, Functor/Applicative/Monad/Alternative con furthest-failure, primitivas |
| Parser.Combinators | ~47 | sepBy, between, chainl1/r1, option, notFollowedBy |
| Parser.Char | ~95 | Lexing: letter/digit/spaces/lexeme/symbol/natural/identifier/reserved |
| Parser.HM | ~140 | Parser completo del lenguaje HM: literals, vars, lambda, app, let, letrec, if, pairs, annotations, types |

Pipeline end-to-end: 38/38 ejemplos pasaron (20 manuales + 18 parseados).

**Mejoras al sistema en sesion 4:**
- Furthest failure en Parser.Core — `<|>` trackea error con mayor posicion
- Smart protocol en rules — 3 niveles: Full/Batch/Write-and-Verify
- ghci_batch tool — ejecuta N comandos en 1 call
- check_module strict — desactiva -fdefer-type-errors durante check
- ppParseError — errores legibles con posicion

### Sesion 5: Opcion D — Mejorar el MCP (commit 28b9ab8)

**Verificacion de tools pendientes de sesion 4 — todos PASS:**

| Tool | Tests | Resultado |
|------|-------|-----------|
| `ghci_batch` basico | 3 comandos [:t map, :t foldr, 1+2] | 3/3 OK |
| `ghci_batch` + reload | mismos comandos con reload:true | 3/3 OK |
| `ghci_batch` + stop_on_error (warning) | [:t map, :t nonExistent, :t foldr] | 3/3 — correcto: warning diferido no es error |
| `ghci_batch` + stop_on_error (real error) | [:t map, "let x = in", :t foldr] | 2/3, se detuvo en parse error |
| `ghci_check_module` strict | modulo con `foo :: Int; foo = True` | Error GHC-83865 detectado como error |
| `ghci_check_module` post-check | :t nonExistent despues de check | Warning (defer restaurado OK) |

**D1: ghci_hole_fits (NUEVO tool, COMPILADO, NECESITA TEST post-restart):**
- `mcp-server/src/tools/hole-fits.ts` — ~230 lineas
- Parsea warnings [GHC-88464] del output de GHCi
- Devuelve JSON con: hole name, expectedType, location, relevantBindings, validFits
- Cada fit tiene: name, type, specialization ("with map @a @b"), source
- Configurable max_fits (default 10)

**D2: Test suite — SALTEADO** (npm install se colgo, problemas de red/registry)

**D3: haskell-monadic.md (NUEVA regla):**
- `.claude/rules/haskell-monadic.md` — ~200 lineas
- Do-notation: <- vs let, last expression, >> vs >>=, void
- Monad transformers: lift vs liftIO, ReaderT/ExceptT/StateT patterns, running stacks
- MTL-style constraints vs concrete stacks, tabla de MonadReader/State/Error/IO/Writer
- Anti-patterns: nesting, return semantica, mixing monads, mapM vs traverse, when/unless
- Debugging monadic type errors: annotate, check :t, ambiguous monad vars

**D4: ghci_diagnostics (NUEVO tool, COMPILADO, NECESITA TEST post-restart):**
- `mcp-server/src/tools/diagnostics.ts` — ~170 lineas
- Dual-pass: strict (-fno-defer-type-errors) para errores reales, luego deferred para typed holes
- Reporte unificado: errors, warnings, holes (con bindings y fits), summary
- Separa hole errors (GHC-88464) de errores reales
- Restaura -fdefer-type-errors al final

**Fix en error-parser.ts:**
- Regex corregida: `[^\n]*` despues de `[GHC-CODE]` para consumir `[-Wflag]` suffixes
- Antes: la regex no matcheaba warnings con `-Wtyped-holes`, `-Wunused-matches`, etc.

**LSP getDiagnostics:** Investigado, no viable sin HLS corriendo. Timeout sin IDE abierto.

## Estadisticas de la sesion 5

- 2 tools nuevos (~400 lineas TypeScript)
- 1 regla nueva (~200 lineas markdown)
- 1 fix en error-parser (regex)
- 6/6 tests de tools pendientes pasaron
- Commit: 28b9ab8

## Notas tecnicas

- Las reglas de `.claude/rules/` se cargan automaticamente en el contexto
- El MCP necesita estar conectado (verificar con ghci_session action=status)
- Si el MCP no esta disponible, rebuild: `cd mcp-server && npx tsc`
- El `.ghci` configura GHCi con `-fdefer-type-errors` y `-fprint-explicit-foralls`
- El auto-reload en ghci_type/ghci_info/ghci_kind hace `:r` antes de cada query
- GHC 9.12.2, GHC2024, Cabal 3.12. Deps: base, containers, array, mtl
- PATH de GHC: export PATH="$HOME/.ghcup/bin:$PATH"
- El parser usa backtracking simple con furthest-failure — sin `try` explicito
- Los 2 tools nuevos (ghci_hole_fits, ghci_diagnostics) necesitan restart del MCP server para activarse
- mcp__ide__getDiagnostics requiere un IDE con LSP activo (HLS). Sin eso, timeout.

## Commits

| # | Hash | Descripcion |
|---|------|-------------|
| 1 | d09f276 | Setup inicial: 3 rules + MCP server con 8 tools |
| 2 | 7ccc86d | Fix sentinel off-by-one + PATH macOS ARM |
| 3 | 9ee2d39 | Hindley-Milner type inference engine (5 modulos) |
| 4 | ecbb127 | 3 new MCP tools: ghci_scaffold, load_all, ghci_check_module |
| 5 | 536a7b2 | Fix race condition en GHCi session restart |
| 6 | 1bb2909 | Expand HM + parser combinators + mejoras sistema |
| 7 | 28b9ab8 | Add hole_fits, diagnostics tools + monadic rules + error parser fix |
