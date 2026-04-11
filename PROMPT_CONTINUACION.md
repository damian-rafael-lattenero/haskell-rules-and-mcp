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
3 archivos de reglas que cambian tu comportamiento al escribir Haskell:
- `haskell-development.md` — Protocolo de tipado incremental con 3 niveles (Full/Batch/Write-and-Verify segun complejidad), type signatures primero, typed holes, :t para verificar subexpresiones
- `haskell-errors.md` — Base de conocimiento de ~25 errores comunes de GHC con codigos, explicaciones y fixes
- `haskell-project.md` — Config del proyecto: GHC 9.12, GHC2024, Cabal, build commands

#### Parte 2: MCP Server "haskell-ghci" (mcp-server/)
Un MCP server en TypeScript que mantiene una sesion GHCi PERSISTENTE con auto-reload.

12 tools:
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
- `ghci_batch` — ejecuta multiples comandos GHCi en un solo roundtrip (NEW, necesita test)

Componentes internos:
- `parsers/cabal-parser.ts` — Parser de .cabal que extrae exposed-modules y hs-source-dirs
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

#### PENDIENTE 1: Reiniciar MCP y testear mejoras (PRIORITARIO)

El MCP server tiene 2 cambios compilados (`npx tsc` OK) que necesitan restart para activarse:

1. **ghci_batch** — Tool nuevo que ejecuta multiples comandos GHCi en un solo roundtrip.
   Para testear: reiniciar Claude Code o el MCP via /mcp, luego:
   - Llamar ghci_batch con commands: [":t map", ":t foldr", "1 + 2"]
   - Verificar que devuelve 3 resultados en un solo call
   - Testear con reload: true
   - Testear con stop_on_error: true y un comando que falle en el medio

2. **ghci_check_module con -fno-defer-type-errors** — Antes, type errors no aparecian
   como errores porque -fdefer-type-errors los diferia. Ahora check_module desactiva
   temporalmente ese flag y lo restaura despues.
   Para testear:
   - Crear un modulo temporal con un type error intencional (ej: `foo :: Int; foo = True`)
   - Correr ghci_check_module sobre ese modulo
   - Verificar que el error de tipo aparece como error (no como warning)
   - Verificar que despues de check_module, ghci_type sigue funcionando con defer habilitado
   - Limpiar el modulo temporal

#### PENDIENTE 2: Elegir la siguiente tarea

Una vez verificadas las mejoras, elegir que hacer:

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

OPCION D — MEJORAR EL MCP:
1. Agregar un tool `ghci_hole_fits` que analice typed holes y devuelva valid fits estructurados
2. Crear un test suite automatizado para el MCP server (jest/vitest)
3. Agregar reglas para monadic code (transformers, do-notation patterns)
4. Integrar con LSP via mcp__ide__getDiagnostics para feedback sin duplicar funcionalidad

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

### Sesion 4: Expandir HM + Parser Combinators + Mejoras al sistema (SIN COMMITEAR)

**Part B — Expandir Hindley-Milner (3 features):**

| Feature | Archivos tocados | Tests |
|---------|-----------------|-------|
| ELetRec (recursion) | Syntax, Infer, Pretty | `let rec f = \x -> if true then x else f x in f :: forall t3. t3 -> t3` |
| Pares (EPair/EFst/ESnd/TProd) | Syntax, Subst, Unify, Infer, Pretty | Construccion, proyeccion, swap polimorfico `(t3,t4) -> (t4,t3)` |
| Annotations (EAnn) | Syntax, Infer, Pretty | Conformidad OK + error UnificationFail en mismatch |

Todas las inference rules se implementaron con el protocolo incremental:
undefined -> ghci_load -> implementar -> ghci_load -> ghci_eval test.
0 errores de compilacion que requirieran backtracking.

**Part C — Parser Combinator Library desde cero (4 modulos):**

| Modulo | Lineas | Contenido |
|--------|--------|-----------|
| Parser.Core | ~100 | Parser type, Functor/Applicative/Monad/Alternative con furthest-failure, primitivas |
| Parser.Combinators | ~47 | sepBy, between, chainl1/r1, option, notFollowedBy |
| Parser.Char | ~95 | Lexing: letter/digit/spaces/lexeme/symbol/natural/identifier/reserved |
| Parser.HM | ~140 | Parser completo del lenguaje HM: literals, vars, lambda, app, let, letrec, if, pairs, annotations, types |

Pipeline end-to-end: texto -> parseProgram -> AST -> runInfer -> Scheme -> ppScheme -> texto
38/38 ejemplos pasaron (20 manuales + 18 parseados).

**5 Mejoras al sistema:**

| # | Mejora | Estado | Detalle |
|---|--------|--------|---------|
| 1 | Furthest failure en Parser.Core | FUNCIONANDO | `<\|>` trackea error con mayor posicion. `"let in"` -> pos 6 en vez de pos 0 |
| 2 | Smart protocol en rules | FUNCIONANDO | 3 niveles: Full (complex), Batch (medium), Write-and-Verify (trivial) |
| 3 | ghci_batch tool | COMPILADO, NECESITA RESTART MCP | Nuevo tool, ejecuta N comandos en 1 call con options reload/stop_on_error |
| 4 | check_module strict | COMPILADO, NECESITA RESTART MCP | Desactiva -fdefer-type-errors durante check, restaura despues |
| 5 | ppParseError | FUNCIONANDO | Errores legibles: `"parse error at position 6: unexpected keyword 'in'"` |

**Verificacion del fix de race condition:**
5/5 restarts consecutivos rapidos — todos OK. Session funcional post-restarts.

## Estadisticas de la sesion 4

- ~500 lineas nuevas de Haskell (5 modulos modificados + 4 creados)
- ~80 lineas de TypeScript (ghci_batch tool + check_module fix + executeBatch method)
- 0 errores de compilacion que requirieran backtracking en todo el Haskell
- ~40+ calls al MCP (ghci_load, ghci_eval, ghci_type, cabal_build)
- 38/38 ejemplos del ejecutable pasaron

## Notas tecnicas

- Las reglas de `.claude/rules/` se cargan automaticamente en el contexto
- El MCP necesita estar conectado (verificar con ghci_session action=status)
- Si el MCP no esta disponible, rebuild: `cd mcp-server && npx tsc`
- El `.ghci` configura GHCi con `-fdefer-type-errors` y `-fprint-explicit-foralls`
- El auto-reload en ghci_type/ghci_info/ghci_kind hace `:r` antes de cada query
- GHC 9.12.2, GHC2024, Cabal 3.12. Deps: base, containers, array, mtl
- PATH de GHC: export PATH="$HOME/.ghcup/bin:$PATH"
- El parser usa backtracking simple con furthest-failure — sin `try` explicito
- Los MCP tools nuevos (ghci_batch, check_module strict) necesitan restart del MCP server

## Archivos modificados en sesion 4 (sin commitear)

Haskell:
- `src/HM/Syntax.hs` — +5 constructores Expr (ELetRec, EPair, EFst, ESnd, EAnn) + TProd en Type
- `src/HM/Subst.hs` — +2 casos TProd en apply/ftv
- `src/HM/Unify.hs` — +1 caso TProd en unify
- `src/HM/Infer.hs` — +5 reglas de inferencia (ELetRec, EPair, EFst, ESnd, EAnn)
- `src/HM/Pretty.hs` — +7 casos en ppType/ppExpr + helper parensAtom
- `src/Parser/Core.hs` — NUEVO: Parser type, instances, furthest-failure, ppParseError
- `src/Parser/Combinators.hs` — NUEVO: combinadores higher-order
- `src/Parser/Char.hs` — NUEVO: lexing y character parsers
- `src/Parser/HM.hs` — NUEVO: parser del lenguaje HM completo
- `app/Main.hs` — +runParsedExample, +parsedExamples, import Parser.HM y Parser.Core
- `haskell-rules-and-mcp.cabal` — +4 modulos en exposed-modules

TypeScript (MCP server):
- `mcp-server/src/ghci-session.ts` — +executeBatch() method
- `mcp-server/src/index.ts` — +ghci_batch tool registration
- `mcp-server/src/tools/check-module.ts` — wrap con -fno-defer-type-errors

Rules:
- `.claude/rules/haskell-development.md` — Protocolo incremental con 3 niveles
