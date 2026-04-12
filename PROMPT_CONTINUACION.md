# Prompt de Continuacion — Haskell Development Assistant (Sesion 7+)

Copia y pega esto en una nueva sesion de Claude Code abierta en este proyecto:

---

## El Prompt

```
Vengo de 6 sesiones de trabajo construyendo un sistema para mejorar como se escribe
codigo Haskell con Claude Code. El sistema tiene 3 partes: Claude Rules, MCP Server,
y un proyecto Haskell de ejemplo (Hindley-Milner type inference + parser combinators).

En la sesion 6 hice un overhaul grande: elimine 536 lineas de reglas tutorial y las
reemplace con reglas de automatizacion orientadas a accion. Tambien agregue nuevos
tools al MCP y mejore los existentes. Los cambios de TypeScript estan compilados pero
el MCP server necesita haberse reiniciado (que pasa al abrir esta sesion nueva).

### PENDIENTE INMEDIATO: Verificar que los cambios de sesion 6 funcionan

Los siguientes cambios fueron compilados (npx tsc OK) pero NO testeados con el MCP
server corriendo el codigo nuevo. Esta sesion deberia tener el server fresco.

#### Test 1: ghci_load con diagnostics y warningActions

Crear un modulo temporal con warnings intencionales:
```haskell
module HM.TestWarn where
import Data.List (sort)  -- unused import
foo x = x + 1            -- missing signature
bar :: Int -> Int
bar x = 42               -- unused match
baz :: Maybe Int -> Int
baz (Just n) = n          -- incomplete patterns
quux :: Int -> String
quux x = _                -- typed hole
```

1. Llamar `ghci_load(module_path="src/HM/TestWarn.hs")` (diagnostics=true por default)
2. Verificar que el output tiene:
   - `warningActions` array con entries categorizadas:
     - category "unused-import", suggestedAction "Remove unused import: Data.List"
     - category "missing-signature", suggestedAction con el tipo inferido
     - category "unused-binding", suggestedAction "Prefix with underscore: _x"
     - category "incomplete-patterns", suggestedAction "Add missing pattern(s): Nothing"
     - category "typed-hole", suggestedAction con info del hole
   - `holes` array con el hole de quux (expectedType "String", fits)
   - `warningFlag` en cada warning (e.g., "-Wunused-imports", "-Wmissing-signatures")
3. Si warningActions NO aparece: el MCP server no levanto el codigo nuevo.
   Fix: `cd mcp-server && npx tsc` y luego `ghci_session(action="restart")`
4. Limpiar el modulo temporal.

#### Test 2: ghci_quickcheck

1. Llamar `ghci_quickcheck(property="\xs -> reverse (reverse xs) == (xs :: [Int])")`
   - Esperado: success=true, passed=100
2. Llamar `ghci_quickcheck(property="\x -> x + 1 == (x :: Int)")`
   - Esperado: success=false, counterexample con algun Int
3. Llamar `ghci_quickcheck(property="\x y -> x + y == y + (x :: Int)", tests=500)`
   - Esperado: success=true, passed=500

#### Test 3: error parser mejorado (warningFlag + expected/actual)

Crear modulo con type error:
```haskell
module HM.TestErr where
foo :: Int
foo = True
```
1. Llamar `ghci_load(module_path="src/HM/TestErr.hs")`
2. Verificar que el error tiene:
   - code: "GHC-83865"
   - expected: "Int"
   - actual: "Bool"
3. Limpiar.

#### Test 4: ghci_load(load_all=true) con diagnostics

1. Llamar `ghci_load(load_all=true)` sobre el proyecto completo
2. Verificar: success=true, 10 modulos cargados, 0 errores
3. Si hay warnings, verificar que warningActions tiene sugerencias

#### Test 5: backward compatibility

1. `cabal run haskell-rules-and-mcp` — los 54+ ejemplos (manuales + parseados) deben pasar
2. `ghci_type(expression="map (+1)")` — debe funcionar normal
3. `ghci_batch(commands=[":t foldr", ":t map", "1 + 2"])` — debe dar 3 resultados OK
4. `ghci_hole_fits(module_path=...)` — sigue funcionando independiente
5. `ghci_diagnostics(module_path=...)` — ahora delega a ghci_load (deprecado pero funcional)

### DESPUES DE VERIFICAR: Implementar algo en Haskell para validar el sistema

La idea es hacer un mini proyecto Haskell que realmente ejercite el loop automatizado:
rules de automatizacion + MCP tools + QuickCheck. Quiero que se vea la diferencia.

**Opcion sugerida: Agregar listas al Hindley-Milner**
- Agregar EList (list literal) y TList (list type) al AST
- Agregar `:` (cons), `head`, `tail`, `null` como built-ins
- Agregar syntactic sugar `[1, 2, 3]` al parser
- Agregar pattern matching basico (ECase)
- Escribir propiedades QuickCheck para el parser/inferencia

Esto ejercita:
- El loop ghci_load → fix warnings automaticamente → recompilar
- ghci_quickcheck para verificar propiedades del parser
- ghci_type y ghci_info para explorar tipos
- La regla de "new Expr constructors require changes in: Syntax + Infer + Pretty + Parser.HM"
- Warning cleanup automatico (incomplete patterns al agregar constructores)

Alternativas si preferis algo mas rapido:
- Agregar solo listas (sin pattern matching) — mas chico
- Implementar un evaluador (interpretar las expresiones ademas de tipificarlas)
- Escribir un QuickCheck test suite para el parser existente

### DESPUES DE IMPLEMENTAR: Evaluacion del sistema

Quiero que al final de la sesion hagas un reporte honesto:

1. **Pros reales**: Que funciono bien del loop automatizado? Los warningActions
   ayudaron? El QuickCheck inline aporto? Se siente la diferencia vs claude code pelado?

2. **Cons reales**: Que friction hubo? Los warnings se categorizaron bien? Hubo
   errores en los regex? Algun tool fallo o devolvio data incorrecta? Las rules
   nuevas guiaron bien el comportamiento o se ignoraron?

3. **Metricas**: Cuantas compilaciones? Cuantos errores? Cuantos warnings arreglados
   automaticamente? Cuantas propiedades QuickCheck corridas? Tiempo estimado vs
   sin el sistema?

4. **Recomendaciones**: Que cambiar, agregar, o quitar para la proxima iteracion?
   Ser brutal — si algo no aporta, decirlo.
```

---

## Estado actual del sistema (post sesion 6)

### Claude Rules (.claude/rules/) — 3 archivos, ~290 lineas

| Archivo | Lineas | Contenido |
|---------|--------|-----------|
| `haskell-project.md` | ~25 | Toolchain, module architecture, conventions del proyecto |
| `haskell-development.md` | ~35 | Compilation discipline, type-first development, typed holes, error recovery |
| `haskell-automation.md` | ~120 | **NUEVO**: Warning action table, error resolution protocol, QuickCheck integration, the loop |

**Eliminados en sesion 6:**
- `haskell-errors.md` (224 lineas) — tutorial de errores GHC, reemplazado por error resolution protocol en automation.md
- `haskell-monadic.md` (312 lineas) — tutorial de monads, eliminado sin reemplazo (Claude ya lo sabe)

### MCP Server "haskell-ghci" — 15 tools

| Tool | Estado | Descripcion |
|------|--------|-------------|
| `ghci_type` | OK | :t con auto-reload |
| `ghci_info` | OK | :i con auto-reload |
| `ghci_kind` | OK | :k con auto-reload |
| `ghci_eval` | OK | Evaluar expresiones |
| `ghci_load` | **MEJORADO** | Ahora con diagnostics param: dual-pass, warningActions, holes |
| `ghci_load(load_all=true)` | **MEJORADO** | Idem, para todos los modulos |
| `cabal_build` | OK | Compilacion completa |
| `hoogle_search` | OK | Buscar en Hoogle |
| `ghci_session` | OK | Status/restart |
| `ghci_scaffold` | OK | Crear stubs para modulos |
| `ghci_check_module` | OK | Browse + exports |
| `ghci_batch` | OK | Multiples comandos en 1 call |
| `ghci_hole_fits` | OK | Typed holes estructurados |
| `ghci_diagnostics` | **DEPRECADO** | Delega a ghci_load(diagnostics=true) |
| `ghci_quickcheck` | **NUEVO** | Correr propiedades QuickCheck inline |

**Componentes internos modificados en sesion 6:**
- `parsers/error-parser.ts` — Nuevo campo `warningFlag` (-Wunused-imports etc.), mejor extraccion expected/actual con fallback regex
- `parsers/warning-categorizer.ts` — **NUEVO**: Categoriza warnings y sugiere acciones concretas (unused-import, missing-signature, incomplete-patterns, unused-binding, typed-hole, etc.)
- `tools/load-module.ts` — Reescrito: integra diagnostics dual-pass, warning categorization, hole parsing
- `tools/diagnostics.ts` — Reducido a wrapper que delega a load-module
- `tools/quickcheck.ts` — **NUEVO**: Importa Test.QuickCheck en GHCi, corre propiedades, parsea output

### Hindley-Milner + Parser (10 modulos Haskell)

**HM Engine** (5 modulos, ~400 lineas):
- AST con 11 constructores de Expr, 4 de Type, 2 de Lit
- Algorithm W con let-polimorfismo, letrec, pares, annotations
- defaultEnv con 13 operadores built-in (+, -, *, ==, /=, <, >, <=, >=, &&, ||, ., negate)

**Parser** (4 modulos, ~480 lineas):
- Operadores infijos con precedencia correcta (|| < && < == < + < * < .)
- Multi-arg lambda: `\x y z -> body`
- Multi-binding let: `let x = 1; y = 2 in x + y`
- "Did you mean?" hints para keywords mal escritas (Levenshtein distance)

**Main.hs**: 54+ ejemplos (26 AST manuales + 28+ parseados)
- Incluye operators, multi-arg lambda, multi-binding let, error cases

**Dependencies**: base, containers, array, mtl, QuickCheck

---

## Historial completo de sesiones

### Sesion 1: Setup inicial (commit d09f276)
- 3 claude rules + MCP server con 8 tools + .mcp.json

### Sesion 2: HM Engine + Fix MCP (commits 7ccc86d, 9ee2d39, ecbb127)
- Fix sentinel off-by-one + PATH macOS ARM
- Hindley-Milner 5 modulos, 0 compilaciones fallidas
- 3 tools nuevos: ghci_scaffold, load_all, ghci_check_module

### Sesion 3: Testing + Fix race condition (commit 536a7b2)
- 3/3 tools nuevos testeados PASS
- Fix race condition en ghci_session restart (kill async, settled flag)

### Sesion 4: Expand HM + Parser Combinators (commit 1bb2909)
- ELetRec, Pares, Annotations en el HM engine
- Parser combinator library desde cero (4 modulos)
- 38/38 ejemplos end-to-end
- ghci_batch tool, check_module strict

### Sesion 5: Mejorar MCP (commit 28b9ab8)
- ghci_hole_fits y ghci_diagnostics (nuevos)
- haskell-monadic.md (nueva regla)
- Fix regex error-parser para [-Wflag]

### Sesion 6: Parser improvements + System overhaul (commits 182428f, 7646104)

**Parser improvements (182428f):**
- Multi-arg lambda: `\x y z -> body`
- Operadores infijos con precedencia: `||, &&, ==, /=, <, >, <=, >=, +, -, *, .`
- Multi-binding let: `let x = 1; y = 2 in body`
- "Did you mean?" para keywords mal escritas
- Pretty printer: collapse nested lambdas, infix notation para operadores
- 13 nuevos parsed examples (total 54+)
- Verificacion: ghci_hole_fits PASS (3/3 holes), ghci_diagnostics PASS (4/4 tests)

**System overhaul (7646104):**
- Rules: -536 lineas tutorial, +120 lineas automation (726 → 290 lineas, -60%)
  - Eliminados: haskell-errors.md, haskell-monadic.md
  - Creado: haskell-automation.md (warning action table, error resolution, QuickCheck integration, the loop)
  - Reescritos: haskell-project.md (invariantes), haskell-development.md (esenciales)
- MCP: error-parser con warningFlag, warning-categorizer (nuevo), ghci_load mejorado con diagnostics+categorization+holes, ghci_diagnostics deprecado, ghci_quickcheck (nuevo)
- Haskell: QuickCheck agregado como dependencia
- Verificacion: TypeScript compila, cabal build OK, 54+ ejemplos pasan, QuickCheck funciona en GHCi (`quickCheck (\xs -> reverse (reverse xs) == (xs :: [Int]))` → +++ OK, passed 100 tests)
- **NOTA**: Los cambios MCP necesitan restart del server para activarse (primera vez en sesion 7)

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
| 8 | 5652ebe | Update continuation prompt, add vitest devDep |
| 9 | 182428f | Enhance parser: multi-arg lambda, infix operators, multi-binding let, typo hints |
| 10 | 7646104 | Overhaul system: action-oriented rules, warning categorizer, QuickCheck tool |

## Notas tecnicas

- GHC 9.12.2, GHC2024, Cabal 3.12. Deps: base, containers, array, mtl, QuickCheck
- El MCP server es TypeScript/Node.js, rebuild con: `cd mcp-server && npx tsc`
- GHCi session es persistente con sentinel-based protocol, auto-reload en type/info/kind
- El `.ghci` configura: `-fdefer-type-errors`, `-ferror-spans`, `-fprint-explicit-foralls`
- El parser usa backtracking simple con furthest-failure — sin `try` explicito
- Las rules se cargan automaticamente en el contexto de cada conversacion
- `haskell-automation.md` es el archivo central: contiene el loop automatizado y la warning action table
- Los operadores desugaran a `EApp (EApp (EVar op) e1) e2)` — new ops necesitan entry en defaultEnv
