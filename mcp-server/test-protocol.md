# MCP Comprehensive Smoke Test Protocol v2

Lee este archivo completo y ejecuta cada paso en orden. Este protocolo ejerce TODAS
las capacidades del MCP server (16 tools + 3 resources), cada parametro, cada modo,
cada edge case conocido, y genera un reporte de feedback estructurado.

## Pre-requisitos

- Estar en el directorio raiz del proyecto `haskell-rules-and-mcp`
- El MCP server `haskell-ghci` debe estar corriendo (verificar con `ghci_session(action="status")`)

## Paso 0: Cleanup

Si existe `playground/smoke-test/`, borrarlo completamente:
```
rm -rf playground/smoke-test/
```

## Paso 1: Crear proyecto smoke-test

Crear un proyecto Haskell minimo en `playground/smoke-test/`:

**playground/smoke-test/smoke-test.cabal:**
```cabal
cabal-version:      3.12
name:               smoke-test
version:            0.1.0.0

library
  exposed-modules:
    SmokeMath
    SmokeList
    SmokeKind
  build-depends:
    base >= 4.21 && < 5,
    QuickCheck >= 2.14
  hs-source-dirs:   src
  default-language:  GHC2024
  ghc-options:       -Wall
```

**playground/smoke-test/cabal.project:**
```
packages: .
with-compiler: ghc-9.12
```

**playground/smoke-test/.ghci:**
```
:set -fdefer-type-errors
:set -ferror-spans
:set prompt "ghci> "
:set prompt-cont "ghci| "
```

**playground/smoke-test/src/SmokeMath.hs:**
```haskell
module SmokeMath where

-- Dejar SIN type signature (para testear missing-signature warning)
double x = x + x

-- Dejar con argumento unused (para testear unused-matches warning)
triple :: Int -> Int
triple x = 3 * 42

-- Dejar con patterns incompletos (para testear incomplete-patterns warning)
safeHead :: [a] -> Maybe a
safeHead (x:_) = Just x
```

**playground/smoke-test/src/SmokeList.hs:**
```haskell
module SmokeList where

import Data.List (sort)  -- unused import (para testear unused-imports warning)

-- Funcion correcta
myLength :: [a] -> Int
myLength []     = 0
myLength (_:xs) = 1 + myLength xs

-- Type error intencional (para testear error resolution)
mySum :: [Int] -> Int
mySum = True
```

**playground/smoke-test/src/SmokeKind.hs:**
```haskell
module SmokeKind where

-- Higher-kinded type para testear ghci_kind
newtype Wrap f a = Wrap { unWrap :: f a }

-- Typeclass con constraint para testear ghci_info en detalle
class Container f where
  empty :: f a
  insert :: a -> f a -> f a
```

Marcar como proyecto descartable:
```
touch playground/smoke-test/.disposable
```

---

## SECCION A: Project Management (ghci_switch_project, ghci_session)

### Paso 2: Project listing

1. `ghci_switch_project()` (sin argumento) → lista de proyectos
2. **Verificar**:
   - Respuesta tiene `projects` array y `activeProject`
   - Lista incluye "hindley-milner" con `dirName`
   - Cada proyecto tiene `name`, `dirName`, `path`, `active`
3. **Anotar**: Los nombres de proyecto son correctos? `dirName` aparece?

### Paso 3: Switch al proyecto smoke-test

1. `ghci_switch_project(project="smoke-test")`
2. **Verificar**: `success: true`, `projectDir` apunta a smoke-test, `alive: true`

### Paso 4: Session status

1. `ghci_session(action="status")`
2. **Verificar**: `alive: true`, `projectDir` contiene "smoke-test"

### Paso 5: Switch a proyecto inexistente (edge case)

1. `ghci_switch_project(project="no-existe")`
2. **Verificar**: `success: false`, error con lista de proyectos disponibles

---

## SECCION B: Compilation & Diagnostics (ghci_load — 6 modos)

### Paso 6: Load all con diagnostics — errores y warnings mezclados

1. `ghci_load(load_all=true, diagnostics=true)`
2. **Esperado**: Al menos 1 error real (mySum = True) en `errors[]` y warnings en `warnings[]`
3. **Verificar**:
   - El error tiene `code: "GHC-83865"`, `expected` y `actual` correctos
   - Los warnings tienen `warningFlag` populated
   - `warningActions` tiene entries para: missing-signature, unused-matches, incomplete-patterns
   - Cada warningAction tiene `suggestedAction` con instrucciones concretas
   - El type error de mySum aparece en `errors[]` (NO como warning deferred)
   - `holes[]` esta vacio (no hay typed holes todavia)
4. **Anotar**: Cuantos errors? Cuantos warningActions? El dual-pass funciono?

### Paso 7: Fix del error primero

1. Arreglar mySum: `mySum [] = 0; mySum (x:xs) = x + mySum xs`
2. `ghci_load(load_all=true, diagnostics=true)` de nuevo
3. **Verificar**: 0 errores, 4 warningActions (incluyendo unused-import)

### Paso 8: Fix automatico de warnings (el automation loop)

Siguiendo la Warning Action Table, arreglar CADA warningAction automaticamente:
1. Leer cada `warningAction` del JSON
2. Aplicar la accion sugerida
3. `ghci_load(load_all=true)` despues de todos los fixes
4. **Verificar**: 0 warnings, 0 errors

### Paso 9: Load single module con diagnostics

1. `ghci_load(module_path="src/SmokeMath.hs", diagnostics=true)`
2. **Verificar**: `success: true`, 0 errors, 0 warnings

### Paso 10: Load single module SIN diagnostics (explicit false)

1. `ghci_load(module_path="src/SmokeMath.hs", diagnostics=false)`
2. **Verificar**: `success: true`, respuesta tiene errors/warnings pero NO dual-pass

### Paso 11: Plain reload (sin argumentos)

1. `ghci_load()` (sin argumentos)
2. **Verificar**: Reload exitoso

### Paso 12: Plain reload con diagnostics

1. `ghci_load(diagnostics=true)`
2. **Verificar**: Dual-pass se ejecuta, 0 errors, 0 warnings

### Paso 13: Load de modulo inexistente (edge case)

1. `ghci_load(module_path="src/NoExiste.hs")`
2. **Verificar**: `success: false`, error contiene "Can't find"
3. **Anotar**: El error de `<no location info>` se parseo correctamente?

---

## SECCION C: Type Exploration (ghci_type, ghci_info, ghci_kind)

### Paso 14: Type checking basico

1. `ghci_type(expression="myLength")` → `[a] -> Int`
2. `ghci_type(expression="safeHead")` → `[a] -> Maybe a`
3. `ghci_type(expression="double")` → `Num a => a -> a`
4. `ghci_type(expression="map (+1)")` → deberia incluir `Num` constraint
5. **Anotar**: Todos los tipos son correctos?

### Paso 15: Type checking — operadores y expresiones complejas

1. `ghci_type(expression="(+)")` → `Num a => a -> a -> a`
2. `ghci_type(expression="(,)")` → `a -> b -> (a, b)`
3. `ghci_type(expression="\\x -> x + (1 :: Int)")` → `Int -> Int`
4. **Anotar**: Parsea correctamente operadores, secciones, lambdas?

### Paso 16: Type checking — nombre inexistente (deferred scope)

1. `ghci_type(expression="nonExistentFunction")`
2. **Verificar**: `success: false` (NO `success: true` con type `p`)
3. **Anotar**: La deteccion de deferred-out-of-scope funciona?

### Paso 17: Info lookup

1. `ghci_info(name="Maybe")` → kind: "data", constructores Nothing/Just, instances
2. `ghci_info(name="Container")` → kind: "class", metodos empty/insert
3. `ghci_info(name="Wrap")` → kind: "newtype"
4. `ghci_info(name="Eq")` → kind: "class"
5. **Anotar**: Kind classification correcta? (data/class/newtype, no "type-synonym")

### Paso 18: Info — nombre inexistente (deferred scope)

1. `ghci_info(name="nonExistentThing")`
2. **Verificar**: `success: false` (NO `success: true` con kind "function" y type "p")

### Paso 19: Kind checking

1. `ghci_kind(type_expression="Maybe")` → `* -> *`
2. `ghci_kind(type_expression="Either")` → `* -> * -> *`
3. `ghci_kind(type_expression="Wrap")` → `(* -> *) -> * -> *` (higher-kinded)
4. `ghci_kind(type_expression="Int")` → `*`
5. **Anotar**: Kinds correctos? Higher-kinded funciona?

---

## SECCION D: Evaluation (ghci_eval, ghci_batch)

### Paso 20: Expression evaluation — output limpio

1. `ghci_eval(expression="myLength [1,2,3,4,5]")` → `output: "5"`
2. `ghci_eval(expression="mySum [1,2,3]")` → `output: "6"`
3. `ghci_eval(expression="safeHead []")` → `output: "Nothing"`
4. `ghci_eval(expression="safeHead [42]")` → `output: "Just 42"`
5. `ghci_eval(expression="double 21")` → `output: "42"`
6. **Verificar**: `output` limpio (sin warnings mezclados). Warnings en `warnings[]` separado.

### Paso 21: Eval — runtime exceptions (deben ser success:false)

1. `ghci_eval(expression="head []")` → `success: false`, output contiene "Exception"
2. `ghci_eval(expression="div 1 0")` → `success: false`, output contiene "divide by zero"
3. **Verificar**: `success: false` para AMBOS (no `true` con la excepcion en output)

### Paso 22: Eval — output indentado (edge case)

1. Probar con expresion que produce output con leading spaces (si es posible)
2. **Verificar**: El resultado no se confunde con continuacion de warning

### Paso 23: Batch commands

1. `ghci_batch(commands=[":t double", ":t triple", "double 21", "myLength [1,2,3]"])`
2. **Verificar**: `allSuccess: true`, `count: 4`, output limpio por comando

### Paso 24: Batch con reload

1. `ghci_batch(commands=[":t myLength"], reload=true)`
2. **Verificar**: `allSuccess: true`, reload previo exitoso

### Paso 25: Batch con stop_on_error

1. `ghci_batch(commands=["1 + 1", ":l nonexistent_xyz.hs", "2 + 2"], stop_on_error=true)`
2. **Verificar**: `allSuccess: false`, solo 2 resultados (se detuvo en el error)

### Paso 26: Batch con array vacio (edge case)

1. `ghci_batch(commands=[])`
2. **Verificar**: `allSuccess: true`, `count: 0`, `results: []`

---

## SECCION E: QuickCheck (ghci_quickcheck)

### Paso 27: Properties que pasan

1. `ghci_quickcheck(property="\\xs -> myLength xs == length (xs :: [Int])")` → PASS
2. `ghci_quickcheck(property="\\xs -> mySum xs == sum (xs :: [Int])")` → PASS
3. `ghci_quickcheck(property="\\x -> double x == x + (x :: Int)")` → PASS
4. **Verificar**: `success: true`, `passed: 100`

### Paso 28: Property que falla (bug en triple)

1. `ghci_quickcheck(property="\\x -> triple x == 3 * (x :: Int)")` → FAIL
2. **Verificar**: `success: false`, `counterexample` presente, `shrinks` >= 0

### Paso 29: QuickCheck con test count custom

1. `ghci_quickcheck(property="\\x -> double x == x + (x :: Int)", tests=500)`
2. **Verificar**: `passed: 500`

### Paso 30: Arreglar bug y recheck

1. Arreglar triple: `triple x = 3 * x`
2. `ghci_load(module_path="src/SmokeMath.hs")`
3. `ghci_quickcheck(property="\\x -> triple x == 3 * (x :: Int)")` → PASS

### Paso 31: QuickCheck — input validation

1. `ghci_quickcheck(property=":! echo pwned")` → `success: false`, error sobre ":"
2. `ghci_quickcheck(property="not a valid property")` → `success: false`, error descriptivo
3. **Verificar**: Rechaza GHCi command injection, maneja output invalido

### Paso 32: QuickCheck — "Gave up!" scenario (si posible)

1. Intentar una property con precondicion restrictiva:
   `ghci_quickcheck(property="\\x -> (x :: Int) > 1000000 ==> x * 2 > 2000000")`
2. **Verificar**: Si da "Gave up!", el error NO debe decir "Couldn't parse".
   Debe tener un mensaje util sobre precondiciones.

---

## SECCION F: Typed Holes (ghci_load + ghci_hole_fits)

### Paso 33: Agregar typed hole

1. Agregar en SmokeMath.hs:
   ```haskell
   mystery :: [Int] -> Int
   mystery xs = _
   ```
2. `ghci_load(module_path="src/SmokeMath.hs", diagnostics=true)`
3. **Verificar**:
   - `holes[]` tiene un entry
   - `expectedType` es `Int`
   - `relevantBindings` incluye `xs :: [Int]`
   - `topFits` tiene sugerencias

### Paso 34: Agregar named typed hole

1. Cambiar a: `mystery xs = _myHole`
2. `ghci_load(module_path="src/SmokeMath.hs", diagnostics=true)`
3. **Verificar**: `holes[0].hole` es `_myHole` (no solo `_`)

### Paso 35: Hole fits detallados

1. `ghci_hole_fits(module_path="src/SmokeMath.hs")`
2. **Verificar**:
   - Cada fit tiene `name`, `type`, `specialization`, `source`
   - `relevantBindings` tiene locations (bound at ...)

### Paso 36: Hole fits con max_fits custom

1. `ghci_hole_fits(module_path="src/SmokeMath.hs", max_fits=3)`
2. **Verificar**: Respuesta OK (puede tener `suppressed: true`)

### Paso 37: Implementar usando hole fit y verificar

1. Implementar mystery con un fit sugerido (e.g., `mystery = length`)
2. `ghci_load(module_path="src/SmokeMath.hs")` → 0 issues

---

## SECCION G: Module Inspection (ghci_check_module)

### Paso 38: Browse module exports

1. `ghci_check_module(module_path="src/SmokeMath.hs")`
2. **Verificar**:
   - `definitions[]` tiene todas las funciones: double, triple, safeHead, mystery
   - Cada definicion tiene `name`, `type`, `kind`
   - `summary.functions` count correcto

### Paso 39: Browse module con typeclasses

1. `ghci_check_module(module_path="src/SmokeKind.hs")`
2. **Verificar**:
   - Wrap aparece (kind: "type" o "data")
   - Container aparece (kind: "class")
   - **Los metodos empty e insert aparecen como definitions SEPARADAS** (no concatenados)
   - empty.type es `f a` (NO contiene "insert" ni "MINIMAL")
   - insert.type es `a -> f a -> f a`

### Paso 40: Browse module con module_name explicito

1. `ghci_check_module(module_path="src/SmokeMath.hs", module_name="SmokeMath")`
2. **Verificar**: `module: "SmokeMath"` en la respuesta

### Paso 41: Browse module — module_name inferido del path

1. `ghci_check_module(module_path="src/SmokeMath.hs")` (sin module_name)
2. **Verificar**: `module: "SmokeMath"` inferido de `src/SmokeMath.hs`

---

## SECCION H: Search & Build (hoogle_search, cabal_build)

### Paso 42: Hoogle — busqueda por tipo

1. `hoogle_search(query="[a] -> Int")` → primer resultado deberia ser `length`
2. **Verificar**: `success: true`, resultados tienen `name`, `module`, `package`, `docs`

### Paso 43: Hoogle — busqueda por nombre

1. `hoogle_search(query="mapM", count=5)` → deberia encontrar mapM
2. **Verificar**: `count` <= 5, resultados incluyen mapM

### Paso 44: Hoogle — busqueda por firma compleja

1. `hoogle_search(query="(a -> b) -> [a] -> [b]")` → deberia encontrar `map`

### Paso 45: Cabal build (sin componente)

1. `cabal_build()` (sin argumentos)
2. **Verificar**: Build exitoso

### Paso 46: Cabal build con componente especifico

1. `cabal_build(component="lib:smoke-test")`
2. **Verificar**: Build exitoso del componente

---

## SECCION I: Scaffolding (ghci_scaffold)

### Paso 47: Scaffold con modulo faltante

1. Agregar "SmokeNew" a exposed-modules en .cabal
2. `ghci_scaffold()`
3. **Verificar**:
   - `created` incluye `src/SmokeNew.hs`
   - `alreadyExist` incluye los 3 modulos existentes
   - El archivo stub tiene `module SmokeNew where`
4. `ghci_load(load_all=true)` → deberia cargar sin errores

---

## SECCION J: MCP Resources (rules)

### Paso 48: Leer resources

1. Leer `rules://haskell/automation` → debe contener Warning Action Table
2. Leer `rules://haskell/development` → debe contener Compilation Discipline
3. Leer `rules://haskell/project-conventions` → debe contener Import Style
4. **Verificar**: Los 3 resources devuelven markdown valido con contenido relevante

---

## SECCION K: Session Management (ghci_session, mcp_restart)

### Paso 49: Session restart

1. `ghci_session(action="restart")`
2. **Verificar**: `success: true`, `alive: true`
3. `ghci_type(expression="double")` → funciona despues del restart

### Paso 50: mcp_restart (GHCi-only, default)

1. `mcp_restart()` (sin argumentos)
2. **Verificar**: `alive: true`, server NO se desconecto
3. `ghci_session(action="status")` → `alive: true`
4. `ghci_type(expression="double")` → funciona

### Paso 51: Project switching round-trip

1. `ghci_switch_project(project="hindley-milner")` → switch
2. `ghci_type(expression="map (+1)")` → funciona con hindley-milner
3. `ghci_switch_project(project="smoke-test")` → volver
4. `ghci_type(expression="double")` → funciona con smoke-test

---

## SECCION L: Edge Cases & Bug Regression

### Paso 52: ghci_type con expresion invalida (deferred scope)

1. `ghci_type(expression="totallyFakeFunction")`
2. **Verificar**: `success: false` (NO `true` con type `p`)

### Paso 53: ghci_info con nombre inexistente (deferred scope)

1. `ghci_info(name="totallyFakeThing")`
2. **Verificar**: `success: false` (NO `true` con kind "function")

### Paso 54: ghci_eval con division por cero

1. `ghci_eval(expression="div 1 0")`
2. **Verificar**: `success: false`, output contiene "divide by zero"

### Paso 55: ghci_eval con head de lista vacia

1. `ghci_eval(expression="head ([] :: [Int])")`
2. **Verificar**: `success: false`, output contiene "Exception"

### Paso 56: ghci_load de modulo inexistente

1. `ghci_load(module_path="src/NoExiste.hs")`
2. **Verificar**: `success: false`, error parseado (no solo en raw)

### Paso 57: ghci_quickcheck con property invalida

1. `ghci_quickcheck(property="not a valid property")`
2. **Verificar**: `success: false`, error descriptivo

### Paso 58: ghci_quickcheck con GHCi command injection

1. `ghci_quickcheck(property=":! echo pwned")`
2. **Verificar**: `success: false`, rechazado por validacion

---

## Paso 59: Escribir reporte

Crear el archivo `mcp-server/test-results/{YYYY-MM-DD}.md` con este formato:

```markdown
# MCP Comprehensive Smoke Test Report — {fecha}

## Summary
- Total tools tested: X/16
- Tools that worked correctly: X
- Tools with issues: X
- Warning categories tested: X/4
- QuickCheck properties: X passed, X failed (expected)
- Edge cases tested: X
- MCP Resources: X/3
- Bug regressions verified: X/12

## Tool Results

### Core Tools
| Tool | Status | Notes |
|------|--------|-------|
| ghci_switch_project (list) | OK/FAIL | ... |
| ghci_switch_project (switch) | OK/FAIL | ... |
| ghci_switch_project (nonexistent) | OK/FAIL | ... |
| ghci_session (status) | OK/FAIL | ... |
| ghci_session (restart) | OK/FAIL | ... |
| ghci_load (load_all + diagnostics) | OK/FAIL | ... |
| ghci_load (single module + diagnostics) | OK/FAIL | ... |
| ghci_load (single module, no diagnostics) | OK/FAIL | ... |
| ghci_load (plain reload) | OK/FAIL | ... |
| ghci_load (reload + diagnostics) | OK/FAIL | ... |
| ghci_load (nonexistent file) | OK/FAIL | ... |
| ghci_type | OK/FAIL | ... |
| ghci_type (operators/lambdas) | OK/FAIL | ... |
| ghci_type (deferred scope) | OK/FAIL | ... |
| ghci_info (data/class/newtype) | OK/FAIL | ... |
| ghci_info (deferred scope) | OK/FAIL | ... |
| ghci_kind | OK/FAIL | ... |
| ghci_eval (clean output) | OK/FAIL | ... |
| ghci_eval (runtime exceptions) | OK/FAIL | ... |
| ghci_batch | OK/FAIL | ... |
| ghci_batch (reload) | OK/FAIL | ... |
| ghci_batch (stop_on_error) | OK/FAIL | ... |
| ghci_batch (empty) | OK/FAIL | ... |
| ghci_quickcheck (pass) | OK/FAIL | ... |
| ghci_quickcheck (fail + counterexample) | OK/FAIL | ... |
| ghci_quickcheck (custom count) | OK/FAIL | ... |
| ghci_quickcheck (input validation) | OK/FAIL | ... |
| ghci_quickcheck (gave up) | OK/FAIL | ... |
| ghci_hole_fits | OK/FAIL | ... |
| ghci_hole_fits (named hole) | OK/FAIL | ... |
| ghci_hole_fits (max_fits) | OK/FAIL | ... |
| ghci_check_module (functions) | OK/FAIL | ... |
| ghci_check_module (class methods) | OK/FAIL | ... |
| ghci_check_module (module_name) | OK/FAIL | ... |
| hoogle_search (type) | OK/FAIL | ... |
| hoogle_search (name + count) | OK/FAIL | ... |
| hoogle_search (complex signature) | OK/FAIL | ... |
| cabal_build | OK/FAIL | ... |
| cabal_build (component) | OK/FAIL | ... |
| ghci_scaffold | OK/FAIL | ... |
| mcp_restart (GHCi-only) | OK/FAIL | ... |
| MCP Resources (3x) | OK/FAIL | ... |

### Diagnostic Pipeline
| Feature | Status | Notes |
|---------|--------|-------|
| Dual-pass compilation (load_all) | OK/FAIL | Type errors in errors[], not warnings |
| Dual-pass compilation (single module) | OK/FAIL | ... |
| Dual-pass compilation (reload) | OK/FAIL | ... |
| Error parsing (GHC-83865 type mismatch) | OK/FAIL | expected/actual extracted? |
| Error parsing (<no location info>) | OK/FAIL | Can't find errors detected? |
| Warning categorization | OK/FAIL | X/4 categories correct |
| Typed hole detection | OK/FAIL | expectedType, relevantBindings, fits? |
| Named hole detection | OK/FAIL | _myHole recognized? |
| Eval output cleaning | OK/FAIL | Warnings separated from result? |
| Batch output cleaning | OK/FAIL | Warnings separated per command? |

### Warning Categorization
| Warning | Detected? | suggestedAction correct? | Auto-fixed? |
|---------|-----------|--------------------------|-------------|
| unused-import | YES/NO | YES/NO | YES/NO |
| missing-signature | YES/NO | YES/NO (multiline?) | YES/NO |
| unused-matches | YES/NO | YES/NO | YES/NO |
| incomplete-patterns | YES/NO | YES/NO | YES/NO |

### Bug Regression Checks (12 bugs from 2026-04-12)
| Bug | Regression test | Status |
|-----|----------------|--------|
| #1 ghci_type deferred scope → success:false | Paso 52 | OK/REGRESSED |
| #2 ghci_eval exceptions → success:false | Paso 54-55 | OK/REGRESSED |
| #3 ghci_load missing file → success:false | Paso 56 | OK/REGRESSED |
| #4 ghci_info kind classification | Paso 17 | OK/REGRESSED |
| #5 check_module method concatenation | Paso 39 | OK/REGRESSED |
| #6 eval indented result swallowed | Paso 22 | OK/REGRESSED |
| #7 cabal test-suite module leak | (unit test) | OK/REGRESSED |
| #8 ghci_info deferred scope | Paso 53 | OK/REGRESSED |
| #9 QuickCheck "Gave up!" | Paso 32 | OK/REGRESSED |
| #10 missing-signature multiline | (unit test) | OK/REGRESSED |
| #11 error parser multiline span | (unit test) | OK/REGRESSED |
| #12 cabal common stanza | (documented, not fixed) | N/A |

### QuickCheck
| Property | Result | Notes |
|----------|--------|-------|
| myLength == length | PASS/FAIL | ... |
| mySum == sum | PASS/FAIL | ... |
| double x == x+x | PASS/FAIL | ... |
| triple x == 3*x (before fix) | FAIL expected | Counterexample? |
| triple x == 3*x (after fix) | PASS/FAIL | ... |
| double (custom 500 tests) | PASS/FAIL | Correct count? |
| input validation (: prefix) | REJECTED | ... |
| gave up scenario | HANDLED | Error message useful? |

### Edge Cases
| Case | Handled? | Notes |
|------|----------|-------|
| ghci_type deferred out-of-scope | YES/NO | success:false, not type "p" |
| ghci_info deferred out-of-scope | YES/NO | success:false |
| ghci_eval runtime exception (head []) | YES/NO | success:false |
| ghci_eval runtime exception (div 0) | YES/NO | success:false |
| switch to nonexistent project | YES/NO | error with available list |
| quickcheck invalid property | YES/NO | ... |
| quickcheck command injection | YES/NO | ... |
| quickcheck gave up | YES/NO | useful error message |
| load nonexistent module | YES/NO | success:false with parsed error |
| batch with empty commands | YES/NO | ... |
| batch stop on error | YES/NO | stops at error command |

## Project Management
| Feature | Status | Notes |
|---------|--------|-------|
| Project listing | OK/FAIL | dirName field present? |
| Project naming | OK/FAIL | names from .cabal? |
| Project switch round-trip | OK/FAIL | Context preserved? |
| Nonexistent project error | OK/FAIL | Lists available? |

## Friction Points
- (listar problemas encontrados)

## What Worked Well
- (listar cosas que funcionaron bien)

## New Bugs Found
- (listar bugs nuevos descubiertos durante este run)

## Recommendations for Next Iteration
- (sugerencias concretas)

## Metrics
- Total compilations: X
- Total tool calls: X
- Warnings auto-fixed: X
- Errors resolved: X
- QuickCheck tests run: X
- Edge cases tested: X
- Bug regressions verified: X/12
```

## Notas para Claude

- NO borres playground/smoke-test/ al final — dejalo para inspeccion
- Si un paso falla, anota el error y segui con el siguiente paso
- Se honesto en el reporte — si algo no funciona, decilo
- El reporte va en `mcp-server/test-results/` con la fecha como nombre
- Cada vez que corras este protocolo, borra smoke-test al PRINCIPIO (Paso 0), no al final
- Contar TODOS los tool calls que hagas para el reporte final
- Los 12 bug regression checks son CRITICOS — si alguno regresa, es bloqueante
