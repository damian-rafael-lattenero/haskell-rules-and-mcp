# MCP Comprehensive Smoke Test Protocol

Lee este archivo completo y ejecuta cada paso en orden. Este protocolo ejerce TODAS
las capacidades del MCP server (16 tools + 3 resources) y genera un reporte de feedback estructurado.

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

1. Llamar `ghci_switch_project()` (sin argumento) → lista de proyectos
2. **Verificar**:
   - Respuesta tiene `projects` array y `activeProject`
   - Lista incluye "hindley-milner" y su `dirName`
   - Cada proyecto tiene `name`, `dirName`, `path`, `active`
3. **Anotar**: Los nombres de proyecto son correctos? `dirName` aparece?

### Paso 3: Switch al proyecto smoke-test

1. Llamar `ghci_switch_project(project="smoke-test")`
2. **Verificar**: `success: true`, `projectDir` apunta a smoke-test, `alive: true`
3. **Anotar**: Funciono? Delay?

### Paso 4: Session status

1. Llamar `ghci_session(action="status")`
2. **Verificar**: `alive: true`, `projectDir` contiene "smoke-test"
3. **Anotar**: Status correcto?

---

## SECCION B: Compilation & Diagnostics (ghci_load — 4 modos)

### Paso 5: Load all con diagnostics — errores y warnings mezclados

1. Llamar `ghci_load(load_all=true, diagnostics=true)`
2. **Esperado**: Al menos 1 error real (mySum = True) en `errors[]` y warnings en `warnings[]`
3. **Verificar**:
   - El error tiene `code: "GHC-83865"`, `expected` y `actual` correctos
   - Los warnings tienen `warningFlag` populated
   - `warningActions` tiene entries para: unused-import, missing-signature, unused-matches, incomplete-patterns
   - Cada warningAction tiene `suggestedAction` con instrucciones concretas
   - El type error de mySum aparece en `errors[]` (NO como warning deferred)
   - `holes[]` esta vacio (no hay typed holes todavia)
4. **Anotar**: Cuantos errors? Cuantos warningActions? El dual-pass funciono (errores reales en errors[])?

### Paso 6: Fix del error primero

1. Arreglar mySum: `mySum [] = 0; mySum (x:xs) = x + mySum xs`
2. Llamar `ghci_load(load_all=true, diagnostics=true)` de nuevo
3. **Verificar**: 0 errores, solo warnings, `warningActions` tiene 4 entries
4. **Anotar**: El error JSON tenia expected/actual correctos?

### Paso 7: Fix automatico de warnings (el automation loop)

Siguiendo la Warning Action Table, arreglar CADA warningAction automaticamente:
1. Leer cada `warningAction` del JSON
2. Aplicar la accion sugerida:
   - unused-import → borrar la linea de import
   - missing-signature → agregar la signature sugerida
   - unused-matches → reemplazar argumento con `_`
   - incomplete-patterns → agregar el pattern faltante
3. Llamar `ghci_load(load_all=true)` despues de todos los fixes
4. **Verificar**: 0 warnings, 0 errors
5. **Anotar**: Cuantos warnings habia? Algun suggestedAction fue incorrecto?

### Paso 8: Load single module con diagnostics

1. Llamar `ghci_load(module_path="src/SmokeMath.hs", diagnostics=true)`
2. **Verificar**: `success: true`, 0 errors, 0 warnings (ya arreglamos todo)
3. **Anotar**: La carga individual funciona?

### Paso 9: Plain reload (sin module_path ni load_all)

1. Llamar `ghci_load()` (sin argumentos)
2. **Verificar**: Reload exitoso, modulos cargados
3. **Anotar**: El reload simple funciona?

### Paso 10: Plain reload con diagnostics

1. Llamar `ghci_load(diagnostics=true)`
2. **Verificar**: Dual-pass se ejecuta, 0 errors, 0 warnings
3. **Anotar**: El reload con diagnostics usa dual-pass?

---

## SECCION C: Type Exploration (ghci_type, ghci_info, ghci_kind)

### Paso 11: Type checking

1. `ghci_type(expression="myLength")` → `[a] -> Int`
2. `ghci_type(expression="safeHead")` → `[a] -> Maybe a`
3. `ghci_type(expression="double")` → `Num a => a -> a`
4. `ghci_type(expression="map (+1)")` → deberia incluir `Num` constraint
5. **Anotar**: Todos los tipos son correctos?

### Paso 12: Info lookup

1. `ghci_info(name="Maybe")` → data type con constructores Nothing, Just
2. `ghci_info(name="Container")` → typeclass con metodos empty, insert
3. `ghci_info(name="Wrap")` → newtype con su kind
4. **Anotar**: Info completa? Instances listadas?

### Paso 13: Kind checking

1. `ghci_kind(type_expression="Maybe")` → `* -> *`
2. `ghci_kind(type_expression="Either")` → `* -> * -> *`
3. `ghci_kind(type_expression="Wrap")` → `(* -> *) -> * -> *`
4. `ghci_kind(type_expression="Int")` → `*`
5. **Anotar**: Kinds correctos? Funciona con higher-kinded types?

---

## SECCION D: Evaluation (ghci_eval, ghci_batch)

### Paso 14: Expression evaluation

1. `ghci_eval(expression="myLength [1,2,3,4,5]")` → `output: "5"` (limpio, sin warnings)
2. `ghci_eval(expression="mySum [1,2,3]")` → `output: "6"`
3. `ghci_eval(expression="safeHead []")` → `output: "Nothing"`
4. `ghci_eval(expression="safeHead [42]")` → `output: "Just 42"`
5. `ghci_eval(expression="double 21")` → `output: "42"`
6. **Verificar**:
   - `output` contiene SOLO el resultado limpio (sin warnings de type-defaults)
   - Si hay warnings, aparecen en campo `warnings[]` separado
   - Si no hay warnings, no hay campo `warnings` ni `raw`
7. **Anotar**: El output esta limpio? Los warnings estan separados?

### Paso 15: Eval con error

1. `ghci_eval(expression="head []")` → deberia dar excepcion
2. **Verificar**: `success: false` o el error aparece en `output`
3. **Anotar**: Como maneja errores de runtime?

### Paso 16: Batch commands

1. `ghci_batch(commands=[":t double", ":t triple", "double 21", "myLength [1,2,3]"])`
2. **Verificar**:
   - `allSuccess: true`, `count: 4`
   - Cada result tiene `command`, `success`, `output` limpio
   - `output` de "double 21" es "42" (sin warnings mezclados)
3. **Anotar**: Batch funciona? Output limpio en cada resultado?

### Paso 17: Batch con reload

1. `ghci_batch(commands=[":t myLength"], reload=true)`
2. **Verificar**: `allSuccess: true`, reload previo no causo errores
3. **Anotar**: El reload previo funciono?

### Paso 18: Batch con stop_on_error

1. `ghci_batch(commands=["1 + 1", ":t nonExistentFunction", "2 + 2"], stop_on_error=true)`
2. **Verificar**:
   - `allSuccess: false`
   - Solo 2 resultados (se detuvo en el error)
   - El primer resultado fue exitoso
3. **Anotar**: stop_on_error funciona correctamente?

---

## SECCION E: QuickCheck (ghci_quickcheck)

### Paso 19: Properties que pasan

1. `ghci_quickcheck(property="\\xs -> myLength xs == length (xs :: [Int])")` → PASS
2. `ghci_quickcheck(property="\\xs -> mySum xs == sum (xs :: [Int])")` → PASS
3. `ghci_quickcheck(property="\\x -> double x == x + (x :: Int)")` → PASS
4. **Verificar**: `success: true`, `passed: 100` para cada una
5. **Anotar**: Todas pasaron?

### Paso 20: Property que falla (bug en triple)

1. `ghci_quickcheck(property="\\x -> triple x == 3 * (x :: Int)")` → FAIL
2. **Verificar**:
   - `success: false`
   - `counterexample` presente y util
   - `shrinks` >= 0
3. **Anotar**: QuickCheck detecto el bug? Counterexample util?

### Paso 21: QuickCheck con test count custom

1. `ghci_quickcheck(property="\\x -> double x == x + (x :: Int)", tests=500)`
2. **Verificar**: `passed: 500`
3. **Anotar**: El count custom funciona?

### Paso 22: Arreglar bug y recheck

1. Arreglar triple: `triple x = 3 * x`
2. `ghci_load(module_path="src/SmokeMath.hs")`
3. `ghci_quickcheck(property="\\x -> triple x == 3 * (x :: Int)")` → ahora PASS
4. **Anotar**: El loop fix → recheck funciono?

---

## SECCION F: Typed Holes (ghci_load + ghci_hole_fits)

### Paso 23: Agregar typed hole

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
4. **Anotar**: Los holes se detectaron? Relevant bindings correctos?

### Paso 24: Hole fits detallados

1. `ghci_hole_fits(module_path="src/SmokeMath.hs")`
2. **Verificar**:
   - Respuesta tiene holes con fits estructurados
   - Cada fit tiene `name`, `type`, `specialization`
3. **Anotar**: Los fits detallados incluyen funciones utiles? Hay refinement fits?

### Paso 25: Implementar usando hole fit

1. Implementar mystery usando un fit sugerido o relevantBindings (e.g., `mystery = length`)
2. `ghci_load(module_path="src/SmokeMath.hs")` → 0 issues
3. **Anotar**: El workflow hole → implement funciono?

---

## SECCION G: Module Inspection (ghci_check_module)

### Paso 26: Browse module exports

1. `ghci_check_module(module_path="src/SmokeMath.hs")`
2. **Verificar**:
   - Respuesta tiene `definitions[]` con tipo y kind para cada export
   - Incluye: double, triple, safeHead, mystery
   - Cada definicion tiene `name`, `type`, `kind` ("function", "type", "data", etc.)
   - `totalDefinitions` es correcto
3. **Anotar**: Todas las definiciones aparecen? Tipos correctos?

### Paso 27: Browse module con tipos complejos

1. `ghci_check_module(module_path="src/SmokeKind.hs")`
2. **Verificar**:
   - Incluye Wrap (kind: "data" o "type"), Container (kind: "class")
   - Los metodos del typeclass aparecen
3. **Anotar**: Tipos complejos se parsean bien?

---

## SECCION H: Search & Build (hoogle_search, cabal_build)

### Paso 28: Hoogle — busqueda por tipo

1. `hoogle_search(query="[a] -> Int")` → primer resultado deberia ser `length`
2. **Verificar**:
   - `success: true`, `count` > 0
   - Cada resultado tiene `name`, `module`, `package`, `docs`
3. **Anotar**: Resultados relevantes?

### Paso 29: Hoogle — busqueda por nombre

1. `hoogle_search(query="mapM", count=5)` → deberia encontrar mapM
2. **Verificar**: `count` <= 5, resultados incluyen mapM
3. **Anotar**: El parametro count funciona?

### Paso 30: Hoogle — busqueda por firma compleja

1. `hoogle_search(query="(a -> b) -> [a] -> [b]")` → deberia encontrar `map`
2. **Anotar**: Busqueda por firma de tipo funciona?

### Paso 31: Cabal build

1. `cabal_build()` (sin componente)
2. **Verificar**: Build exitoso o errores parseados
3. **Anotar**: Build funciona? Errores parseados correctamente?

### Paso 32: Cabal build con componente

1. `cabal_build(component="lib:smoke-test")`
2. **Verificar**: Build exitoso del componente especifico
3. **Anotar**: Componente especifico funciona?

---

## SECCION I: Scaffolding (ghci_scaffold)

### Paso 33: Scaffold con modulo faltante

1. Agregar un modulo nuevo al .cabal que NO tiene archivo fuente:
   ```
   Editar smoke-test.cabal: agregar "SmokeNew" a exposed-modules
   ```
2. `ghci_scaffold()`
3. **Verificar**:
   - `created` incluye el path del stub creado para SmokeNew
   - `alreadyExist` incluye SmokeMath, SmokeList, SmokeKind
   - El archivo stub existe con `module SmokeNew where`
4. Llamar `ghci_load(load_all=true)` → deberia cargar sin errores
5. **Anotar**: Scaffold creo el stub? Load funciono despues?

---

## SECCION J: MCP Resources (rules)

### Paso 34: Leer resources

1. Leer `rules://haskell/automation` → debe contener Warning Action Table
2. Leer `rules://haskell/development` → debe contener Compilation Discipline
3. Leer `rules://haskell/project-conventions` → debe contener Import Style
4. **Verificar**: Los 3 resources devuelven markdown valido con contenido relevante
5. **Anotar**: Algun resource fallo?

---

## SECCION K: Session Management (ghci_session, mcp_restart)

### Paso 35: Session restart

1. `ghci_session(action="restart")`
2. **Verificar**: `success: true`, `alive: true`
3. `ghci_type(expression="double")` → funciona despues del restart
4. **Anotar**: Restart limpio? Funciones accesibles despues?

### Paso 36: mcp_restart (GHCi-only, default)

1. `mcp_restart()` (sin argumentos)
2. **Verificar**:
   - `success: true`
   - Mensaje indica "GHCi session restarted. MCP server still running."
   - `alive: true`
3. En la SIGUIENTE tool call, verificar que el server sigue vivo:
   - `ghci_session(action="status")` → `alive: true`
4. `ghci_type(expression="double")` → funciona
5. **Anotar**: El server NO se desconecto? GHCi reinicio correctamente?

### Paso 37: Project switching round-trip

1. `ghci_switch_project(project="hindley-milner")` → switch
2. `ghci_type(expression="map (+1)")` → funciona con hindley-milner
3. `ghci_switch_project(project="smoke-test")` → volver
4. `ghci_type(expression="double")` → funciona con smoke-test
5. **Anotar**: Round-trip limpio? Cada proyecto mantiene su contexto?

---

## SECCION L: Edge Cases

### Paso 38: ghci_type con expresion invalida

1. `ghci_type(expression="nonExistentFunction")`
2. **Verificar**: `success: false`, mensaje de error informativo
3. **Anotar**: Error manejado correctamente?

### Paso 39: ghci_eval con division por cero

1. `ghci_eval(expression="div 1 0")`
2. **Verificar**: Error de runtime capturado
3. **Anotar**: Excepciones de runtime se manejan?

### Paso 40: ghci_switch_project a proyecto inexistente

1. `ghci_switch_project(project="no-existe")`
2. **Verificar**: `success: false`, error con lista de proyectos disponibles
3. **Anotar**: Error informativo?

### Paso 41: ghci_quickcheck con property invalida

1. `ghci_quickcheck(property="not a valid property")`
2. **Verificar**: `success: false`, error descriptivo
3. **Anotar**: Error manejado?

### Paso 42: ghci_load de modulo inexistente

1. `ghci_load(module_path="src/NoExiste.hs")`
2. **Verificar**: Error en la respuesta
3. **Anotar**: Error informativo?

---

## Paso 43: Escribir reporte

Crear el archivo `mcp-server/test-results/{YYYY-MM-DD}.md` con este formato:

```markdown
# MCP Comprehensive Smoke Test Report — {fecha}

## Summary
- Total tools tested: X/16
- Tools that worked correctly: X
- Tools with issues: X
- Warning categories tested: X/X
- QuickCheck properties: X passed, X failed (expected)
- Edge cases tested: X
- MCP Resources: X/3

## Tool Results

### Core Tools
| Tool | Status | Notes |
|------|--------|-------|
| ghci_switch_project (list) | OK/FAIL | ... |
| ghci_switch_project (switch) | OK/FAIL | ... |
| ghci_session (status) | OK/FAIL | ... |
| ghci_session (restart) | OK/FAIL | ... |
| ghci_load (load_all + diagnostics) | OK/FAIL | ... |
| ghci_load (single module) | OK/FAIL | ... |
| ghci_load (plain reload) | OK/FAIL | ... |
| ghci_load (reload + diagnostics) | OK/FAIL | ... |
| ghci_type | OK/FAIL | ... |
| ghci_info | OK/FAIL | ... |
| ghci_kind | OK/FAIL | ... |
| ghci_eval | OK/FAIL | ... |
| ghci_batch | OK/FAIL | ... |
| ghci_batch (reload) | OK/FAIL | ... |
| ghci_batch (stop_on_error) | OK/FAIL | ... |
| ghci_quickcheck | OK/FAIL | ... |
| ghci_quickcheck (custom count) | OK/FAIL | ... |
| ghci_hole_fits | OK/FAIL | ... |
| ghci_check_module | OK/FAIL | ... |
| hoogle_search (type) | OK/FAIL | ... |
| hoogle_search (name + count) | OK/FAIL | ... |
| cabal_build | OK/FAIL | ... |
| cabal_build (component) | OK/FAIL | ... |
| ghci_scaffold | OK/FAIL | ... |
| mcp_restart (GHCi-only) | OK/FAIL | ... |
| MCP Resources | OK/FAIL | ... |

### Diagnostic Pipeline
| Feature | Status | Notes |
|---------|--------|-------|
| Dual-pass compilation (load_all) | OK/FAIL | Type errors in errors[], not warnings |
| Dual-pass compilation (single) | OK/FAIL | ... |
| Dual-pass compilation (reload) | OK/FAIL | ... |
| Error parsing (GHC-83865) | OK/FAIL | expected/actual extracted? |
| Warning categorization | OK/FAIL | X/4 categories correct |
| Typed hole detection | OK/FAIL | expectedType, relevantBindings, fits? |
| Eval output cleaning | OK/FAIL | Warnings separated from result? |
| Batch output cleaning | OK/FAIL | Warnings separated per command? |

### Warning Categorization
| Warning | Detected? | suggestedAction correct? | Auto-fixed? |
|---------|-----------|--------------------------|-------------|
| unused-import | YES/NO | YES/NO | YES/NO |
| missing-signature | YES/NO | YES/NO | YES/NO |
| unused-matches | YES/NO | YES/NO | YES/NO |
| incomplete-patterns | YES/NO | YES/NO | YES/NO |

### Error Resolution
| Error | Detected? | expected/actual correct? | In errors[] (not warnings)? |
|-------|-----------|--------------------------|----------------------------|
| Type mismatch (GHC-83865) | YES/NO | YES/NO | YES/NO |

### QuickCheck
| Property | Result | Notes |
|----------|--------|-------|
| myLength == length | PASS/FAIL | ... |
| mySum == sum | PASS/FAIL | ... |
| double x == x+x | PASS/FAIL | ... |
| triple x == 3*x (before fix) | FAIL expected | Counterexample? |
| triple x == 3*x (after fix) | PASS/FAIL | ... |
| double (custom 500 tests) | PASS/FAIL | Correct count? |

### Edge Cases
| Case | Handled? | Notes |
|------|----------|-------|
| ghci_type invalid expr | YES/NO | ... |
| ghci_eval runtime error | YES/NO | ... |
| switch to nonexistent project | YES/NO | ... |
| quickcheck invalid property | YES/NO | ... |
| load nonexistent module | YES/NO | ... |

## Project Management
| Feature | Status | Notes |
|---------|--------|-------|
| Project listing | OK/FAIL | dirName field present? |
| Project naming | OK/FAIL | hindley-milner shows correct name? |
| Project switch round-trip | OK/FAIL | Context preserved per project? |

## Friction Points
- (listar problemas encontrados)

## What Worked Well
- (listar cosas que funcionaron bien)

## Recommendations for Next Iteration
- (sugerencias concretas)

## Metrics
- Total compilations: X
- Total tool calls: X
- Warnings auto-fixed: X
- Errors resolved: X
- QuickCheck tests run: X
- Edge cases tested: X
```

## Notas para Claude

- NO borres playground/smoke-test/ al final — dejalo para inspeccion
- Si un paso falla, anota el error y segui con el siguiente paso
- Se honesto en el reporte — si algo no funciona, decilo
- El reporte va en `mcp-server/test-results/` con la fecha como nombre
- Cada vez que corras este protocolo, borra smoke-test al PRINCIPIO (Paso 0), no al final
- Contar TODOS los tool calls que hagas para el reporte final
