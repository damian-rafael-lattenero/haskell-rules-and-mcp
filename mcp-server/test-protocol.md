# MCP Smoke Test Protocol

Lee este archivo completo y ejecuta cada paso en orden. Este protocolo ejerce todas
las capacidades del MCP server y genera un reporte de feedback estructurado.

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

Marcar como proyecto descartable:
```
touch playground/smoke-test/.disposable
```

## Paso 2: Switch al proyecto smoke-test

1. Llamar `ghci_switch_project(project="smoke-test")`
2. **Verificar**: respuesta con `success: true`, projectDir apunta a smoke-test
3. **Anotar**: Funciono el switch? Hubo delay? Errores?

## Paso 3: Primer ghci_load — errores y warnings mezclados

1. Llamar `ghci_load(load_all=true, diagnostics=true)`
2. **Esperado**: Al menos 1 error (mySum = True) y varios warnings
3. **Verificar**:
   - El error tiene `code: "GHC-83865"`, `expected: "Int"` o `"[Int] -> Int"`, `actual: "Bool"`
   - Los warnings tienen `warningFlag` populated
   - `warningActions` tiene entries para: unused-import, missing-signature, unused-matches, incomplete-patterns
   - Cada warningAction tiene `suggestedAction` con instrucciones concretas
4. **Anotar**: Cuantos warningActions? Se categorizaron bien? Falto alguno?

## Paso 4: Fix del error primero

Siguiendo el Error Resolution Table:
1. Leer el error de `mySum` — deberia ser type mismatch
2. Arreglar: `mySum [] = 0; mySum (x:xs) = x + mySum xs`
3. Llamar `ghci_load(load_all=true)` de nuevo
4. **Verificar**: 0 errores, solo warnings
5. **Anotar**: El error JSON tenia expected/actual correctos?

## Paso 5: Fix automatico de warnings (el automation loop)

Siguiendo la Warning Action Table, arreglar CADA warningAction automaticamente:
1. Leer cada `warningAction` del JSON
2. Aplicar la accion sugerida:
   - unused-import → borrar la linea de import
   - missing-signature → agregar la signature sugerida
   - unused-matches → reemplazar argumento con `_`
   - incomplete-patterns → agregar el pattern faltante
3. Llamar `ghci_load(load_all=true)` despues de cada fix (o al final de todos)
4. **Verificar**: 0 warnings, 0 errors
5. **Anotar**: Cuantos warnings habia? Cuantos se arreglaron automaticamente? Algun suggestedAction fue incorrecto?

## Paso 6: Verificar herramientas de exploracion

1. `ghci_type(expression="myLength")` → deberia dar `[a] -> Int`
2. `ghci_type(expression="safeHead")` → deberia dar `[a] -> Maybe a`
3. `ghci_info(name="Maybe")` → deberia mostrar data type con constructores
4. `ghci_eval(expression="myLength [1,2,3,4,5]")` → deberia dar `5`
5. `ghci_eval(expression="mySum [1,2,3]")` → deberia dar `6`
6. `ghci_eval(expression="safeHead []")` → deberia dar `Nothing`
7. `ghci_batch(commands=[":t double", ":t triple", "double 21"])` → 3 resultados
8. `hoogle_search(query="[a] -> Int")` → deberia encontrar `length`
9. **Anotar**: Todas funcionaron? Alguna dio output inesperado?

## Paso 7: QuickCheck inline

1. `ghci_quickcheck(property="\xs -> myLength xs == length (xs :: [Int])")` → PASS
2. `ghci_quickcheck(property="\xs -> mySum xs == sum (xs :: [Int])")` → PASS
3. `ghci_quickcheck(property="\x -> double x == x + (x :: Int)")` → PASS
4. `ghci_quickcheck(property="\x -> triple x == 3 * (x :: Int)")` → FAIL (triple tiene bug!)
5. **Anotar**: QuickCheck detecto el bug en triple? El counterexample fue util? Cuanto tardo?

## Paso 8: Arreglar bug descubierto por QuickCheck

1. Leer el counterexample de triple
2. Arreglar triple: `triple x = 3 * x`
3. `ghci_load(module_path="src/SmokeMath.hs")`
4. `ghci_quickcheck(property="\x -> triple x == 3 * (x :: Int)")` → ahora PASS
5. **Anotar**: El loop QuickCheck → fix → recheck funciono fluido?

## Paso 9: Typed holes

1. Agregar una funcion con hole en SmokeMath.hs:
   ```haskell
   mystery :: [Int] -> Int
   mystery xs = _
   ```
2. `ghci_load(module_path="src/SmokeMath.hs", diagnostics=true)`
3. **Verificar**: `holes` array tiene un entry con expectedType `Int`, relevantBindings incluye `xs :: [Int]`, fits incluye funciones relevantes
4. `ghci_hole_fits(module_path="src/SmokeMath.hs")` → deberia dar fits detallados
5. Implementar usando un fit sugerido (e.g., `mystery xs = mySum xs` o `mystery = myLength`)
6. `ghci_load` → verificar 0 issues
7. **Anotar**: Los hole fits fueron utiles? Sugirieron algo relevante?

## Paso 10: MCP Resources (rules)

1. Leer el resource `rules://haskell/automation` (usar `ReadMcpResourceTool` si disponible, o verificar via E2E que existe)
2. **Verificar**: Devuelve contenido markdown con la warning action table
3. Leer `rules://haskell/development`
4. Leer `rules://haskell/project-conventions`
5. **Anotar**: Los 3 resources estan disponibles? El contenido es correcto?

## Paso 11: Project switching

1. `ghci_switch_project()` (sin argumento) → lista de proyectos
2. **Verificar**: Lista incluye "hindley-milner" y "smoke-test"
3. `ghci_switch_project(project="hindley-milner")` → switch
4. `ghci_type(expression="map (+1)")` → deberia funcionar con el proyecto HM
5. `ghci_switch_project(project="smoke-test")` → volver
6. `ghci_type(expression="double")` → deberia funcionar con smoke-test
7. **Anotar**: El switch fue limpio? Hubo errores? Cuanto tardo?

## Paso 12: mcp_restart

1. Llamar `mcp_restart()`
2. En la siguiente tool call, verificar que el server reinicio
3. `ghci_session(action="status")` → alive
4. **Anotar**: El restart fue limpio? Claude Code reconecto automaticamente?

## Paso 13: Escribir reporte

Crear el archivo `mcp-server/test-results/{YYYY-MM-DD}.md` con este formato:

```markdown
# MCP Smoke Test Report — {fecha}

## Summary
- Total tools tested: X/16
- Tools that worked correctly: X
- Tools with issues: X
- Warnings categorized: X/X
- QuickCheck properties: X passed, X failed (expected)
- Bugs found by QuickCheck: X

## Tool Results

| Tool | Status | Notes |
|------|--------|-------|
| ghci_switch_project | OK/FAIL | ... |
| ghci_load (diagnostics) | OK/FAIL | ... |
| ghci_type | OK/FAIL | ... |
| ghci_eval | OK/FAIL | ... |
| ghci_info | OK/FAIL | ... |
| ghci_batch | OK/FAIL | ... |
| ghci_quickcheck | OK/FAIL | ... |
| ghci_hole_fits | OK/FAIL | ... |
| ghci_load (warnings) | OK/FAIL | ... |
| hoogle_search | OK/FAIL | ... |
| mcp_restart | OK/FAIL | ... |
| MCP Resources | OK/FAIL | ... |

## Warning Categorization

| Warning | Detected? | suggestedAction correct? | Auto-fixed? |
|---------|-----------|--------------------------|-------------|
| unused-import | YES/NO | YES/NO | YES/NO |
| missing-signature | YES/NO | YES/NO | YES/NO |
| unused-matches | YES/NO | YES/NO | YES/NO |
| incomplete-patterns | YES/NO | YES/NO | YES/NO |

## Error Resolution

| Error | Detected? | expected/actual correct? | Fixed following table? |
|-------|-----------|--------------------------|----------------------|
| Type mismatch (GHC-83865) | YES/NO | YES/NO | YES/NO |

## QuickCheck

| Property | Result | Notes |
|----------|--------|-------|
| myLength == length | PASS/FAIL | ... |
| mySum == sum | PASS/FAIL | ... |
| double x == x+x | PASS/FAIL | ... |
| triple x == 3*x | FAIL→fix→PASS | Bug detected by QC |

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
```

## Notas para Claude

- NO borres playground/smoke-test/ al final — dejalo para inspeccion
- Si un paso falla, anota el error y segui con el siguiente paso
- Se honesto en el reporte — si algo no funciona, decilo
- El reporte va en `mcp-server/test-results/` con la fecha como nombre
- Cada vez que corras este protocolo, borra smoke-test al PRINCIPIO (Paso 0), no al final
