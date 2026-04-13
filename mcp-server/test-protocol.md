# MCP Development Test Protocol v3

Lee este archivo completo y ejecuta el desarrollo en orden. Este protocolo NO es una
checklist de tools — es un **desarrollo real** de un mini proyecto Haskell que ejerce
los 26 tools de forma natural, como los usaria un desarrollador.

Al final, genera un reporte honesto evaluando la experiencia vs HLS+VSCode.

## Pre-requisitos

- Estar en el directorio raiz del proyecto `haskell-rules-and-mcp`
- El MCP server `haskell-ghci` debe estar corriendo
- Verificar: `ghci_session(action="status")` → `alive: true`
- Si hay `_notice` sobre rules no instaladas, correr `ghci_setup()`

## Paso 0: Cleanup

```
rm -rf playground/dev-test/
```

---

## FASE A: Bootstrap del proyecto

### Paso 1: Crear proyecto

Crear `playground/dev-test/` con un `.cabal`, `.ghci`, y `cabal.project` standard.
El proyecto se llama `calc` y va a ser una **calculadora con variables y evaluacion**.

```cabal
cabal-version:      3.12
name:               calc
version:            0.1.0.0

library
  exposed-modules:
    Calc.Syntax
    Calc.Eval
    Calc.Pretty
  build-depends:
    base >= 4.21 && < 5,
    containers,
    QuickCheck >= 2.14
  hs-source-dirs:   src
  default-language:  GHC2024
  ghc-options:       -Wall
```

Marcar como descartable: `touch playground/dev-test/.disposable`

### Paso 2: Scaffold y setup

1. `ghci_switch_project(project="dev-test")`
2. `ghci_scaffold()` → debe crear los 3 stubs
3. `ghci_session(action="restart")` → reiniciar con nuevo .cabal
4. `ghci_load(load_all=true)` → debe cargar los 3 stubs sin errores

**Anotar**: Scaffold creo los archivos? Load fue exitoso?

---

## FASE B: Implementar Calc.Syntax (AST + Arbitrary)

### Paso 3: Definir el AST

Implementar en `src/Calc/Syntax.hs`:

```haskell
module Calc.Syntax where

data Expr
  = Lit Double
  | Var String
  | Add Expr Expr
  | Mul Expr Expr
  | Let String Expr Expr   -- let x = e1 in e2
  deriving (Show, Eq)
```

**Usar el flujo real**:
1. Escribir el codigo
2. `ghci_load(module_path="src/Calc/Syntax.hs", diagnostics=true)` → 0 errores
3. Si hay warnings, arreglarlos siguiendo `warningActions`
4. `ghci_info(name="Expr")` → verificar que muestra los 5 constructores
5. `ghci_type(expression="Let")` → verificar tipo del constructor

**Anotar**: El loop edit→compile→fix funciono fluido?

### Paso 4: Agregar Arbitrary instance

Agregar al mismo modulo la instance de Arbitrary para QuickCheck:

```haskell
import Test.QuickCheck
import Data.Map.Strict qualified as Map  -- para Eval despues
```

Usar `ghci_add_import(name="Arbitrary")` para verificar que sugiere `Test.QuickCheck`.

Implementar `instance Arbitrary Expr` con generacion size-controlled.
Si no sabes el tipo exacto de `sized`, usar `ghci_type(expression="sized")`.
Si no sabes que funciones hay en QuickCheck, usar `ghci_complete(prefix="arb")`.

**Anotar**: `ghci_add_import` sugirio bien? `ghci_complete` ayudo?

---

## FASE C: Implementar Calc.Eval (Evaluador con errores)

### Paso 5: Definir el evaluador con typed holes

Implementar `src/Calc/Eval.hs` usando typed holes para explorar:

```haskell
module Calc.Eval where

import Data.Map.Strict qualified as Map
import Calc.Syntax

type Env = Map.Map String Double

data EvalError
  = UnboundVariable String
  | DivisionByZero
  deriving (Show, Eq)

eval :: Env -> Expr -> Either EvalError Double
eval env expr = case expr of
  Lit n       -> Right n
  Var x       -> _  -- usar typed hole
  Add e1 e2   -> _  -- usar typed hole
  Mul e1 e2   -> _  -- usar typed hole
  Let x e1 e2 -> _  -- usar typed hole
```

**Flujo con typed holes**:
1. Escribir con `_` en los cases
2. `ghci_load(module_path="src/Calc/Eval.hs", diagnostics=true)` → ver holes
3. Para cada hole, leer `expectedType` y `relevantBindings`
4. `ghci_hole_fits(module_path="src/Calc/Eval.hs")` → ver fits detallados
5. Implementar cada case basandose en los fits
6. Compilar de nuevo hasta 0 issues

**Cosas a probar durante la implementacion**:
- `ghci_type(expression="Map.lookup")` → entender la firma
- `ghci_doc(name="Map.lookup")` → leer documentacion
- `ghci_goto(name="EvalError")` → saltar a la definicion
- Si falta un import: `ghci_add_import(name="Map.lookup")`

**Anotar**: Los typed holes guiaron la implementacion? Los fits fueron utiles?

### Paso 6: Verificar con QuickCheck

Escribir propiedades:

```haskell
-- En GHCi via ghci_quickcheck:
-- 1. Lit siempre evalua bien
\n -> eval Map.empty (Lit n) == Right n

-- 2. Add es conmutativa
\e1 e2 -> eval Map.empty (Add (Lit e1) (Lit e2)) == eval Map.empty (Add (Lit e2) (Lit e1))

-- 3. Let binding funciona
\x n -> eval Map.empty (Let "x" (Lit n) (Var "x")) == Right n

-- 4. Variable no definida falla
eval Map.empty (Var "undefined_var") == Left (UnboundVariable "undefined_var")
```

Correr cada una con `ghci_quickcheck`. Si alguna falla, leer el counterexample y arreglar.

**Anotar**: Todas las properties pasan? Algun counterexample interesante?

---

## FASE D: Implementar Calc.Pretty (Pretty-printer + roundtrip)

### Paso 7: Pretty-printer

Implementar `src/Calc/Pretty.hs`:

```haskell
module Calc.Pretty where

import Calc.Syntax

pretty :: Expr -> String
```

Implementar `pretty` con parentizacion correcta.

**Flujo**:
1. Escribir la implementacion
2. `ghci_load` → compilar
3. `ghci_eval(expression="pretty (Add (Lit 1) (Mul (Lit 2) (Lit 3)))")` → verificar output
4. `ghci_eval(expression="pretty (Let \"x\" (Lit 5) (Add (Var \"x\") (Lit 1)))")` → verificar let

### Paso 8: Verificar API completa

1. `ghci_check_module(module_path="src/Calc/Syntax.hs")` → ver exports
2. `ghci_check_module(module_path="src/Calc/Eval.hs")` → ver exports
3. `ghci_check_module(module_path="src/Calc/Pretty.hs")` → ver exports

**Anotar**: La API se ve limpia? Los tipos tienen sentido?

---

## FASE E: Refactoring y navegacion

### Paso 9: Encontrar y renombrar

1. `ghci_references(name="eval")` → ver donde se usa
2. `ghci_rename(old_name="eval", new_name="evaluate")` → preview del rename
3. **Aplicar el rename** (usar Edit tool en cada archivo)
4. `ghci_load(load_all=true)` → verificar que compila
5. Re-correr las quickcheck properties con el nuevo nombre

**Anotar**: references encontro todas las ocurrencias? rename preview fue correcto?

### Paso 10: Agregar Div con error handling

Agregar un constructor `Div Expr Expr` a Syntax, implementar en Eval con division-by-zero check,
agregar a Pretty.

**Flujo real**:
1. Editar Syntax.hs → agregar `Div Expr Expr`
2. `ghci_load(load_all=true, diagnostics=true)` → debe dar warnings de incomplete patterns
3. Leer `warningActions` → deben indicar los patterns faltantes en Eval y Pretty
4. Arreglar cada warning
5. Compilar → 0 issues
6. `ghci_quickcheck(property="\\a -> eval Map.empty (Div (Lit a) (Lit 0)) == Left DivisionByZero")`

**Anotar**: El ciclo de agregar constructor fue fluido? Los warnings guiaron los cambios?

---

## FASE F: Code quality (opcional, depende de herramientas instaladas)

### Paso 11: Formatting

1. `ghci_format(module_path="src/Calc/Syntax.hs")` → ver si cambia algo
2. Si hay formatter: `ghci_format(module_path="src/Calc/Eval.hs", write=true)` → formatear

### Paso 12: Linting

1. `ghci_lint(module_path="src/Calc/Eval.hs")` → ver sugerencias de hlint

**Anotar**: Format/lint disponibles? Sugerencias utiles?

---

## FASE G: Session management

### Paso 13: Restart y recovery

1. `ghci_session(action="restart")` → reiniciar
2. `ghci_type(expression="evaluate")` → funciona despues del restart?
3. `mcp_restart()` → restart GHCi-only
4. `ghci_load(load_all=true)` → todo sigue funcionando?

### Paso 14: Project switching

1. `ghci_switch_project(project="hindley-milner")` → cambiar
2. `ghci_type(expression="map (+1)")` → funciona con hindley-milner?
3. `ghci_switch_project(project="dev-test")` → volver
4. `ghci_type(expression="evaluate")` → contexto preservado?

---

## Paso 15: Escribir reporte

Crear `mcp-server/test-results/{YYYY-MM-DD}-dev.md` con este formato:

```markdown
# MCP Development Test Report — {fecha}

## Resumen
- Proyecto: calc (calculadora con variables)
- Modulos implementados: X/3
- Funciones implementadas: X
- QuickCheck properties: X passed, X failed
- Warnings auto-fixed: X
- Errors resolved: X
- Tools usados: X/26

## Experiencia de desarrollo

### Flujo edit→compile→fix
| Aspecto | Rating (1-5) | Notas |
|---------|-------------|-------|
| Velocidad del loop | X | Cuanto tarda compile? |
| Calidad de warningActions | X | Fueron actionables? |
| Error messages | X | Fueron claros? expected/actual? |
| Typed holes workflow | X | Los fits ayudaron? |
| QuickCheck integration | X | Counterexamples utiles? |

### Navegacion y discovery
| Tool | Rating (1-5) | Notas |
|------|-------------|-------|
| ghci_goto | X | Encontro la definicion? |
| ghci_complete | X | Sugirio cosas utiles? |
| ghci_doc | X | Documentacion disponible? |
| ghci_imports | X | Info util? |
| ghci_add_import | X | Sugirio el modulo correcto? |
| ghci_references | X | Encontro todas las refs? |
| ghci_rename | X | Preview correcto? |

### Comparacion con HLS + VSCode

| Feature | MCP + Claude Code | HLS + VSCode | Ganador |
|---------|------------------|-------------|---------|
| Diagnostics → fix | Auto (warningActions) | Manual (click Quick Fix) | ? |
| Type exploration | ghci_type/info/kind | Hover | ? |
| Go-to-definition | ghci_goto | Ctrl+Click | ? |
| Find references | ghci_references | Shift+F12 | ? |
| Rename | ghci_rename + Edit | F2 | ? |
| Completions | ghci_complete | As-you-type | ? |
| Typed holes | ghci_hole_fits | Code action | ? |
| Property testing | ghci_quickcheck | No existe | ? |
| Import management | ghci_add_import | Code action | ? |
| Formatting | ghci_format | Format on save | ? |
| Linting | ghci_lint | hlint plugin | ? |
| Eval in context | ghci_eval/batch | No equivalent | ? |
| Overall workflow | Autonomous loop | Manual interaction | ? |

### Comparacion con GHC API directa

| Aspecto | MCP (via GHCi) | GHC API directa | Nota |
|---------|---------------|-----------------|------|
| Setup complexity | Bajo (npm + cabal) | Alto (Haskell project) | |
| Diagnostic quality | Parsed + categorized | Raw access to all | |
| Performance | Process per command | In-process | |
| Flexibility | 26 tools, extensible | Full compiler access | |
| Maintenance | TypeScript parsers | Haskell, breaks on GHC upgrades | |

### Que funciono bien
- (listar las cosas que funcionaron naturalmente)

### Friction points
- (listar donde el flujo se trabo o fue incomodo)

### Bugs encontrados
- (listar bugs nuevos si los hay)

### Que usaria un dev Haskell?
- Junior: (usaria esto? por que si/no?)
- Intermedio: (usaria esto? por que si/no?)
- Senior: (usaria esto? por que si/no?)

### Veredicto
- Rating general (1-10): X
- Reemplaza HLS? (si/no/parcial): X
- Lo usaria con Claude Code? (si/no): X
- Lo usaria SIN Claude Code (standalone MCP)? (si/no): X

## Metricas
- Total compilations: X
- Total tool calls: X
- Warnings auto-fixed: X
- Errors resolved: X
- QuickCheck tests run: X
- Tools used: X/26
- Tools NOT used: (listar cuales y por que)
```

## Notas para Claude

- Este NO es un smoke test. Es un desarrollo real. Usa los tools como los usarias naturalmente.
- NO testees tools por separado. Usalos en contexto: "necesito el tipo de X" → ghci_type, no "voy a probar ghci_type".
- Si algo no funciona, anota el error y segui. No intentes 10 veces.
- Se BRUTAL en el reporte. Si un tool no aporta valor, decilo. Si el flujo es mejor que VSCode en algo, decilo tambien.
- El reporte va en `mcp-server/test-results/` con la fecha + `-dev` como nombre.
- NO borres `playground/dev-test/` al final — dejalo para inspeccion.
- Contar TODOS los tool calls para las metricas.
