# Prompt de Continuacion — Haskell Incremental Type-Checking System

Copia y pega esto en una nueva sesion de Claude Code abierta en este proyecto:

---

## El Prompt

```
Estoy trabajando en un sistema para mejorar como se escribe codigo Haskell con Claude Code.
El proyecto tiene varias partes, algunas ya implementadas y testeadas, otras recien agregadas
que necesitan testing end-to-end.

### Lo que ya existe y funciona

#### Parte 1: Claude Rules (.claude/rules/)
3 archivos de reglas que cambian tu comportamiento al escribir Haskell:
- `haskell-development.md` — Protocolo de tipado incremental: NUNCA escribir mas de 1 funcion sin compilar, type signatures primero, typed holes, :t para verificar subexpresiones
- `haskell-errors.md` — Base de conocimiento de ~25 errores comunes de GHC con codigos, explicaciones y fixes
- `haskell-project.md` — Config del proyecto: GHC 9.12, GHC2024, Cabal, build commands

#### Parte 2: MCP Server "haskell-ghci" (mcp-server/)
Un MCP server en TypeScript que mantiene una sesion GHCi PERSISTENTE con auto-reload.

Tools ORIGINALES (ya testeados y funcionando):
- `ghci_type` — :t con auto-reload
- `ghci_info` — :i con auto-reload
- `ghci_kind` — :k con auto-reload
- `ghci_eval` — evaluar expresiones puras
- `ghci_load` — cargar/recargar modulos con errores parseados
- `cabal_build` — compilacion completa con errores parseados
- `hoogle_search` — buscar funciones en Hoogle por tipo o nombre
- `ghci_session` — status/restart de la sesion GHCi

Tools NUEVOS (recien implementados, necesitan testing end-to-end):
- `ghci_scaffold` — Lee el .cabal, detecta modulos sin archivo fuente, crea stubs automaticamente. Resuelve el problema de tener que crear stubs a mano antes de reiniciar GHCi.
- `ghci_load` con `load_all=true` — Lee el .cabal y carga TODOS los modulos de la library en GHCi de una vez con scope completo (:m + *Module). Mas liviano que cabal_build porque es interpretado.
- `ghci_check_module` — Carga un modulo y devuelve un resumen estructurado de todos sus exports con tipos (via :browse). Muestra funciones, types, data, classes, y errores de compilacion si los hay.

Componentes compartidos nuevos:
- `parsers/cabal-parser.ts` — Parser de .cabal que extrae exposed-modules y hs-source-dirs
- `ghci-session.ts` — Nuevo metodo `loadModules()` que hace :l + :m + para cargar multiples modulos con scope

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

### Estado del MCP
El MCP esta configurado en .mcp.json. Despues de reiniciar Claude Code, los tools nuevos
deberian estar disponibles. Si no:
1. `cd mcp-server && npm install && npx tsc`
2. Reiniciar Claude Code en el proyecto

### Lo que necesito que hagas

OPCION A — TESTEAR LOS 3 TOOLS NUEVOS:
Los tools ghci_scaffold, ghci_load(load_all), y ghci_check_module fueron implementados y
testeados parcialmente (TypeScript compila, tests manuales via node pasan), pero necesitan
testing end-to-end real usandolos como MCP tools:

1. Verificar que los 3 tools nuevos aparecen en tu lista de tools disponibles
2. Testear ghci_scaffold:
   - Agrega un modulo ficticio al .cabal (ej: HM.NewModule)
   - Llama ghci_scaffold
   - Verifica que creo el stub src/HM/NewModule.hs
   - Reinicia GHCi y verifica que arranca sin error
   - Limpia (saca el modulo del .cabal, borra el archivo)
3. Testear ghci_load con load_all=true:
   - Llama ghci_load con load_all=true
   - Verifica que carga los 6 modulos de la library
   - Verifica que los nombres estan en scope (ghci_eval con runInfer, ppType, etc)
4. Testear ghci_check_module:
   - Llama ghci_check_module con module_path="src/HM/Syntax.hs"
   - Verifica que devuelve las 6 definiciones (Name, TVar, Lit, Expr, Type, Scheme)
   - Llama con module_path="src/HM/Infer.hs"
   - Verifica que devuelve TypeEnv, runInfer, inferExpr con sus tipos
   - Testea con un modulo que tenga un error: introduce un error en un archivo,
     llama ghci_check_module, verifica que reporta el error estructurado, arregla el error
5. Reporta que funciono y que no, y si hay bugs, arreglalos

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
- Implementacion del MCP server con 8 tools
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

## Notas tecnicas

- Las reglas de `.claude/rules/` se cargan automaticamente en el contexto
- El MCP necesita estar conectado (verificar con ghci_session action=status)
- Si el MCP no esta disponible, rebuild: `cd mcp-server && npx tsc`
- El `.ghci` configura GHCi con `-fdefer-type-errors` y `-fprint-explicit-foralls`
- El auto-reload en ghci_type/ghci_info/ghci_kind hace `:r` antes de cada query
- GHC 9.12.2, GHC2024, Cabal 3.12. Deps: base, containers, array, mtl
- PATH de GHC: export PATH="$HOME/.ghcup/bin:$PATH"
