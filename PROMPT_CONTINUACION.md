# Prompt de Continuacion — Haskell Incremental Type-Checking System

Copia y pega esto en una nueva sesion de Claude Code abierta en este proyecto:

---

## El Prompt

```
Estoy trabajando en un sistema para mejorar drasticamente como escribis codigo Haskell.
El proyecto tiene dos partes ya implementadas:

### Parte 1: Claude Rules (.claude/rules/)
3 archivos de reglas que cambian tu comportamiento al escribir Haskell:
- `haskell-development.md` — Protocolo de tipado incremental: NUNCA escribir mas de 1 funcion sin compilar, type signatures primero, typed holes, :t para verificar subexpresiones, estrategia de subagentes paralelos para type-checking
- `haskell-errors.md` — Base de conocimiento de ~25 errores comunes de GHC con codigos, explicaciones y fixes
- `haskell-project.md` — Config del proyecto: GHC 9.12, GHC2024, Cabal, build commands

### Parte 2: MCP Server "haskell-ghci" (mcp-server/)
Un MCP server en TypeScript que mantiene una sesion GHCi PERSISTENTE con auto-reload.
8 tools disponibles:
- `ghci_type` — :t con auto-reload (siempre ve el codigo mas reciente)
- `ghci_info` — :i con auto-reload
- `ghci_kind` — :k con auto-reload
- `ghci_eval` — evaluar expresiones puras
- `ghci_load` — cargar/recargar modulos con errores parseados a JSON
- `cabal_build` — compilacion completa con errores parseados
- `hoogle_search` — buscar funciones en Hoogle por tipo o nombre
- `ghci_session` — status/restart de la sesion GHCi

El MCP esta configurado en .mcp.json. Si no esta conectado, hay que:
1. `cd mcp-server && npm install && npx tsc`
2. Reiniciar Claude Code en el proyecto

### Estado actual del Haskell
Solo tiene un hello world (src/Lib.hs con greet, app/Main.hs).
GHC 9.12.2, GHC2024, Cabal 3.12. Deps: base, containers, array.
PATH de GHC: export PATH="$HOME/.ghcup/bin:$PATH"

### Lo que necesito que hagas

OPCION A — TEST ANTES/DESPUES:
Quiero una prueba comparativa. Vamos a implementar un mini-proyecto en Haskell que sea lo suficientemente complejo como para que los errores de tipo sean un problema real.

Propuesta: un evaluador de expresiones aritmeticas con tipos (un mini type-checker):
- Un AST con Lit, Add, Mul, Neg, Let, Var, IfZero
- Un type system con TInt y TBool
- Un evaluador que respete los tipos
- Pattern matching, Maybe, Map, recursion — todas cosas que generan errores de tipos

Paso 1: Implementalo SIN usar los tools del MCP (solo cabal build al final, como harias normalmente). Conta cuantas compilaciones fallidas tenes y cuanto tardas.

Paso 2: Borra lo que hiciste y reimplementalo USANDO el protocolo de las reglas + los tools del MCP (ghci_type, ghci_load, etc). Compila incrementalmente, usa :t para verificar subexpresiones, usa hoogle_search si necesitas buscar una funcion por tipo. Conta compilaciones fallidas y tiempo.

Paso 3: Reporta la comparacion.

OPCION B — EXPANDIR EL SISTEMA:
Mejoras posibles al MCP y las reglas:
1. Agregar un tool `ghci_hole_fits` que analice typed holes y devuelva los valid fits como lista estructurada
2. Mejorar el error parser para que parsee errores multi-linea de GHC 9.12 mas robustamente
3. Agregar un tool que haga "type-check this entire module and give me a summary" en un solo call
4. Agregar reglas para monadic code (transformers, do-notation patterns)
5. Crear un test suite automatizado para el MCP server

OPCION C — MINI PROYECTO HASKELL:
Implementa algo interesante en Haskell usando el sistema completo (reglas + MCP):
- Un parser combinador desde cero (estilo Parsec simplificado)
- Un pequeno sistema de tipos Hindley-Milner
- Un interprete de lambda calculus con De Bruijn indices
- Un constraint solver

Usa los MCP tools activamente: ghci_type para verificar cada funcion, hoogle_search para encontrar funciones utiles, ghci_eval para testear incrementalmente.

Decime cual opcion queres hacer (o una combinacion).
```

---

## Notas tecnicas para el prompt

- Las reglas de `.claude/rules/` se cargan automaticamente en el contexto — no hay que hacer nada
- El MCP necesita estar conectado (verificar con ghci_session action=status)
- Si el MCP no esta disponible, los tools no van a aparecer. Rebuild: `cd mcp-server && npx tsc`
- El `.ghci` configura GHCi con `-fdefer-type-errors` y `-fprint-explicit-foralls`
- El auto-reload en ghci_type/ghci_info/ghci_kind hace `:r` antes de cada query
