# Veredicto de mercado — haskell-flows MCP (E5, 2026-10-01)

Base: dogfood propio (E2, F1–F27), autopsia (E3), propuesta de
condensación (E4) + research competitivo (GitHub API, 2026-10-01).

## El paisaje competitivo (medido, no opinado)

| Repo | Stars | Qué es |
|---|---|---|
| drshade/haskell-mcp-server | 49 | SDK/framework MCP **en** Haskell (no tool de dev) |
| phoityne/pty-mcp-server | 42 | ghci/cabal/ghc vía **PTY** (terminal cruda al agente) |
| buecking/hs-mcp | 24 | SDK MCP en Haskell |
| juspay/fdep-mcp-server | 4 | 40+ tools de análisis estático (Juspay = shop Haskell grande) |
| hoogle-mcp / gooh | 1 | wrappers de hoogle |
| **haskell-rules-and-mcp (este)** | **4** | 36 tools, property-first, in-process GHC |

Hechos duros:
1. **La categoría entera es marginal**: el competidor directo más
   traccionado (42★) es el de arquitectura MÍNIMAL (PTY = terminal),
   no el de superficie curada. Señal de mercado: los agentes
   resuelven Haskell con terminal + LSP; la curación de tools no
   demostró demanda.
2. **Traction propia**: 4★ / 0 forks / 0 watchers en ~6 meses.
3. **El sustituto es gratis y ya está integrado**: los hosts de
   agentes (opencode, Cursor, Claude Code) integran **LSP nativo** —
   `ghc_type/info/browse/goto/doc/hole` compiten contra HLS gratis.
   Y `cabal test` + QuickCheck a mano compiten contra el property
   loop — mi propio dogfood lo probó: el bug real (`EVar ""`) lo
   encontró el camino sustituto cuando el pipeline del MCP no podía.

## El argumento «los modelos crecieron mucho» — corta en ambos sentidos

**En contra**: los modelos frontera escriben Haskell razonable, saben
pedir `cabal test`, y el valor de scaffold/wrappers tiende a cero. La
parte del MCP que funciona (scaffold, deps, gate) es commodity.

**A favor**: Haskell es un lenguaje de **baja densidad de corpus** para
LLMs — los modelos son peores en Haskell que en TS/Python
proporcionalmente, y lo que peor hacen es exactamente lo que el
property-first ataca: invariantes semánticos invisibles
(soundness, roundtrips, leyes que nadie escribe). **Ningún
competidor hace law-suggestion.** Si funcionara confiablemente, sería
el único en su categoría — mi sesión lo probó en miniatura: cuando QC
corrió, encontró el bug real que el type-checker jamás vería.

## Diagnóstico estructural (por qué no crece armónicamente)

La cadena de evidencia de E2/E3 lo muestra como un solo patrón:

1. **El diferenciador es la parte rota.** El asset único
   (sesión GHC in-process + property loop) es donde viven los bugs
   estructurales: 3 vehículos de ejecución divergentes (F24–F26),
   transporte zombi (F13/15), deadlock de authoría (F16). Lo que
   funciona es commodity. Inversión de valor.
2. **La superficie creció más rápido que los invariants.** 36 tools ×
   envelope × nextStep × fases = espacio de estados que los tests
   (700 unit aislados + 72 e2e de happy-path) no cubren
   composicionalmente. Por eso cada dogfood produce un batch B-1..B-7:
   se testean tools, no composiciones (mi sesión rompió donde sus
   audits dieron ✅).
3. **Bus factor 1 contra un runtime hostil** (GHC API + cabal + locks +
   FFI) — el git log muestra fixes sintomáticos en bucle.

## Veredicto

**¿Lugar real en el mercado HOY? No.** Categoría sin demanda
demostrada, sustitutos gratis integrados en los hosts, diferenciador
roto, mantenimiento de runtime pesado para un maintainer solo.

**¿Prueba conceptual? Sí — de las interesantes.** Demuestra tres cosas
con valor real más allá del repo:
1. Los agentes NEEDED la disciplina de envelope
   (status/kind/remediation/nextStep) — es el mejor asset del código
   y un patrón exportable a cualquier MCP.
2. El concepto property-first (laws sugeridas → verificadas →
   persistidas → re-playeadas) apunta al hueco correcto del
   ecosistema: **confianza semántica**, no sintaxis (eso ya lo da el
   type system gratis).
3. La dogfood-honestidad del repo (audits públicos, tags de
   provenance) es ejemplar.

**¿Asset personal? Sí, alto.** 25.7k LOC + 700 tests + audits
autocríticos + este análisis = pieza de portfolio seria.

## Recomendación estratégica

1. **No invertir en arreglar 36 tools.** Ejecutar la condensación E4
   (12 tools) + los 5 arreglos estructurales, con la métrica de
   éxito definida ahí (HM dogfood ≤25 calls, 0 breaks, properties
   persistidas ≥3, cold-start ≤60s).
2. **Construir sobre ghcide/HLS como librería**, no hand-rolled GHC
   session. El hard problem que este MCP resolvió a medias (sesión
   GHC viva, reactiva, estructurada) ya está resuelto por ghcide —
   un MVP «property-first loop sobre ghcide + cabal test» de ~3k LOC
   tendría el diferenciador SIN el runtime hostil.
3. Si la condensación + ghcide llegan al verdecito, el pitch NO es
   «MCP con 12 tools» sino **«correctness layer for Haskell agents»**:
   laws auto-sugeridas + regression auto-persistido como política de
   confianza — vendible como plugin de host (opencode/Claude Code) o
   hasta como GitHub Action, nicho donde el sustituto gratis no llega
   (nadie auto-persiste properties descubiertas).
4. Si el objetivo es aprendizaje/marca personal: congelar feature
   growth, publicar el post-mortem (los docs de dogfood son oro), y
   dejar el repo como referencia honesta de ingeniería.

## TL;DR

Hoy: POC valioso, no producto. El mercado que imagina existe
(Haskell + agentes + confianza), pero la entrada ganadora es
**ghcide + property-first mínimo y confiable**, no 36 tools. La
condensación de E4 es el puente; ghcide es el cimiento; la
law-suggestion es la única moeta defendible.
