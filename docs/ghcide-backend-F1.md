# F1 — Adaptador IdeSession + flag HASKELL_FLOWS_BACKEND (2026-10-02)

Estado: **DONE** — master verde, backend ghcide serviendo 3 tools por
el protocolo MCP completo, backend default intacto.

## Qué se construyó

| Pieza | Archivo | Nota |
|---|---|---|
| Sesión ghcide embebida | `src/HaskellFlows/Ghc/IdeSession.hs` | boot via `defaultMain` forked + `Custom (IdeCommand …)` bloqueante; `GhcSessionDeps` para eval/type; `evalGhcEnv` runner; diagnósticos estructurados por severidad |
| Generador de `hie.yaml` | ídem (`hieYamlFromCabal`, `ensureHieYaml`) | parse two-pass de stanzas lib/test-suite; se escribe en boot si falta (F0: sin él el cradle degrada a base-only) |
| Router strangler | `src/HaskellFlows/Tool/IdeBacked.hs` | `routeIde` sirve `ghc_check_module` / `ghc_eval` / `ghc_type` cuando el flag está on; todo lo demás cae al handler legacy |
| Flag de backend | `Mcp/Server.hs` | `srvBackend` + `srvIdeSession` (MVar singleton, mismo shape que `srvGhcSession`); `HASKELL_FLOWS_BACKEND=ghcide\|ghcapi` (default ghcapi) |
| Tests | `test/Spec.hs` | 3 unit tests del generador (render lib+test, defaults de dirs, reject sin name) |

## Smoke protocolar (FIFO harness contra playground/hm-opencode)

| Call | Resultado | Tiempo |
|---|---|---|
| `ghc_eval 1+2*3` (cold, incluye boot+hie.yaml+cradle) | `ok, output=7, backend=ghcide` | **3.1s** (legacy cold: 592s) |
| `ghc_type map (+1) [1,2,3]` | `ok, forall {b}. Num b => [b]` | 0.56s |
| `ghc_check_module src/Hm/Parser.hs` | `ok, No type errors` | ~0.5s |
| `ghc_check_module src/Hm/Broken.hs` (roto, efímero) | `failed, compile_error, 1 error(s) — ghcide backend` | ~0.5s |
| `ghc_eval foldr (+) 0 [1..100]` (caliente) | `ok, 5050` | 0.54s |
| Default backend (sin flag), mismo eval | `ok, 7`, sin campo backend | ✓ intacto |

Suite completa: **PASS** (700+ tests, default backend).

## Hallazgos nuevos (para auditoría y F2+)

1. **F28 — ghcide secuestra stdout**: `defaultMain` (incluso con
   `Custom`) redirige el stdout del proceso a stderr (reserva fd 1
   para el LSP que cree poseer). El transport stdio del MCP muere en
   silencio: toda respuesta post-boot cae en stderr y el cliente ve
   timeouts. **Fix aplicado**: `dup stdOutput` antes del fork +
   `dupTo` restore tras el handoff del IdeState. Si ghcide volviera a
   tocar fd 1 en runtime (no observado), el fix sería darles
   `argsHandleIn/argsHandleOut` apuntando a /dev/null — queda
   documentado como plan B.
2. **F29 — el generador de hie.yaml es crítico y frágil**: un yaml con
   paths/componentes malformados degrada a "could not resolve GHC
   session" SIN diagnóstico del porqué (el error real aparece solo en
   stderr del server). El handler debería distinguir
   cradle-resolution-failure de other-failures y devolver el texto del
   cradle en el envelope. (F3: agregar al check de sugerencias.)
3. El handler `ghc_check_module` ghcide responde `ok / No type errors`
   también para archivos inexistentes (TypeCheck de un no-file no
   produce diagnósticos). Legacy valida existencia — paridad pendiente
   (F2).

## Deuda explícita heredada a F2+

- Transport concurrente (F13/F15 sigue vivo — no era scope de F1).
- `ghc_eval` ghcide: sin sanitize, sin capture de stdout del expr, sin
  fallback IO. Anchor siempre src/ (lib component). El diferencial
  test-component (QuickCheck visible) llega con F3 (property pipeline).
- `routeIde` no cubre `ghc_load` — el resto de las tools siguen
  tocando ApiSession bajo el flag (documento: NO mezclar
  `ghc_load`+`ghc_eval` en una misma sesión con flag on, hasta F2).

## Cómo reproducir el smoke

```bash
cabal install exe:haskell-flows-mcp --installdir=$HOME/.local/bin \
  --install-method=copy --overwrite-policy=always
HASKELL_FLOWS_BACKEND=ghcide HASKELL_PROJECT_DIR=<proyecto> \
  ~/.local/bin/haskell-flows-mcp
# → initialize; tools/call ghc_eval {"expression":"1 + 2 * 3"}
#   → {"status":"ok","result":{"output":"7","backend":"ghcide"}}
```
