# F2 — Transport concurrente + watchdog + warmup (2026-10-02)

Estado: **DONE** — zombie F13/15 muerto, presupuesto de eval honesto,
suite completa PASS.

## Qué cambió (`Mcp/Transport.hs`, reescrito)

1. **Dispatch concurrente**: el reader thread solo lee. Cada request
   corre en un worker (pool acotado por
   `HASKELL_FLOWS_MAX_CONCURRENT_CALLS`, default 4, QSem). Un handler
   wedgeado ya no bloquea el reader ni a otros requests. Las
   respuestas pueden llegar desordenadas — los ids JSON-RPC llevan la
   correlación.
2. **Deliver-once gate + watchdog**: por request, un MVar gate; worker
   y watchdog corren para cerrarlo — exactamente UNA respuesta llega al
   wire, la otra se descarta (log). El watchdog dispara a
   `outerToolCeiling + 5s` y entrega un error JSON-RPC de timeout:
   cubre handlers trabados en secciones no-interrumpibles donde
   `System.Timeout` no puede disparar.
3. **Warmup en background**: con `HASKELL_FLOWS_BACKEND=ghcide`, el
   IdeState bootea al arranque (log "warmup: ghcide session ready").
   `HASKELL_FLOWS_WARMUP=0` lo desactiva.
4. **Excepciones de handler → respuesta JSON-RPC internal error**
   (antes: swallow a stderr = silencio eterno para el cliente).

Fixes acompañantes:
- `IdeBacked.withIdeSession` ahora corre el handler FUERA del MVar de
  boot (antes: un call lento retenía el lock y serializaba todos los
  calls ghcide).
- **F30 — laziness vs presupuesto**: `compileExpr` devuelve el String
  como thunk; `last [1..]` corría recién al serializar el envelope,
  FUERA del timeout de 30s. Fix: `evaluate (length s)` dentro de la
  ventana. Verificado: timeout estructurado **inner_timeout a los
  31.3s**, y el error ya no se reporta como `compile_error`.
- `Server` pasa a exportarse como `Server (..)` (era abstracto; el
  transport es consumidor interno de confianza).

## Smoke (FIFO harness, backend ghcide)

| Escenario | Resultado |
|---|---|
| Warmup al arranque | "warmup: booting…" → "ready" en stderr antes del 1er call |
| Blast paralelo (3 evals sin esperar respuesta) | 3/3 respondidos (42, 3, 42) — imposible en el transport secuencial |
| `ghc_eval "last [1..]"` (F13 regression) | `failed, inner_timeout` a los 31.3s |
| Eval posterior al infinito | `ok, 54` — **el server sobrevive** (antes: zombie total) |

Suite: PASS (700+ tests + 2 F2 nuevos: deliverOnce first-wins,
deliverOnce runs-winner-action).

## Deuda para F3+

- Watchdog no verificado end-to-end (requiere un hang no-interrumpible
  real; su cobertura es el gate unit-test + la lógica compartida con
  deliverOnce).
- Warmup bootea el IdeState pero NO el primer `GhcSessionDeps`
  (cradle+componente): el primer eval en sesión caliente paga ~4.6s.
  Warmup de componente: F3.
- Respuestas pueden llegar desordenadas: hosts MCP lo toleran por spec;
  nota para el doc de compatibilidad de F6.
- `show (expr)` wrapper: exprs IO aún no soportadas en el backend
  ghcide (F3, junto con el capture de stdout).
