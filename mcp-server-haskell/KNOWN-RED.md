# KNOWN-RED — burn-down ledger

Estado: **0 rojos — 399/399 e2e verde @ f906758** (T2 completo: VEREDICTO DONE en 575 s).

Regla permanente: ningún commit sin `scripts/gate.sh t1` verde. T2 antes
de pushear. Todo e2e/probe va por `scripts/hangwatch.py` (veredicto
DONE/HANG — nunca bare).

## Burn-down (351/390 @ 2b78e6e → 395/395)

| Commit | Qué quemó |
|---|---|
| d169fed | gate utilizable: hangwatch persistente, t0 falla de verdad, canario único, greps errexit-safe |
| c342f13 | sanitize en la frontera eval/type (#127 segfault, oversized, newline/sentinel); runner de acciones IO (`show <$>` era coerce mentiroso); sibling path 5 niveles (t/→x/); UTF-8 en load+project; anchors filtrados por UTF-8; política de retry parametrizada |
| 49955d1 | **causa raíz familia QC**: exposePackages' vía `listUnitInfo`+`setProgramDynFlags` (lookupPackageName/modifyDynFlags no exponen nada en scaffolds base-only) — Mutation, WorkflowHelp, DiskFull, Arithmetic, Lifecycle, MissingArb parcial; classifier de holes (GHC-88464→warnings, GHC-58427 artifact filtrado); ReplayOutcome con error honesto |
| 979520b | #74: moduleKeyOf canónica + propertyKeyMatches (path↔nombre) + gates.properties REAL en check_module (replay del store); anchors dados inexistentes filtrados (BadDependency GetModSummary) |
| 5d9713f | dedup keep-first (el foldr-prepend reordenaba el anchor dado detrás de test/Spec.hs → GHC-87543 ambigüedad); qcExpr determinismo tipa String (era [String] → coerce mentiroso → toEnum crash); summary "N runs passed"; frase check_project |
| a52b74f | persist del anchor GANADOR (sitio de definición, no el hint); steering ghc_property(arbitrary) en missing_instance |
| d81c5a6 | fixtureRoot absoluto desde el binario + cwd explícito del subprocess corpus (el leak de setCurrentDirectory de ghcide rompía los escenarios tardíos) |

## Lecciones (memorizadas)

- `lookupPackageName` solo mapea la unidad PREFERIDA por nombre; exponer
  por UnitId requiere enumerar `listUnitInfo` y aplicar
  `setProgramDynFlags` (modifyDynFlags no recomputa visibilidad).
- Un coerce mentiroso (`unsafeCoerce hv :: String` sobre un `[String]`)
  crashea LEJOS: `toEnum{GeneralCategory}` sobre punteros-como-Chars.
  Ante un crash así, typecheck el expr generado standalone primero.
- Un dedup foldr-prepend conserva la ÚLTIMA ocurrencia: si el given
  aparece dos veces, pierde su prioridad silenciosamente.
- El CWD del proceso de tests NO es estable (leak de ghcide): toda ruta
  de e2e debe resolverse desde getExecutablePath o fijar cwd explícito.

## Escenarios-hueco aprobados (próxima ola)
1. Poison-recovery bajo ghcide (excepción/timeout mid-flow → próxima
   llamada boota limpio) — pinea el ciclo de vida de la sesión.
2. Ciclo multi-unit (lib+test+exe: eval tocando ambos componentes,
   module-add mid-sesión, re-eval) — pinea el fix de exposición de
   paquetes y el orden de anchors.
