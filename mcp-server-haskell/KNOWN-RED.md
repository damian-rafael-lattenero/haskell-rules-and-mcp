# KNOWN-RED — burn-down ledger (e2e: 351/390 green @ 2b78e6e)

Regla: ningún commit sin `scripts/gate.sh t1` verde. T2 antes de pushear.
Todo e2e va por hangwatch (veredicto DONE/HANG — nunca bare).

## Familia A · Cross-component interface-cache (14) — LA PROFUNDA
Hipótesis única: al descubrirse un componente (lib+test) mid-action, el
restart aborta el warm; un .hi se lee de un dir de cache con hash stale
(GHC-47808 "withBinaryFile: does not exist") o el import sintético del
target no tiene bytecode en la unidad ancla.
- [ ] Mutation testing (3) — oráculo bug-finding
- [ ] Missing Arbitrary (2) — falta el bytecode del target para el
      import sintético `Calc` (kind=missing_instance YA clasifica)
- [ ] ghcide parity matrix (6) — props fallan DESPUÉS de check_project
      en secuencia (aislada 10/10-ish) — mismo transitorio
- [ ] Regression scope fix (1) + Arithmetic Evaluator (2)
Próximo: probe mínimo `probes/crosscomp.py` (create→add→load→prop en
subprocess, ~30 s) para iterar sin suite.

## Familia B · Property-store lifecycle (10)
- [ ] Property lifecycle (4) — list/export/audit shapes tras mover
      renderStored/listResult a PropertyStore
- [ ] Property store race (4) — 2 clientes, saves concurrentes
- [ ] Gates properties (1) + CheckModule props gate (1) — leen el
      store con claves módulo-Path vs módulo-nombre (#74)
Próximo: probe `probes/store.py` contra un proyecto fixture.

## Familia C · Envelopes / contratos de shape (9) — mecánicas
- [ ] Exploratory (1) — check de `inspect:doc` que CORTAMOS:
      eliminar el check del escenario (decisión de superficie, no bug)
- [ ] Typed holes (1), Load hole diagnostics (1) — GHC-58427/88464
      artifact filtering en el shape del load
- [ ] Oversized (2) — rechazo en frontera 256 KiB (budget del eval)
- [ ] Non-UTF-8 (2) — error graceful en load
- [ ] RCE contract (2) — documenta capacidad IO del eval
- [ ] Corpus transport (1), Workflow help (1), Disk full (1),
      load_failed shape (1), Cross-validation (1)

## Escenarios-hueco aprobados (crear tras quemar rojos)
1. Poison-recovery bajo ghcide (excepción/timeout mid-flow → próxima
   llamada boota limpio)
2. Ciclo multi-unit (lib+test+exe: eval tocando ambos componentes,
   module-add mid-sesión, re-eval)
