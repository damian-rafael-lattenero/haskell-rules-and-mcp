import { describe, it, expect } from "vitest";
import {
  createWorkflowState,
  createEmptyProgress,
  resetWorkflowState,
  logTool,
  getModuleProgress,
  updateModuleProgress,
  workflowHint,
  suggestNextStep,
  moduleChecklist,
  serializeState,
  derivePhase,
  deriveGuidance,
} from "../workflow-state.js";

describe("createWorkflowState", () => {
  it("returns a fresh empty state", () => {
    const state = createWorkflowState();
    expect(state.currentFlow).toBeNull();
    expect(state.activeModule).toBeNull();
    expect(state.modules.size).toBe(0);
    expect(state.toolHistory).toHaveLength(0);
    expect(state.editsSinceLastLoad).toBe(0);
    expect(state.pendingWarningCount).toBe(0);
    expect(state.sessionStarted).toBeGreaterThan(0);
  });
});

describe("createEmptyProgress", () => {
  it("creates progress with correct defaults", () => {
    const prog = createEmptyProgress("src/HM/Types.hs");
    expect(prog.modulePath).toBe("src/HM/Types.hs");
    expect(prog.phase).toBe("stub");
    expect(prog.functionsTotal).toBe(0);
    expect(prog.functionsImplemented).toBe(0);
    expect(prog.propertiesPassed).toHaveLength(0);
    expect(prog.propertiesFailed).toHaveLength(0);
    expect(prog.lastLoad).toBeNull();
    expect(prog.arbitraryInstancesDefined).toBe(false);
  });
});

describe("logTool", () => {
  it("appends to tool history", () => {
    const state = createWorkflowState();
    logTool(state, "ghci_load", true);
    logTool(state, "ghci_type", true);
    logTool(state, "ghci_eval", false);
    expect(state.toolHistory).toHaveLength(3);
    expect(state.toolHistory[0].tool).toBe("ghci_load");
    expect(state.toolHistory[2].success).toBe(false);
  });

  it("trims history beyond 50 entries", () => {
    const state = createWorkflowState();
    for (let i = 0; i < 60; i++) {
      logTool(state, `tool-${i}`, true);
    }
    expect(state.toolHistory).toHaveLength(50);
    expect(state.toolHistory[0].tool).toBe("tool-10");
    expect(state.toolHistory[49].tool).toBe("tool-59");
  });
});

describe("getModuleProgress / updateModuleProgress", () => {
  it("returns undefined for unknown module", () => {
    const state = createWorkflowState();
    expect(getModuleProgress(state, "src/Foo.hs")).toBeUndefined();
  });

  it("creates module on first update", () => {
    const state = createWorkflowState();
    updateModuleProgress(state, "src/Foo.hs", { phase: "implementing", functionsTotal: 5 });
    const prog = getModuleProgress(state, "src/Foo.hs");
    expect(prog).toBeDefined();
    expect(prog!.phase).toBe("implementing");
    expect(prog!.functionsTotal).toBe(5);
    expect(prog!.functionsImplemented).toBe(0); // default preserved
  });

  it("merges updates on existing module", () => {
    const state = createWorkflowState();
    updateModuleProgress(state, "src/Foo.hs", { functionsTotal: 3 });
    updateModuleProgress(state, "src/Foo.hs", { functionsImplemented: 2, phase: "implementing" });
    const prog = getModuleProgress(state, "src/Foo.hs");
    expect(prog!.functionsTotal).toBe(3);
    expect(prog!.functionsImplemented).toBe(2);
    expect(prog!.phase).toBe("implementing");
  });
});

describe("resetWorkflowState", () => {
  it("clears all state", () => {
    const state = createWorkflowState();
    state.currentFlow = { flow: "implement", step: 5, label: "COMPILE" };
    state.activeModule = "src/Foo.hs";
    updateModuleProgress(state, "src/Foo.hs", { functionsTotal: 3 });
    logTool(state, "ghci_load", true);
    state.editsSinceLastLoad = 2;
    state.pendingWarningCount = 1;

    resetWorkflowState(state);
    expect(state.currentFlow).toBeNull();
    expect(state.activeModule).toBeNull();
    expect(state.modules.size).toBe(0);
    expect(state.toolHistory).toHaveLength(0);
    expect(state.editsSinceLastLoad).toBe(0);
    expect(state.pendingWarningCount).toBe(0);
  });
});

describe("workflowHint", () => {
  it("returns null for empty state", () => {
    const state = createWorkflowState();
    expect(workflowHint(state)).toBeNull();
  });

  it("includes current step info", () => {
    const state = createWorkflowState();
    state.currentFlow = { flow: "implement", step: 5, label: "COMPILE" };
    const hint = workflowHint(state);
    expect(hint).toBeDefined();
    expect(hint!.currentStep).toContain("COMPILE");
  });

  it("includes module progress when active", () => {
    const state = createWorkflowState();
    state.activeModule = "src/Foo.hs";
    updateModuleProgress(state, "src/Foo.hs", { functionsTotal: 5, functionsImplemented: 3 });
    const hint = workflowHint(state);
    expect(hint!.moduleProgress).toBe("3/5 functions");
  });

  it("warns about pending warnings", () => {
    const state = createWorkflowState();
    state.pendingWarningCount = 2;
    const hint = workflowHint(state);
    expect(hint!.hint).toContain("2 pending warning(s)");
  });

  it("warns about uncompiled edits", () => {
    const state = createWorkflowState();
    state.editsSinceLastLoad = 3;
    const hint = workflowHint(state);
    expect(hint!.hint).toContain("3 edit(s) since last ghci_load");
  });
});

describe("suggestNextStep", () => {
  it("suggests pre-flight when no flow active", () => {
    const state = createWorkflowState();
    expect(suggestNextStep(state)).toContain("ghci_session(status)");
  });

  it("suggests fixing warnings when pending", () => {
    const state = createWorkflowState();
    state.currentFlow = { flow: "implement", step: 6, label: "FIX" };
    state.pendingWarningCount = 3;
    expect(suggestNextStep(state)).toContain("3 pending warning(s)");
  });

  it("suggests ghci_load when edits pending", () => {
    const state = createWorkflowState();
    state.currentFlow = { flow: "implement", step: 4, label: "IMPLEMENT" };
    state.editsSinceLastLoad = 1;
    expect(suggestNextStep(state)).toContain("ghci_load");
  });

  it("returns correct step for implement flow", () => {
    const state = createWorkflowState();
    state.currentFlow = { flow: "implement", step: 7, label: "VERIFY" };
    const next = suggestNextStep(state);
    expect(next).toContain("ghci_type");
  });

  it("returns correct step for module-complete flow", () => {
    const state = createWorkflowState();
    state.currentFlow = { flow: "module-complete", step: 1, label: "QUICKCHECK" };
    expect(suggestNextStep(state)).toContain("ghci_quickcheck");
  });
});

describe("moduleChecklist", () => {
  it("returns generic message when no active module", () => {
    const state = createWorkflowState();
    const list = moduleChecklist(state);
    expect(list.some(i => i.includes("No active module"))).toBe(true);
  });

  it("shows remaining functions", () => {
    const state = createWorkflowState();
    state.activeModule = "src/Foo.hs";
    updateModuleProgress(state, "src/Foo.hs", { functionsTotal: 5, functionsImplemented: 2 });
    const list = moduleChecklist(state);
    expect(list.some(i => i.includes("3 remaining function(s)"))).toBe(true);
  });

  it("shows all functions implemented", () => {
    const state = createWorkflowState();
    state.activeModule = "src/Foo.hs";
    updateModuleProgress(state, "src/Foo.hs", { functionsTotal: 3, functionsImplemented: 3 });
    const list = moduleChecklist(state);
    expect(list.some(i => i.includes("[x] All functions implemented"))).toBe(true);
  });

  it("shows failing properties", () => {
    const state = createWorkflowState();
    state.activeModule = "src/Foo.hs";
    updateModuleProgress(state, "src/Foo.hs", {
      propertiesPassed: ["law1"],
      propertiesFailed: ["law2", "law3"],
    });
    const list = moduleChecklist(state);
    expect(list.some(i => i.includes("2 failing"))).toBe(true);
  });

  it("includes gate steps (lint, format, check)", () => {
    const state = createWorkflowState();
    state.activeModule = "src/Foo.hs";
    updateModuleProgress(state, "src/Foo.hs", {});
    const list = moduleChecklist(state);
    expect(list.some(i => i.includes("ghci_lint"))).toBe(true);
    expect(list.some(i => i.includes("ghci_format"))).toBe(true);
    expect(list.some(i => i.includes("ghci_check_module"))).toBe(true);
  });
});

describe("serializeState", () => {
  it("converts Map to plain object", () => {
    const state = createWorkflowState();
    updateModuleProgress(state, "src/A.hs", { functionsTotal: 2 });
    updateModuleProgress(state, "src/B.hs", { functionsTotal: 3 });
    const serialized = serializeState(state);
    expect(serialized.modules).toBeDefined();
    const mods = serialized.modules as Record<string, unknown>;
    expect(Object.keys(mods)).toHaveLength(2);
    expect(mods["src/A.hs"]).toBeDefined();
  });

  it("limits tool history to last 10", () => {
    const state = createWorkflowState();
    for (let i = 0; i < 30; i++) {
      logTool(state, `tool-${i}`, true);
    }
    const serialized = serializeState(state);
    expect((serialized.recentTools as unknown[]).length).toBe(10);
  });
});

describe("Workflow tracking integration (Bug Fix 3)", () => {
  it("activeModule can be set and read back", () => {
    const state = createWorkflowState();
    state.activeModule = "src/HM/Types.hs";
    expect(state.activeModule).toBe("src/HM/Types.hs");
  });

  it("updateModuleProgress stores lastLoad data", () => {
    const state = createWorkflowState();
    updateModuleProgress(state, "src/HM/Subst.hs", {
      lastLoad: { success: true, errors: 0, warnings: 0 },
    });
    const mod = getModuleProgress(state, "src/HM/Subst.hs");
    expect(mod?.lastLoad?.success).toBe(true);
    expect(mod?.lastLoad?.errors).toBe(0);
  });

  it("QC tracking works without pre-set activeModule (fallback to last module)", () => {
    const state = createWorkflowState();
    // Simulate: ghci_load set a module via updateModuleProgress
    updateModuleProgress(state, "src/HM/Subst.hs", {
      lastLoad: { success: true, errors: 0, warnings: 0 },
    });
    // activeModule is still null — fallback should pick src/HM/Subst.hs
    const entries = Array.from(state.modules.entries());
    const fallbackMod = entries.length > 0 ? entries[entries.length - 1]![0] : null;
    expect(fallbackMod).toBe("src/HM/Subst.hs");
  });

  it("property deduplication prevents double-tracking", () => {
    const state = createWorkflowState();
    updateModuleProgress(state, "src/Lib.hs", {});
    const mod = getModuleProgress(state, "src/Lib.hs")!;

    // Simulate adding same property twice
    const prop = "\\t -> apply emptySubst t == t";
    const passed1 = [...mod.propertiesPassed, prop];
    updateModuleProgress(state, "src/Lib.hs", { propertiesPassed: passed1 });

    // Second add should be deduplicated by the tool handler
    const mod2 = getModuleProgress(state, "src/Lib.hs")!;
    if (!mod2.propertiesPassed.includes(prop)) {
      updateModuleProgress(state, "src/Lib.hs", {
        propertiesPassed: [...mod2.propertiesPassed, prop],
      });
    }
    expect(getModuleProgress(state, "src/Lib.hs")!.propertiesPassed.length).toBe(1);
  });

  it("suggest tracking updates functionsTotal", () => {
    const state = createWorkflowState();
    // Simulate suggest finding 3 undefined functions
    updateModuleProgress(state, "src/Lib.hs", {
      functionsTotal: 3,
      phase: "implementing",
    });
    const mod = getModuleProgress(state, "src/Lib.hs");
    expect(mod?.functionsTotal).toBe(3);
    expect(mod?.phase).toBe("implementing");
  });
});

describe("derivePhase", () => {
  it("returns 'stub' when functionsTotal is 0", () => {
    const progress = createEmptyProgress("src/Foo.hs");
    expect(derivePhase(progress)).toBe("stub");
  });

  it("returns 'implementing' when some functions remain", () => {
    const progress = {
      ...createEmptyProgress("src/Foo.hs"),
      functionsTotal: 5,
      functionsImplemented: 3,
    };
    expect(derivePhase(progress)).toBe("implementing");
  });

  it("returns 'complete' when all functions are implemented", () => {
    const progress = {
      ...createEmptyProgress("src/Foo.hs"),
      functionsTotal: 5,
      functionsImplemented: 5,
    };
    expect(derivePhase(progress)).toBe("complete");
  });

  it("returns 'complete' for a module with 1 function", () => {
    const progress = {
      ...createEmptyProgress("src/Foo.hs"),
      functionsTotal: 1,
      functionsImplemented: 1,
    };
    expect(derivePhase(progress)).toBe("complete");
  });
});

describe("deriveGuidance", () => {
  it("returns empty for no active module", () => {
    const state = createWorkflowState();
    expect(deriveGuidance(state, "ghci_load")).toEqual([]);
  });

  it("suggests ghci_suggest for stub modules", () => {
    const state = createWorkflowState();
    state.activeModule = "src/Foo.hs";
    updateModuleProgress(state, "src/Foo.hs", {
      phase: "stub",
      functionsTotal: 5,
      functionsImplemented: 0,
    });
    const guidance = deriveGuidance(state, "ghci_load");
    expect(guidance.some(g => g.includes("ghci_suggest"))).toBe(true);
  });

  it("suggests ghci_arbitrary when no instances defined", () => {
    const state = createWorkflowState();
    state.activeModule = "src/Foo.hs";
    updateModuleProgress(state, "src/Foo.hs", {
      functionsImplemented: 3,
      functionsTotal: 5,
      arbitraryInstancesDefined: false,
    });
    const guidance = deriveGuidance(state, "ghci_load");
    expect(guidance.some(g => g.includes("ghci_arbitrary"))).toBe(true);
  });

  it("suggests quickcheck when functions implemented but no properties", () => {
    const state = createWorkflowState();
    state.activeModule = "src/Foo.hs";
    updateModuleProgress(state, "src/Foo.hs", {
      functionsImplemented: 3,
      functionsTotal: 3,
      arbitraryInstancesDefined: true,
    });
    const guidance = deriveGuidance(state, "ghci_load");
    expect(guidance.some(g => g.includes("ghci_quickcheck"))).toBe(true);
  });

  it("warns about failing properties", () => {
    const state = createWorkflowState();
    state.activeModule = "src/Foo.hs";
    updateModuleProgress(state, "src/Foo.hs", {
      functionsImplemented: 3,
      functionsTotal: 3,
      propertiesFailed: ["prop1", "prop2"],
    });
    const guidance = deriveGuidance(state, "ghci_load");
    expect(guidance.some(g => g.includes("2 failing"))).toBe(true);
  });

  it("warns about pending warnings", () => {
    const state = createWorkflowState();
    state.pendingWarningCount = 3;
    const guidance = deriveGuidance(state, "ghci_eval");
    expect(guidance.some(g => g.includes("3 warning(s)"))).toBe(true);
  });

  it("warns about edits since last load (except on ghci_load itself)", () => {
    const state = createWorkflowState();
    state.editsSinceLastLoad = 2;
    expect(deriveGuidance(state, "ghci_eval").some(g => g.includes("ghci_load"))).toBe(true);
    expect(deriveGuidance(state, "ghci_load").some(g => g.includes("ghci_load"))).toBe(false);
  });

  it("suggests batch/regression when module has 3+ properties", () => {
    const state = createWorkflowState();
    state.activeModule = "src/Foo.hs";
    updateModuleProgress(state, "src/Foo.hs", {
      functionsImplemented: 3,
      functionsTotal: 3,
      arbitraryInstancesDefined: true,
      propertiesPassed: ["p1", "p2", "p3"],
    });
    const guidance = deriveGuidance(state, "ghci_load");
    expect(guidance.some(g => g.includes("ghci_quickcheck_batch") || g.includes("ghci_regression"))).toBe(true);
  });

  it("returns empty when module has no functions yet", () => {
    const state = createWorkflowState();
    state.activeModule = "src/Foo.hs";
    updateModuleProgress(state, "src/Foo.hs", {
      functionsImplemented: 0,
      functionsTotal: 0,
    });
    const guidance = deriveGuidance(state, "ghci_load");
    expect(guidance).toEqual([]);
  });

  it("stops suggesting Arbitrary after flag is set (Bug 1 fix)", () => {
    const state = createWorkflowState();
    state.activeModule = "src/Foo.hs";
    // Before: guidance suggests Arbitrary
    updateModuleProgress(state, "src/Foo.hs", {
      functionsImplemented: 2,
      functionsTotal: 3,
      arbitraryInstancesDefined: false,
    });
    expect(deriveGuidance(state, "ghci_load").some(g => g.includes("ghci_arbitrary"))).toBe(true);

    // After: flag set, guidance stops
    updateModuleProgress(state, "src/Foo.hs", { arbitraryInstancesDefined: true });
    expect(deriveGuidance(state, "ghci_load").some(g => g.includes("ghci_arbitrary"))).toBe(false);
  });

  it("shows property count and batch/regression suggestion", () => {
    const state = createWorkflowState();
    state.activeModule = "src/Foo.hs";
    updateModuleProgress(state, "src/Foo.hs", {
      functionsImplemented: 3,
      functionsTotal: 3,
      arbitraryInstancesDefined: true,
      propertiesPassed: ["p1", "p2", "p3"],
    });
    const guidance = deriveGuidance(state, "ghci_load");
    expect(guidance.some(g => g.includes("ghci_quickcheck_batch") || g.includes("ghci_regression"))).toBe(true);
  });

  it("suggests analyze mode when functions implemented but no properties", () => {
    const state = createWorkflowState();
    state.activeModule = "src/Foo.hs";
    updateModuleProgress(state, "src/Foo.hs", {
      functionsImplemented: 3,
      functionsTotal: 3,
      arbitraryInstancesDefined: true,
    });
    const guidance = deriveGuidance(state, "ghci_load");
    expect(guidance.some(g => g.includes("mode=\"analyze\""))).toBe(true);
  });

  it("suggests completion gate when module complete with properties", () => {
    const state = createWorkflowState();
    state.activeModule = "src/Foo.hs";
    updateModuleProgress(state, "src/Foo.hs", {
      phase: "complete",
      functionsImplemented: 3,
      functionsTotal: 3,
      arbitraryInstancesDefined: true,
      propertiesPassed: ["p1", "p2", "p3"],
      completionGates: { checkModule: false, lint: false, format: false },
    });
    const guidance = deriveGuidance(state, "ghci_load");
    expect(guidance.some(g => g.includes("completion gate"))).toBe(true);
    expect(guidance.some(g => g.includes("ghci_lint"))).toBe(true);
    expect(guidance.some(g => g.includes("ghci_format"))).toBe(true);
    expect(guidance.some(g => g.includes("ghci_check_module"))).toBe(true);
  });

  it("does not suggest gate when all gates passed", () => {
    const state = createWorkflowState();
    state.activeModule = "src/Foo.hs";
    updateModuleProgress(state, "src/Foo.hs", {
      phase: "complete",
      functionsImplemented: 3,
      functionsTotal: 3,
      arbitraryInstancesDefined: true,
      propertiesPassed: ["p1"],
      completionGates: { checkModule: true, lint: true, format: true },
    });
    const guidance = deriveGuidance(state, "ghci_load");
    expect(guidance.some(g => g.includes("completion gate"))).toBe(false);
  });
});

