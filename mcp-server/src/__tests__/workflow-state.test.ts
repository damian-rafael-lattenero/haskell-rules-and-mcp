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
