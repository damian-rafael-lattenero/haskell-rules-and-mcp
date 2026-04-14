/**
 * Workflow State Tracker — tracks development progress across tool calls.
 *
 * Provides structured awareness of which FLOW/step the developer is on,
 * module-level progress (functions implemented, properties passed), and
 * pending actions (warnings to fix, loads to run).
 */

export type FlowId =
  | "preflight"
  | "new-project"
  | "new-module"
  | "implement"
  | "incremental-qc"
  | "explore"
  | "module-complete"
  | "add-dep";

export interface FlowStep {
  flow: FlowId;
  step: number;
  label: string;
}

export interface ModuleProgress {
  modulePath: string;
  phase: "stub" | "implementing" | "complete";
  functionsTotal: number;
  functionsImplemented: number;
  propertiesPassed: string[];
  propertiesFailed: string[];
  lastLoad: { success: boolean; errors: number; warnings: number } | null;
  arbitraryInstancesDefined: boolean;
  completionGates: {
    checkModule: boolean;
    lint: boolean;
    format: boolean;
  };
}

export interface ToolExecution {
  tool: string;
  timestamp: number;
  success: boolean;
}

export interface WorkflowState {
  currentFlow: FlowStep | null;
  activeModule: string | null;
  modules: Map<string, ModuleProgress>;
  optionalTooling: {
    lint: "unknown" | "available" | "unavailable";
    format: "unknown" | "available" | "unavailable";
    hls: "unknown" | "available" | "unavailable";
  };
  toolHistory: ToolExecution[];
  editsSinceLastLoad: number;
  pendingWarningCount: number;
  sessionStarted: number;
}

const MAX_HISTORY = 50;

export function createEmptyProgress(modulePath: string): ModuleProgress {
  return {
    modulePath,
    phase: "stub",
    functionsTotal: 0,
    functionsImplemented: 0,
    propertiesPassed: [],
    propertiesFailed: [],
    lastLoad: null,
    arbitraryInstancesDefined: false,
    completionGates: { checkModule: false, lint: false, format: false },
  };
}

export function createWorkflowState(): WorkflowState {
  return {
    currentFlow: null,
    activeModule: null,
    modules: new Map(),
    optionalTooling: { lint: "unknown", format: "unknown", hls: "unknown" },
    toolHistory: [],
    editsSinceLastLoad: 0,
    pendingWarningCount: 0,
    sessionStarted: Date.now(),
  };
}

export function logTool(
  state: WorkflowState,
  tool: string,
  success: boolean
): void {
  state.toolHistory.push({ tool, timestamp: Date.now(), success });
  if (state.toolHistory.length > MAX_HISTORY) {
    state.toolHistory.shift();
  }
}

export function getModuleProgress(
  state: WorkflowState,
  modulePath: string
): ModuleProgress | undefined {
  return state.modules.get(modulePath);
}

export function updateModuleProgress(
  state: WorkflowState,
  modulePath: string,
  updates: Partial<ModuleProgress>
): void {
  const existing = state.modules.get(modulePath) ?? createEmptyProgress(modulePath);
  state.modules.set(modulePath, { ...existing, ...updates });
}

export function resetWorkflowState(state: WorkflowState): void {
  state.currentFlow = null;
  state.activeModule = null;
  state.modules.clear();
  state.optionalTooling = { lint: "unknown", format: "unknown", hls: "unknown" };
  state.toolHistory = [];
  state.editsSinceLastLoad = 0;
  state.pendingWarningCount = 0;
  state.sessionStarted = Date.now();
}

/** Generate a compact status summary for injection into tool responses. */
export function workflowHint(state: WorkflowState): Record<string, unknown> | null {
  const hints: Record<string, unknown> = {};

  if (state.currentFlow) {
    hints.currentStep = `FLOW ${state.currentFlow.flow} step ${state.currentFlow.step} (${state.currentFlow.label})`;
  }

  if (state.activeModule) {
    const mod = state.modules.get(state.activeModule);
    if (mod) {
      hints.moduleProgress = `${mod.functionsImplemented}/${mod.functionsTotal} functions`;
      hints.propertiesPassed = mod.propertiesPassed.length;
      hints.propertiesFailed = mod.propertiesFailed.length;
    }
  }

  if (state.pendingWarningCount > 0) {
    hints.hint = `Fix ${state.pendingWarningCount} pending warning(s) before continuing`;
  } else if (state.editsSinceLastLoad > 0) {
    hints.hint = `${state.editsSinceLastLoad} edit(s) since last ghci_load — compile to verify`;
  }

  return Object.keys(hints).length > 0 ? hints : null;
}

/** Determine what the next step should be based on current state. */
export function suggestNextStep(state: WorkflowState): string {
  if (!state.currentFlow) {
    return "Run ghci_session(status) to start (FLOW 1: Pre-Flight)";
  }

  const { flow, step } = state.currentFlow;

  if (state.pendingWarningCount > 0) {
    return `Fix ${state.pendingWarningCount} pending warning(s) — zero tolerance policy`;
  }

  if (state.editsSinceLastLoad > 0) {
    return "Run ghci_load to compile recent edits";
  }

  if (flow === "implement") {
    const steps: Record<number, string> = {
      1: "Replace = undefined with = _ (hole phase)",
      2: "ghci_load(diagnostics=true) — read hole analysis",
      3: "Explore: ghci_type / ghci_info / hoogle_search",
      4: "Implement the function body",
      5: "ghci_load(diagnostics=true) — verify compiles",
      6: "Fix all errors and warnings",
      7: "ghci_type(\"functionName\") — verify type",
      8: "ghci_eval(\"functionName sampleArg\") — test behavior",
      9: "ghci_quickcheck if a law is now testable (FLOW 4.5)",
    };
    return steps[step] ?? "Continue implementing";
  }

  if (flow === "module-complete") {
    const steps: Record<number, string> = {
      1: "ghci_quickcheck — test COMPLETE algebraic contract",
      2: "ghci_check_module — review API summary",
      3: "ghci_lint — code quality pass",
      4: "ghci_format — formatting pass",
    };
    return steps[step] ?? "Module complete gate finished — start next module";
  }

  return "Continue with the current flow";
}

/** Generate a checklist for completing the active module. */
export function moduleChecklist(state: WorkflowState): string[] {
  const mod = state.activeModule ? state.modules.get(state.activeModule) : undefined;
  const items: string[] = [];

  if (!mod) {
    items.push("[ ] No active module — load one with ghci_load");
    return items;
  }

  const remaining = mod.functionsTotal - mod.functionsImplemented;
  if (remaining > 0) {
    items.push(`[ ] Implement ${remaining} remaining function(s)`);
  } else {
    items.push("[x] All functions implemented");
  }

  if (!mod.arbitraryInstancesDefined) {
    items.push("[ ] Add Arbitrary instances for data types");
  } else {
    items.push("[x] Arbitrary instances defined");
  }

  if (mod.propertiesPassed.length === 0 && mod.propertiesFailed.length === 0) {
    items.push("[ ] Run QuickCheck properties (none tested yet)");
  } else if (mod.propertiesFailed.length > 0) {
    items.push(`[ ] Fix ${mod.propertiesFailed.length} failing propert(ies)`);
  } else {
    items.push(`[x] ${mod.propertiesPassed.length} propert(ies) passing`);
  }

  items.push("[ ] ghci_check_module — review API");
  items.push(
    state.optionalTooling.lint === "unavailable"
      ? "[~] ghci_lint — recommended (tool unavailable)"
      : "[ ] ghci_lint — code quality"
  );
  items.push(
    state.optionalTooling.format === "unavailable"
      ? "[~] ghci_format — recommended (tool unavailable)"
      : "[ ] ghci_format — formatting"
  );

  return items;
}

/** Derive the module phase from function counts instead of relying solely on suggest tool. */
export function derivePhase(p: ModuleProgress): ModuleProgress["phase"] {
  if (p.functionsTotal === 0) return "stub";
  if (p.functionsImplemented < p.functionsTotal) return "implementing";
  return "complete";
}

/**
 * Derive contextual guidance based on the actual state of the active module.
 * Returns actionable strings the agent should follow. Replaces the old mode system
 * with state-driven recommendations that are always relevant.
 */
export function deriveGuidance(state: WorkflowState, toolName: string): string[] {
  const guidance: string[] = [];
  const mod = state.activeModule ? state.modules.get(state.activeModule) : undefined;

  // Pending warnings — always highest priority
  if (state.pendingWarningCount > 0) {
    guidance.push(`${state.pendingWarningCount} warning(s) — fix (zero tolerance)`);
  }

  // Edits since last load — compile first
  if (state.editsSinceLastLoad > 0 && toolName !== "ghci_load") {
    guidance.push(`${state.editsSinceLastLoad} edit(s) since last load — run ghci_load`);
  }

  if (!mod) return guidance;

  // Module has undefined stubs and nothing implemented yet
  if (mod.phase === "stub" && mod.functionsTotal > 0 && mod.functionsImplemented === 0) {
    guidance.push(`${mod.functionsTotal} undefined stub(s) — run ghci_suggest to see hole fits`);
  }

  // Has functions but no Arbitrary instances in ANY loaded module
  // (Arbitrary may be defined in a different module, e.g. Syntax.hs)
  const anyArbitraryDefined = [...state.modules.values()].some(m => m.arbitraryInstancesDefined);
  if (!anyArbitraryDefined && mod.functionsImplemented > 0) {
    guidance.push("No Arbitrary instances in any module — run ghci_arbitrary for data types before QuickCheck");
  }

  // Functions implemented but no QC properties yet — suggest analyze mode for discovery.
  // Suppressed once properties have passed (guidance already achieved its goal).
  if (
    mod.functionsImplemented > 0 &&
    mod.propertiesPassed.length === 0 &&
    mod.propertiesFailed.length === 0
  ) {
    guidance.push(
      `Functions implemented but untested — run ghci_suggest(module_path="${mod.modulePath}", mode="analyze") for property suggestions, then ghci_quickcheck`
    );
  }

  // Failing properties
  if (mod.propertiesFailed.length > 0) {
    guidance.push(`${mod.propertiesFailed.length} failing property(ies) — fix before continuing`);
  }

  // Suggest batch when many individual properties exist
  if (mod.propertiesPassed.length >= 3 && mod.propertiesFailed.length === 0) {
    guidance.push(
      `${mod.propertiesPassed.length} properties — use ghci_quickcheck_batch or ghci_regression(action="run") for efficient re-testing`
    );
  }

  // Module-complete gate: individual hint per missing gate.
  // Fires as soon as any properties pass — no need to wait for phase === "complete".
  if (mod.propertiesPassed.length > 0 && mod.propertiesFailed.length === 0) {
    const gates = mod.completionGates ?? { checkModule: false, lint: false, format: false };
    const lintSatisfied = gates.lint || state.optionalTooling.lint === "unavailable";
    const formatSatisfied = gates.format || state.optionalTooling.format === "unavailable";
    if (!gates.checkModule) {
      guidance.push(
        `Properties pass — run ghci_check_module(module_path="${mod.modulePath}") to review exported API before moving on`
      );
    }
    if (!lintSatisfied) {
      guidance.push(
        `Run ghci_lint(module_path="${mod.modulePath}") for code quality pass (module-complete gate)`
      );
    } else if (!gates.lint && state.optionalTooling.lint === "unavailable") {
      guidance.push(
        "ghci_lint is unavailable in this environment — recommended but not blocking for module completion"
      );
    }
    if (!formatSatisfied) {
      guidance.push(
        `Run ghci_format(module_path="${mod.modulePath}", write=true) to normalize formatting (module-complete gate)`
      );
    } else if (!gates.format && state.optionalTooling.format === "unavailable") {
      guidance.push(
        "ghci_format is unavailable in this environment — recommended but not blocking for module completion"
      );
    }
  }

  // Session-close hint: all tracked modules have all gates complete
  const allModules = [...state.modules.values()];
  const allGatesComplete =
    allModules.length > 0 &&
    allModules.every(
      (m) =>
        m.completionGates?.checkModule &&
        (m.completionGates?.lint || state.optionalTooling.lint === "unavailable") &&
        (m.completionGates?.format || state.optionalTooling.format === "unavailable")
    );
  if (allGatesComplete) {
    guidance.push(
      "All modules complete — export test suite with ghci_quickcheck_export(output_path=\"test/Spec.hs\"), then cabal_test and cabal_build to verify the package"
    );
  } else if (state.modules.size > 1) {
    // Multi-module project: suggest regression when all modules have properties
    const allImplemented = allModules.every(
      (m) => m.functionsTotal > 0 && m.functionsImplemented >= m.functionsTotal
    );
    const hasProperties = allModules.some((m) => m.propertiesPassed.length > 0);
    const noFailures = allModules.every((m) => m.propertiesFailed.length === 0);
    if (allImplemented && hasProperties && noFailures && guidance.length === 0) {
      guidance.push("All modules complete — run ghci_regression to verify all properties");
    }
  }

  return guidance;
}

export interface WorkflowHelpResult {
  suggested_tools: string[];
  reasoning: string;
  steps: string[];
}

/**
 * Context-aware help: looks at the current workflow state and returns
 * actionable next-step guidance for the LLM agent.
 *
 * Decision tree:
 * 1. Nothing loaded yet → load a module
 * 2. Load failed → fix errors
 * 3. Pending warnings → fix warnings
 * 4. No arbitrary instances but functions exist → define arbitrary
 * 5. No properties tested → run quickcheck
 * 6. Properties failing → fix implementation
 * 7. All properties pass but gates missing → run check_module/lint/format
 * 8. Everything done → run regression
 */
export function workflowHelp(state: WorkflowState): WorkflowHelpResult {
  const recentTools = new Set(state.toolHistory.slice(-10).map((t) => t.tool));
  const mod = state.activeModule ? state.modules.get(state.activeModule) : undefined;

  // 1. Fresh session — nothing loaded
  if (state.toolHistory.length === 0 || !recentTools.has("ghci_load")) {
    return {
      suggested_tools: ["ghci_load", "ghci_session"],
      reasoning: "No modules have been loaded yet. Start by loading your module to get compilation feedback.",
      steps: [
        "Run ghci_load(module_path='src/YourModule.hs', diagnostics=true) to compile and see errors/warnings",
        "Fix any errors reported, then reload",
        "Once clean, proceed to implement functions",
      ],
    };
  }

  // 2. Last load failed
  if (mod?.lastLoad && !mod.lastLoad.success && mod.lastLoad.errors > 0) {
    return {
      suggested_tools: ["ghci_load", "ghci_type"],
      reasoning: `The last compilation had ${mod.lastLoad.errors} error(s). Fix them before continuing.`,
      steps: [
        "Read the error messages from the last ghci_load result",
        "Fix the type errors or missing definitions",
        "Re-run ghci_load(diagnostics=true) to verify",
      ],
    };
  }

  // 3. Pending warnings
  if (state.pendingWarningCount > 0) {
    return {
      suggested_tools: ["ghci_load"],
      reasoning: `There are ${state.pendingWarningCount} pending warning(s). Zero tolerance — fix them now.`,
      steps: [
        "Check the warningActions from the last ghci_load result",
        "Apply each suggestedAction (add type signatures, remove unused imports, etc.)",
        "Re-run ghci_load to confirm warnings are gone",
      ],
    };
  }

  // 4. Need Arbitrary instances
  if (mod && mod.functionsImplemented > 0) {
    const anyArbitrary = [...state.modules.values()].some((m) => m.arbitraryInstancesDefined);
    if (!anyArbitrary) {
      return {
        suggested_tools: ["ghci_arbitrary", "ghci_quickcheck"],
        reasoning: "Functions are implemented but no Arbitrary instances found. QuickCheck needs them to generate test data.",
        steps: [
          "Run ghci_arbitrary(type_name='YourType') to generate Arbitrary instances",
          "Add the generated instance to your Syntax/Types module",
          "Re-run ghci_load, then proceed to ghci_quickcheck",
        ],
      };
    }
  }

  // 5. No properties tested yet
  if (mod && mod.functionsImplemented > 0 && mod.propertiesPassed.length === 0 && mod.propertiesFailed.length === 0) {
    return {
      suggested_tools: ["ghci_quickcheck", "ghci_quickcheck_batch"],
      reasoning: "Functions are implemented but no QuickCheck properties have been run. Test your algebraic laws.",
      steps: [
        "Identify algebraic laws for your functions (identity, associativity, roundtrip, etc.)",
        "Run ghci_quickcheck(property='\\\\x -> ...', incremental=true)",
        "If unsure what to test, use ghci_quickcheck(property='suggest', function_name='yourFunc')",
      ],
    };
  }

  // 6. Failing properties
  if (mod && mod.propertiesFailed.length > 0) {
    return {
      suggested_tools: ["ghci_eval", "ghci_trace"],
      reasoning: `${mod.propertiesFailed.length} QuickCheck property(ies) are failing. Fix the implementation.`,
      steps: [
        "Examine the counterexample from the failing ghci_quickcheck result",
        "Use ghci_eval to manually test edge cases",
        "Use ghci_trace to debug intermediate values if needed",
        "Fix the implementation, reload, and re-run quickcheck",
      ],
    };
  }

  // 7. Properties pass but completion gates missing
  if (mod && mod.propertiesPassed.length > 0) {
    const gates = mod.completionGates;
    const missing: string[] = [];
    if (!gates.checkModule) missing.push("ghci_check_module");
    if (!gates.lint && state.optionalTooling.lint !== "unavailable") missing.push("ghci_lint");
    if (!gates.format && state.optionalTooling.format !== "unavailable") missing.push("ghci_format");

    if (missing.length > 0) {
      return {
        suggested_tools: missing,
        reasoning: `Properties pass! Complete the module-complete gate: ${missing.join(", ")} still needed.`,
        steps: [
          ...missing.map((t) => `Run ${t}(module_path='${state.activeModule ?? "src/YourModule.hs"}')`),
          "Fix any issues found",
          "Then run ghci_regression to persist all properties",
        ],
      };
    }
  }

  // 8. Everything looks complete
  return {
    suggested_tools: ["ghci_regression", "ghci_quickcheck_export", "cabal_test"],
    reasoning: "Module looks complete. Run regression to verify all saved properties still pass.",
    steps: [
      "Run ghci_regression(action='run') to re-run all saved properties",
      "Run ghci_quickcheck_export() to generate a persistent test file",
      "Run cabal_test to verify the generated test-suite executes correctly",
      "Consider starting the next module with ghci_scaffold",
    ],
  };
}

/** Serialize state for MCP resource / JSON response. */
export function serializeState(state: WorkflowState): Record<string, unknown> {
  const modules: Record<string, ModuleProgress> = {};
  for (const [k, v] of state.modules) {
    modules[k] = v;
  }

  return {
    currentFlow: state.currentFlow,
    activeModule: state.activeModule,
    modules,
    optionalTooling: state.optionalTooling,
    recentTools: state.toolHistory.slice(-10),
    editsSinceLastLoad: state.editsSinceLastLoad,
    pendingWarningCount: state.pendingWarningCount,
    sessionStarted: state.sessionStarted,
  };
}

export function setOptionalToolAvailability(
  state: WorkflowState,
  tool: "lint" | "format" | "hls",
  status: "unknown" | "available" | "unavailable"
): void {
  state.optionalTooling[tool] = status;
}
