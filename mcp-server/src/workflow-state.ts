/**
 * Workflow State Tracker — tracks development progress across tool calls.
 *
 * Provides structured awareness of which FLOW/step the developer is on,
 * module-level progress (functions implemented, properties passed), and
 * pending actions (warnings to fix, loads to run).
 */

export type WorkflowMode = "guided" | "medium" | "expert";

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
}

export interface ToolExecution {
  tool: string;
  timestamp: number;
  success: boolean;
}

export interface WorkflowState {
  mode: WorkflowMode | null;
  currentFlow: FlowStep | null;
  activeModule: string | null;
  modules: Map<string, ModuleProgress>;
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
  };
}

export function createWorkflowState(): WorkflowState {
  return {
    mode: null,
    currentFlow: null,
    activeModule: null,
    modules: new Map(),
    toolHistory: [],
    editsSinceLastLoad: 0,
    pendingWarningCount: 0,
    sessionStarted: Date.now(),
  };
}

/** The mode selection prompt shown when mode is not yet set. */
export const MODE_SELECTION_PROMPT =
  "🎯 **Select your Haskell development mode** before continuing:\n\n" +
  "| Mode | Best for | Mandatory tools |\n" +
  "|------|----------|----------------|\n" +
  "| `guided` | Beginners, unfamiliar codebases, complex type-level code | load, type, hole_fits, suggest, quickcheck |\n" +
  "| `medium` | Intermediate devs, familiar with Haskell but new to this codebase | load, quickcheck, suggest (first time only) |\n" +
  "| `expert` | Experienced Haskell devs who know the types | load, eval, quickcheck |\n\n" +
  "→ **Set your mode with:** `ghci_mode(mode=\"guided\")`, `ghci_mode(mode=\"medium\")`, or `ghci_mode(mode=\"expert\")`\n\n" +
  "_You can switch modes at any time with `ghci_mode`._";

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
  // Preserve mode across resets — user chose it deliberately
  state.currentFlow = null;
  state.activeModule = null;
  state.modules.clear();
  state.toolHistory = [];
  state.editsSinceLastLoad = 0;
  state.pendingWarningCount = 0;
  state.sessionStarted = Date.now();
}

/** Generate a compact status summary for injection into tool responses. */
export function workflowHint(state: WorkflowState): Record<string, unknown> | null {
  const hints: Record<string, unknown> = {};

  if (state.mode) {
    hints.mode = state.mode;
  }

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
  items.push("[ ] ghci_lint — code quality");
  items.push("[ ] ghci_format — formatting");

  return items;
}

/** Derive the module phase from function counts instead of relying solely on suggest tool. */
export function derivePhase(p: ModuleProgress): ModuleProgress["phase"] {
  if (p.functionsTotal === 0) return "stub";
  if (p.functionsImplemented < p.functionsTotal) return "implementing";
  return "complete";
}

/** Serialize state for MCP resource / JSON response. */
export function serializeState(state: WorkflowState): Record<string, unknown> {
  const modules: Record<string, ModuleProgress> = {};
  for (const [k, v] of state.modules) {
    modules[k] = v;
  }

  return {
    mode: state.mode,
    currentFlow: state.currentFlow,
    activeModule: state.activeModule,
    modules,
    recentTools: state.toolHistory.slice(-10),
    editsSinceLastLoad: state.editsSinceLastLoad,
    pendingWarningCount: state.pendingWarningCount,
    sessionStarted: state.sessionStarted,
  };
}
