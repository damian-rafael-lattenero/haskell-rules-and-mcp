import { McpServer } from "@modelcontextprotocol/sdk/server/mcp.js";
import { StdioServerTransport } from "@modelcontextprotocol/sdk/server/stdio.js";
import { z } from "zod";
import path from "node:path";
import { readFile, writeFile, mkdir, stat } from "node:fs/promises";
import { GhciSession } from "./ghci-session.js";
import { resetQuickCheckState } from "./tools/quickcheck.js";
import { discoverProjects } from "./project-manager.js";
import { RULES_REGISTRY, loadRule } from "./resources/rules.js";
import { parseEvalOutput } from "./parsers/eval-output-parser.js";
import { createRulesChecker, type ToolContext } from "./tools/registry.js";
import { parseCabalModules, moduleToFilePath, getLibrarySrcDir } from "./parsers/cabal-parser.js";
import {
  createWorkflowState,
  resetWorkflowState,
  logTool,
  getModuleProgress as getModProgress,
  updateModuleProgress as updateModProgress,
  setOptionalToolAvailability,
  serializeState,
  workflowHint,
  suggestNextStep,
  moduleChecklist,
  deriveGuidance,
  workflowHelp,
} from "./workflow-state.js";

// Tool register functions
import { register as registerTypeCheck } from "./tools/type-check.js";
import { register as registerTypeInfo } from "./tools/type-info.js";
import { register as registerLoadModule } from "./tools/load-module.js";
import { register as registerBuild } from "./tools/build.js";
import { register as registerTest } from "./tools/test.js";
import { register as registerHoogleSearch } from "./tools/hoogle.js";
import { register as registerScaffold } from "./tools/scaffold.js";
import { register as registerCheckModule } from "./tools/check-module.js";
import { register as registerApplyExports } from "./tools/apply-exports.js";
import { register as registerDiagnostics } from "./tools/diagnostics.js";
import { register as registerHoleFits } from "./tools/hole-fits.js";
import { register as registerQuickCheck, registerBatch as registerQuickCheckBatch } from "./tools/quickcheck.js";
import { register as registerGoto } from "./tools/goto.js";
import { register as registerComplete } from "./tools/complete.js";
import { register as registerDoc } from "./tools/doc.js";
import { register as registerImports } from "./tools/imports.js";
import { register as registerFormat } from "./tools/format.js";
import { register as registerLint } from "./tools/lint.js";
import { register as registerAddImport } from "./tools/add-import.js";
import { register as registerReferences } from "./tools/references.js";
import { register as registerRename } from "./tools/rename.js";
import { register as registerSetup } from "./tools/setup.js";
import { register as registerSuggest } from "./tools/suggest.js";
import { register as registerArbitrary } from "./tools/arbitrary.js";
import { register as registerTrace } from "./tools/trace.js";
import { register as registerRegression } from "./tools/regression.js";
import { register as registerInit } from "./tools/init.js";
import { register as registerExportTests } from "./tools/export-tests.js";
import { register as registerDeps } from "./tools/deps.js";
import { register as registerValidateCabal } from "./tools/validate-cabal.js";
import { register as registerHole } from "./tools/hole.js";
import { register as registerRefactor } from "./tools/refactor.js";
import { register as registerFlags } from "./tools/flags.js";
import { register as registerProfile } from "./tools/profile.js";
import { register as registerHls } from "./tools/hls.js";
import { register as registerWatch } from "./tools/watch.js";
import { register as registerFuzzParser } from "./tools/fuzz-parser.js";
import { register as registerEquiv } from "./tools/equiv.js";
import { register as registerPropertyLifecycle } from "./tools/property-lifecycle.js";

// Base directory: the project root (parent of mcp-server/)
const BASE_DIR = path.resolve(import.meta.dirname, "..", "..");

// Active project directory — mutable, can be switched at runtime.
// Defaults to HASKELL_PROJECT_DIR env var, or the current working directory.
let projectDir =
  process.env.HASKELL_PROJECT_DIR ?? process.cwd();

// Load workflow instructions (single source of truth for Claude)
const WORKFLOW_PATH = path.resolve(import.meta.dirname, "..", "rules", "haskell-mcp-workflow.md");
let workflowInstructions: string;
try {
  workflowInstructions = await readFile(WORKFLOW_PATH, "utf-8");
} catch {
  workflowInstructions = "Use haskell-flows MCP tools for all Haskell operations. Never use Bash for cabal/ghc/ghci.";
}

const server = new McpServer(
  { name: "haskell-flows", version: "0.4.0" },
  { instructions: workflowInstructions },
);

let ghciSession: GhciSession | null = null;

async function getSession(): Promise<GhciSession> {
  // Check if session is alive and healthy
  if (ghciSession?.isAlive()) {
    const health = ghciSession.getHealth();
    if (health.status === 'corrupted') {
      // Auto-recovery: restart corrupted session
      await ghciSession.restart();
    }
    return ghciSession;
  }
  const session = new GhciSession(
    projectDir,
    process.env.HASKELL_LIBRARY_TARGET
  );
  ghciSession = session;
  await session.start();
  session.on("exit", () => {
    if (ghciSession === session) {
      ghciSession = null;
    }
  });
  return session;
}

// --- Rules checker (cached, reset on project switch) ---
const rulesChecker = createRulesChecker(() => projectDir, () => BASE_DIR);

// --- Workflow State ---
const workflowState = createWorkflowState();

// --- Tool Context ---
const ctx: ToolContext = {
  getSession,
  getProjectDir: () => projectDir,
  getBaseDir: () => BASE_DIR,
  resetQuickCheckState,
  getRulesNotice: rulesChecker.check,
  resetRulesCache: rulesChecker.reset,
  getWorkflowState: () => workflowState,
  logToolExecution: (tool, success) => logTool(workflowState, tool, success),
  getModuleProgress: (path) => getModProgress(workflowState, path),
  updateModuleProgress: (path, updates) => updateModProgress(workflowState, path, updates),
  setOptionalToolAvailability: (tool, status) => setOptionalToolAvailability(workflowState, tool, status),
  invalidateProjectsCache: () => {
    projectsCache = null;
  },
};

// --- Register all tools from their modules ---
registerTypeCheck(server, ctx);
registerTypeInfo(server, ctx);
registerLoadModule(server, ctx);
registerBuild(server, ctx);
registerTest(server, ctx);
registerHoogleSearch(server, ctx);
registerScaffold(server, ctx);
registerCheckModule(server, ctx);
registerApplyExports(server, ctx);
registerDiagnostics(server, ctx);
registerHoleFits(server, ctx);
registerQuickCheck(server, ctx);
registerQuickCheckBatch(server, ctx);
registerGoto(server, ctx);
registerComplete(server, ctx);
registerDoc(server, ctx);
registerImports(server, ctx);
registerFormat(server, ctx);
registerLint(server, ctx);
registerAddImport(server, ctx);
registerReferences(server, ctx);
registerRename(server, ctx);
registerSetup(server, ctx);
registerSuggest(server, ctx);
registerArbitrary(server, ctx);
registerTrace(server, ctx);
registerRegression(server, ctx);
registerInit(server, ctx);
registerExportTests(server, ctx);
registerDeps(server, ctx);
registerValidateCabal(server, ctx);
registerHole(server, ctx);
registerRefactor(server, ctx);
registerFlags(server, ctx);
registerProfile(server, ctx);
registerHls(server, ctx);
registerWatch(server, ctx);
registerFuzzParser(server, ctx);
registerEquiv(server, ctx);
registerPropertyLifecycle(server, ctx);

// --- Tool: ghci_fix_warning (inline) ---
server.tool(
  "ghci_fix_warning",
  "Auto-fix common GHC warnings like unused-matches (GHC-40910), unused-imports (GHC-38417), etc. " +
  "Can preview the fix (apply=false) or apply it directly (apply=true).",
  {
    file: z.string().describe("File path relative to project root"),
    line: z.number().describe("Line number where the warning occurs"),
    code: z.string().describe("GHC warning code (e.g. GHC-40910, GHC-38417)"),
    apply: z.boolean().optional().describe("If true, apply fix; if false, return patch only (default: false)")
  },
  async ({ file, line, code, apply }) => {
    const { fixWarning } = await import("./tools/fix-warning.js");
    const result = await fixWarning(projectDir, file, line, code, apply ?? false);
    
    return {
      content: [{ type: "text", text: JSON.stringify(result, null, 2) }]
    };
  }
);

// --- Tool: ghci_kind (inline, simple) ---
server.tool(
  "ghci_kind",
  "Get the kind of a Haskell type expression using GHCi :k. Useful for understanding higher-kinded types.",
  {
    type_expression: z.string().describe(
      'The type expression to get the kind of. Examples: "Maybe", "Either String", "Functor"'
    ),
  },
  async ({ type_expression }) => {
    const session = await getSession();
    const result = await session.kindOf(type_expression);
    return {
      content: [{ type: "text", text: JSON.stringify({ success: result.success, output: result.output }) }],
    };
  }
);

// --- Tool: ghci_eval (inline, uses parseEvalOutput) ---
server.tool(
  "ghci_eval",
  "Evaluate a Haskell expression in GHCi and return the result. Useful for testing pure functions. " +
    "Supports multi-line evaluation: use the 'statements' parameter for sequential bindings, " +
    "or put newlines in 'expression' for auto-block detection.",
  {
    expression: z.string().describe(
      'The expression to evaluate. Examples: "map (+1) [1,2,3]", "show (Just 42)"'
    ),
    statements: z.array(z.string()).optional().describe(
      "Array of lines executed as a GHCi :{ :} block. " +
        "Both 'x = 42' and 'let x = 42' work for bindings (auto-prefixed with 'let' if needed). " +
        "The last line should be the expression to evaluate. " +
        "GHCi commands (starting with :) are NOT supported inside blocks."
    ),
    timeout_ms: z.number().int().positive().optional().describe(
      "Optional timeout in milliseconds for evaluation. Default: 30000."
    ),
  },
  async ({ expression, statements, timeout_ms }) => {
    const session = await getSession();
    const timeout = timeout_ms ?? 30_000;
    const withTimeout = async <T>(promise: Promise<T>): Promise<T> =>
      await Promise.race([
        promise,
        new Promise<T>((_, reject) =>
          setTimeout(() => reject(new Error(`Evaluation timeout after ${timeout}ms`)), timeout)
        ),
      ]);
    let result;
    try {
      if (statements && statements.length > 0) {
      // Group consecutive bare bindings into single let blocks to support
      // recursive and mutually recursive definitions in GHCi.
      const fixed = groupBindingsIntoLetBlocks(statements);
      // Extract imports — they can't go inside :{ :} blocks.
      // Execute them separately before the block.
      const imports = fixed.filter(s => s.trim().startsWith("import "));
      const nonImports = fixed.filter(s => !s.trim().startsWith("import "));
      for (const imp of imports) {
        await withTimeout(session.execute(imp));
      }
      result = nonImports.length > 0
        ? await withTimeout(session.executeBlock(nonImports))
        : { output: "", success: true };
      } else if (expression.includes("\n")) {
        result = await withTimeout(session.executeBlock(expression.split("\n")));
      } else {
        result = await withTimeout(session.execute(expression));
      }
    } catch (err) {
      const message = err instanceof Error ? err.message : String(err);
      if (message.toLowerCase().includes("timeout")) {
        // If eval times out, the running GHCi command may still be executing.
        // Reset the session proactively so subsequent tool calls don't stall.
        try {
          if (ghciSession) {
            await ghciSession.kill();
            ghciSession = null;
          }
        } catch {
          // non-fatal; caller already gets timeout error
        }
      }
      return {
        content: [{
          type: "text",
          text: JSON.stringify({
            success: false,
            error: message,
          }),
        }],
      };
    }
    const parsed = parseEvalOutput(result.output);
    const isException = parsed.result.startsWith("*** Exception:");
    const guidance = deriveGuidance(workflowState, "ghci_eval");
    // Get type of result via :t it (non-fatal)
    let resultType: string | undefined;
    if (result.success && !isException) {
      try {
        const typeResult = await session.execute(":t it");
        if (typeResult.success && typeResult.output.includes("::")) {
          resultType = typeResult.output.replace(/^it\s*::\s*/, "").trim();
        }
      } catch { /* non-fatal */ }
    }
    return {
      content: [{
        type: "text",
        text: JSON.stringify({
          success: result.success && !isException,
          output: parsed.result,
          ...(resultType ? { type: resultType } : {}),
          ...(parsed.warnings.length > 0 ? { warnings: parsed.warnings } : {}),
          ...(parsed.result !== parsed.raw ? { raw: parsed.raw } : {}),
          ...(guidance.length > 0 ? { _guidance: guidance } : {}),
        }),
      }],
    };
  }
);

// --- Tool: ghci_session ---
server.tool(
  "ghci_session",
  "Manage the GHCi session: restart it or check its status. Use 'restart' after changing .cabal file or adding new modules.",
  {
    action: z.enum(["status", "restart", "stats"]).describe('"status" to check if GHCi is alive, "restart" to restart the session, "stats" for workflow/project summary'),
  },
  async ({ action }) => {
    if (action === "status") {
      const alive = ghciSession?.isAlive() ?? false;
      const notice = await ctx.getRulesNotice();
      const response: Record<string, unknown> = { alive, projectDir };
      if (notice) response._info = notice;
      // When no active session, surface available projects so the LLM can switch
      if (!alive) {
        try {
          const projects = await discoverProjects(BASE_DIR);
          const others = projects.filter((p) => p.path !== projectDir);
          if (others.length > 0) {
            response._hint =
              `No active session. Available projects: ${others.map((p) => p.name).join(", ")}. ` +
              `Call ghci_switch_project(project="<name>") to activate one.`;
          }
        } catch {
          // Non-fatal: project discovery failure should not break status
        }
      }
      return { content: [{ type: "text", text: JSON.stringify(response) }] };
    }
    if (action === "stats") {
      const modules = [...workflowState.modules.values()];
      const totalProperties = modules.reduce((acc, mod) => acc + mod.propertiesPassed.length, 0);
      const gatesComplete = modules.filter(
        (mod) =>
          mod.completionGates.checkModule &&
          mod.completionGates.lint &&
          mod.completionGates.format
      ).length;
      return {
        content: [{
          type: "text",
          text: JSON.stringify({
            success: true,
            projectDir,
            sessionAlive: ghciSession?.isAlive() ?? false,
            activeModule: workflowState.activeModule,
            modulesTracked: modules.length,
            modulesGateComplete: gatesComplete,
            totalProperties,
            pendingWarningCount: workflowState.pendingWarningCount,
            editsSinceLastLoad: workflowState.editsSinceLastLoad,
            recentTools: workflowState.toolHistory.slice(-10),
          }),
        }],
      };
    }
    resetQuickCheckState();
    if (ghciSession) { await ghciSession.kill(); ghciSession = null; }
    const session = await getSession();
    return {
      content: [{ type: "text", text: JSON.stringify({ success: true, message: "GHCi session restarted", alive: session.isAlive() }) }],
    };
  }
);

// --- Tool: mcp_restart ---
server.tool(
  "mcp_restart",
  "Restart the GHCi session. Use after .cabal changes, new modules, or dependency updates. " +
    "Kills the current GHCi process and starts a fresh one in the same project directory. " +
    "For TypeScript code changes (after 'cd mcp-server && npx tsc'), the user must start a new Claude Code session — " +
    "do NOT use process.exit() as it permanently disconnects tools from the conversation.",
  {},
  async () => {
    resetQuickCheckState();
    if (ghciSession) { await ghciSession.kill(); ghciSession = null; }
    const session = await getSession();
    return {
      content: [{ type: "text", text: JSON.stringify({ success: true, message: "GHCi session restarted. MCP server still running.", alive: session.isAlive() }) }],
    };
  }
);

// --- Project Discovery Cache ---
let projectsCache: { projects: Awaited<ReturnType<typeof discoverProjects>>; timestamp: number } | null = null;
const CACHE_TTL_MS = 5000; // 5 seconds

async function getProjects(searchDir: string, forceRefresh = false): Promise<Awaited<ReturnType<typeof discoverProjects>>> {
  const now = Date.now();
  if (!forceRefresh && projectsCache && (now - projectsCache.timestamp) < CACHE_TTL_MS) {
    return projectsCache.projects;
  }
  const projects = await discoverProjects(searchDir, 3);
  projectsCache = { projects, timestamp: now };
  return projects;
}

// --- Tool: ghci_switch_project ---
server.tool(
  "ghci_switch_project",
  "List available Haskell projects or switch to a different one. " +
    "Projects are discovered recursively from subdirectories containing .cabal files (max depth: 3). " +
    "Omit the project parameter to list available projects. " +
    "Use search_dir to search in a specific subdirectory.",
  {
    project: z.string().optional().describe("Project name to switch to. Omit to list available projects."),
    search_dir: z.string().optional().describe(
      "Optional: subdirectory to search for projects (relative to workspace root). " +
      "Default: workspace root (searches everywhere recursively up to depth 3)."
    ),
    refresh: z.boolean().optional().describe(
      "Optional: force refresh of project cache. Use after creating new projects with ghci_init."
    ),
  },
  async ({ project, search_dir, refresh }) => {
    const searchPath = search_dir ? path.join(BASE_DIR, search_dir) : BASE_DIR;
    const projects = await getProjects(searchPath, refresh ?? false);
    if (!project) {
      return {
        content: [{
          type: "text",
          text: JSON.stringify({
            projects: projects.map((p) => ({ name: p.name, dirName: p.dirName, path: p.path, active: p.path === projectDir })),
            activeProject: projects.find((p) => p.path === projectDir)?.name ?? "unknown",
          }),
        }],
      };
    }
    const target = projects.find((p) => p.name === project || p.path.endsWith(project));
    if (!target) {
      return {
        content: [{ type: "text", text: JSON.stringify({ success: false, error: `Project '${project}' not found. Available: ${projects.map((p) => p.name).join(", ")}` }) }],
      };
    }
    resetQuickCheckState();
    resetWorkflowState(workflowState);
    if (ghciSession) { await ghciSession.kill(); ghciSession = null; }

    // Auto-scaffold before switching so GHCi finds all source files.
    let scaffoldedModules: string[] = [];
    try {
      const { handleScaffold } = await import("./tools/scaffold.js");
      const scaffoldResult = JSON.parse(await handleScaffold(target.path));
      if (scaffoldResult.created?.length > 0) {
        scaffoldedModules = scaffoldResult.created;
      }
    } catch {
      // Non-fatal: scaffold may fail if no .cabal or other issue
    }

    // Only commit the switch AFTER GHCi starts successfully.
    // Mutating projectDir before startup can leave the server pointing at a
    // broken project when cabal/ghci fails (e.g. empty .cabal file).
    const previousProjectDir = projectDir;
    projectDir = target.path;
    rulesChecker.reset();

    let session: GhciSession;
    try {
      session = await getSession();
    } catch (err) {
      // Rollback: restore the previous project so the server is still usable.
      projectDir = previousProjectDir;
      rulesChecker.reset();
      const msg = err instanceof Error ? err.message : String(err);
      return {
        content: [{ type: "text", text: JSON.stringify({
          success: false,
          error: `Failed to start GHCi in project '${target.name}': ${msg}. ` +
            `Project directory not changed — still in '${previousProjectDir}'.`,
        }) }],
      };
    }

    // Auto-load all library modules so names are in scope immediately
    let modulesLoaded: string[] = [];
    try {
      const cabalModules = await parseCabalModules(target.path);
      const srcDir = await getLibrarySrcDir(target.path);
      const paths = cabalModules.library.map((mod) => moduleToFilePath(mod, srcDir));
      if (paths.length > 0) {
        await session.loadModules(paths, cabalModules.library);
        modulesLoaded = paths;
      }
    } catch {
      // Non-fatal: modules can be loaded manually with ghci_load
    }

    return {
      content: [{ type: "text", text: JSON.stringify({
        success: true,
        message: `Switched to project '${target.name}'`,
        projectDir: target.path,
        alive: session.isAlive(),
        ...(scaffoldedModules.length > 0 ? { scaffolded: scaffoldedModules } : {}),
        ...(modulesLoaded.length > 0 ? { modulesLoaded: modulesLoaded.length } : {}),
      }) }],
    };
  }
);

// --- Tool: ghci_batch ---
server.tool(
  "ghci_batch",
  "Execute multiple GHCi commands in a single atomic call. Returns all results as a JSON array " +
    "with each result aligned to its command (no offset issues). " +
    "Ideal for: running several :t/:i/eval commands without roundtrips, " +
    "testing multiple expressions after an edit, or batch type-checking. " +
    "Optionally reloads modules first and stops on first error.",
  {
    commands: z.array(z.string()).describe('List of GHCi commands to execute. Examples: [":t map", ":t foldr", "1 + 2"]'),
    reload: z.boolean().optional().describe("If true, reload modules (:r) before executing commands"),
    stop_on_error: z.boolean().optional().describe("If true, stop executing after the first failed command"),
  },
  async ({ commands, reload, stop_on_error }) => {
    const session = await getSession();
    const { results, allSuccess } = await session.executeBatch(commands, {
      reload: reload ?? false,
      stopOnError: stop_on_error ?? false,
    });
    return {
      content: [{
        type: "text",
        text: JSON.stringify({
          allSuccess,
          count: results.length,
          results: results.map((r, i) => {
            const parsed = parseEvalOutput(r.output);
            return {
              command: commands[i],
              success: r.success,
              output: parsed.result,
              ...(parsed.warnings.length > 0 ? { warnings: parsed.warnings } : {}),
            };
          }),
        }),
      }],
    };
  }
);

// --- Tool: ghci_workflow ---
server.tool(
  "ghci_workflow",
  "Query the development workflow state: current flow/step, module progress, next action, or checklist. " +
    "Use to understand where you are in the development process and what to do next.",
  {
    action: z.enum(["status", "next", "progress", "checklist", "help"]).describe(
      '"status": full workflow state summary. "next": what step to do next. ' +
      '"progress": per-module progress (functions, properties). "checklist": TODO list for active module. ' +
      '"help": context-aware guidance with suggested_tools, reasoning, and steps.'
    ),
  },
  async ({ action }) => {
    if (action === "status") {
      return {
        content: [{ type: "text", text: JSON.stringify(serializeState(workflowState)) }],
      };
    }
    if (action === "next") {
      return {
        content: [{ type: "text", text: JSON.stringify({ nextStep: suggestNextStep(workflowState) }) }],
      };
    }
    if (action === "progress") {
      const modules: Record<string, unknown> = {};
      for (const [k, v] of workflowState.modules) {
        modules[k] = {
          phase: v.phase,
          functions: `${v.functionsImplemented}/${v.functionsTotal}`,
          propertiesPassed: v.propertiesPassed.length,
          propertiesFailed: v.propertiesFailed.length,
          arbitraryDefined: v.arbitraryInstancesDefined,
        };
      }
      return {
        content: [{ type: "text", text: JSON.stringify({ activeModule: workflowState.activeModule, modules }) }],
      };
    }
    if (action === "help") {
      return {
        content: [{ type: "text", text: JSON.stringify(workflowHelp(workflowState)) }],
      };
    }
    // checklist
    return {
      content: [{ type: "text", text: JSON.stringify({ checklist: moduleChecklist(workflowState) }) }],
    };
  }
);

// --- MCP Resources: Haskell Rules ---
for (const rule of RULES_REGISTRY) {
  server.registerResource(rule.name, rule.uri, {
    description: rule.description,
    mimeType: "text/markdown",
  }, async (uri) => ({
    contents: [{ uri: uri.toString(), text: await loadRule(rule), mimeType: "text/markdown" }],
  }));
}

// --- MCP Resource: Workflow State ---
server.registerResource("workflow-state", "workflow://haskell/state", {
  description: "Current workflow progress: active flow, module status, pending actions",
  mimeType: "application/json",
}, async (uri) => ({
  contents: [{ uri: uri.toString(), text: JSON.stringify(serializeState(workflowState)), mimeType: "application/json" }],
}));

/**
 * Group consecutive bare bindings (non-last lines) into single `let` blocks.
 * Supports recursive and mutually recursive definitions by merging them
 * into one `let` with continuation-line indentation.
 *
 * Example: ["f 0 = 1", "f n = n * f (n-1)", "f 5"]
 *       → ["let f 0 = 1\n    f n = n * f (n-1)", "f 5"]
 */
export function groupBindingsIntoLetBlocks(statements: string[]): string[] {
  if (statements.length === 0) return [];

  const result: string[] = [];
  let bindingGroup: string[] = [];

  const isBareBinding = (s: string): boolean => {
    const trimmed = s.trim();
    if (trimmed.startsWith("let ") || trimmed.startsWith("import ") ||
        trimmed.startsWith(":") || trimmed === "") return false;
    return /^[\w'][\w']*(\s+\S+)*\s+=(?!=)/.test(trimmed);
  };

  const flushGroup = () => {
    if (bindingGroup.length === 0) return;
    // Join as a single let block with 4-space continuation indentation
    const firstLine = `let ${bindingGroup[0]}`;
    const rest = bindingGroup.slice(1).map(s => `    ${s}`);
    result.push([firstLine, ...rest].join("\n"));
    bindingGroup = [];
  };

  for (let i = 0; i < statements.length; i++) {
    const isLast = i === statements.length - 1;
    if (isLast) {
      flushGroup();
      result.push(statements[i]!);
    } else if (isBareBinding(statements[i]!)) {
      bindingGroup.push(statements[i]!);
    } else {
      flushGroup();
      result.push(statements[i]!);
    }
  }

  return result;
}

// --- Agent instructions cache sync ---
//
// ARCHITECTURE: Two complementary layers cover all MCP-compatible agents.
//
// Layer 1 — MCP protocol (universal, automatic):
//   McpServer({ instructions: ... }) sends the instructions in the MCP
//   initialize response. Every spec-compliant client receives them:
//     • Claude Code      — reads server_info.instructions from protocol
//     • GitHub Copilot   — reads from MCP protocol in VS Code
//     • Zed              — reads from MCP protocol
//     • Continue.dev     — reads from MCP protocol
//
// Layer 2 — file cache (agent-specific):
//   Some agents maintain a local file cache of server instructions so they
//   survive reconnects without re-reading the protocol. We sync to all
//   detected file-cache agents at startup.
//
//   Known file-cache agents:
//     • Cursor   → ~/.cursor/projects/<id>/mcps/<server>/INSTRUCTIONS.md
//     • Windsurf → ~/.windsurf/projects/<id>/mcps/<server>/INSTRUCTIONS.md
//
//   Detection: only sync to agents whose config directory actually exists.
//   This avoids creating phantom directories for non-installed agents.

/**
 * Agent descriptor for file-cache sync.
 * Add new entries here when a new agent with file-cache semantics is found.
 */
export interface AgentCacheSpec {
  /** Human-readable name (for logging/testing). */
  name: string;
  /** Subdirectory of HOME that indicates the agent is installed. */
  configDir: string;
  /**
   * Given home dir, project ID, and MCP server name, return the full path
   * to the INSTRUCTIONS.md file that the agent reads.
   */
  instructionsPath: (home: string, projectId: string, serverName: string) => string;
}

/**
 * Registry of all known file-cache agents.
 *
 * PROTOCOL-ONLY agents are NOT listed here — they get instructions from the
 * MCP protocol `initialize` response and need no file sync:
 *
 *   • Claude Code — reads `instructions` from MCP protocol on every startup.
 *     Uses ~/.claude/projects/ for conversation state but has NO mcps cache.
 *     Path encoding: /Users/foo/bar → -Users-foo-bar (leading dash, unlike Cursor).
 *     Already covered by McpServer({ instructions: ... }) — no entry needed.
 *
 *   • GitHub Copilot (VS Code) — reads from MCP protocol.
 *   • Zed, Continue.dev, etc.  — read from MCP protocol.
 *
 * Add an entry below ONLY for agents that maintain a persistent INSTRUCTIONS.md
 * file-cache that is NOT re-read from the protocol on every session.
 */
export const AGENT_CACHE_SPECS: AgentCacheSpec[] = [
  {
    name: "cursor",
    configDir: ".cursor",
    instructionsPath: (home, projectId, serverName) =>
      path.join(home, ".cursor", "projects", projectId, "mcps", serverName, "INSTRUCTIONS.md"),
  },
  {
    name: "windsurf",
    configDir: ".windsurf",
    instructionsPath: (home, projectId, serverName) =>
      path.join(home, ".windsurf", "projects", projectId, "mcps", serverName, "INSTRUCTIONS.md"),
  },
];

/**
 * Encode an absolute workspace path as the project ID used by Cursor-style
 * agents (strip leading "/" and replace remaining "/" with "-").
 *
 *   /Users/foo/bar/project  →  Users-foo-bar-project
 */
export function encodeProjectId(workspaceRoot: string): string {
  return workspaceRoot.replace(/^\//, "").replace(/\//g, "-");
}

/**
 * Sync MCP server instructions to all detected file-cache agents.
 *
 * Runs at server startup — any edit to haskell-mcp-workflow.md is
 * automatically picked up the next time the MCP server starts, for every
 * installed agent simultaneously.
 *
 * Non-fatal: errors are silently swallowed so the server always starts.
 */
export async function syncAgentInstructionsCaches(
  workspaceRoot: string,
  instructions: string,
  specs: AgentCacheSpec[] = AGENT_CACHE_SPECS,
  overrideHome?: string
): Promise<{ synced: string[]; skipped: string[] }> {
  const home = overrideHome ?? process.env.HOME ?? process.env.USERPROFILE ?? "";
  const synced: string[] = [];
  const skipped: string[] = [];

  if (!home) return { synced, skipped: specs.map((s) => s.name) };

  const projectId = encodeProjectId(workspaceRoot);
  const SERVER_NAME = "user-haskell-flows";

  await Promise.all(
    specs.map(async (spec) => {
      try {
        // Only sync to agents that are actually installed.
        await stat(path.join(home, spec.configDir));

        const cachePath = spec.instructionsPath(home, projectId, SERVER_NAME);
        await mkdir(path.dirname(cachePath), { recursive: true });
        await writeFile(cachePath, instructions, "utf-8");
        synced.push(spec.name);
      } catch {
        // Agent not installed, or write failed — skip silently.
        skipped.push(spec.name);
      }
    })
  );

  return { synced, skipped };
}

// Auto-sync the Cursor/Windsurf agent caches so serverUseInstructions stay
// up-to-date without manual copy-paste.
void syncAgentInstructionsCaches(BASE_DIR, workflowInstructions);

// --- Start the server ---
async function main() {
  const transport = new StdioServerTransport();
  await server.connect(transport);
  console.error(`haskell-flows MCP server running (project: ${projectDir})`);
}

main().catch((err) => {
  console.error("Fatal error:", err);
  process.exit(1);
});
