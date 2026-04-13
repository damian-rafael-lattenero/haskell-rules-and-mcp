import { McpServer } from "@modelcontextprotocol/sdk/server/mcp.js";
import { StdioServerTransport } from "@modelcontextprotocol/sdk/server/stdio.js";
import { z } from "zod";
import path from "node:path";
import { readFile } from "node:fs/promises";
import { GhciSession } from "./ghci-session.js";
import { resetQuickCheckState } from "./tools/quickcheck.js";
import { discoverProjects, getPlaygroundDir } from "./project-manager.js";
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
  serializeState,
  workflowHint,
  suggestNextStep,
  moduleChecklist,
  deriveGuidance,
} from "./workflow-state.js";

// Tool register functions
import { register as registerTypeCheck } from "./tools/type-check.js";
import { register as registerTypeInfo } from "./tools/type-info.js";
import { register as registerLoadModule } from "./tools/load-module.js";
import { register as registerBuild } from "./tools/build.js";
import { register as registerHoogleSearch } from "./tools/hoogle.js";
import { register as registerScaffold } from "./tools/scaffold.js";
import { register as registerCheckModule } from "./tools/check-module.js";
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
  if (ghciSession?.isAlive()) {
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
};

// --- Register all tools from their modules ---
registerTypeCheck(server, ctx);
registerTypeInfo(server, ctx);
registerLoadModule(server, ctx);
registerBuild(server, ctx);
registerHoogleSearch(server, ctx);
registerScaffold(server, ctx);
registerCheckModule(server, ctx);
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
  },
  async ({ expression, statements }) => {
    const session = await getSession();
    let result;
    if (statements && statements.length > 0) {
      // Group consecutive bare bindings into single let blocks to support
      // recursive and mutually recursive definitions in GHCi.
      const fixed = groupBindingsIntoLetBlocks(statements);
      // Extract imports — they can't go inside :{ :} blocks.
      // Execute them separately before the block.
      const imports = fixed.filter(s => s.trim().startsWith("import "));
      const nonImports = fixed.filter(s => !s.trim().startsWith("import "));
      for (const imp of imports) {
        await session.execute(imp);
      }
      result = nonImports.length > 0
        ? await session.executeBlock(nonImports)
        : { output: "", success: true };
    } else if (expression.includes("\n")) {
      result = await session.executeBlock(expression.split("\n"));
    } else {
      result = await session.execute(expression);
    }
    const parsed = parseEvalOutput(result.output);
    const isException = parsed.result.startsWith("*** Exception:");
    const guidance = deriveGuidance(workflowState, "ghci_eval");
    return {
      content: [{
        type: "text",
        text: JSON.stringify({
          success: result.success && !isException,
          output: parsed.result,
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
    action: z.enum(["status", "restart"]).describe('"status" to check if GHCi is alive, "restart" to restart the session'),
  },
  async ({ action }) => {
    if (action === "status") {
      const alive = ghciSession?.isAlive() ?? false;
      const notice = await ctx.getRulesNotice();
      const response: Record<string, unknown> = { alive, projectDir };
      if (notice) response._info = notice;
      return { content: [{ type: "text", text: JSON.stringify(response) }] };
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

// --- Tool: ghci_switch_project ---
server.tool(
  "ghci_switch_project",
  "List available Haskell projects or switch to a different one. " +
    "Projects are discovered from the playground/ directory. " +
    "Omit the project parameter to list available projects.",
  {
    project: z.string().optional().describe("Project name to switch to. Omit to list available projects."),
  },
  async ({ project }) => {
    const playgroundDir = getPlaygroundDir(BASE_DIR);
    const projects = await discoverProjects(playgroundDir);
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
    projectDir = target.path;
    rulesChecker.reset(); // Re-check rules in new project

    // Auto-scaffold: if .cabal lists modules without source files, create stubs
    // so GHCi can start. Without this, cabal repl crashes on missing sources.
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

    const session = await getSession();

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
    action: z.enum(["status", "next", "progress", "checklist"]).describe(
      '"status": full workflow state summary. "next": what step to do next. ' +
      '"progress": per-module progress (functions, properties). "checklist": TODO list for active module.'
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
