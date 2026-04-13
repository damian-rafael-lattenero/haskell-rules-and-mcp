import { McpServer } from "@modelcontextprotocol/sdk/server/mcp.js";
import { StdioServerTransport } from "@modelcontextprotocol/sdk/server/stdio.js";
import { z } from "zod";
import path from "node:path";
import { GhciSession } from "./ghci-session.js";
import { resetQuickCheckState } from "./tools/quickcheck.js";
import { discoverProjects, getPlaygroundDir } from "./project-manager.js";
import { RULES_REGISTRY, loadRule } from "./resources/rules.js";
import { parseEvalOutput } from "./parsers/eval-output-parser.js";
import { createRulesChecker, type ToolContext } from "./tools/registry.js";

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
import { register as registerQuickCheck } from "./tools/quickcheck.js";
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

// Base directory: the project root (parent of mcp-server/)
const BASE_DIR = path.resolve(import.meta.dirname, "..", "..");

// Active project directory — mutable, can be switched at runtime
let projectDir =
  process.env.HASKELL_PROJECT_DIR ??
  path.join(BASE_DIR, "playground", "hindley-milner");

const server = new McpServer({
  name: "haskell-ghci",
  version: "0.3.0",
});

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
const rulesChecker = createRulesChecker(() => projectDir);

// --- Tool Context ---
const ctx: ToolContext = {
  getSession,
  getProjectDir: () => projectDir,
  getBaseDir: () => BASE_DIR,
  resetQuickCheckState,
  getRulesNotice: rulesChecker.check,
  resetRulesCache: rulesChecker.reset,
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
  "Evaluate a Haskell expression in GHCi and return the result. Useful for testing pure functions.",
  {
    expression: z.string().describe(
      'The expression to evaluate. Examples: "map (+1) [1,2,3]", "show (Just 42)"'
    ),
  },
  async ({ expression }) => {
    const session = await getSession();
    const result = await session.execute(expression);
    const parsed = parseEvalOutput(result.output);
    const isException = parsed.result.startsWith("*** Exception:");
    return {
      content: [{
        type: "text",
        text: JSON.stringify({
          success: result.success && !isException,
          output: parsed.result,
          ...(parsed.warnings.length > 0 ? { warnings: parsed.warnings } : {}),
          ...(parsed.result !== parsed.raw ? { raw: parsed.raw } : {}),
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
      if (notice) response._notice = notice;
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
  "Restart the GHCi session (default) or the entire MCP server process. " +
    "Default behavior restarts only GHCi — use after .cabal changes, new modules, or dependency updates. " +
    "Set full_restart=true to also exit the MCP server process — use only after recompiling TypeScript " +
    "with 'cd mcp-server && npx tsc'. WARNING: full_restart=true will disconnect the MCP client temporarily.",
  {
    full_restart: z.boolean().optional().describe(
      "If true, exit the MCP server process (for TypeScript code reload). Default: false (GHCi restart only)."
    ),
  },
  async ({ full_restart }) => {
    resetQuickCheckState();
    if (ghciSession) { await ghciSession.kill(); ghciSession = null; }
    if (full_restart) {
      setTimeout(() => process.exit(0), 100);
      return {
        content: [{ type: "text", text: JSON.stringify({ success: true, message: "MCP server exiting for full restart. Next tool call will reconnect." }) }],
      };
    }
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
    if (ghciSession) { await ghciSession.kill(); ghciSession = null; }
    projectDir = target.path;
    rulesChecker.reset(); // Re-check rules in new project
    const session = await getSession();
    return {
      content: [{ type: "text", text: JSON.stringify({ success: true, message: `Switched to project '${target.name}'`, projectDir: target.path, alive: session.isAlive() }) }],
    };
  }
);

// --- Tool: ghci_batch ---
server.tool(
  "ghci_batch",
  "Execute multiple GHCi commands in a single call. Returns all results as a JSON array. " +
    "Useful for running several :t, :i, or eval commands without multiple roundtrips. " +
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

// --- MCP Resources: Haskell Rules ---
for (const rule of RULES_REGISTRY) {
  server.registerResource(rule.name, rule.uri, {
    description: rule.description,
    mimeType: "text/markdown",
  }, async (uri) => ({
    contents: [{ uri: uri.toString(), text: await loadRule(rule), mimeType: "text/markdown" }],
  }));
}

// --- Start the server ---
async function main() {
  const transport = new StdioServerTransport();
  await server.connect(transport);
  console.error(`haskell-ghci MCP server running (project: ${projectDir})`);
}

main().catch((err) => {
  console.error("Fatal error:", err);
  process.exit(1);
});
