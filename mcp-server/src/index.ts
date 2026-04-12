import { McpServer } from "@modelcontextprotocol/sdk/server/mcp.js";
import { StdioServerTransport } from "@modelcontextprotocol/sdk/server/stdio.js";
import { z } from "zod";
import path from "node:path";
import { GhciSession } from "./ghci-session.js";
import { handleTypeCheck } from "./tools/type-check.js";
import { handleTypeInfo } from "./tools/type-info.js";
import { handleLoadModule } from "./tools/load-module.js";
import { handleBuild } from "./tools/build.js";
import { handleHoogleSearch } from "./tools/hoogle.js";
import { handleScaffold } from "./tools/scaffold.js";
import { handleCheckModule } from "./tools/check-module.js";
import { handleHoleFits } from "./tools/hole-fits.js";
import { handleDiagnostics } from "./tools/diagnostics.js";
import { handleQuickCheck, resetQuickCheckState } from "./tools/quickcheck.js";
import { discoverProjects, getPlaygroundDir } from "./project-manager.js";
import { RULES_REGISTRY, loadRule } from "./resources/rules.js";
import { parseEvalOutput } from "./parsers/eval-output-parser.js";

// Base directory: the project root (parent of mcp-server/)
const BASE_DIR = path.resolve(import.meta.dirname, "..", "..");

// Active project directory — mutable, can be switched at runtime
let projectDir =
  process.env.HASKELL_PROJECT_DIR ??
  path.join(BASE_DIR, "playground", "hindley-milner");

const server = new McpServer({
  name: "haskell-ghci",
  version: "0.2.0",
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

// --- Tool: ghci_type ---
server.tool(
  "ghci_type",
  "Get the type of a Haskell expression using GHCi :t. Use to verify types of subexpressions before composing them.",
  {
    expression: z
      .string()
      .describe(
        'The Haskell expression to type-check. Examples: "map (+1)", "foldr", "Just . show"'
      ),
  },
  async ({ expression }) => {
    const session = await getSession();
    const result = await handleTypeCheck(session, { expression });
    return { content: [{ type: "text", text: result }] };
  }
);

// --- Tool: ghci_info ---
server.tool(
  "ghci_info",
  "Get detailed info about a Haskell name (function, type, typeclass) using GHCi :i. Shows definition, instances, and module.",
  {
    name: z
      .string()
      .describe(
        'The name to look up. Examples: "Functor", "Map.Map", "Maybe", "(++)"'
      ),
  },
  async ({ name }) => {
    const session = await getSession();
    const result = await handleTypeInfo(session, { name });
    return { content: [{ type: "text", text: result }] };
  }
);

// --- Tool: ghci_load ---
server.tool(
  "ghci_load",
  "Load or reload Haskell modules in GHCi. Returns parsed compilation errors and warnings. " +
    "Without module_path: reloads current modules (:r). " +
    "With module_path: loads that specific module. " +
    "With load_all=true: reads .cabal and loads ALL library modules at once (lighter than cabal_build).",
  {
    module_path: z
      .string()
      .optional()
      .describe(
        'Path to a module to load. If omitted, reloads current modules. Examples: "src/Lib.hs"'
      ),
    load_all: z
      .boolean()
      .optional()
      .describe(
        "If true, reads the .cabal file and loads ALL library modules into GHCi at once."
      ),
    diagnostics: z
      .boolean()
      .optional()
      .describe(
        "If true, runs dual-pass compilation (strict errors + typed holes) and categorizes warnings with suggested fix actions. " +
          "Defaults to true for module_path/load_all, false for plain reload."
      ),
  },
  async ({ module_path, load_all, diagnostics }) => {
    const session = await getSession();
    const result = await handleLoadModule(session, { module_path, load_all, diagnostics }, projectDir);
    return { content: [{ type: "text", text: result }] };
  }
);

// --- Tool: ghci_kind ---
server.tool(
  "ghci_kind",
  "Get the kind of a Haskell type expression using GHCi :k. Useful for understanding higher-kinded types.",
  {
    type_expression: z
      .string()
      .describe(
        'The type expression to get the kind of. Examples: "Maybe", "Either String", "Functor"'
      ),
  },
  async ({ type_expression }) => {
    const session = await getSession();
    const result = await session.kindOf(type_expression);
    return {
      content: [
        {
          type: "text",
          text: JSON.stringify({
            success: result.success,
            output: result.output,
          }),
        },
      ],
    };
  }
);

// --- Tool: ghci_eval ---
server.tool(
  "ghci_eval",
  "Evaluate a Haskell expression in GHCi and return the result. Useful for testing pure functions.",
  {
    expression: z
      .string()
      .describe(
        'The expression to evaluate. Examples: "map (+1) [1,2,3]", "show (Just 42)"'
      ),
  },
  async ({ expression }) => {
    const session = await getSession();
    const result = await session.execute(expression);
    const parsed = parseEvalOutput(result.output);
    const isException = parsed.result.startsWith("*** Exception:");
    return {
      content: [
        {
          type: "text",
          text: JSON.stringify({
            success: result.success && !isException,
            output: parsed.result,
            ...(parsed.warnings.length > 0
              ? { warnings: parsed.warnings }
              : {}),
            ...(parsed.result !== parsed.raw ? { raw: parsed.raw } : {}),
          }),
        },
      ],
    };
  }
);

// --- Tool: cabal_build ---
server.tool(
  "cabal_build",
  "Run 'cabal build' to compile the project. Returns parsed GHC errors/warnings. Use for full compilation checks.",
  {
    component: z
      .string()
      .optional()
      .describe(
        'Component to build. Examples: "lib:my-package", "exe:my-package". Defaults to all.'
      ),
  },
  async ({ component }) => {
    const result = await handleBuild(projectDir, { component });
    return { content: [{ type: "text", text: result }] };
  }
);

// --- Tool: hoogle_search ---
server.tool(
  "hoogle_search",
  'Search Hoogle for Haskell functions by name or type signature. Example: search "(a -> b) -> [a] -> [b]" to find "map".',
  {
    query: z
      .string()
      .describe(
        'Search query: function name ("mapM") or type signature ("(a -> b) -> [a] -> [b]")'
      ),
    count: z
      .number()
      .optional()
      .describe("Number of results (default 10, max 30)"),
  },
  async ({ query, count }) => {
    const result = await handleHoogleSearch({ query, count });
    return { content: [{ type: "text", text: result }] };
  }
);

// --- Tool: ghci_check_module ---
server.tool(
  "ghci_check_module",
  "Load a module and return a structured summary of all its exported definitions with types. " +
    "Shows: total definitions, functions with signatures, type aliases, data types, classes, " +
    "and any compilation errors or warnings. Use for a quick overview of a module's API.",
  {
    module_path: z
      .string()
      .describe(
        'Path to the module to check. Examples: "src/HM/Infer.hs", "src/Lib.hs"'
      ),
    module_name: z
      .string()
      .optional()
      .describe(
        'Haskell module name (optional, inferred from path). Examples: "HM.Infer", "Lib"'
      ),
  },
  async ({ module_path, module_name }) => {
    const session = await getSession();
    const result = await handleCheckModule(session, {
      module_path,
      module_name,
    });
    return { content: [{ type: "text", text: result }] };
  }
);

// --- Tool: ghci_diagnostics ---
server.tool(
  "ghci_diagnostics",
  "Full diagnostic check for a Haskell module. Runs a strict compilation pass to find real type errors, " +
    "then a deferred pass to collect typed-hole information. Returns a unified report with: " +
    "errors, warnings, typed holes (with relevant bindings and suggested fits). " +
    "Use as a single-call alternative to running ghci_load + ghci_check_module + ghci_hole_fits separately.",
  {
    module_path: z
      .string()
      .describe(
        'Path to the module to diagnose. Examples: "src/HM/Infer.hs", "src/Lib.hs"'
      ),
  },
  async ({ module_path }) => {
    const session = await getSession();
    const result = await handleDiagnostics(session, { module_path }, projectDir);
    return { content: [{ type: "text", text: result }] };
  }
);

// --- Tool: ghci_hole_fits ---
server.tool(
  "ghci_hole_fits",
  "Load a module containing typed holes (_) and return structured analysis of each hole: " +
    "expected type, relevant bindings in scope, and valid hole fits that GHC suggests. " +
    "Use when exploring what expressions could fill a gap in your code.",
  {
    module_path: z
      .string()
      .describe(
        'Path to a module containing typed holes. Examples: "src/HM/Infer.hs"'
      ),
    max_fits: z
      .number()
      .optional()
      .describe(
        "Maximum number of valid hole fits to show per hole (default 10, GHC default is 6)"
      ),
  },
  async ({ module_path, max_fits }) => {
    const session = await getSession();
    const result = await handleHoleFits(session, { module_path, max_fits });
    return { content: [{ type: "text", text: result }] };
  }
);

// --- Tool: ghci_scaffold ---
server.tool(
  "ghci_scaffold",
  "Read the .cabal file, find library modules that don't have source files yet, and create minimal stubs. " +
    "Use after adding new modules to the .cabal file, before restarting GHCi. " +
    "This prevents the 'can't find source for Module' error on GHCi startup.",
  {},
  async () => {
    const result = await handleScaffold(projectDir);
    return { content: [{ type: "text", text: result }] };
  }
);

// --- Tool: ghci_session ---
server.tool(
  "ghci_session",
  "Manage the GHCi session: restart it or check its status. Use 'restart' after changing .cabal file or adding new modules.",
  {
    action: z
      .enum(["status", "restart"])
      .describe('"status" to check if GHCi is alive, "restart" to restart the session'),
  },
  async ({ action }) => {
    if (action === "status") {
      const alive = ghciSession?.isAlive() ?? false;
      return {
        content: [
          {
            type: "text",
            text: JSON.stringify({ alive, projectDir: projectDir }),
          },
        ],
      };
    }

    // restart — kill waits for the old process to fully exit
    resetQuickCheckState();
    if (ghciSession) {
      await ghciSession.kill();
      ghciSession = null;
    }
    const session = await getSession();
    return {
      content: [
        {
          type: "text",
          text: JSON.stringify({
            success: true,
            message: "GHCi session restarted",
            alive: session.isAlive(),
          }),
        },
      ],
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
    full_restart: z
      .boolean()
      .optional()
      .describe(
        "If true, exit the MCP server process (for TypeScript code reload). " +
          "Default: false (GHCi restart only)."
      ),
  },
  async ({ full_restart }) => {
    resetQuickCheckState();
    if (ghciSession) {
      await ghciSession.kill();
      ghciSession = null;
    }

    if (full_restart) {
      setTimeout(() => process.exit(0), 100);
      return {
        content: [
          {
            type: "text",
            text: JSON.stringify({
              success: true,
              message:
                "MCP server exiting for full restart. Next tool call will reconnect.",
            }),
          },
        ],
      };
    }

    // Default: GHCi-only restart, MCP server stays alive
    const session = await getSession();
    return {
      content: [
        {
          type: "text",
          text: JSON.stringify({
            success: true,
            message: "GHCi session restarted. MCP server still running.",
            alive: session.isAlive(),
          }),
        },
      ],
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
    project: z
      .string()
      .optional()
      .describe(
        "Project name to switch to. Omit to list available projects."
      ),
  },
  async ({ project }) => {
    const playgroundDir = getPlaygroundDir(BASE_DIR);
    const projects = await discoverProjects(playgroundDir);

    if (!project) {
      // List mode
      const current = projectDir;
      return {
        content: [
          {
            type: "text",
            text: JSON.stringify({
              projects: projects.map((p) => ({
                name: p.name,
                dirName: p.dirName,
                path: p.path,
                active: p.path === current,
              })),
              activeProject: projects.find((p) => p.path === current)?.name ?? "unknown",
            }),
          },
        ],
      };
    }

    // Switch mode
    const target = projects.find(
      (p) => p.name === project || p.path.endsWith(project)
    );
    if (!target) {
      return {
        content: [
          {
            type: "text",
            text: JSON.stringify({
              success: false,
              error: `Project '${project}' not found. Available: ${projects.map((p) => p.name).join(", ")}`,
            }),
          },
        ],
      };
    }

    // Kill current session and switch
    resetQuickCheckState();
    if (ghciSession) {
      await ghciSession.kill();
      ghciSession = null;
    }
    projectDir = target.path;
    const session = await getSession();

    return {
      content: [
        {
          type: "text",
          text: JSON.stringify({
            success: true,
            message: `Switched to project '${target.name}'`,
            projectDir: target.path,
            alive: session.isAlive(),
          }),
        },
      ],
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
    commands: z
      .array(z.string())
      .describe(
        'List of GHCi commands to execute. Examples: [":t map", ":t foldr", "1 + 2"]'
      ),
    reload: z
      .boolean()
      .optional()
      .describe("If true, reload modules (:r) before executing commands"),
    stop_on_error: z
      .boolean()
      .optional()
      .describe("If true, stop executing after the first failed command"),
  },
  async ({ commands, reload, stop_on_error }) => {
    const session = await getSession();
    const { results, allSuccess } = await session.executeBatch(commands, {
      reload: reload ?? false,
      stopOnError: stop_on_error ?? false,
    });
    return {
      content: [
        {
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
                ...(parsed.warnings.length > 0
                  ? { warnings: parsed.warnings }
                  : {}),
              };
            }),
          }),
        },
      ],
    };
  }
);

// --- Tool: ghci_quickcheck ---
server.tool(
  "ghci_quickcheck",
  "Run a QuickCheck property in GHCi. The property should be a Haskell expression of type `Testable prop => prop`. " +
    "Returns structured results: pass/fail, test count, counterexample if any. " +
    "Requires QuickCheck to be available as a project dependency.",
  {
    property: z
      .string()
      .describe(
        'QuickCheck property expression. Examples: "\\xs -> reverse (reverse xs) == (xs :: [Int])", ' +
          '"\\x -> x + 0 == (x :: Int)"'
      ),
    tests: z
      .number()
      .optional()
      .describe("Number of tests to run (default 100)"),
    verbose: z
      .boolean()
      .optional()
      .describe("If true, print each test case (default false)"),
  },
  async ({ property, tests, verbose }) => {
    const session = await getSession();
    const result = await handleQuickCheck(session, { property, tests, verbose });
    return { content: [{ type: "text", text: result }] };
  }
);

// --- MCP Resources: Haskell Rules ---
for (const rule of RULES_REGISTRY) {
  server.registerResource(rule.name, rule.uri, {
    description: rule.description,
    mimeType: "text/markdown",
  }, async (uri) => ({
    contents: [
      {
        uri: uri.toString(),
        text: await loadRule(rule),
        mimeType: "text/markdown",
      },
    ],
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
