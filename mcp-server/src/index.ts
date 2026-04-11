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

// Project directory is the parent of mcp-server/
const PROJECT_DIR =
  process.env.HASKELL_PROJECT_DIR ??
  path.resolve(import.meta.dirname, "..", "..");

const server = new McpServer({
  name: "haskell-ghci",
  version: "0.1.0",
});

let ghciSession: GhciSession | null = null;

async function getSession(): Promise<GhciSession> {
  if (ghciSession?.isAlive()) {
    return ghciSession;
  }
  const session = new GhciSession(PROJECT_DIR);
  ghciSession = session;
  await session.start();
  session.on("exit", () => {
    // Only nullify if this is still the active session —
    // a restart may have already replaced ghciSession with a new instance.
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
  },
  async ({ module_path, load_all }) => {
    const session = await getSession();
    const result = await handleLoadModule(session, { module_path, load_all }, PROJECT_DIR);
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

// --- Tool: cabal_build ---
server.tool(
  "cabal_build",
  "Run 'cabal build' to compile the project. Returns parsed GHC errors/warnings. Use for full compilation checks.",
  {
    component: z
      .string()
      .optional()
      .describe(
        'Component to build. Examples: "lib:haskell-rules-and-mcp", "exe:haskell-rules-and-mcp". Defaults to all.'
      ),
  },
  async ({ component }) => {
    const result = await handleBuild(PROJECT_DIR, { component });
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

// --- Tool: ghci_scaffold ---
server.tool(
  "ghci_scaffold",
  "Read the .cabal file, find library modules that don't have source files yet, and create minimal stubs. " +
    "Use after adding new modules to the .cabal file, before restarting GHCi. " +
    "This prevents the 'can't find source for Module' error on GHCi startup.",
  {},
  async () => {
    const result = await handleScaffold(PROJECT_DIR);
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
            text: JSON.stringify({ alive, projectDir: PROJECT_DIR }),
          },
        ],
      };
    }

    // restart — kill waits for the old process to fully exit
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
            results: results.map((r, i) => ({
              command: commands[i],
              success: r.success,
              output: r.output,
            })),
          }),
        },
      ],
    };
  }
);

// --- Start the server ---
async function main() {
  const transport = new StdioServerTransport();
  await server.connect(transport);
  console.error(`haskell-ghci MCP server running (project: ${PROJECT_DIR})`);
}

main().catch((err) => {
  console.error("Fatal error:", err);
  process.exit(1);
});
