import { describe, it, expect, beforeAll, afterAll } from "vitest";
import { Client } from "@modelcontextprotocol/sdk/client/index.js";
import { StdioClientTransport } from "@modelcontextprotocol/sdk/client/stdio.js";
import { execSync } from "node:child_process";
import path from "node:path";
import { setupIsolatedFixture, type IsolatedFixture } from "../helpers/isolated-fixture.js";

const SERVER_SCRIPT = path.resolve(
  import.meta.dirname,
  "../../../dist/index.js"
);

// Extend PATH to include ghcup bin for GHC detection
const GHCUP_BIN = path.join(process.env.HOME ?? "", ".ghcup", "bin");
const TEST_PATH = `${GHCUP_BIN}:${process.env.PATH}`;

const GHC_AVAILABLE = (() => {
  try {
    execSync("ghc --version", {
      stdio: "pipe",
      env: { ...process.env, PATH: TEST_PATH },
    });
    return true;
  } catch {
    return false;
  }
})();

describe.runIf(GHC_AVAILABLE)("MCP Protocol E2E", () => {
  let client: Client;
  let transport: StdioClientTransport;
  let fixture: IsolatedFixture;
  let FIXTURE_DIR: string;

  beforeAll(async () => {
    fixture = await setupIsolatedFixture("test-project", "mcp-protocol");
    FIXTURE_DIR = fixture.dir;
    transport = new StdioClientTransport({
      command: "node",
      args: [SERVER_SCRIPT],
      env: {
        ...process.env,
        PATH: TEST_PATH,
        HASKELL_PROJECT_DIR: FIXTURE_DIR,
        HASKELL_LIBRARY_TARGET: "lib:test-project",
      },
    });
    client = new Client(
      { name: "test-client", version: "0.1.0" },
      { capabilities: {} }
    );
    await client.connect(transport);
  }, 60_000);

  afterAll(async () => {
    try {
      await client.close();
    } catch {
      // Ignore close errors
    }
    await fixture.cleanup();
  });

  it("lists available tools", async () => {
    const result = await client.listTools();
    const names = result.tools.map((t) => t.name);
    expect(names).toContain("ghci_type");
    expect(names).toContain("ghci_load");
    expect(names).toContain("ghci_quickcheck");
    expect(names).toContain("ghci_session");
    expect(names).toContain("ghci_switch_project");
    // mcp_restart was removed in favor of ghci_session(action="restart").
    // Keep this negative assertion so a future accidental re-add is caught.
    expect(names).not.toContain("mcp_restart");
    expect(names.length).toBeGreaterThanOrEqual(15);
  });

  it("calls ghci_session status", async () => {
    const result = await client.callTool({
      name: "ghci_session",
      arguments: { action: "status" },
    });
    const text = (result.content as Array<{ type: string; text: string }>)[0]!
      .text;
    const parsed = JSON.parse(text);
    expect(parsed).toHaveProperty("alive");
    expect(parsed).toHaveProperty("projectDir");
    expect(parsed.projectDir).toContain("test-project");
  });

  it("calls ghci_session stats", async () => {
    const result = await client.callTool({
      name: "ghci_session",
      arguments: { action: "stats" },
    });
    const parsed = JSON.parse((result.content as Array<{ type: string; text: string }>)[0]!.text);
    expect(parsed.success).toBe(true);
    expect(parsed).toHaveProperty("modulesTracked");
    expect(parsed).toHaveProperty("recentTools");
  });

  it("calls ghci_type on a function", async () => {
    const result = await client.callTool({
      name: "ghci_type",
      arguments: { expression: "add" },
    });
    const text = (result.content as Array<{ type: string; text: string }>)[0]!
      .text;
    const parsed = JSON.parse(text);
    expect(parsed.success).toBe(true);
    expect(parsed.type).toContain("Int");
  });

  it("calls ghci_load on a module", async () => {
    const result = await client.callTool({
      name: "ghci_load",
      arguments: { module_path: "src/TestLib.hs" },
    });
    const text = (result.content as Array<{ type: string; text: string }>)[0]!
      .text;
    const parsed = JSON.parse(text);
    expect(parsed.success).toBe(true);
  });

  it("calls ghci_eval", async () => {
    const result = await client.callTool({
      name: "ghci_eval",
      arguments: { expression: "add 10 20" },
    });
    const text = (result.content as Array<{ type: string; text: string }>)[0]!
      .text;
    const parsed = JSON.parse(text);
    expect(parsed.success).toBe(true);
    expect(parsed.output).toContain("30");
  });

  it("ghci_eval supports timeout_ms", async () => {
    const result = await client.callTool({
      name: "ghci_eval",
      arguments: { expression: "let loop = loop in loop", timeout_ms: 200 },
    });
    const parsed = JSON.parse((result.content as Array<{ type: string; text: string }>)[0]!.text);
    expect(parsed.success).toBe(false);
    expect(parsed.error).toContain("timeout");
  });

  it("calls ghci_switch_project in list mode", async () => {
    const result = await client.callTool({
      name: "ghci_switch_project",
      arguments: {},
    });
    const text = (result.content as Array<{ type: string; text: string }>)[0]!
      .text;
    const parsed = JSON.parse(text);
    expect(parsed).toHaveProperty("projects");
    // May be empty array since test-project is set via env, not discovered from subdirectories
    expect(parsed).toHaveProperty("activeProject");
  });

  // --- ghci_info ---
  it("calls ghci_info on a data type", async () => {
    const result = await client.callTool({
      name: "ghci_info",
      arguments: { name: "Maybe" },
    });
    const parsed = JSON.parse((result.content as any)[0].text);
    expect(parsed.success).toBe(true);
    expect(parsed.definition).toContain("Nothing");
  });

  // --- ghci_kind ---
  it("calls ghci_kind on a type", async () => {
    const result = await client.callTool({
      name: "ghci_kind",
      arguments: { type_expression: "Maybe" },
    });
    const parsed = JSON.parse((result.content as any)[0].text);
    expect(parsed.success).toBe(true);
    expect(parsed.output).toContain("* -> *");
  });

  // --- ghci_batch ---
  it("calls ghci_batch with multiple commands", async () => {
    const result = await client.callTool({
      name: "ghci_batch",
      arguments: { commands: [":t add", "add 1 2"] },
    });
    const parsed = JSON.parse((result.content as any)[0].text);
    expect(parsed.allSuccess).toBe(true);
    expect(parsed.count).toBe(2);
    expect(parsed.results[0].output).toContain("Int");
    expect(parsed.results[1].output).toContain("3");
  });

  // --- ghci_eval with exception (Bug Fix 2) ---
  it("marks runtime exception as failure", async () => {
    const result = await client.callTool({
      name: "ghci_eval",
      arguments: { expression: "head ([] :: [Int])" },
    });
    const parsed = JSON.parse((result.content as any)[0].text);
    expect(parsed.success).toBe(false);
    expect(parsed.output).toContain("Exception");
  });

  // --- ghci_check_module ---
  it("calls ghci_check_module", async () => {
    const result = await client.callTool({
      name: "ghci_check_module",
      arguments: { module_path: "src/TestLib.hs" },
    });
    const parsed = JSON.parse((result.content as any)[0].text);
    expect(parsed.success).toBe(true);
    expect(parsed.definitions.length).toBeGreaterThan(0);
  });

  // --- ghci_quickcheck ---
  it("calls ghci_quickcheck with a passing property", async () => {
    const result = await client.callTool({
      name: "ghci_quickcheck",
      arguments: { property: "\\x y -> add x y == x + (y :: Int)" },
    });
    const parsed = JSON.parse((result.content as any)[0].text);
    expect(parsed.success).toBe(true);
    expect(parsed.passed).toBe(100);
  });

  it("ghci_quickcheck accepts module_path as alias for module", async () => {
    const result = await client.callTool({
      name: "ghci_quickcheck",
      arguments: {
        property: "\\x y -> add x y == x + (y :: Int)",
        module_path: "src/TestLib.hs",
      },
    });
    const parsed = JSON.parse((result.content as any)[0].text);
    expect(parsed.success).toBe(true);
    expect(parsed.passed).toBe(100);
  });

  it("ghci_quickcheck schema lists module_path parameter", async () => {
    const result = await client.listTools();
    const qcTool = result.tools.find((t) => t.name === "ghci_quickcheck");
    expect(qcTool).toBeDefined();
    const paramNames = Object.keys(qcTool?.inputSchema?.properties ?? {});
    expect(paramNames).toContain("module_path");
    expect(paramNames).toContain("module");
  });

  // --- ghci_load with diagnostics ---
  it("calls ghci_load with diagnostics", async () => {
    const result = await client.callTool({
      name: "ghci_load",
      arguments: { module_path: "src/TestLib.hs", diagnostics: true },
    });
    const parsed = JSON.parse((result.content as any)[0].text);
    expect(parsed.success).toBe(true);
    expect(parsed).toHaveProperty("warningActions");
    expect(parsed).toHaveProperty("holes");
  });

  // --- ghci_session restart ---
  it("can restart session and continue working", async () => {
    const restartResult = await client.callTool({
      name: "ghci_session",
      arguments: { action: "restart" },
    });
    const restartParsed = JSON.parse((restartResult.content as any)[0].text);
    expect(restartParsed.success).toBe(true);

    // After restart, tools should still work
    const typeResult = await client.callTool({
      name: "ghci_type",
      arguments: { expression: "add" },
    });
    const typeParsed = JSON.parse((typeResult.content as any)[0].text);
    expect(typeParsed.success).toBe(true);
  });

  // --- ghci_goto ---
  it("calls ghci_goto on local function", async () => {
    const result = await client.callTool({ name: "ghci_goto", arguments: { name: "add" } });
    const parsed = JSON.parse((result.content as any)[0].text);
    expect(parsed.success).toBe(true);
    expect(parsed.location).toBeDefined();
  });

  // --- ghci_complete ---
  it("calls ghci_complete", async () => {
    const result = await client.callTool({ name: "ghci_complete", arguments: { prefix: "ad" } });
    const parsed = JSON.parse((result.content as any)[0].text);
    expect(parsed.success).toBe(true);
    expect(parsed.completions).toContain("add");
  });

  // --- ghci_doc ---
  it("calls ghci_doc", async () => {
    const result = await client.callTool({ name: "ghci_doc", arguments: { name: "map" } });
    const parsed = JSON.parse((result.content as any)[0].text);
    expect(parsed.success).toBe(true);
    expect(parsed.name).toBe("map");
  });

  // --- ghci_imports ---
  it("calls ghci_imports", async () => {
    const result = await client.callTool({ name: "ghci_imports", arguments: {} });
    const parsed = JSON.parse((result.content as any)[0].text);
    expect(parsed.success).toBe(true);
    expect(parsed).toHaveProperty("imports");
  });

  // --- ghci_references ---
  it("calls ghci_references", async () => {
    const result = await client.callTool({ name: "ghci_references", arguments: { name: "add" } });
    const parsed = JSON.parse((result.content as any)[0].text);
    expect(parsed.success).toBe(true);
    expect(parsed.count).toBeGreaterThan(0);
  });

  // --- ghci_rename ---
  it("calls ghci_rename preview", async () => {
    const result = await client.callTool({ name: "ghci_rename", arguments: { old_name: "add", new_name: "addInts" } });
    const parsed = JSON.parse((result.content as any)[0].text);
    expect(parsed.success).toBe(true);
    expect(parsed.totalReferences).toBeGreaterThan(0);
  });

  // --- ghci_session (no mode) ---
  it("ghci_session status has no mode fields", async () => {
    const result = await client.callTool({ name: "ghci_session", arguments: { action: "status" } });
    const parsed = JSON.parse((result.content as any)[0].text);
    expect(parsed._modeSelection).toBeUndefined();
    expect(parsed.mode).toBeUndefined();
  });

  // --- ghci_workflow help action ---
  it("ghci_workflow action:help returns suggested_tools and reasoning", async () => {
    const result = await client.callTool({
      name: "ghci_workflow",
      arguments: { action: "help" },
    });
    const parsed = JSON.parse((result.content as any)[0].text);
    expect(Array.isArray(parsed.suggested_tools)).toBe(true);
    expect(parsed.suggested_tools.length).toBeGreaterThan(0);
    expect(typeof parsed.reasoning).toBe("string");
    expect(parsed.reasoning.length).toBeGreaterThan(0);
    expect(Array.isArray(parsed.steps)).toBe(true);
    expect(parsed.steps.length).toBeGreaterThanOrEqual(2);
  });

  // --- Stack support ---
  // --- ghci_hls ---
  it("ghci_hls appears in listTools()", async () => {
    const result = await client.listTools();
    const names = result.tools.map((t) => t.name);
    expect(names).toContain("ghci_hls");
  });

  it("ghci_hls available action always returns a response (no crash)", async () => {
    const result = await client.callTool({
      name: "ghci_hls",
      arguments: { action: "available" },
    });
    const parsed = JSON.parse((result.content as any)[0].text);
    // Must have success:true and available:boolean regardless of HLS installation
    expect(parsed.success).toBe(true);
    expect(typeof parsed.available).toBe("boolean");
  });


  // --- ghci_create_project (replaces ghci_init) ---
  it("ghci_create_project schema exposes expected params", async () => {
    const result = await client.listTools();
    const tool = result.tools.find((t) => t.name === "ghci_create_project");
    expect(tool).toBeDefined();
    const paramNames = Object.keys(tool?.inputSchema?.properties ?? {});
    expect(paramNames).toContain("name");
    expect(paramNames).toContain("modules");
    expect(paramNames).toContain("with_test_suite");
    expect(paramNames).toContain("switch_to_it");
    expect((tool?.inputSchema as { required?: string[] })?.required).toContain("name");
  });

  it("ghci_workflow help: suggested_tools are valid tool names in listTools()", async () => {
    const listResult = await client.listTools();
    const allNames = new Set(listResult.tools.map((t) => t.name));

    const helpResult = await client.callTool({
      name: "ghci_workflow",
      arguments: { action: "help" },
    });
    const parsed = JSON.parse((helpResult.content as any)[0].text);
    for (const toolName of parsed.suggested_tools) {
      expect(allNames.has(toolName), `"${toolName}" not in tool list`).toBe(true);
    }
  });

  // --- multi-package: ghci_deps with cabal.project awareness ---
  it("ghci_deps list works correctly on the test fixture", async () => {
    const result = await client.callTool({
      name: "ghci_deps",
      arguments: { action: "list" },
    });
    const parsed = JSON.parse((result.content as any)[0].text);
    expect(parsed.success).toBe(true);
    expect(Array.isArray(parsed.dependencies)).toBe(true);
  });

  // --- tool listing (all new tools included) ---
  it("lists tools without ghci_mode", async () => {
    const result = await client.listTools();
    const names = result.tools.map((t) => t.name);
    expect(names).not.toContain("ghci_mode");
    expect(names).toContain("ghci_goto");
    expect(names).toContain("ghci_complete");
    expect(names).toContain("ghci_doc");
    expect(names).toContain("ghci_imports");
    expect(names).toContain("ghci_format");
    expect(names).toContain("ghci_lint");
    expect(names).toContain("ghci_add_import");
    expect(names).toContain("ghci_references");
    expect(names).toContain("ghci_rename");
    // New tools from improvements
    expect(names).toContain("ghci_deps");
    expect(names).toContain("ghci_hole");
    expect(names).toContain("ghci_refactor");
    // `ghci_flags` and `ghci_profile` were removed in Fase 2.
    expect(names).toContain("ghci_hls");
    expect(names).toContain("ghci_create_project");
    expect(names).toContain("ghci_add_modules");
    expect(names.length).toBeGreaterThanOrEqual(30);
  });

  // --- Resources ---
  it("lists resources including rules", async () => {
    const result = await client.listResources();
    const uris = result.resources.map(r => r.uri);
    expect(uris).toContain("rules://haskell/mcp-workflow");
    expect(uris).toContain("rules://haskell/project-conventions");
  });

  it("reads mcp-workflow rule resource", async () => {
    const result = await client.readResource({ uri: "rules://haskell/mcp-workflow" });
    const text = (result.contents[0] as any).text;
    expect(text).toContain("PRIME DIRECTIVE");
  });

  // --- ghci_create_project schema includes with_test_suite (replaces old ghci_init test_suite) ---
  it("ghci_create_project schema includes with_test_suite param", async () => {
    const result = await client.listTools();
    const tool = result.tools.find((t) => t.name === "ghci_create_project");
    expect(tool).toBeDefined();
    const paramNames = Object.keys(tool?.inputSchema?.properties ?? {});
    expect(paramNames).toContain("with_test_suite");
  });

  // --- New: ghci_quickcheck schema includes tests_module param ---
  it("ghci_quickcheck schema includes tests_module param", async () => {
    const result = await client.listTools();
    const qcTool = result.tools.find((t) => t.name === "ghci_quickcheck");
    expect(qcTool).toBeDefined();
    const paramNames = Object.keys(qcTool?.inputSchema?.properties ?? {});
    expect(paramNames).toContain("tests_module");
  });

  // --- New: ghci_quickcheck_batch schema includes tests_module param ---
  it("ghci_quickcheck_batch schema includes tests_module param", async () => {
    const result = await client.listTools();
    const batchTool = result.tools.find((t) => t.name === "ghci_quickcheck_batch");
    expect(batchTool).toBeDefined();
    const paramNames = Object.keys(batchTool?.inputSchema?.properties ?? {});
    expect(paramNames).toContain("tests_module");
  });

  // --- New: ghci_regression save alias returns explanatory message ---
  it("ghci_regression action=save returns explanatory message", async () => {
    const result = await client.callTool({
      name: "ghci_regression",
      arguments: { action: "save" },
    });
    const parsed = JSON.parse((result.content as any)[0].text);
    expect(parsed.saved).toBe(false);
    expect(parsed.message).toContain("auto-saved");
    expect(parsed.tip).toContain("tests_module");
  });

  // --- New: ghci_load raw does not contain GHC-32850 ---
  it("ghci_load raw output does not contain GHC-32850 when loading single module", async () => {
    const result = await client.callTool({
      name: "ghci_load",
      arguments: { module_path: "src/TestModule.hs", diagnostics: true },
    });
    const parsed = JSON.parse((result.content as any)[0].text);
    const raw: string = parsed.raw ?? "";
    expect(raw).not.toContain("GHC-32850");
    expect(raw).not.toContain("-Wmissing-home-modules");
  });
});
