import { describe, it, expect, beforeAll, afterAll } from "vitest";
import { Client } from "@modelcontextprotocol/sdk/client/index.js";
import { StdioClientTransport } from "@modelcontextprotocol/sdk/client/stdio.js";
import { execSync } from "node:child_process";
import path from "node:path";

const FIXTURE_DIR = path.resolve(
  import.meta.dirname,
  "../fixtures/test-project"
);
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

  beforeAll(async () => {
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
  });

  it("lists available tools", async () => {
    const result = await client.listTools();
    const names = result.tools.map((t) => t.name);
    expect(names).toContain("ghci_type");
    expect(names).toContain("ghci_load");
    expect(names).toContain("ghci_quickcheck");
    expect(names).toContain("ghci_session");
    expect(names).toContain("ghci_switch_project");
    expect(names).toContain("mcp_restart");
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

  it("calls ghci_switch_project in list mode", async () => {
    const result = await client.callTool({
      name: "ghci_switch_project",
      arguments: {},
    });
    const text = (result.content as Array<{ type: string; text: string }>)[0]!
      .text;
    const parsed = JSON.parse(text);
    expect(parsed).toHaveProperty("projects");
    // May be empty array since test-project is set via env, not in playground/
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

  // --- tool listing updated ---
  it("lists all 25 tools", async () => {
    const result = await client.listTools();
    const names = result.tools.map((t) => t.name);
    expect(names).toContain("ghci_goto");
    expect(names).toContain("ghci_complete");
    expect(names).toContain("ghci_doc");
    expect(names).toContain("ghci_imports");
    expect(names).toContain("ghci_format");
    expect(names).toContain("ghci_lint");
    expect(names).toContain("ghci_add_import");
    expect(names).toContain("ghci_references");
    expect(names).toContain("ghci_rename");
    expect(names.length).toBeGreaterThanOrEqual(25);
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
});
