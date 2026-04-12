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
});
