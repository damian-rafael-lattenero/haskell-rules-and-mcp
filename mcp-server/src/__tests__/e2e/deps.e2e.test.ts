/**
 * E2E: ghci_deps tool via MCP protocol.
 * Tests add/remove/list roundtrip against the real MCP server.
 * Restores cabal file in afterAll to keep fixture pristine.
 */
import { describe, it, expect, beforeAll, afterAll } from "vitest";
import { Client } from "@modelcontextprotocol/sdk/client/index.js";
import { StdioClientTransport } from "@modelcontextprotocol/sdk/client/stdio.js";
import { execSync } from "node:child_process";
import { readFile, writeFile } from "node:fs/promises";
import path from "node:path";
import { setupIsolatedFixture, type IsolatedFixture } from "../helpers/isolated-fixture.js";

const SERVER_SCRIPT = path.resolve(import.meta.dirname, "../../../dist/index.js");
const GHCUP_BIN = path.join(process.env.HOME ?? "", ".ghcup", "bin");
const TEST_PATH = `${GHCUP_BIN}:${process.env.PATH}`;

const GHC_AVAILABLE = (() => {
  try {
    execSync("ghc --version", { stdio: "pipe", env: { ...process.env, PATH: TEST_PATH } });
    return true;
  } catch { return false; }
})();

function callTool(client: Client, name: string, args: Record<string, unknown> = {}) {
  return client.callTool({ name, arguments: args });
}

function parseResult(result: Awaited<ReturnType<Client["callTool"]>>): any {
  const text = (result.content as Array<{ type: string; text: string }>)[0]!.text;
  return JSON.parse(text);
}

describe.runIf(GHC_AVAILABLE)("E2E: ghci_deps", () => {
  let client: Client;
  let transport: StdioClientTransport;
  let originalCabal: string;
  let fixture: IsolatedFixture;
  let FIXTURE_DIR: string;
  let CABAL_FILE: string;

  beforeAll(async () => {
    fixture = await setupIsolatedFixture("test-project", "deps-e2e");
    FIXTURE_DIR = fixture.dir;
    CABAL_FILE = path.join(FIXTURE_DIR, "test-project.cabal");
    originalCabal = await readFile(CABAL_FILE, "utf-8");

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
    client = new Client({ name: "deps-e2e-client", version: "0.1.0" }, { capabilities: {} });
    await client.connect(transport);
  }, 60_000);

  afterAll(async () => {
    // Restore original cabal file
    await writeFile(CABAL_FILE, originalCabal, "utf-8");
    try { await client.close(); } catch { /* ignore */ }
    await fixture.cleanup();
  });

  it("ghci_deps appears in listTools()", async () => {
    const result = await client.listTools();
    const names = result.tools.map((t) => t.name);
    expect(names).toContain("ghci_deps");
  });

  it("list returns base and QuickCheck from fixture", async () => {
    const result = parseResult(await callTool(client, "ghci_deps", { action: "list" }));
    expect(result.success).toBe(true);
    const names = result.dependencies.map((d: { name: string }) => d.name);
    expect(names).toContain("base");
    expect(names).toContain("QuickCheck");
  });

  it("add + list roundtrip: containers appears after add", async () => {
    const addResult = parseResult(
      await callTool(client, "ghci_deps", { action: "add", package: "containers" })
    );
    expect(addResult.success).toBe(true);

    const listResult = parseResult(await callTool(client, "ghci_deps", { action: "list" }));
    const names = listResult.dependencies.map((d: { name: string }) => d.name);
    expect(names).toContain("containers");
  });

  it("remove: containers absent after remove", async () => {
    const removeResult = parseResult(
      await callTool(client, "ghci_deps", { action: "remove", package: "containers" })
    );
    expect(removeResult.success).toBe(true);

    const listResult = parseResult(await callTool(client, "ghci_deps", { action: "list" }));
    const names = listResult.dependencies.map((d: { name: string }) => d.name);
    expect(names).not.toContain("containers");
  });

  it("add already-present returns already_present", async () => {
    const result = parseResult(
      await callTool(client, "ghci_deps", { action: "add", package: "base" })
    );
    expect(result.success).toBe(true);
    expect(result.status).toBe("already_present");
  });

  it("graph returns nodes and edges for the fixture project", async () => {
    const result = parseResult(
      await callTool(client, "ghci_deps", { action: "graph" })
    );
    expect(result.success).toBe(true);
    expect(Array.isArray(result.nodes)).toBe(true);
    expect(Array.isArray(result.edges)).toBe(true);
    expect(Array.isArray(result.cycles)).toBe(true);
    expect(Array.isArray(result.orphans)).toBe(true);
    // fixture has TestLib module
    expect(result.nodes).toContain("TestLib");
  });

  it("graph has no cycles in a clean project", async () => {
    const result = parseResult(
      await callTool(client, "ghci_deps", { action: "graph" })
    );
    expect(result.success).toBe(true);
    // A clean fixture project should have no circular imports
    expect(result.cycles).toHaveLength(0);
  });
});
