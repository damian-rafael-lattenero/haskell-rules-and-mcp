/**
 * E2E: ghci_refactor tool via MCP protocol.
 * Writes a temporary module, renames a binding, verifies it compiles with ghci_load.
 */
import { describe, it, expect, beforeAll, afterAll } from "vitest";
import { Client } from "@modelcontextprotocol/sdk/client/index.js";
import { StdioClientTransport } from "@modelcontextprotocol/sdk/client/stdio.js";
import { execSync } from "node:child_process";
import { writeFile, unlink, readFile } from "node:fs/promises";
import path from "node:path";

const FIXTURE_DIR = path.resolve(import.meta.dirname, "../fixtures/test-project");
const SERVER_SCRIPT = path.resolve(import.meta.dirname, "../../../dist/index.js");
const CABAL_FILE = path.join(FIXTURE_DIR, "test-project.cabal");
const REFACTOR_MODULE = path.join(FIXTURE_DIR, "src", "RefactorTest.hs");
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
  return JSON.parse((result.content as Array<{ type: string; text: string }>)[0]!.text);
}

describe.runIf(GHC_AVAILABLE)("E2E: ghci_refactor", () => {
  let client: Client;
  let transport: StdioClientTransport;
  let originalCabal: string;

  beforeAll(async () => {
    originalCabal = await readFile(CABAL_FILE, "utf-8");

    // Add RefactorTest to exposed-modules
    const updatedCabal = originalCabal.replace(
      "exposed-modules:  TestLib",
      "exposed-modules:  TestLib\n                  RefactorTest"
    );
    await writeFile(CABAL_FILE, updatedCabal, "utf-8");

    // Create the refactor test module
    await writeFile(
      REFACTOR_MODULE,
      `module RefactorTest where

oldName :: Int -> Int
oldName x = x + 1

useOldName :: Int -> Int
useOldName n = oldName (oldName n)
`,
      "utf-8"
    );

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
    client = new Client({ name: "refactor-e2e-client", version: "0.1.0" }, { capabilities: {} });
    await client.connect(transport);
    await callTool(client, "ghci_session", { action: "restart" });
  }, 60_000);

  afterAll(async () => {
    try { await unlink(REFACTOR_MODULE); } catch { /* ignore */ }
    await writeFile(CABAL_FILE, originalCabal, "utf-8");
    try { await client.close(); } catch { /* ignore */ }
  });

  it("ghci_refactor appears in listTools()", async () => {
    const result = await client.listTools();
    const names = result.tools.map((t) => t.name);
    expect(names).toContain("ghci_refactor");
  });

  it("rename_local renames binding and module still compiles (apply=true)", async () => {
    // Rename oldName → increment. Fase 4 changed the default to preview —
    // callers must now opt in to mutation with apply=true.
    const refactorResult = parseResult(
      await callTool(client, "ghci_refactor", {
        action: "rename_local",
        module_path: "src/RefactorTest.hs",
        old_name: "oldName",
        new_name: "increment",
        apply: true,
      })
    );
    expect(refactorResult.success).toBe(true);
    expect(refactorResult.applied).toBe(true);
    expect(refactorResult.changed).toBeGreaterThan(0);
    expect(refactorResult.diff.length).toBeGreaterThan(0);

    // Verify the file was modified
    const content = await readFile(REFACTOR_MODULE, "utf-8");
    expect(content).toContain("increment");
    expect(content).not.toContain("oldName");

    // Verify it still compiles
    await callTool(client, "ghci_session", { action: "restart" });
    const loadResult = parseResult(
      await callTool(client, "ghci_load", {
        module_path: "src/RefactorTest.hs",
        diagnostics: true,
      })
    );
    expect(loadResult.success).toBe(true);
    expect(loadResult.errors).toHaveLength(0);
  });

  it("rename_local with non-existent name returns changed:0", async () => {
    const result = parseResult(
      await callTool(client, "ghci_refactor", {
        action: "rename_local",
        module_path: "src/RefactorTest.hs",
        old_name: "doesNotExist",
        new_name: "anything",
      })
    );
    expect(result.success).toBe(true);
    expect(result.changed).toBe(0);
  });
});
