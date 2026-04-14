/**
 * E2E: ghci_profile tool via MCP protocol.
 * Tests the suggest action (static analysis — no GHC needed).
 */
import { describe, it, expect, beforeAll, afterAll } from "vitest";
import { Client } from "@modelcontextprotocol/sdk/client/index.js";
import { StdioClientTransport } from "@modelcontextprotocol/sdk/client/stdio.js";
import { execSync } from "node:child_process";
import path from "node:path";

const FIXTURE_DIR = path.resolve(import.meta.dirname, "../fixtures/test-project");
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
  return JSON.parse((result.content as Array<{ type: string; text: string }>)[0]!.text);
}

describe.runIf(GHC_AVAILABLE)("E2E: ghci_profile", () => {
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
    client = new Client({ name: "profile-e2e-client", version: "0.1.0" }, { capabilities: {} });
    await client.connect(transport);
  }, 60_000);

  afterAll(async () => {
    try { await client.close(); } catch { /* ignore */ }
  });

  it("ghci_profile appears in listTools()", async () => {
    const result = await client.listTools();
    const names = result.tools.map((t) => t.name);
    expect(names).toContain("ghci_profile");
  });

  it("suggest action on TestLib returns suggestions array", async () => {
    const result = parseResult(
      await callTool(client, "ghci_profile", {
        action: "suggest",
        module_path: "src/TestLib.hs",
      })
    );
    expect(result.success).toBe(true);
    expect(Array.isArray(result.suggestions)).toBe(true);
    expect(typeof result.summary).toBe("string");
  });

  it("suggest returns proper structure for each suggestion", async () => {
    const result = parseResult(
      await callTool(client, "ghci_profile", {
        action: "suggest",
        module_path: "src/TestLib.hs",
      })
    );
    for (const s of result.suggestions) {
      expect(typeof s.line).toBe("number");
      expect(typeof s.issue).toBe("string");
      expect(typeof s.suggestion).toBe("string");
    }
  });

  it("suggest on non-existent module returns error", async () => {
    const result = parseResult(
      await callTool(client, "ghci_profile", {
        action: "suggest",
        module_path: "src/DoesNotExist.hs",
      })
    );
    expect(result.success).toBe(false);
    expect(result.error).toBeDefined();
  });
});
