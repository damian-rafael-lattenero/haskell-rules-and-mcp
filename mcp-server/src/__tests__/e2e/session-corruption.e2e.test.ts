import { describe, it, expect, beforeAll, afterAll } from "vitest";
import { Client } from "@modelcontextprotocol/sdk/client/index.js";
import { StdioClientTransport } from "@modelcontextprotocol/sdk/client/stdio.js";
import path from "node:path";

const TEST_PROJECT = path.resolve(import.meta.dirname, "..", "fixtures", "test-project");

describe("Session Corruption E2E", () => {
  let client: Client;
  let transport: StdioClientTransport;

  beforeAll(async () => {
    transport = new StdioClientTransport({
      command: "node",
      args: [path.resolve(import.meta.dirname, "..", "..", "..", "dist", "index.js")],
      env: {
        ...process.env,
        HASKELL_PROJECT_DIR: TEST_PROJECT,
      },
    });

    client = new Client(
      { name: "test-client", version: "1.0.0" },
      { capabilities: {} }
    );

    await client.connect(transport);
  }, 30000);

  afterAll(async () => {
    await client.close();
  });

  async function callTool(name: string, args: Record<string, unknown>) {
    const response = await client.request(
      { method: "tools/call", params: { name, arguments: args } },
      { timeout: 60000 }
    );
    return response;
  }

  it("handles timeout → corruption → restart flow", async () => {
    // Execute a normal command first
    const result1 = await callTool("ghci_type", { expression: "map" });
    expect(result1.content[0].text).toContain("map");
    
    // Cause a timeout with a very short timeout
    try {
      await callTool("ghci_eval", {
        expression: "let loop = loop in loop",
        timeout_ms: 100
      });
      expect.fail("Should have thrown timeout error");
    } catch (error: any) {
      expect(error.message || error.toString()).toMatch(/timeout|timed out/i);
    }
    
    // Wait a bit for recovery
    await new Promise(r => setTimeout(r, 500));
    
    // Next command should auto-recover and work
    const result2 = await callTool("ghci_type", { expression: "foldr" });
    expect(result2.content[0].text).toContain("foldr");
  }, 30000);

  it("rejects dangerous batch commands", async () => {
    try {
      await callTool("ghci_batch", {
        commands: [":t map", ":set +m", ":t foldr"]
      });
      expect.fail("Should have rejected dangerous command");
    } catch (error: any) {
      expect(error.message || error.toString()).toMatch(/Dangerous GHCi command/i);
    }
    
    // Session should still work after rejection
    const result = await callTool("ghci_type", { expression: "map" });
    expect(result.content[0].text).toContain("map");
  });

  it("batch execution works normally with safe commands", async () => {
    const result = await callTool("ghci_batch", {
      commands: [":t map", ":t foldr", "1 + 1"]
    });
    
    const data = JSON.parse(result.content[0].text);
    expect(data.allSuccess).toBe(true);
    expect(data.results).toHaveLength(3);
  });
});
