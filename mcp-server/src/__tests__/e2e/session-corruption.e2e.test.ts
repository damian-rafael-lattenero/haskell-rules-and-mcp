import { describe, it, expect, beforeAll, afterAll } from "vitest";
import { Client } from "@modelcontextprotocol/sdk/client/index.js";
import { StdioClientTransport } from "@modelcontextprotocol/sdk/client/stdio.js";
import path from "node:path";
import { setupIsolatedFixture, type IsolatedFixture } from "../helpers/isolated-fixture.js";

describe("Session Corruption E2E", () => {
  let client: Client;
  let transport: StdioClientTransport;
  let fixture: IsolatedFixture;

  beforeAll(async () => {
    fixture = await setupIsolatedFixture("test-project", "session-corruption");
    transport = new StdioClientTransport({
      command: "node",
      args: [path.resolve(import.meta.dirname, "..", "..", "..", "dist", "index.js")],
      env: {
        ...process.env,
        HASKELL_PROJECT_DIR: fixture.dir,
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
    await fixture.cleanup();
  });

  async function callTool(name: string, args: Record<string, unknown>) {
    return client.callTool({ name, arguments: args });
  }

  function parseResult(result: Awaited<ReturnType<Client["callTool"]>>): any {
    const text = (result.content as Array<{ type: string; text: string }>)[0]!.text;
    try {
      return JSON.parse(text);
    } catch {
      return { rawText: text };
    }
  }

  it("handles timeout → corruption → restart flow", async () => {
    // Execute a normal command first
    const result1 = parseResult(await callTool("ghci_type", { expression: "map" }));
    expect(result1.success).toBe(true);
    expect(result1.expression).toBe("map");
    expect(result1.type ?? result1.raw).toContain("->");
    
    // Cause a timeout with a very short timeout
    const timeoutResult = parseResult(
      await callTool("ghci_eval", {
        expression: "let loop = loop in loop",
        timeout_ms: 100,
      })
    );
    expect(timeoutResult.success).toBe(false);
    expect(timeoutResult.error).toMatch(/timeout|timed out/i);
    
    // Wait a bit for recovery
    await new Promise(r => setTimeout(r, 500));
    
    // Next command should auto-recover and work
    const result2 = parseResult(await callTool("ghci_type", { expression: "foldr" }));
    expect(result2.success).toBe(true);
    expect(result2.expression).toBe("foldr");
    expect(result2.type ?? result2.raw).toContain("->");
  }, 30000);

  it("rejects dangerous batch commands", async () => {
    let rejectedMessage: string | null = null;
    try {
      const response = parseResult(await callTool("ghci_batch", {
        commands: [":t map", ":set +m", ":t foldr"]
      }));
      rejectedMessage =
        response.error ??
        response.message ??
        response.rawText ??
        JSON.stringify(response);
    } catch (error: any) {
      rejectedMessage = error.message || error.toString();
    }
    expect(rejectedMessage ?? "").toMatch(/Dangerous GHCi command/i);
    
    // Session should still work after rejection
    const result = await callTool("ghci_type", { expression: "map" });
    expect(result.content[0].text).toContain("map");
  });

  it("batch execution works normally with safe commands", async () => {
    const result = parseResult(await callTool("ghci_batch", {
      commands: [":t map", ":t foldr", "1 + 1"]
    }));
    expect(result.allSuccess).toBe(true);
    expect(result.results).toHaveLength(3);
  });
});
