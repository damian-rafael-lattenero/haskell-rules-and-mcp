/**
 * E2E: `mcp_reload_code` is correctly registered on the MCP surface and
 * responds to dry-run calls with the expected shape.
 *
 * We NEVER pass `confirm: true` in this suite — that would kill the child
 * process mid-test. Dry-run coverage is enough to pin:
 *   • the tool is listed by the server
 *   • the schema accepts no args (default = dry-run)
 *   • the response JSON carries the Phase-6 diagnostic fields
 *
 * Unit tests (mcp-reload-code.test.ts) exercise every branch of the
 * handler including the confirm=true path via injected dependencies.
 */
import { afterAll, beforeAll, describe, expect, it } from "vitest";
import { Client } from "@modelcontextprotocol/sdk/client/index.js";
import { StdioClientTransport } from "@modelcontextprotocol/sdk/client/stdio.js";
import { mkdtempSync, rmSync } from "node:fs";
import os from "node:os";
import path from "node:path";

const SERVER_SCRIPT = path.resolve(import.meta.dirname, "../../../dist/index.js");
const GHCUP_BIN = path.join(process.env.HOME ?? "", ".ghcup", "bin");
const TEST_PATH = `${GHCUP_BIN}:${process.env.PATH}`;

function parseResult(result: Awaited<ReturnType<Client["callTool"]>>): any {
  return JSON.parse((result.content as Array<{ type: string; text: string }>)[0]!.text);
}

describe("E2E: mcp_reload_code", () => {
  let client: Client;
  let transport: StdioClientTransport;
  let workspace: string;

  beforeAll(async () => {
    workspace = mkdtempSync(path.join(os.tmpdir(), "e2e-reload-code-"));
    transport = new StdioClientTransport({
      command: process.execPath,
      args: [SERVER_SCRIPT],
      env: {
        ...process.env,
        PATH: TEST_PATH,
        HASKELL_PROJECT_DIR: workspace,
      } as Record<string, string>,
    });
    client = new Client({ name: "e2e-reload-code", version: "0.0.1" });
    await client.connect(transport);
  }, 30_000);

  afterAll(async () => {
    try { await client.close(); } catch { /* ignore */ }
    try { rmSync(workspace, { recursive: true, force: true }); } catch { /* ignore */ }
  });

  it("is listed among the registered tools", async () => {
    const tools = await client.listTools();
    const names = tools.tools.map((t) => t.name);
    expect(names).toContain("mcp_reload_code");
  });

  it("dry-run (no args) returns a well-formed report", async () => {
    const result = parseResult(
      await client.callTool({ name: "mcp_reload_code", arguments: {} })
    );
    expect(result.success).toBe(true);
    expect(result.scheduledRestart).toBe(false);
    expect(typeof result.reason).toBe("string");
    expect(result.reason).toContain("Dry-run");
    // Diagnostic fields are present — agents use these to decide if a
    // restart would actually do anything.
    expect(result.bootTime).toMatch(/\d{4}-\d{2}-\d{2}T/);
    expect(typeof result.bundleAheadByMs).toBe("number");
  });

  it("explicit confirm=false is equivalent to dry-run (never exits)", async () => {
    const result = parseResult(
      await client.callTool({
        name: "mcp_reload_code",
        arguments: { confirm: false },
      })
    );
    expect(result.success).toBe(true);
    expect(result.scheduledRestart).toBe(false);
  });

  it("rejects unknown fields via strict Zod (CWE-20 hardening)", async () => {
    // Strict schema rejects unrecognized params. The MCP SDK surfaces this
    // as a RESOLVED result with `isError: true` (not a rejected promise),
    // carrying a structured "unrecognized_keys" diagnostic from Zod.
    const result = await client.callTool({
      name: "mcp_reload_code",
      arguments: { confirm: false, extraField: "ignored" } as Record<string, unknown>,
    });
    expect(result.isError).toBe(true);
    const text = (result.content as Array<{ type: string; text: string }>)[0]!.text;
    expect(text).toContain("unrecognized_keys");
    expect(text).toContain("extraField");
    // And — critically — the tool did NOT schedule any exit as a side effect
    // of the validation failure. The server is still responsive:
    const followup = parseResult(
      await client.callTool({ name: "mcp_reload_code", arguments: {} })
    );
    expect(followup.success).toBe(true);
    expect(followup.scheduledRestart).toBe(false);
  });
});
