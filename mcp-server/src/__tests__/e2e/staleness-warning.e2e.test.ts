/**
 * E2E: when the live MCP bundle on disk is newer than the running process,
 * every tool response should carry a `_warning` field telling the user to
 * restart Claude Desktop.
 *
 * Setup: we spawn the MCP, then set `dist/index.js` mtime to ~10 minutes
 * in the future. The next tool call triggers the first staleness probe,
 * which stats the bundle and sees `bundleAheadByMs >> threshold` → the
 * middleware injects the warning.
 *
 * We restore the original mtime in `afterAll` so we don't poison
 * subsequent test runs or the running dev MCP.
 */
import { afterAll, beforeAll, describe, expect, it } from "vitest";
import { Client } from "@modelcontextprotocol/sdk/client/index.js";
import { StdioClientTransport } from "@modelcontextprotocol/sdk/client/stdio.js";
import { mkdtempSync, rmSync } from "node:fs";
import { stat, utimes } from "node:fs/promises";
import os from "node:os";
import path from "node:path";

const BUNDLE_PATH = path.resolve(
  import.meta.dirname,
  "../../../dist/index.js"
);
const SERVER_SCRIPT = BUNDLE_PATH;
const GHCUP_BIN = path.join(process.env.HOME ?? "", ".ghcup", "bin");
const TEST_PATH = `${GHCUP_BIN}:${process.env.PATH}`;

function parseResult(result: Awaited<ReturnType<Client["callTool"]>>): any {
  return JSON.parse(
    (result.content as Array<{ type: string; text: string }>)[0]!.text
  );
}

describe("E2E: staleness-warning middleware", () => {
  let client: Client;
  let transport: StdioClientTransport;
  let workspace: string;
  let originalMtime: Date;

  beforeAll(async () => {
    workspace = mkdtempSync(path.join(os.tmpdir(), "e2e-staleness-"));

    // Remember the real mtime so we can restore it after the test.
    originalMtime = (await stat(BUNDLE_PATH)).mtime;

    transport = new StdioClientTransport({
      command: process.execPath,
      args: [SERVER_SCRIPT],
      env: {
        ...process.env,
        PATH: TEST_PATH,
        HASKELL_PROJECT_DIR: workspace,
      } as Record<string, string>,
    });
    client = new Client({ name: "e2e-staleness", version: "0.0.1" });
    await client.connect(transport);

    // Now that the process has booted, advance the bundle mtime to
    // simulate "maintainer rebuilt 10 minutes from now".
    const future = new Date(Date.now() + 10 * 60 * 1000);
    await utimes(BUNDLE_PATH, future, future);
  }, 30_000);

  afterAll(async () => {
    try {
      await utimes(BUNDLE_PATH, originalMtime, originalMtime);
    } catch {
      /* best effort — dev environment tolerates a slightly off mtime */
    }
    try { await client.close(); } catch { /* ignore */ }
    try { rmSync(workspace, { recursive: true, force: true }); } catch { /* ignore */ }
  });

  it("injects _warning about the stale bundle into a normal tool response", async () => {
    const result = parseResult(
      await client.callTool({
        name: "ghci_toolchain_status",
        arguments: { include_matrix: false, include_runtime: false },
      })
    );
    // Well-formed response
    expect(result).toBeTypeOf("object");
    // Middleware reached it
    expect(result._warning).toMatch(/MCP bundle on disk is/);
    expect(result._warning).toMatch(/Restart Claude Desktop/);
  }, 30_000);

  it("warning re-appears on subsequent calls (cache serves the same result)", async () => {
    const r = parseResult(
      await client.callTool({
        name: "ghci_workflow",
        arguments: { action: "status" },
      })
    );
    expect(r._warning).toMatch(/MCP bundle on disk is/);
  }, 30_000);
});
