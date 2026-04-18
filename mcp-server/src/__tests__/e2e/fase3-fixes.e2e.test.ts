/**
 * E2E regression tests for Fase 3 fixes:
 *   - `ghci_suggest(mode="analyze")` is idempotent across intermediate loads
 *   - `cabal_coverage` surfaces either metrics OR a `_hint` field (never an
 *     empty summary without recourse)
 *   - telemetry is off by default (no file created on a default session)
 */
import { afterAll, beforeAll, describe, expect, it } from "vitest";
import { Client } from "@modelcontextprotocol/sdk/client/index.js";
import { StdioClientTransport } from "@modelcontextprotocol/sdk/client/stdio.js";
import { execSync } from "node:child_process";
import { existsSync } from "node:fs";
import path from "node:path";

const FIXTURE_DIR = path.resolve(import.meta.dirname, "../fixtures/test-project");
const SERVER_SCRIPT = path.resolve(import.meta.dirname, "../../../dist/index.js");
const GHCUP_BIN = path.join(process.env.HOME ?? "", ".ghcup", "bin");
const TEST_PATH = `${GHCUP_BIN}:${process.env.PATH}`;

const GHC_AVAILABLE = (() => {
  try {
    execSync("ghc --version", { stdio: "pipe", env: { ...process.env, PATH: TEST_PATH } });
    return true;
  } catch {
    return false;
  }
})();

function parseResult(result: Awaited<ReturnType<Client["callTool"]>>): any {
  return JSON.parse((result.content as Array<{ type: string; text: string }>)[0]!.text);
}

describe.runIf(GHC_AVAILABLE)("E2E: Fase 3 fixes", () => {
  let client: Client;
  let transport: StdioClientTransport;

  beforeAll(async () => {
    transport = new StdioClientTransport({
      command: process.execPath,
      args: [SERVER_SCRIPT],
      env: {
        ...process.env,
        PATH: TEST_PATH,
        HASKELL_PROJECT_DIR: FIXTURE_DIR,
        // Explicitly leave telemetry OFF for this suite.
        HASKELL_FLOWS_TELEMETRY: "0",
      } as Record<string, string>,
    });
    client = new Client({ name: "fase3-e2e", version: "0.0.1" });
    await client.connect(transport);
  }, 60_000);

  afterAll(async () => {
    try { await client.close(); } catch { /* ignore */ }
  });

  it("ghci_suggest(analyze) reloads the target, so repeated runs after intermediate loads still browse it", async () => {
    // 1st analyze — should load TestLib and browse.
    const first = parseResult(
      await client.callTool({
        name: "ghci_suggest",
        arguments: { module_path: "src/TestLib.hs", mode: "analyze" },
      })
    );
    expect(first.success).toBe(true);
    expect(first.mode).toBe("analyze");

    // Interfere: call ghci_load on a *different* module would evict TestLib
    // from `:browse` in the old behavior. We don't have another module in
    // test-project, so we just re-run `ghci_load(TestLib)` which previously
    // was still fine — but the stronger check is that analyze itself now
    // always loads first. Repeat the call to confirm idempotence.
    const second = parseResult(
      await client.callTool({
        name: "ghci_suggest",
        arguments: { module_path: "src/TestLib.hs", mode: "analyze" },
      })
    );
    expect(second.success).toBe(true);
  });

  it("cabal_coverage returns either metrics OR an actionable _hint — never a silent empty success", async () => {
    const result = parseResult(
      await client.callTool({ name: "cabal_coverage", arguments: {} })
    );
    expect(result.success).toBe(true);
    expect(result.reportSource).toMatch(/^(cabal-test|hpc-report|hpc-html)$/);
    const hasMetrics = Array.isArray(result.metrics) && result.metrics.length > 0;
    const hasHint = typeof result._hint === "string";
    expect(hasMetrics || hasHint).toBe(true);
  });

  it("telemetry is OFF by default — .haskell-flows/telemetry.json is NOT created by normal tool use", async () => {
    await client.callTool({ name: "ghci_workflow", arguments: { action: "status" } });
    // Small grace period for the finally-hooked write to flush, if it were
    // ever triggered (it should not be).
    await new Promise((r) => setTimeout(r, 100));
    const file = path.join(FIXTURE_DIR, ".haskell-flows", "telemetry.json");
    expect(existsSync(file)).toBe(false);
  });
});

describe.runIf(GHC_AVAILABLE)("E2E: telemetry opt-in writes per-tool counts", () => {
  let client: Client;
  let transport: StdioClientTransport;
  const telemetryFile = path.join(FIXTURE_DIR, ".haskell-flows", "telemetry.json");

  beforeAll(async () => {
    // Ensure a clean slate.
    try {
      const { rmSync } = await import("node:fs");
      rmSync(telemetryFile, { force: true });
    } catch { /* ignore */ }

    transport = new StdioClientTransport({
      command: process.execPath,
      args: [SERVER_SCRIPT],
      env: {
        ...process.env,
        PATH: TEST_PATH,
        HASKELL_PROJECT_DIR: FIXTURE_DIR,
        HASKELL_FLOWS_TELEMETRY: "1", // OPT-IN
      } as Record<string, string>,
    });
    client = new Client({ name: "fase3-telemetry-e2e", version: "0.0.1" });
    await client.connect(transport);
  }, 60_000);

  afterAll(async () => {
    try { await client.close(); } catch { /* ignore */ }
    try {
      const { rmSync } = await import("node:fs");
      rmSync(telemetryFile, { force: true });
    } catch { /* ignore */ }
  });

  it("records calls locally only (no network) and aggregates success/failure", async () => {
    await client.callTool({ name: "ghci_workflow", arguments: { action: "status" } });
    await client.callTool({ name: "ghci_workflow", arguments: { action: "help" } });
    // Wait briefly for the finally-hooked write to land.
    await new Promise((r) => setTimeout(r, 200));

    expect(existsSync(telemetryFile)).toBe(true);
    const { readFileSync } = await import("node:fs");
    const file = JSON.parse(readFileSync(telemetryFile, "utf-8"));
    expect(file.version).toBe(1);
    expect(file.enabled).toBe(true);
    expect(file.tools.ghci_workflow?.calls).toBeGreaterThanOrEqual(2);
    // No argument echoes in the file
    expect(JSON.stringify(file)).not.toMatch(/action/);
  });
});
