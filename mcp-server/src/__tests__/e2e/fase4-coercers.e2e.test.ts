/**
 * Fase 4 E2E: the boundary-tolerant coercers (zBool / zNum / zArray / zRecord)
 * let MCP tools accept the string-serialized forms the Claude↔MCP bridge
 * sometimes emits. These tests invoke real tools via the MCP protocol with
 * deliberately stringified payloads and assert the calls succeed.
 */
import { afterAll, beforeAll, describe, expect, it } from "vitest";
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
  } catch {
    return false;
  }
})();

function parseResult(result: Awaited<ReturnType<Client["callTool"]>>): any {
  return JSON.parse((result.content as Array<{ type: string; text: string }>)[0]!.text);
}

describe.runIf(GHC_AVAILABLE)("E2E: Fase 4 coercers via MCP protocol", () => {
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
        HASKELL_FLOWS_TELEMETRY: "0",
      } as Record<string, string>,
    });
    client = new Client({ name: "fase4-coercers-e2e", version: "0.0.1" });
    await client.connect(transport);
  }, 60_000);

  afterAll(async () => {
    try { await client.close(); } catch { /* ignore */ }
  });

  it('accepts a string-serialized boolean for `diagnostics` on ghci_load', async () => {
    const res = parseResult(
      await client.callTool({
        name: "ghci_load",
        arguments: {
          module_path: "src/TestLib.hs",
          // deliberately-stringified boolean — what the Claude bridge emits
          diagnostics: "true" as unknown as boolean,
        },
      })
    );
    expect(res.success).toBe(true);
  });

  it('accepts a string-serialized array for `commands` on ghci_batch', async () => {
    const res = parseResult(
      await client.callTool({
        name: "ghci_batch",
        arguments: {
          // deliberately-stringified array — what the Claude bridge emits
          commands: '[":t map", ":k Maybe"]' as unknown as string[],
        },
      })
    );
    // ghci_batch returns `{ results, allSuccess }` (no `success` field).
    expect(Array.isArray(res.results)).toBe(true);
    expect(res.results.length).toBe(2);
  });

  it('accepts a string-serialized record for `signatures` on ghci_add_modules', async () => {
    // This test mutates the fixture — it creates a Fase4Probe module, asserts
    // the stringified-record payload was accepted, then restores the fixture.
    const { readFileSync, writeFileSync, existsSync, rmSync } = await import("node:fs");
    const cabalPath = path.join(FIXTURE_DIR, "test-project.cabal");
    const probePath = path.join(FIXTURE_DIR, "src", "Fase4Probe.hs");
    const cabalBefore = readFileSync(cabalPath, "utf-8");
    try {
      const res = parseResult(
        await client.callTool({
          name: "ghci_add_modules",
          arguments: {
            modules: '["Fase4Probe"]' as unknown as string[],
            signatures: '{"Fase4Probe":["probe :: Int"]}' as unknown as Record<string, string[]>,
          },
        })
      );
      expect(res.success).toBe(true);
      expect(existsSync(probePath)).toBe(true);
    } finally {
      // Restore fixture so we don't pollute the repo with test byproducts.
      writeFileSync(cabalPath, cabalBefore, "utf-8");
      try { rmSync(probePath, { force: true }); } catch { /* ignore */ }
    }
  }, 30_000);

  it("still rejects unknown keys (strict semantics preserved)", async () => {
    const res = await client.callTool({
      name: "ghci_load",
      arguments: {
        module_path: "src/TestLib.hs",
        // @ts-expect-error — deliberately unknown key
        totally_unknown_key: "whatever",
      },
    });
    expect(res.isError).toBe(true);
    const text = (res.content as Array<{ type: string; text: string }>)[0]!.text;
    expect(text).toMatch(/Unrecognized key|Input validation/);
  });

  it("ghci_refactor(rename_local) defaults to preview (does NOT mutate without apply=true)", async () => {
    // First snapshot whatever file we're going to poke.
    const { readFileSync, writeFileSync } = await import("node:fs");
    const lib = path.join(FIXTURE_DIR, "src", "TestLib.hs");
    const before = readFileSync(lib, "utf-8");
    try {
      const res = parseResult(
        await client.callTool({
          name: "ghci_refactor",
          arguments: {
            action: "rename_local",
            module_path: "src/TestLib.hs",
            old_name: "add",
            new_name: "fase4RenameProbe",
            // intentionally omit apply — must default to preview
          },
        })
      );
      expect(res.success).toBe(true);
      expect(res.applied).toBe(false);
      // File content must be unchanged
      const after = readFileSync(lib, "utf-8");
      expect(after).toBe(before);
    } finally {
      // Defensive: restore anyway
      writeFileSync(lib, before, "utf-8");
    }
  });
});
