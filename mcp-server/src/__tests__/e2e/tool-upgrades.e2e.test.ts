import { afterAll, beforeAll, describe, expect, it } from "vitest";
import { Client } from "@modelcontextprotocol/sdk/client/index.js";
import { StdioClientTransport } from "@modelcontextprotocol/sdk/client/stdio.js";
import { execSync } from "node:child_process";
import { readFile, rm, writeFile } from "node:fs/promises";
import path from "node:path";
import { setupIsolatedFixture, type IsolatedFixture } from "../helpers/isolated-fixture.js";

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
  const text = (result.content as Array<{ type: string; text: string }>)[0]!.text;
  return JSON.parse(text);
}

describe.runIf(GHC_AVAILABLE)("E2E Tool Upgrades", () => {
  let client: Client;
  let transport: StdioClientTransport;
  let originalSpec = "";
  let fixture: IsolatedFixture;
  let FIXTURE_DIR: string;
  let TEMP_EXPORTS_MODULE: string;
  let PROPERTY_STORE_DIR: string;
  let TEST_SPEC_FILE: string;

  beforeAll(async () => {
    fixture = await setupIsolatedFixture("test-project", "tool-upgrades");
    FIXTURE_DIR = fixture.dir;
    TEMP_EXPORTS_MODULE = path.join(FIXTURE_DIR, "src", "TempExportsE2E.hs");
    PROPERTY_STORE_DIR = path.join(FIXTURE_DIR, ".haskell-flows");
    TEST_SPEC_FILE = path.join(FIXTURE_DIR, "test", "Spec.hs");
    originalSpec = await readFile(TEST_SPEC_FILE, "utf8");
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
    client = new Client({ name: "tool-upgrades-client", version: "0.1.0" }, { capabilities: {} });
    await client.connect(transport);
  }, 60_000);

  afterAll(async () => {
    try {
      await client.close();
    } catch {
      // ignore
    }
    await rm(TEMP_EXPORTS_MODULE, { force: true });
    await rm(PROPERTY_STORE_DIR, { recursive: true, force: true });
    await writeFile(TEST_SPEC_FILE, originalSpec, "utf8");
    await fixture.cleanup();
  });

  it("cabal_test succeeds through MCP", async () => {
    const result = parseResult(await client.callTool({ name: "cabal_test", arguments: {} }));
    expect(result.success).toBe(true);
    expect(result.summary).toContain("Tests passed");
  });

  it("ghci_apply_exports rewrites the module header through MCP", async () => {
    await writeFile(
      TEMP_EXPORTS_MODULE,
      `module TempExportsE2E where\n\nfoo :: Int\nfoo = 1\n`,
      "utf8"
    );
    const result = parseResult(
      await client.callTool({
        name: "ghci_apply_exports",
        arguments: {
          module_path: "src/TempExportsE2E.hs",
          suggested_export_list: "module TempExportsE2E\n  ( foo\n  ) where",
        },
      })
    );
    expect(result.success).toBe(true);
    const content = await readFile(TEMP_EXPORTS_MODULE, "utf8");
    expect(content).toContain("( foo");
  });


  it("ghci_quickcheck_export validates the exported test suite", async () => {
    await rm(PROPERTY_STORE_DIR, { recursive: true, force: true });
    const qc = parseResult(
      await client.callTool({
        name: "ghci_quickcheck",
        arguments: {
          property: "\\x y -> add x y == x + (y :: Int)",
          module_path: "src/TestLib.hs",
        },
      })
    );
    expect(qc.success).toBe(true);

    const exported = parseResult(
      await client.callTool({
        name: "ghci_quickcheck_export",
        arguments: { output_path: "test/Spec.hs", module: "src/TestLib.hs" },
      })
    );
    expect(exported.success).toBe(true);
    expect(exported.testRun?.success).toBe(true);
  });

  // Longer timeout: on first run this triggers auto-download of hlint
  // (~136MB) because the fixture project calls ghci_lint. Subsequent runs
  // hit the cached binary in vendor-tools/ and finish in seconds.
  it("workflow guidance downgrades lint/format when tools are unavailable", { timeout: 180_000 }, async () => {
    const load = parseResult(
      await client.callTool({
        name: "ghci_load",
        arguments: { module_path: "src/TestLib.hs", diagnostics: true },
      })
    );
    expect(load.success).toBe(true);

    const qc = parseResult(
      await client.callTool({
        name: "ghci_quickcheck",
        arguments: {
          property: "\\x y -> add x y == x + (y :: Int)",
          module_path: "src/TestLib.hs",
        },
      })
    );
    expect(qc.success).toBe(true);

    const lint = parseResult(
      await client.callTool({
        name: "ghci_lint",
        arguments: { module_path: "src/TestLib.hs" },
      })
    );
    // Two paths: (a) hlint installed/auto-downloaded → real lint runs with
    // lint_tool="hlint" and degraded undefined; (b) hlint unavailable → the
    // wrapper falls back to basic-lint-rules with degraded:true,
    // gateEligible:false, and _primary_failure pointing at hlint.
    // Post-Fase4 the URL is working so the auto-download path (a) is reachable.
    // Accept either to avoid a network-dependent test.
    if (lint.lint_tool === "hlint") {
      expect(typeof lint.success).toBe("boolean");
    } else {
      expect(lint.degraded).toBe(true);
      expect(lint.gateEligible).toBe(false);
      expect(lint.lint_tool).toBe("basic-lint-rules");
      expect(lint._primary_failure?.lint_tool).toBe("hlint");
    }

    const evalResult = parseResult(
      await client.callTool({
        name: "ghci_eval",
        arguments: { expression: "add 1 2" },
      })
    );
    // _guidance surfaces only when there are actionable hints — may be
    // undefined when everything is in a steady state.
    if (Array.isArray(evalResult._guidance) && evalResult._guidance.length > 0) {
      // No hard assertion on phrasing — just verify the shape is sane.
      for (const item of evalResult._guidance) {
        expect(typeof item).toBe("string");
      }
    }
  });
});
