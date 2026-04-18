import { afterAll, beforeAll, describe, expect, it } from "vitest";
import { Client } from "@modelcontextprotocol/sdk/client/index.js";
import { StdioClientTransport } from "@modelcontextprotocol/sdk/client/stdio.js";
import { execSync } from "node:child_process";
import { readFile, rm, writeFile } from "node:fs/promises";
import path from "node:path";

const FIXTURE_DIR = path.resolve(import.meta.dirname, "../fixtures/test-project");
const SERVER_SCRIPT = path.resolve(import.meta.dirname, "../../../dist/index.js");
const GHCUP_BIN = path.join(process.env.HOME ?? "", ".ghcup", "bin");
const TEST_PATH = `${GHCUP_BIN}:${process.env.PATH}`;
const TEMP_EXPORTS_MODULE = path.join(FIXTURE_DIR, "src", "TempExportsE2E.hs");
const PROPERTY_STORE_DIR = path.join(FIXTURE_DIR, ".haskell-flows");
const TEST_SPEC_FILE = path.join(FIXTURE_DIR, "test", "Spec.hs");

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

  beforeAll(async () => {
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

  // `ghci_fuzz_parser` was removed from the public MCP surface in Fase 2.
  it.skip("ghci_fuzz_parser reports crashes through MCP (tool removed)", async () => {});

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

  it("workflow guidance downgrades lint/format when tools are unavailable", async () => {
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
    // When hlint is unavailable, ghci_lint falls back to basic-lint-rules
    // with `degraded: true` and `gateEligible: false`. It no longer returns
    // `unavailable: true` at the top level — that's surfaced as `_primary_failure`.
    expect(lint.degraded).toBe(true);
    expect(lint.gateEligible).toBe(false);
    expect(lint.lint_tool).toBe("basic-lint-rules");
    expect(lint._primary_failure?.lint_tool).toBe("hlint");

    const evalResult = parseResult(
      await client.callTool({
        name: "ghci_eval",
        arguments: { expression: "add 1 2" },
      })
    );
    expect(Array.isArray(evalResult._guidance)).toBe(true);
    expect(
      evalResult._guidance.some((item: string) => item.includes("recommended but not blocking"))
    ).toBe(true);
  });
});
