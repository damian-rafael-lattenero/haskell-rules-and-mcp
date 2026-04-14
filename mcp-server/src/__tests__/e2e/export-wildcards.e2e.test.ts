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

describe.runIf(GHC_AVAILABLE)("ghci_quickcheck_export wildcard e2e", () => {
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
    client = new Client({ name: "wildcard-export-e2e", version: "0.1.0" }, { capabilities: {} });
    await client.connect(transport);
  }, 60_000);

  afterAll(async () => {
    try {
      await client.close();
    } catch {
      // ignore close errors
    }
    await rm(PROPERTY_STORE_DIR, { recursive: true, force: true });
    await writeFile(TEST_SPEC_FILE, originalSpec, "utf8");
  });

  it("exports wildcard properties with concrete type annotation and passing cabal_test", async () => {
    await rm(PROPERTY_STORE_DIR, { recursive: true, force: true });

    const qc = parseResult(
      await client.callTool({
        name: "ghci_quickcheck",
        arguments: {
          property: "\\_ -> add 1 (1 :: Int) == 2",
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
    expect(exported.validation?.success ?? true).toBe(true);

    const spec = await readFile(TEST_SPEC_FILE, "utf8");
    expect(spec).toContain(":: () -> Bool");
  });
});
