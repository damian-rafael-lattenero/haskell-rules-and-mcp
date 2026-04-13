/**
 * E2E Workflow Test: Simulates a real development flow via MCP protocol.
 *
 * Flow:
 * 1. Add a buggy module to the fixture project
 * 2. Compile and detect errors + warnings
 * 3. Fix the error
 * 4. Re-compile and detect warnings
 * 5. Fix warnings using warningActions
 * 6. Verify clean compilation
 * 7. Run QuickCheck properties
 * 8. Use ghci_eval to test manually
 * 9. Use ghci_check_module to review API
 * 10. Clean up the temporary module
 */
import { describe, it, expect, beforeAll, afterAll } from "vitest";
import { Client } from "@modelcontextprotocol/sdk/client/index.js";
import { StdioClientTransport } from "@modelcontextprotocol/sdk/client/stdio.js";
import { execSync } from "node:child_process";
import { writeFile, unlink, readFile } from "node:fs/promises";
import path from "node:path";

const FIXTURE_DIR = path.resolve(
  import.meta.dirname,
  "../fixtures/test-project"
);
const SERVER_SCRIPT = path.resolve(
  import.meta.dirname,
  "../../../dist/index.js"
);
const GHCUP_BIN = path.join(process.env.HOME ?? "", ".ghcup", "bin");
const TEST_PATH = `${GHCUP_BIN}:${process.env.PATH}`;

const WORKFLOW_MODULE = path.join(FIXTURE_DIR, "src", "WorkflowTest.hs");
const CABAL_FILE = path.join(FIXTURE_DIR, "test-project.cabal");

const GHC_AVAILABLE = (() => {
  try {
    execSync("ghc --version", {
      stdio: "pipe",
      env: { ...process.env, PATH: TEST_PATH },
    });
    return true;
  } catch {
    return false;
  }
})();

function callTool(client: Client, name: string, args: Record<string, unknown> = {}) {
  return client.callTool({ name, arguments: args });
}

function parseResult(result: Awaited<ReturnType<Client["callTool"]>>): any {
  const text = (result.content as Array<{ type: string; text: string }>)[0]!.text;
  return JSON.parse(text);
}

describe.runIf(GHC_AVAILABLE)("E2E Workflow: Development Loop", () => {
  let client: Client;
  let transport: StdioClientTransport;
  let originalCabal: string;

  beforeAll(async () => {
    // Save original cabal file
    originalCabal = await readFile(CABAL_FILE, "utf-8");

    // Add WorkflowTest to exposed-modules
    const updatedCabal = originalCabal.replace(
      "exposed-modules:  TestLib",
      "exposed-modules:  TestLib\n                  WorkflowTest"
    );
    await writeFile(CABAL_FILE, updatedCabal, "utf-8");

    // Start MCP server
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
    client = new Client(
      { name: "workflow-test-client", version: "0.1.0" },
      { capabilities: {} }
    );
    await client.connect(transport);

    // Restart GHCi so it picks up the new cabal
    await callTool(client, "ghci_session", { action: "restart" });
  }, 60_000);

  afterAll(async () => {
    // Clean up: remove temp module, restore cabal
    try { await unlink(WORKFLOW_MODULE); } catch { /* ignore */ }
    await writeFile(CABAL_FILE, originalCabal, "utf-8");
    try { await client.close(); } catch { /* ignore */ }
  });

  // --- Step 1: Create a buggy module ---
  it("step 1: scaffold creates stub for new module", async () => {
    const result = parseResult(await callTool(client, "ghci_scaffold"));
    expect(result.success).toBe(true);
    expect(result.created).toContain("src/WorkflowTest.hs");
  });

  // --- Step 2: Write buggy code and compile ---
  it("step 2: compile detects type error", async () => {
    // Write a module with: type error + missing signature + unused import
    await writeFile(
      WORKFLOW_MODULE,
      `module WorkflowTest where

import Data.List (sort)

double x = x + x

broken :: Int -> Int
broken = True
`,
      "utf-8"
    );

    // Restart to pick up file change
    await callTool(client, "ghci_session", { action: "restart" });

    const result = parseResult(
      await callTool(client, "ghci_load", {
        module_path: "src/WorkflowTest.hs",
        diagnostics: true,
      })
    );

    expect(result.success).toBe(false);
    expect(result.errors.length).toBeGreaterThan(0);
    // Should detect GHC-83865 type mismatch
    const typeError = result.errors.find((e: any) => e.code === "GHC-83865");
    expect(typeError).toBeDefined();
    expect(typeError.expected).toContain("Int");
    expect(typeError.actual).toContain("Bool");
  });

  // --- Step 3: Fix the type error ---
  it("step 3: fix error, compile shows warnings only", async () => {
    await writeFile(
      WORKFLOW_MODULE,
      `module WorkflowTest where

import Data.List (sort)

double x = x + x

broken :: Int -> Int
broken x = x * 2
`,
      "utf-8"
    );

    const result = parseResult(
      await callTool(client, "ghci_load", {
        module_path: "src/WorkflowTest.hs",
        diagnostics: true,
      })
    );

    expect(result.success).toBe(true);
    expect(result.errors).toHaveLength(0);
    // Should have warnings: unused-import, missing-signature
    expect(result.warnings.length).toBeGreaterThan(0);
    expect(result.warningActions.length).toBeGreaterThan(0);

    // Verify actionable categories exist
    const categories = result.warningActions.map((a: any) => a.category);
    expect(categories).toContain("unused-import");
  });

  // --- Step 4: Fix all warnings ---
  it("step 4: fix warnings, compile clean", async () => {
    await writeFile(
      WORKFLOW_MODULE,
      `module WorkflowTest where

double :: Num a => a -> a
double x = x + x

broken :: Int -> Int
broken x = x * 2
`,
      "utf-8"
    );

    const result = parseResult(
      await callTool(client, "ghci_load", {
        module_path: "src/WorkflowTest.hs",
        diagnostics: true,
      })
    );

    expect(result.success).toBe(true);
    expect(result.errors).toHaveLength(0);
    expect(result.warningActions).toHaveLength(0);
  });

  // --- Step 5: Verify with QuickCheck ---
  it("step 5: quickcheck property passes", async () => {
    const result = parseResult(
      await callTool(client, "ghci_quickcheck", {
        property: "\\x -> double x == x + (x :: Int)",
      })
    );

    expect(result.success).toBe(true);
    expect(result.passed).toBe(100);
  });

  // --- Step 6: QuickCheck catches a bug ---
  it("step 6: quickcheck detects bug with counterexample", async () => {
    // broken doubles instead of identity — quickcheck will catch mismatch
    const result = parseResult(
      await callTool(client, "ghci_quickcheck", {
        property: "\\x -> broken x == (x :: Int)",
      })
    );

    expect(result.success).toBe(false);
    expect(result.counterexample).toBeDefined();
  });

  // --- Step 7: Eval manual testing ---
  it("step 7: eval returns correct results", async () => {
    const r1 = parseResult(await callTool(client, "ghci_eval", { expression: "double 21" }));
    expect(r1.success).toBe(true);
    expect(r1.output).toContain("42");

    const r2 = parseResult(await callTool(client, "ghci_eval", { expression: "broken 5" }));
    expect(r2.success).toBe(true);
    expect(r2.output).toContain("10");
  });

  // --- Step 8: Type checking ---
  it("step 8: type check returns correct types", async () => {
    const r1 = parseResult(await callTool(client, "ghci_type", { expression: "double" }));
    expect(r1.success).toBe(true);
    expect(r1.type).toContain("Num");
    expect(r1.type).toContain("a -> a");

    const r2 = parseResult(await callTool(client, "ghci_type", { expression: "broken" }));
    expect(r2.success).toBe(true);
    expect(r2.type).toContain("Int -> Int");
  });

  // --- Step 9: Module API review ---
  it("step 9: check_module shows all exports", async () => {
    const result = parseResult(
      await callTool(client, "ghci_check_module", {
        module_path: "src/WorkflowTest.hs",
      })
    );

    expect(result.success).toBe(true);
    expect(result.module).toBe("WorkflowTest");
    const names = result.definitions.map((d: any) => d.name);
    expect(names).toContain("double");
    expect(names).toContain("broken");
    expect(result.summary.functions).toBe(2);
  });

  // --- Step 10: Batch operations ---
  it("step 10: batch returns multiple results", async () => {
    const result = parseResult(
      await callTool(client, "ghci_batch", {
        commands: [":t double", ":t broken", "double 10", "broken 10"],
        reload: true,
      })
    );

    expect(result.allSuccess).toBe(true);
    expect(result.count).toBe(4);
    expect(result.results[0].output).toContain("Num");
    expect(result.results[2].output).toContain("20");
    expect(result.results[3].output).toContain("20");
  });

  // --- Step 11: Sentinel sync — no off-by-one ---
  it("step 11: sequential evals return their own results (no offset)", async () => {
    const r1 = parseResult(await callTool(client, "ghci_eval", { expression: "double 1" }));
    const r2 = parseResult(await callTool(client, "ghci_eval", { expression: "double 2" }));
    const r3 = parseResult(await callTool(client, "ghci_eval", { expression: "double 3" }));

    expect(r1.output).toContain("2");
    expect(r2.output).toContain("4");
    expect(r3.output).toContain("6");

    // Critically: r2 must NOT contain r1's result
    expect(r2.output).not.toContain("2\n");
  });

  it("step 12: type after eval returns type, not eval result", async () => {
    const evalResult = parseResult(
      await callTool(client, "ghci_eval", { expression: "broken 7" })
    );
    expect(evalResult.output).toContain("14");

    const typeResult = parseResult(
      await callTool(client, "ghci_type", { expression: "broken" })
    );
    expect(typeResult.success).toBe(true);
    expect(typeResult.type).toContain("Int -> Int");
    expect(typeResult.type).not.toContain("14");
  });

  // --- Step 13-17: EXHAUSTIVE sentinel offset tests via MCP protocol ---
  it("step 13: 10 sequential evals all return correct results", async () => {
    for (let i = 1; i <= 10; i++) {
      const r = parseResult(
        await callTool(client, "ghci_eval", { expression: `double ${i}` })
      );
      expect(r.success).toBe(true);
      expect(r.output).toContain(String(i * 2));
    }
  });

  it("step 14: load → eval → load → eval stays aligned", async () => {
    for (let i = 0; i < 3; i++) {
      await callTool(client, "ghci_load", { module_path: "src/WorkflowTest.hs" });
      const r = parseResult(
        await callTool(client, "ghci_eval", { expression: `double ${i + 10}` })
      );
      expect(r.output).toContain(String((i + 10) * 2));
    }
  });

  it("step 15: quickcheck after eval returns QC result, not eval output", async () => {
    const evalR = parseResult(
      await callTool(client, "ghci_eval", { expression: "double 99" })
    );
    expect(evalR.output).toContain("198");

    const qcR = parseResult(
      await callTool(client, "ghci_quickcheck", {
        property: "\\x -> double x == x + (x :: Int)",
      })
    );
    expect(qcR.success).toBe(true);
    expect(qcR.passed).toBe(100);
  });

  it("step 16: eval after quickcheck returns eval result, not QC output", async () => {
    await callTool(client, "ghci_quickcheck", {
      property: "\\x -> double x == x + (x :: Int)",
    });

    const r = parseResult(
      await callTool(client, "ghci_eval", { expression: "broken 3" })
    );
    expect(r.output).toContain("6");
    expect(r.output).not.toContain("OK");
    expect(r.output).not.toContain("passed");
  });
});
