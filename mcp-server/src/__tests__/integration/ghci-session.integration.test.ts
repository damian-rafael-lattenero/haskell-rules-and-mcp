import { describe, it, expect, beforeAll, afterAll } from "vitest";
import { execSync } from "node:child_process";
import path from "node:path";
import { GhciSession } from "../../ghci-session.js";
import { setupIsolatedFixture, type IsolatedFixture } from "../helpers/isolated-fixture.js";

// Extend PATH to include ghcup bin for GHC detection
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

describe.runIf(GHC_AVAILABLE)("GHCi Session Integration", () => {
  let session: GhciSession;
  let fixture: IsolatedFixture;
  let FIXTURE_DIR: string;

  beforeAll(async () => {
    fixture = await setupIsolatedFixture("test-project", "ghci-session");
    FIXTURE_DIR = fixture.dir;
    session = new GhciSession(FIXTURE_DIR, "lib:test-project");
    await session.start();
  }, 120_000);

  afterAll(async () => {
    if (session?.isAlive()) {
      await session.kill();
    }
    await fixture.cleanup();
  });

  it("starts and reports alive", () => {
    expect(session.isAlive()).toBe(true);
  });

  it("gets type of a function", async () => {
    const result = await session.typeOf("add");
    expect(result.success).toBe(true);
    expect(result.output).toContain("Int -> Int -> Int");
  });

  it("evaluates an expression", async () => {
    const result = await session.execute("add 1 2");
    expect(result.success).toBe(true);
    expect(result.output).toContain("3");
  });

  it("loads a module", async () => {
    const result = await session.loadModule("src/TestLib.hs");
    expect(result.success).toBe(true);
  });

  it("reloads modules", async () => {
    const result = await session.reload();
    expect(result.success).toBe(true);
  });

  it("gets info about a function", async () => {
    const result = await session.infoOf("add");
    expect(result.success).toBe(true);
    expect(result.output).toContain("Int -> Int -> Int");
  });

  it("runs quickcheck property", async () => {
    await session.execute("import Test.QuickCheck");
    const result = await session.execute(
      "quickCheck (\\x y -> add x y == x + (y :: Int))"
    );
    expect(result.output).toContain("OK");
  });

  it("handles type error gracefully", async () => {
    const result = await session.execute(":t True + 1");
    // This should produce an error, not crash
    expect(result).toBeDefined();
  });

  it("survives restart", async () => {
    await session.restart();
    expect(session.isAlive()).toBe(true);
    const result = await session.typeOf("add");
    expect(result.success).toBe(true);
  });

  // --- kindOf ---
  it("gets kind of a type constructor", async () => {
    const result = await session.kindOf("Maybe");
    expect(result.success).toBe(true);
    expect(result.output).toContain("* -> *");
  });

  it("gets kind of base type", async () => {
    const result = await session.kindOf("Int");
    expect(result.success).toBe(true);
    expect(result.output).toContain("*");
  });

  // --- executeBatch ---
  it("executes batch commands", async () => {
    const { results, allSuccess } = await session.executeBatch([":t add", "add 1 2", ":t greet"]);
    expect(allSuccess).toBe(true);
    expect(results).toHaveLength(3);
    expect(results[0].output).toContain("Int");
    expect(results[1].output).toContain("3");
    expect(results[2].output).toContain("String");
  });

  it("batch with reload", async () => {
    const { results, allSuccess } = await session.executeBatch([":t add"], { reload: true });
    expect(allSuccess).toBe(true);
    expect(results[0].output).toContain("Int");
  });

  it("batch stop on error", async () => {
    // Use :l with nonexistent file — this produces "error:" in output
    // (unlike runtime exceptions which use "*** Exception:" format)
    const { results, allSuccess } = await session.executeBatch(
      ["1 + 1", ":l nonexistent_file_xyz.hs", "2 + 2"],
      { stopOnError: true }
    );
    expect(allSuccess).toBe(false);
    expect(results.length).toBeLessThanOrEqual(2);
  });

  // --- loadModules ---
  it("loads multiple modules", async () => {
    const result = await session.loadModules(["src/TestLib.hs"], ["TestLib"]);
    expect(result.success).toBe(true);
  });

  // --- isAlive after kill ---
  it("reports not alive after kill", async () => {
    const tempSession = new GhciSession(FIXTURE_DIR, "lib:test-project");
    await tempSession.start();
    expect(tempSession.isAlive()).toBe(true);
    await tempSession.kill();
    expect(tempSession.isAlive()).toBe(false);
  }, 60_000);

  // --- Fix 1: Command queue serializes concurrent calls ---
  it("handles concurrent execute calls via command queue", async () => {
    const [r1, r2, r3] = await Promise.all([
      session.execute("1 + 1"),
      session.execute("2 + 2"),
      session.execute("3 + 3"),
    ]);
    expect(r1.output).toContain("2");
    expect(r2.output).toContain("4");
    expect(r3.output).toContain("6");
  });

  it("handles concurrent typeOf and infoOf", async () => {
    const [typeResult, infoResult] = await Promise.all([
      session.typeOf("add"),
      session.infoOf("add"),
    ]);
    expect(typeResult.success).toBe(true);
    expect(typeResult.output).toContain("Int -> Int -> Int");
    expect(infoResult.success).toBe(true);
    expect(infoResult.output).toContain("Int -> Int -> Int");
  });

  // --- Fix 2: Leading whitespace preserved in eval output ---
  it("preserves leading spaces in eval output", async () => {
    const result = await session.execute('putStrLn "  hello"');
    expect(result.output).toMatch(/^ {2}hello/);
  });

  // ==========================================================================
  // Fix 9: Sentinel sync — EXHAUSTIVE off-by-one tests
  // These tests verify that EVERY execute() returns its OWN result,
  // not the previous command's. Each test targets a different scenario.
  // ==========================================================================

  it("10 sequential putStrLn commands return correct results (no offset)", async () => {
    const results: string[] = [];
    for (let i = 1; i <= 10; i++) {
      const r = await session.execute(`putStrLn "val-${i}"`);
      results.push(r.output.trim());
    }
    expect(results).toEqual(
      Array.from({ length: 10 }, (_, i) => `val-${i + 1}`)
    );
  });

  it("typeOf returns type, never reload output", async () => {
    // Call typeOf multiple times — it uses reloadThenExecute internally
    for (let i = 0; i < 3; i++) {
      const result = await session.typeOf("add");
      expect(result.success).toBe(true);
      expect(result.output).toContain("Int -> Int -> Int");
      expect(result.output).not.toContain("Ok,");
      expect(result.output).not.toContain("module");
    }
  });

  it("eval after load returns eval result, not load output", async () => {
    await session.loadModule("src/TestLib.hs");
    const result = await session.execute("add 10 20");
    // Must be "30", not compilation output
    expect(result.output).toContain("30");
    expect(result.output).not.toContain("Compiling");
    expect(result.output).not.toContain("Ok,");
  });

  it("alternating load → eval → load → eval stays aligned", async () => {
    for (let i = 0; i < 3; i++) {
      await session.loadModule("src/TestLib.hs");
      const r = await session.execute(`putStrLn "iter-${i}"`);
      expect(r.output.trim()).toBe(`iter-${i}`);
    }
  });

  it("typeOf after eval returns type, not eval result", async () => {
    const evalResult = await session.execute('putStrLn "EVAL_OUTPUT"');
    expect(evalResult.output.trim()).toBe("EVAL_OUTPUT");

    const typeResult = await session.typeOf("add");
    expect(typeResult.output).toContain("Int -> Int -> Int");
    expect(typeResult.output).not.toContain("EVAL_OUTPUT");
  });

  it("eval after typeOf returns eval result, not type", async () => {
    const typeResult = await session.typeOf("greet");
    expect(typeResult.output).toContain("String -> String");

    const evalResult = await session.execute('putStrLn "AFTER_TYPE"');
    expect(evalResult.output.trim()).toBe("AFTER_TYPE");
    expect(evalResult.output).not.toContain("String -> String");
  });

  it("infoOf returns info, not previous output", async () => {
    await session.execute('putStrLn "BEFORE_INFO"');
    const info = await session.infoOf("add");
    expect(info.output).toContain("Int -> Int -> Int");
    expect(info.output).not.toContain("BEFORE_INFO");
  });

  it("rapid fire: 20 distinct commands all return correct results", async () => {
    const expected: string[] = [];
    const actual: string[] = [];
    for (let i = 0; i < 20; i++) {
      const tag = `rapid-${String(i).padStart(2, "0")}`;
      expected.push(tag);
      const r = await session.execute(`putStrLn "${tag}"`);
      actual.push(r.output.trim());
    }
    expect(actual).toEqual(expected);
  });

  it("fresh session: first 5 commands all correct (no init offset)", async () => {
    const fresh = new GhciSession(FIXTURE_DIR, "lib:test-project");
    await fresh.start();
    try {
      const results: string[] = [];
      for (let i = 0; i < 5; i++) {
        const r = await fresh.execute(`putStrLn "fresh-${i}"`);
        results.push(r.output.trim());
      }
      expect(results).toEqual(["fresh-0", "fresh-1", "fresh-2", "fresh-3", "fresh-4"]);
    } finally {
      await fresh.kill();
    }
  }, 60_000);

  it("after restart: commands return correct results immediately", async () => {
    await session.restart();
    const r1 = await session.execute('putStrLn "post-restart-1"');
    expect(r1.output.trim()).toBe("post-restart-1");
    const r2 = await session.execute('putStrLn "post-restart-2"');
    expect(r2.output.trim()).toBe("post-restart-2");
    // Restore session state for remaining tests
    await session.loadModule("src/TestLib.hs");
  });

  // --- Bug 5: Persistent imports survive reload ---
  it("persistent import survives :r reload", async () => {
    await session.addPersistentImport("import Data.List (sort)");
    const before = await session.execute("sort [3,1,2]");
    expect(before.output).toContain("[1,2,3]");

    // Reload — should re-apply the import
    await session.reload();
    const after = await session.execute("sort [5,3,1]");
    expect(after.output).toContain("[1,3,5]");
  });

  it("persistent import survives :l loadModule", async () => {
    await session.addPersistentImport("import Data.Char (toUpper)");
    await session.loadModule("src/TestLib.hs");
    const result = await session.execute("toUpper 'a'");
    expect(result.output).toContain("'A'");
  });

  // --- Common extensions enabled at init ---
  it("ScopedTypeVariables is enabled by default", async () => {
    const result = await session.execute(':set | grep "ScopedTypeVariables"');
    // ScopedTypeVariables should be on — if not, the grep would return nothing
    // Alternative check: use it directly
    const testResult = await session.execute('(\\(x :: Int) -> x + 1) 5');
    expect(testResult.output).toContain("6");
  });
});
