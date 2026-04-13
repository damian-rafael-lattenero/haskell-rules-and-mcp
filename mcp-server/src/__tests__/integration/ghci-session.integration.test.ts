import { describe, it, expect, beforeAll, afterAll } from "vitest";
import { execSync } from "node:child_process";
import path from "node:path";
import { GhciSession } from "../../ghci-session.js";

const FIXTURE_DIR = path.resolve(
  import.meta.dirname,
  "../fixtures/test-project"
);

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

  beforeAll(async () => {
    session = new GhciSession(FIXTURE_DIR, "lib:test-project");
    await session.start();
  }, 60_000);

  afterAll(async () => {
    if (session?.isAlive()) {
      await session.kill();
    }
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
});
