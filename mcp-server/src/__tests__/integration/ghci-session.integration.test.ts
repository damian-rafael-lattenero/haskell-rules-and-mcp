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
});
