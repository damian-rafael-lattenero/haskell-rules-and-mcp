import { describe, it, expect, beforeAll, afterAll } from "vitest";
import { execSync } from "node:child_process";
import path from "node:path";
import { GhciSession } from "../../ghci-session.js";
import { handleTypeCheck } from "../../tools/type-check.js";
import { handleTypeInfo } from "../../tools/type-info.js";
import { handleCheckModule } from "../../tools/check-module.js";
import { handleLoadModule } from "../../tools/load-module.js";
import { handleQuickCheck, resetQuickCheckState } from "../../tools/quickcheck.js";

const FIXTURE_DIR = path.resolve(import.meta.dirname, "../fixtures/test-project");
const GHCUP_BIN = path.join(process.env.HOME ?? "", ".ghcup", "bin");
const TEST_PATH = `${GHCUP_BIN}:${process.env.PATH}`;

const GHC_AVAILABLE = (() => {
  try {
    execSync("ghc --version", { stdio: "pipe", env: { ...process.env, PATH: TEST_PATH } });
    return true;
  } catch { return false; }
})();

describe.runIf(GHC_AVAILABLE)("Tool Handlers Integration", () => {
  let session: GhciSession;

  beforeAll(async () => {
    session = new GhciSession(FIXTURE_DIR, "lib:test-project");
    await session.start();
  }, 60_000);

  afterAll(async () => {
    if (session?.isAlive()) await session.kill();
  });

  // --- ghci_type ---
  describe("handleTypeCheck", () => {
    it("returns type of known function", async () => {
      const result = JSON.parse(await handleTypeCheck(session, { expression: "add" }));
      expect(result.success).toBe(true);
      expect(result.type).toContain("Int -> Int -> Int");
    });

    it("returns type of Prelude function", async () => {
      const result = JSON.parse(await handleTypeCheck(session, { expression: "map" }));
      expect(result.success).toBe(true);
      expect(result.type).toContain("->");
    });

    it("handles partial application", async () => {
      const result = JSON.parse(await handleTypeCheck(session, { expression: "add 1" }));
      expect(result.success).toBe(true);
      expect(result.type).toContain("Int -> Int");
    });

    it("handles lambda expression", async () => {
      const result = JSON.parse(await handleTypeCheck(session, { expression: "\\x -> x + (1 :: Int)" }));
      expect(result.success).toBe(true);
      expect(result.type).toContain("Int");
    });
  });

  // --- ghci_info ---
  describe("handleTypeInfo", () => {
    it("returns info for data type", async () => {
      const result = JSON.parse(await handleTypeInfo(session, { name: "Maybe" }));
      expect(result.success).toBe(true);
      expect(result.definition).toContain("Nothing");
      expect(result.definition).toContain("Just");
    });

    it("returns info for class", async () => {
      const result = JSON.parse(await handleTypeInfo(session, { name: "Eq" }));
      expect(result.success).toBe(true);
      expect(result.kind).toBe("class");
    });
  });

  // --- ghci_load ---
  describe("handleLoadModule", () => {
    it("loads a single module", async () => {
      const result = JSON.parse(await handleLoadModule(session, { module_path: "src/TestLib.hs" }));
      expect(result.success).toBe(true);
      expect(result.errors).toEqual([]);
    });

    it("loads with diagnostics", async () => {
      const result = JSON.parse(await handleLoadModule(session, { module_path: "src/TestLib.hs", diagnostics: true }));
      expect(result.success).toBe(true);
      expect(result).toHaveProperty("errors");
      expect(result).toHaveProperty("warnings");
      expect(result).toHaveProperty("warningActions");
      expect(result).toHaveProperty("holes");
    });

    it("reloads current modules", async () => {
      const result = JSON.parse(await handleLoadModule(session, {}));
      expect(result.success).toBe(true);
    });

    it("loads all library modules", async () => {
      const result = JSON.parse(await handleLoadModule(session, { load_all: true }, FIXTURE_DIR));
      expect(result.success).toBe(true);
      expect(result.modules).toBeDefined();
    });

    it("returns errors for nonexistent module", async () => {
      const result = JSON.parse(await handleLoadModule(session, { module_path: "src/DoesNotExist.hs" }));
      expect(result.success).toBe(false);
    });
  });

  // --- ghci_check_module ---
  describe("handleCheckModule", () => {
    it("returns definitions for TestLib", async () => {
      const result = JSON.parse(await handleCheckModule(session, { module_path: "src/TestLib.hs" }));
      expect(result.success).toBe(true);
      expect(result.compiled).toBe(true);
      const names = result.definitions.map((d: any) => d.name);
      expect(names).toContain("add");
      expect(names).toContain("greet");
    });
  });

  // --- ghci_quickcheck ---
  describe("handleQuickCheck", () => {
    beforeAll(() => resetQuickCheckState());

    it("passes a correct property", async () => {
      const result = JSON.parse(await handleQuickCheck(session, {
        property: "\\x y -> add x y == x + (y :: Int)",
      }));
      expect(result.success).toBe(true);
      expect(result.passed).toBe(100);
    });

    it("fails an incorrect property", async () => {
      const result = JSON.parse(await handleQuickCheck(session, {
        property: "\\x -> add x 0 == (0 :: Int)",
      }));
      expect(result.success).toBe(false);
      expect(result.counterexample).toBeDefined();
    });

    it("respects custom test count", async () => {
      const result = JSON.parse(await handleQuickCheck(session, {
        property: "\\x y -> add x y == x + (y :: Int)",
        tests: 50,
      }));
      expect(result.success).toBe(true);
      expect(result.passed).toBe(50);
    });

    it("rejects GHCi command injection", async () => {
      const result = JSON.parse(await handleQuickCheck(session, {
        property: ":! echo pwned",
      }));
      expect(result.success).toBe(false);
      expect(result.error).toContain("cannot start with ':'");
    });

    it("rejects overly long properties", async () => {
      const result = JSON.parse(await handleQuickCheck(session, {
        property: "x".repeat(2001),
      }));
      expect(result.success).toBe(false);
      expect(result.error).toContain("too long");
    });
  });
});
