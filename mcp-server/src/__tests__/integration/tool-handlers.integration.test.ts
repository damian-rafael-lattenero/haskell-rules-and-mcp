import { describe, it, expect, beforeAll, afterAll } from "vitest";
import { execSync } from "node:child_process";
import path from "node:path";
import { GhciSession } from "../../ghci-session.js";
import { handleTypeCheck } from "../../tools/type-check.js";
import { handleTypeInfo } from "../../tools/type-info.js";
import { handleCheckModule } from "../../tools/check-module.js";
import { handleLoadModule } from "../../tools/load-module.js";
import { handleQuickCheck, resetQuickCheckState } from "../../tools/quickcheck.js";
import { handleGoto } from "../../tools/goto.js";
import { handleComplete } from "../../tools/complete.js";
import { handleDoc } from "../../tools/doc.js";
import { handleImports } from "../../tools/imports.js";
import { handleReferences } from "../../tools/references.js";
import { handleRename } from "../../tools/rename.js";

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

  // --- ghci_goto ---
  describe("handleGoto", () => {
    it("finds local definition", async () => {
      const result = JSON.parse(await handleGoto(session, { name: "add" }));
      expect(result.success).toBe(true);
      expect(result.location).toBeDefined();
      expect(result.location.file).toContain("TestLib.hs");
      expect(result.location.line).toBeGreaterThan(0);
    });

    it("finds library definition", async () => {
      const result = JSON.parse(await handleGoto(session, { name: "map" }));
      expect(result.success).toBe(true);
      expect(result.location).toBeDefined();
    });

    it("returns error for nonexistent name", async () => {
      const result = JSON.parse(await handleGoto(session, { name: "nonExistentXYZ" }));
      expect(result.success).toBe(false);
    });
  });

  // --- ghci_complete ---
  describe("handleComplete", () => {
    it("returns completions for a prefix", async () => {
      const result = JSON.parse(await handleComplete(session, { prefix: "ad" }));
      expect(result.success).toBe(true);
      expect(result.completions).toContain("add");
    });

    it("returns empty for no matches", async () => {
      const result = JSON.parse(await handleComplete(session, { prefix: "zzzzNonExistent" }));
      expect(result.success).toBe(true);
      expect(result.completions).toHaveLength(0);
    });
  });

  // --- ghci_doc ---
  describe("handleDoc", () => {
    it("handles doc lookup without crashing", async () => {
      const result = JSON.parse(await handleDoc(session, { name: "map" }));
      expect(result.success).toBe(true);
      expect(result.name).toBe("map");
    });
  });

  // --- ghci_imports ---
  describe("handleImports", () => {
    it("returns current imports", async () => {
      const result = JSON.parse(await handleImports(session));
      expect(result.success).toBe(true);
      expect(result.imports).toBeDefined();
      expect(result.count).toBeGreaterThanOrEqual(0);
    });
  });

  // --- ghci_references ---
  describe("handleReferences", () => {
    it("finds references to add", async () => {
      const result = JSON.parse(await handleReferences(FIXTURE_DIR, { name: "add" }));
      expect(result.success).toBe(true);
      expect(result.count).toBeGreaterThan(0);
    });
  });

  // --- ghci_rename ---
  describe("handleRename", () => {
    it("previews rename", async () => {
      const result = JSON.parse(await handleRename(FIXTURE_DIR, { oldName: "add", newName: "addInts" }));
      expect(result.success).toBe(true);
      expect(result.totalReferences).toBeGreaterThan(0);
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

    it("stores tests_module in property store when provided", async () => {
      const { mkdtemp, rm } = await import("node:fs/promises");
      const { loadStore } = await import("../../property-store.js");
      const os = await import("node:os");
      const pathMod = await import("node:path");
      const tmpDir = await mkdtemp(pathMod.join(os.tmpdir(), "qc-tests-module-int-"));
      try {
        resetQuickCheckState();
        await handleQuickCheck(
          session,
          {
            property: "\\x y -> add x y == x + (y :: Int)",
            module_path: "src/TestModule.hs",
            tests_module: "src/AnotherModule.hs",
          },
          undefined,
          tmpDir
        );
        const store = await loadStore(tmpDir);
        expect(store.properties).toHaveLength(1);
        expect(store.properties[0]!.module).toBe("src/TestModule.hs");
        expect(store.properties[0]!.tests_module).toBe("src/AnotherModule.hs");
      } finally {
        await rm(tmpDir, { recursive: true, force: true });
      }
    });
  });

  describe("GHC-32850 quirk isolation", () => {
    it("ghci_load raw output does not contain GHC-32850 when single module loaded", async () => {
      const result = JSON.parse(
        await handleLoadModule(session, { module_path: "src/TestModule.hs", diagnostics: true }, FIXTURE_DIR)
      );
      // raw must never contain the quirk warning
      expect(result.raw ?? "").not.toContain("GHC-32850");
    });
  });
});
