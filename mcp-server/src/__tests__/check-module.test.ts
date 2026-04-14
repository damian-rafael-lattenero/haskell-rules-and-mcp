import { describe, it, expect, vi } from "vitest";
import { handleCheckModule } from "../tools/check-module.js";
import { createMockSession } from "./helpers/mock-session.js";
import type { GhciResult } from "../ghci-session.js";

describe("handleCheckModule", () => {
  function makeSession(browseOutput: string, loadSuccess = true) {
    let callCount = 0;
    return createMockSession({
      loadModule: { output: loadSuccess ? "Ok, one module loaded." : "Failed, no modules loaded.", success: loadSuccess },
      execute: async (cmd: string): Promise<GhciResult> => {
        if (cmd.startsWith(":set")) return { output: "", success: true };
        if (cmd.startsWith(":browse")) return { output: browseOutput, success: true };
        return { output: "", success: true };
      },
    });
  }

  it("parses function definitions", async () => {
    const session = makeSession("double :: Num a => a -> a\ntriple :: Int -> Int");
    const result = JSON.parse(await handleCheckModule(session, { module_path: "src/Foo.hs", module_name: "Foo" }));
    expect(result.success).toBe(true);
    expect(result.definitions).toHaveLength(2);
    expect(result.definitions[0].name).toBe("double");
    expect(result.definitions[0].type).toBe("Num a => a -> a");
    expect(result.definitions[0].kind).toBe("function");
    expect(result.definitions[1].name).toBe("triple");
  });

  it("parses class with methods (Bug Fix 5)", async () => {
    const browse = "type Container :: (* -> *) -> Constraint\nclass Container f where\n  empty :: f a\n  insert :: a -> f a -> f a\n  {-# MINIMAL empty, insert #-}";
    const session = makeSession(browse);
    const result = JSON.parse(await handleCheckModule(session, { module_path: "src/K.hs", module_name: "K" }));
    expect(result.success).toBe(true);
    const names = result.definitions.map((d: any) => d.name);
    expect(names).toContain("Container");
    expect(names).toContain("empty");
    expect(names).toContain("insert");
    // Verify methods have correct types (not concatenated)
    const emptyDef = result.definitions.find((d: any) => d.name === "empty");
    expect(emptyDef.type).toBe("f a");
    const insertDef = result.definitions.find((d: any) => d.name === "insert");
    expect(insertDef.type).toBe("a -> f a -> f a");
  });

  it("parses data type with kind annotation", async () => {
    const browse = "type Maybe :: * -> *\ndata Maybe a = Nothing | Just a";
    const session = makeSession(browse);
    const result = JSON.parse(await handleCheckModule(session, { module_path: "src/M.hs", module_name: "M" }));
    const maybeDef = result.definitions.find((d: any) => d.name === "Maybe");
    expect(maybeDef).toBeDefined();
  });

  it("parses newtype", async () => {
    const browse = "type Wrap :: (* -> *) -> * -> *\nnewtype Wrap f a = Wrap (f a)";
    const session = makeSession(browse);
    const result = JSON.parse(await handleCheckModule(session, { module_path: "src/W.hs", module_name: "W" }));
    expect(result.success).toBe(true);
  });

  it("handles module with compilation error", async () => {
    const session = createMockSession({
      loadModule: {
        output: "src/Bad.hs:3:7-10: error: [GHC-83865]\n    Couldn't match expected type",
        success: false,
      },
      execute: async (): Promise<GhciResult> => ({ output: "", success: true }),
    });
    const result = JSON.parse(await handleCheckModule(session, { module_path: "src/Bad.hs" }));
    expect(result.success).toBe(false);
    expect(result.compiled).toBe(false);
    expect(result.errors.length).toBeGreaterThan(0);
  });

  it("infers module name from path", async () => {
    const session = makeSession("foo :: Int -> Int");
    const result = JSON.parse(await handleCheckModule(session, { module_path: "src/HM/Infer.hs" }));
    expect(result.module).toBe("HM.Infer");
  });

  it("handles empty browse output", async () => {
    const session = makeSession("");
    const result = JSON.parse(await handleCheckModule(session, { module_path: "src/Empty.hs", module_name: "Empty" }));
    expect(result.success).toBe(true);
    expect(result.definitions).toEqual([]);
  });

  it("handles class without methods (no where)", async () => {
    const browse = "class Eq a";
    const session = makeSession(browse);
    const result = JSON.parse(await handleCheckModule(session, { module_path: "src/E.hs", module_name: "E" }));
    const eq = result.definitions.find((d: any) => d.name === "Eq");
    expect(eq).toBeDefined();
    expect(eq.kind).toBe("class");
  });

  it("does not concatenate pragma into function type", async () => {
    const browse = "class Monad m where\n  return :: a -> m a\n  (>>=) :: m a -> (a -> m b) -> m b\n  {-# MINIMAL (>>=) #-}";
    const session = makeSession(browse);
    const result = JSON.parse(await handleCheckModule(session, { module_path: "src/M.hs", module_name: "M" }));
    const returnDef = result.definitions.find((d: any) => d.name === "return");
    expect(returnDef).toBeDefined();
    expect(returnDef.type).toBe("a -> m a");
    expect(returnDef.type).not.toContain("MINIMAL");
  });

  it("handles summary counts correctly", async () => {
    const browse = "class Container f where\n  empty :: f a\nfoo :: Int -> Int\ntype Bar :: *\ndata Bar = MkBar";
    const session = makeSession(browse);
    const result = JSON.parse(await handleCheckModule(session, { module_path: "src/S.hs", module_name: "S" }));
    expect(result.summary.classes).toBeGreaterThanOrEqual(1);
    expect(result.summary.functions).toBeGreaterThanOrEqual(1);
  });

  // ─── Bug fix: GHC-32850 must not appear in warnings array ──────────────────

  describe("GHC-32850 suppression (Bug Fix 4)", () => {
    it("does not include GHC-32850 (-Wmissing-home-modules) in warnings", async () => {
      // GHC-32850 is a GHCi session artifact that fires when a single module is
      // loaded instead of the full package via 'cabal repl'.  It is NOT a real
      // code warning and must be suppressed so the LLM doesn't try to fix it.
      const sessionWithArtifact = createMockSession({
        loadModule: {
          output:
            "<no location info>: warning: [GHC-32850] [-Wmissing-home-modules]\n" +
            "    These modules are needed for compilation but not listed in your .cabal file\n" +
            "Ok, one module loaded.",
          success: true,
        },
        execute: async (cmd: string): Promise<GhciResult> => {
          if (cmd.startsWith(":browse")) return { output: "foo :: Int -> Int", success: true };
          return { output: "", success: true };
        },
      });

      const result = JSON.parse(
        await handleCheckModule(sessionWithArtifact, { module_path: "src/Foo.hs", module_name: "Foo" })
      );

      expect(result.success).toBe(true);
      const warningCodes = (result.warnings ?? []).map((w: { code?: string }) => w.code);
      expect(warningCodes).not.toContain("GHC-32850");
      expect(result.summary.warnings).toBe(0);
    });

    it("keeps real warnings that are not GHC-32850", async () => {
      const sessionWithRealWarning = createMockSession({
        loadModule: {
          output:
            "src/Foo.hs:2:1-22: warning: [GHC-66111] [-Wunused-imports]\n" +
            "    The import of 'Data.List' is redundant\n" +
            "<no location info>: warning: [GHC-32850] [-Wmissing-home-modules]\n" +
            "    These modules are needed for compilation\n" +
            "Ok, one module loaded.",
          success: true,
        },
        execute: async (cmd: string): Promise<GhciResult> => {
          if (cmd.startsWith(":browse")) return { output: "foo :: Int -> Int", success: true };
          return { output: "", success: true };
        },
      });

      const result = JSON.parse(
        await handleCheckModule(sessionWithRealWarning, { module_path: "src/Foo.hs", module_name: "Foo" })
      );

      expect(result.success).toBe(true);
      const warningCodes = (result.warnings ?? []).map((w: { code?: string }) => w.code);
      expect(warningCodes).not.toContain("GHC-32850");
      // The unused-import warning should still be present
      expect(warningCodes).toContain("GHC-66111");
      expect(result.summary.warnings).toBe(1);
    });
  });
});
