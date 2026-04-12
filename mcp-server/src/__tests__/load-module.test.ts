import { describe, it, expect } from "vitest";
import { handleLoadModule } from "../tools/load-module.js";
import { createMockSession } from "./helpers/mock-session.js";
import type { GhciResult } from "../ghci-session.js";

/**
 * Helper to create a mock session for load-module tests.
 * The load module handler calls execute() for :set commands and loadModule/reload.
 */
function makeLoadSession(opts: {
  loadOutput?: string;
  loadSuccess?: boolean;
  reloadOutput?: string;
  reloadSuccess?: boolean;
}) {
  return createMockSession({
    execute: async (_cmd: string): Promise<GhciResult> => {
      // :set commands always succeed
      return { output: "", success: true };
    },
    loadModule: {
      output: opts.loadOutput ?? "Ok, one module loaded.",
      success: opts.loadSuccess ?? true,
    },
    reload: {
      output: opts.reloadOutput ?? "Ok, one module loaded.",
      success: opts.reloadSuccess ?? true,
    },
  });
}

describe("handleLoadModule", () => {
  // --- Plain reload ---
  describe("plain reload (no args)", () => {
    it("succeeds with clean compilation", async () => {
      const session = makeLoadSession({ reloadOutput: "Ok, one module loaded." });
      const result = JSON.parse(await handleLoadModule(session, {}));
      expect(result.success).toBe(true);
      expect(result.errors).toEqual([]);
      expect(result.warnings).toEqual([]);
    });

    it("returns warnings on reload", async () => {
      const session = makeLoadSession({
        reloadOutput:
          "[1 of 1] Compiling Foo ( src/Foo.hs, interpreted )\n" +
          "src/Foo.hs:3:1-6: warning: [GHC-38417] [-Wmissing-signatures]\n" +
          "    Top-level binding with no type signature:\n" +
          "      foo :: Int\nOk, one module loaded.",
      });
      const result = JSON.parse(await handleLoadModule(session, {}));
      expect(result.success).toBe(true);
      expect(result.warnings.length).toBeGreaterThan(0);
    });
  });

  // --- Single module ---
  describe("single module", () => {
    it("loads module successfully", async () => {
      const session = makeLoadSession({});
      const result = JSON.parse(await handleLoadModule(session, { module_path: "src/Foo.hs" }));
      expect(result.success).toBe(true);
    });

    it("detects type errors", async () => {
      const session = makeLoadSession({
        loadOutput:
          "src/Foo.hs:5:9-12: error: [GHC-83865]\n" +
          "    \u2022 Couldn\u2019t match expected type \u2018Int\u2019 with actual type \u2018Bool\u2019\n" +
          "    \u2022 In the expression: True\nFailed, no modules loaded.",
        loadSuccess: false,
      });
      const result = JSON.parse(await handleLoadModule(session, { module_path: "src/Foo.hs" }));
      expect(result.success).toBe(false);
      expect(result.errors.length).toBeGreaterThan(0);
      expect(result.errors[0].code).toBe("GHC-83865");
    });

    it("detects 'Can't find' error for nonexistent file (Bug Fix 3)", async () => {
      const session = makeLoadSession({
        loadOutput: "<no location info>: error: [GHC-49196] Can't find src/NoExiste.hs\n\nFailed, unloaded all modules.",
        loadSuccess: false,
      });
      const result = JSON.parse(await handleLoadModule(session, { module_path: "src/NoExiste.hs" }));
      expect(result.success).toBe(false);
      expect(result.errors.length).toBeGreaterThan(0);
      expect(result.errors[0].message).toContain("Can't find");
    });

    it("detects warnings with categories", async () => {
      const session = makeLoadSession({
        loadOutput:
          "src/Foo.hs:2:1-22: warning: [GHC-66111] [-Wunused-imports]\n" +
          "    The import of \u2018Data.List\u2019 is redundant\nOk, one module loaded.",
      });
      const result = JSON.parse(await handleLoadModule(session, { module_path: "src/Foo.hs" }));
      expect(result.success).toBe(true);
      expect(result.warningActions.length).toBeGreaterThan(0);
      expect(result.warningActions[0].category).toBe("unused-import");
    });
  });

  // --- Diagnostics mode ---
  describe("diagnostics mode", () => {
    it("defaults to diagnostics=true for module_path", async () => {
      const session = makeLoadSession({});
      const result = JSON.parse(await handleLoadModule(session, { module_path: "src/Foo.hs" }));
      // Diagnostics runs dual-pass — we verify the response has the expected structure
      expect(result).toHaveProperty("errors");
      expect(result).toHaveProperty("warnings");
      expect(result).toHaveProperty("warningActions");
      expect(result).toHaveProperty("holes");
    });

    it("defaults to diagnostics=false for plain reload", async () => {
      const session = makeLoadSession({});
      const result = JSON.parse(await handleLoadModule(session, {}));
      expect(result).toHaveProperty("errors");
      expect(result).toHaveProperty("warnings");
    });

    it("can be explicitly disabled", async () => {
      const session = makeLoadSession({});
      const result = JSON.parse(await handleLoadModule(session, { module_path: "src/Foo.hs", diagnostics: false }));
      expect(result.success).toBe(true);
    });
  });

  // --- Response format ---
  describe("response format", () => {
    it("includes summary on success", async () => {
      const session = makeLoadSession({});
      const result = JSON.parse(await handleLoadModule(session, { module_path: "src/Foo.hs" }));
      expect(result.summary).toBeTruthy();
    });

    it("includes summary on error", async () => {
      const session = makeLoadSession({
        loadOutput: "src/Foo.hs:1:1: error: [GHC-83865]\n    Type error\nFailed.",
        loadSuccess: false,
      });
      const result = JSON.parse(await handleLoadModule(session, { module_path: "src/Foo.hs" }));
      expect(result.summary).toBeTruthy();
    });

    it("includes raw output", async () => {
      const session = makeLoadSession({ loadOutput: "Ok, one module loaded." });
      const result = JSON.parse(await handleLoadModule(session, { module_path: "src/Foo.hs" }));
      expect(result.raw).toBeTruthy();
    });
  });

  // --- Hole detection ---
  describe("typed holes", () => {
    it("detects typed holes in diagnostics mode", async () => {
      const holeOutput =
        "src/Foo.hs:5:9: warning: [GHC-88464] [-Wtyped-holes]\n" +
        "    \u2022 Found hole: _ :: Int\n" +
        "    \u2022 In an equation for 'foo': foo x = _\n" +
        "    \u2022 Relevant bindings include\n" +
        "        x :: String (bound at src/Foo.hs:5:5)\n" +
        "        foo :: String -> Int (bound at src/Foo.hs:5:1)\n" +
        "      Valid hole fits include\n" +
        "        maxBound :: forall a. Bounded a => a\n" +
        "          with maxBound @Int\n" +
        "   |\n" +
        "5 |   foo x = _\n" +
        "   |           ^\n" +
        "Ok, one module loaded.";

      // For dual-pass, the second pass (deferred) has holes.
      // Simulate: strict pass succeeds clean, deferred pass has holes.
      let loadCount = 0;
      const session = createMockSession({
        execute: async (): Promise<GhciResult> => ({ output: "", success: true }),
        loadModule: async (): Promise<GhciResult> => {
          loadCount++;
          if (loadCount === 1) {
            // Strict pass - no errors
            return { output: "Ok, one module loaded.", success: true };
          }
          // Deferred pass - has holes
          return { output: holeOutput, success: true };
        },
      });

      const result = JSON.parse(await handleLoadModule(session, { module_path: "src/Foo.hs", diagnostics: true }));
      expect(result.success).toBe(true);
      expect(result.holes.length).toBeGreaterThan(0);
      expect(result.holes[0].expectedType).toBe("Int");
      expect(result.holes[0].relevantBindings.length).toBeGreaterThan(0);
    });
  });
});
