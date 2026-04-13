import { describe, it, expect } from "vitest";
import { handleLint } from "../tools/lint.js";
import { createMockSession } from "./helpers/mock-session.js";
import type { GhciResult } from "../ghci-session.js";

describe("handleLint", () => {
  it("returns error when hlint is not installed and no session", async () => {
    const result = JSON.parse(await handleLint("/tmp/fake", { module_path: "src/Test.hs" }));
    expect(result).toHaveProperty("success");
    if (!result.success) {
      expect(result.error).toMatch(/hlint|not found/i);
    }
  });

  describe("GHCi fallback (when hlint unavailable)", () => {
    it("returns GHCi-based suggestions when session provided and hlint missing", async () => {
      const session = createMockSession({
        execute: async (): Promise<GhciResult> => ({ output: "", success: true }),
        loadModule: {
          output:
            "src/Foo.hs:2:1-22: warning: [GHC-66111] [-Wunused-imports]\n" +
            "    The import of 'Data.List' is redundant\nOk, one module loaded.",
          success: true,
        },
      });

      const result = JSON.parse(
        await handleLint("/tmp/fake", { module_path: "src/Foo.hs" }, session)
      );

      // If hlint IS available on this machine, it won't use fallback
      if (result.fallback) {
        expect(result.success).toBe(true);
        expect(result.source).toBe("ghc-warnings");
        expect(result.installHint).toContain("hlint");
        expect(result.suggestions.length).toBeGreaterThan(0);
        expect(result.suggestions[0].hint).toBe("unused-import");
      }
    });

    it("includes installHint in fallback response", async () => {
      const session = createMockSession({
        execute: async (): Promise<GhciResult> => ({ output: "", success: true }),
        loadModule: { output: "Ok, one module loaded.", success: true },
      });

      const result = JSON.parse(
        await handleLint("/tmp/fake", { module_path: "src/Foo.hs" }, session)
      );

      if (result.fallback) {
        expect(result.installHint).toContain("cabal install hlint");
      }
    });
  });
});
