import { describe, it, expect } from "vitest";
import { handleLint } from "../tools/lint.js";

describe("handleLint", () => {
  it("returns unavailable when hlint is not available", async () => {
    const result = JSON.parse(await handleLint("/tmp/fake", { module_path: "src/Test.hs" }));
    if (result.success) {
      expect(result.lint_tool).toBe("hlint");
    } else {
      expect(result.unavailable).toBe(true);
      expect(result.error).toMatch(/hlint|not available|not found/i);
    }
  });

  it("does not use fallback lint when hlint is unavailable even with session", async () => {
    const result = JSON.parse(await handleLint("/tmp/fake", { module_path: "src/Foo.hs" }, {}));
    if (!result.success) {
      expect(result.fallback).toBeUndefined();
      expect(result.unavailable).toBe(true);
    }
  });
});
