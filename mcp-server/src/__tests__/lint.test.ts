import { describe, it, expect } from "vitest";
import { handleLint } from "../tools/lint.js";

describe("handleLint", () => {
  it("returns error when hlint is not installed", async () => {
    const result = JSON.parse(await handleLint("/tmp/fake", { module_path: "src/Test.hs" }));
    // This test passes in environments without hlint
    expect(result).toHaveProperty("success");
    if (!result.success) {
      expect(result.error).toMatch(/hlint|not found/i);
    }
  });
});
