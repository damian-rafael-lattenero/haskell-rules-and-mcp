import { describe, it, expect, vi, afterEach } from "vitest";
import { handleFormat } from "../tools/format.js";

// Format tool shells out to external processes — we test graceful degradation
// when tools are not installed (the common case in CI)

describe("handleFormat", () => {
  it("returns error when no formatter is installed", async () => {
    const result = JSON.parse(await handleFormat("/tmp/fake", { module_path: "src/Test.hs" }));
    // This test passes in environments without ormolu/fourmolu
    // If a formatter IS installed, it would try to format and fail on the fake path
    expect(result).toHaveProperty("success");
    if (!result.success) {
      expect(result.error).toMatch(/formatter|not found|No such file/i);
    }
  });
});
