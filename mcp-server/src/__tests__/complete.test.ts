import { describe, it, expect } from "vitest";
import { handleComplete } from "../tools/complete.js";
import { createMockSession } from "./helpers/mock-session.js";

describe("handleComplete", () => {
  it("returns completions for valid prefix", async () => {
    const session = createMockSession({
      completionsOf: {
        output: `3 3 "ma"\n"map"\n"mapM"\n"max"`,
        success: true,
      },
    });
    const result = JSON.parse(await handleComplete(session, { prefix: "ma" }));
    expect(result.success).toBe(true);
    expect(result.completions).toContain("map");
    expect(result.total).toBe(3);
    expect(result.prefix).toBe("ma");
  });

  it("returns empty for no matches", async () => {
    const session = createMockSession({
      completionsOf: {
        output: `0 0 "zzz"`,
        success: true,
      },
    });
    const result = JSON.parse(await handleComplete(session, { prefix: "zzz" }));
    expect(result.success).toBe(true);
    expect(result.completions).toEqual([]);
  });

  it("handles session error", async () => {
    const session = createMockSession({
      completionsOf: { output: "Error: something", success: false },
    });
    const result = JSON.parse(await handleComplete(session, { prefix: "x" }));
    expect(result.success).toBe(false);
  });
});
