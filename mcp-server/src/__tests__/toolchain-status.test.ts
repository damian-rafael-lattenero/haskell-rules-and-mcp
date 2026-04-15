import { describe, expect, it } from "vitest";
import { handleToolchainStatus } from "../tools/toolchain-status.js";

describe("handleToolchainStatus", () => {
  it("returns release matrix diagnostics without runtime probes", async () => {
    const parsed = JSON.parse(
      await handleToolchainStatus({ include_matrix: true, include_runtime: false })
    );
    expect(parsed.success).toBe(true);
    expect(Array.isArray(parsed.releaseMatrix)).toBe(true);
    expect(parsed.releaseMatrixSummary.total).toBeGreaterThan(0);
  });
});
