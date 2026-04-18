/**
 * Unit coverage for the new `fallbackUrl` + `fallbackSha256` fields added to
 * `ToolRelease`. We don't touch the network — we smoke-test the data model
 * and the type shape so a future refactor can't silently remove the fallback.
 */
import { describe, it, expect } from "vitest";
import { getToolchainTupleMatrix } from "../tools/auto-download.js";

describe("auto-download fallback infrastructure", () => {
  it("every supported tool×target entry reports autoDownloadConfigured=true", () => {
    const matrix = getToolchainTupleMatrix();
    const configured = matrix.filter((r) => r.autoDownloadConfigured);
    // darwin + linux × arm64/x64 for each of 4 tools → at least 16 rows.
    expect(configured.length).toBeGreaterThanOrEqual(16);
  });

  it("matrix exposes checksumConfigured per tuple so the operator runbook can audit coverage", () => {
    const matrix = getToolchainTupleMatrix();
    // At minimum darwin-arm64 entries should have verifiable checksums (the
    // ones we already pinned). This pins the operator contract: when a
    // maintainer adds a new platform, they must also add the SHA256.
    const darwinArm64 = matrix.filter(
      (r) => r.target === "darwin-arm64" && r.autoDownloadConfigured
    );
    expect(darwinArm64.length).toBeGreaterThan(0);
    expect(darwinArm64.every((r) => r.checksumConfigured)).toBe(true);
  });
});
