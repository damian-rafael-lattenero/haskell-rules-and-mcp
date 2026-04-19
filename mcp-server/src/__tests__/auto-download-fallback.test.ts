/**
 * Unit coverage for the `ToolchainTupleStatus` matrix produced by
 * `getToolchainTupleMatrix`. We don't touch the network — we smoke-test the
 * data model and the operator-facing shape so a future refactor can't
 * silently break the diagnostic.
 *
 * Post-Phase-6.2 platform-honesty cleanup: unsupported targets appear as
 * `autoDownloadConfigured: false` with a `note` explaining the fallback.
 * Primary dev target (darwin-arm64) MUST be fully configured.
 */
import { describe, it, expect } from "vitest";
import { getToolchainTupleMatrix } from "../tools/auto-download.js";

describe("auto-download toolchain matrix", () => {
  it("primary dev target (darwin-arm64) is fully configured for every tool", async () => {
    const matrix = await getToolchainTupleMatrix();
    const darwinArm64 = matrix.filter((r) => r.target === "darwin-arm64");
    // 4 tools × 1 target = 4 rows, each with configured + checksum.
    expect(darwinArm64.length).toBe(4);
    expect(darwinArm64.every((r) => r.autoDownloadConfigured)).toBe(true);
    expect(darwinArm64.every((r) => r.checksumConfigured)).toBe(true);
    // The primary target must NOT carry the "not configured" note.
    expect(darwinArm64.every((r) => r.note === undefined)).toBe(true);
  });

  it("unsupported targets carry an explanatory 'note' and are not silently missing", async () => {
    const matrix = await getToolchainTupleMatrix();
    const unsupported = matrix.filter((r) => !r.autoDownloadConfigured);
    // win32 (2 archs × 4 tools) + linux-arm64/linux-x64/darwin-x64 (3 × 4) = 20.
    expect(unsupported.length).toBeGreaterThanOrEqual(16);
    // Every unsupported row must carry a `note` — agents should never see
    // an empty "autoDownloadConfigured:false" without explanation.
    expect(unsupported.every((r) => typeof r.note === "string")).toBe(true);
    // For non-dev targets the note directs to host PATH fallback.
    const nonDev = unsupported.filter((r) => r.target !== "darwin-arm64");
    expect(nonDev.every((r) => r.note!.includes("host PATH"))).toBe(true);
  });

  it("operator-audit invariant: 4 tools total × 6 targets = 24 rows", async () => {
    const matrix = await getToolchainTupleMatrix();
    expect(matrix.length).toBe(24);
    const tools = new Set(matrix.map((r) => r.tool));
    const targets = new Set(matrix.map((r) => r.target));
    expect(tools.size).toBe(4);
    expect(targets.size).toBe(6);
  });
});
