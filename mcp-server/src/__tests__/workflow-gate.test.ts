/**
 * Unit coverage for the `ghci_workflow(action="gate")` orchestrator.
 * We never spawn cabal; the test exercises the control flow by mocking
 * `handleCabalTest` and `handleBuild` at the module boundary and supplying a
 * fake GhciSession that returns a zero-property store path.
 */
import { describe, it, expect, vi, beforeEach } from "vitest";
import { mkdtemp, rm } from "node:fs/promises";
import { tmpdir } from "node:os";
import path from "node:path";

vi.mock("../tools/test.js", () => ({
  handleCabalTest: vi.fn(),
}));
vi.mock("../tools/build.js", () => ({
  handleBuild: vi.fn(),
  register: vi.fn(),
}));

import { handleWorkflowGate } from "../tools/workflow-gate.js";
import { handleCabalTest } from "../tools/test.js";
import { handleBuild } from "../tools/build.js";
import type { GhciSession } from "../ghci-session.js";

function fakeSession(): GhciSession {
  return {
    execute: async () => ({ success: true, output: "" }),
    loadModule: async () => ({ success: true, output: "" }),
    isAlive: () => true,
  } as unknown as GhciSession;
}

describe("handleWorkflowGate", () => {
  let dir: string;
  beforeEach(async () => {
    dir = await mkdtemp(path.join(tmpdir(), "gate-test-"));
    vi.clearAllMocks();
  });

  it("reports success when all three steps pass (empty regression + mocked cabal OK)", async () => {
    (handleCabalTest as unknown as ReturnType<typeof vi.fn>).mockResolvedValue(
      JSON.stringify({ success: true, summary: "Tests passed" })
    );
    (handleBuild as unknown as ReturnType<typeof vi.fn>).mockResolvedValue(
      JSON.stringify({ success: true, summary: "Built" })
    );

    const report = await handleWorkflowGate(fakeSession(), dir);
    expect(report.success).toBe(true);
    expect(report.steps.regression.status).toBe("pass");
    expect(report.steps.cabal_test.status).toBe("pass");
    expect(report.steps.cabal_build.status).toBe("pass");
    expect(report.summary).toContain("regression=pass");
    expect(report.summary).toContain("cabal_test=pass");
    expect(report.summary).toContain("cabal_build=pass");

    await rm(dir, { recursive: true, force: true });
  });

  it("reports success=false when any step fails — but still runs every step", async () => {
    (handleCabalTest as unknown as ReturnType<typeof vi.fn>).mockResolvedValue(
      JSON.stringify({ success: false, summary: "Tests failed", errors: [{ line: 1 }] })
    );
    (handleBuild as unknown as ReturnType<typeof vi.fn>).mockResolvedValue(
      JSON.stringify({ success: true, summary: "Built" })
    );

    const report = await handleWorkflowGate(fakeSession(), dir);
    expect(report.success).toBe(false);
    expect(report.steps.cabal_test.status).toBe("fail");
    expect(report.steps.cabal_build.status).toBe("pass"); // ran despite earlier failure
    // Both mocks were called — no short-circuit.
    expect((handleCabalTest as unknown as ReturnType<typeof vi.fn>).mock.calls.length).toBe(1);
    expect((handleBuild as unknown as ReturnType<typeof vi.fn>).mock.calls.length).toBe(1);

    await rm(dir, { recursive: true, force: true });
  });

  it("honors skip_cabal_test to avoid spawning cabal", async () => {
    (handleBuild as unknown as ReturnType<typeof vi.fn>).mockResolvedValue(
      JSON.stringify({ success: true })
    );

    const report = await handleWorkflowGate(fakeSession(), dir, {
      skip_cabal_test: true,
    });
    expect(report.steps.cabal_test.status).toBe("skip");
    expect(report.steps.cabal_build.status).toBe("pass");
    expect((handleCabalTest as unknown as ReturnType<typeof vi.fn>).mock.calls.length).toBe(0);

    await rm(dir, { recursive: true, force: true });
  });

  it("reports success=false when ALL steps are skipped (no ran steps)", async () => {
    const report = await handleWorkflowGate(fakeSession(), dir, {
      skip_regression: true,
      skip_cabal_test: true,
      skip_cabal_build: true,
    });
    expect(report.success).toBe(false); // ranSteps.length === 0
    expect(report.steps.regression.status).toBe("skip");
    expect(report.steps.cabal_test.status).toBe("skip");
    expect(report.steps.cabal_build.status).toBe("skip");

    await rm(dir, { recursive: true, force: true });
  });

  it("attaches durationMs to each step", async () => {
    (handleCabalTest as unknown as ReturnType<typeof vi.fn>).mockResolvedValue(
      JSON.stringify({ success: true })
    );
    (handleBuild as unknown as ReturnType<typeof vi.fn>).mockResolvedValue(
      JSON.stringify({ success: true })
    );
    const report = await handleWorkflowGate(fakeSession(), dir);
    expect(report.totalDurationMs).toBeGreaterThanOrEqual(0);
    expect(report.steps.regression.durationMs).toBeGreaterThanOrEqual(0);
    expect(report.steps.cabal_test.durationMs).toBeGreaterThanOrEqual(0);
    expect(report.steps.cabal_build.durationMs).toBeGreaterThanOrEqual(0);

    await rm(dir, { recursive: true, force: true });
  });
});
