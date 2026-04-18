/**
 * Orchestrates regression + cabal_test + cabal_build into a single gated
 * "is this ready?" report. Used by the ghci_workflow(action="gate") action.
 *
 * Semantics:
 *   - ALWAYS runs all three (unless skip flags are set). We do NOT short-circuit
 *     on first failure, because the caller benefits from seeing the full
 *     picture in one response.
 *   - `success: true` iff EVERY step that ran reported success.
 *   - Each step's output includes duration_ms and its raw JSON response so
 *     the agent can drill in without a second tool call.
 */
import { runRegression, type RegressionOutcome } from "./regression.js";
import { handleCabalTest } from "./test.js";
import { handleBuild } from "./build.js";
import type { GhciSession } from "../ghci-session.js";

export interface GateStepResult<T> {
  status: "pass" | "fail" | "skip";
  durationMs: number;
  details: T | null;
}

export interface GateReport {
  success: boolean;
  totalDurationMs: number;
  steps: {
    regression: GateStepResult<RegressionOutcome>;
    cabal_test: GateStepResult<unknown>;
    cabal_build: GateStepResult<unknown>;
  };
  summary: string;
}

export interface GateOptions {
  skip_regression?: boolean;
  skip_cabal_test?: boolean;
  skip_cabal_build?: boolean;
  /** Forwarded to runRegression. */
  module?: string;
}

async function runRegressionStep(
  session: GhciSession,
  projectDir: string,
  opts: GateOptions
): Promise<GateStepResult<RegressionOutcome>> {
  if (opts.skip_regression) {
    return { status: "skip", durationMs: 0, details: null };
  }
  const outcome = await runRegression(session, projectDir, { module: opts.module });
  return {
    status: outcome.failed === 0 ? "pass" : "fail",
    durationMs: outcome.durationMs,
    details: outcome,
  };
}

async function runCabalStep(
  projectDir: string,
  label: "cabal_test" | "cabal_build",
  skip: boolean | undefined
): Promise<GateStepResult<unknown>> {
  if (skip) return { status: "skip", durationMs: 0, details: null };
  const start = Date.now();
  const raw = label === "cabal_test"
    ? await handleCabalTest(projectDir, {})
    : await handleBuild(projectDir, {});
  const parsed = JSON.parse(raw);
  return {
    status: parsed.success ? "pass" : "fail",
    durationMs: Date.now() - start,
    details: parsed,
  };
}

/**
 * Run the three gate steps serially. All steps receive the chance to run
 * regardless of earlier failures — callers see the full state.
 */
export async function handleWorkflowGate(
  session: GhciSession,
  projectDir: string,
  opts: GateOptions = {}
): Promise<GateReport> {
  const gateStart = Date.now();

  const regression = await runRegressionStep(session, projectDir, opts);
  const cabalTest = await runCabalStep(projectDir, "cabal_test", opts.skip_cabal_test);
  const cabalBuild = await runCabalStep(projectDir, "cabal_build", opts.skip_cabal_build);

  const ranSteps = [regression, cabalTest, cabalBuild].filter((s) => s.status !== "skip");
  const anyFailed = ranSteps.some((s) => s.status === "fail");
  const success = ranSteps.length > 0 && !anyFailed;

  const summaryParts: string[] = [];
  summaryParts.push(`regression=${regression.status}`);
  summaryParts.push(`cabal_test=${cabalTest.status}`);
  summaryParts.push(`cabal_build=${cabalBuild.status}`);

  return {
    success,
    totalDurationMs: Date.now() - gateStart,
    steps: {
      regression,
      cabal_test: cabalTest,
      cabal_build: cabalBuild,
    },
    summary: summaryParts.join(", "),
  };
}
