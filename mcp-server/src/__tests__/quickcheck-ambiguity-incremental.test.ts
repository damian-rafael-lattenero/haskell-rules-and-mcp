/**
 * NEW-2 coverage (Phase 5.1): `_ambiguityHint` MUST be emitted on BOTH the
 * incremental and non-incremental return paths of `handleQuickCheck`.
 *
 * Regression guard for a Phase 5 gap discovered in session 3: the OBS-3
 * hint was added but only on the non-incremental return path, so agents
 * calling `ghci_quickcheck(..., incremental: true)` during active
 * development — the most common pattern — never saw the hint. Active
 * debuggers got the raw GHC error, exactly what Phase 5 intended to
 * replace.
 */
import { describe, it, expect, vi } from "vitest";

import { handleQuickCheck, resetQuickCheckState } from "../tools/quickcheck.js";
import { createMockSession } from "./helpers/mock-session.js";

const AMBIGUOUS_OUTPUT = `
<interactive>:76:50: error: [GHC-39999]
    • Ambiguous type variable ‘a0’ arising from a use of ‘==’
      prevents the constraint ‘(Eq a0)’ from being solved.
      Probable fix: use a type annotation to specify what ‘a0’ should be.
`;

function sessionReturningAmbiguousError() {
  return createMockSession({
    execute: async (cmd: unknown) => {
      const c = String(cmd);
      // Pre-flight typecheck (`:t (...)`) — this is where the ambiguity
      // error lands FIRST in handleQuickCheck's flow; the code returns
      // early here, so the mock must produce the error at THIS step (not
      // at the later `quickCheck` invocation) for the test to cover the
      // realistic path.
      if (c.startsWith(":t ")) {
        return { success: false, output: AMBIGUOUS_OUTPUT };
      }
      if (c.includes("quickCheck") || c.includes("__qcProp")) {
        return {
          success: false,
          output: AMBIGUOUS_OUTPUT + "(deferred type error)\nExpr{}\n",
        };
      }
      return { success: true, output: "" };
    },
  });
}

describe("handleQuickCheck ambiguity hint (NEW-2)", () => {
  it("includes _ambiguityHint in the incremental response", async () => {
    resetQuickCheckState();
    const session = sessionReturningAmbiguousError();
    const raw = await handleQuickCheck(
      session,
      {
        property: "\\e -> eval emptyEnv (simplify e) == eval emptyEnv e",
        incremental: true,
      },
      undefined,
      "/tmp/nonexistent-project"
    );
    const parsed = JSON.parse(raw);
    expect(parsed._ambiguityHint).toBeDefined();
    expect(parsed._ambiguityHint).toContain("type annotation");
    expect(parsed._ambiguityHint).toContain("Either Error Int");
    // The user-facing `hint` must also reference the ambiguity specifically,
    // not the generic "Incremental property FAILED. Fix before continuing."
    expect(parsed.hint).toContain("ambiguous");
  });

  it("includes _ambiguityHint in the non-incremental response too", async () => {
    resetQuickCheckState();
    const session = sessionReturningAmbiguousError();
    const raw = await handleQuickCheck(
      session,
      {
        property: "\\e -> eval emptyEnv (simplify e) == eval emptyEnv e",
      },
      undefined,
      "/tmp/nonexistent-project"
    );
    const parsed = JSON.parse(raw);
    expect(parsed._ambiguityHint).toBeDefined();
    expect(parsed._ambiguityHint).toContain("type annotation");
    expect(parsed._nextStep).toContain("ambiguous");
  });

  it("omits _ambiguityHint when the error is something else", async () => {
    resetQuickCheckState();
    const session = createMockSession({
      execute: async (cmd: unknown) => {
        const c = String(cmd);
        if (c.includes("quickCheck") || c.includes("__qcProp")) {
          return {
            success: false,
            output:
              "*** Failed! Falsifiable (after 3 tests):\nLit 0\n",
          };
        }
        return { success: true, output: "" };
      },
    });
    const raw = await handleQuickCheck(
      session,
      {
        property: "\\x -> x + 1 == x",
        incremental: true,
      },
      undefined,
      "/tmp/nonexistent-project"
    );
    const parsed = JSON.parse(raw);
    expect(parsed._ambiguityHint).toBeUndefined();
  });

  it("still marks the incremental result as failed when ambiguity fires", async () => {
    resetQuickCheckState();
    const session = sessionReturningAmbiguousError();
    const raw = await handleQuickCheck(
      session,
      {
        property: "\\e -> foo == bar",
        incremental: true,
      },
      undefined,
      "/tmp/nonexistent-project"
    );
    const parsed = JSON.parse(raw);
    // Ambiguity is a compilation failure — success must be false.
    expect(parsed.success).toBe(false);
    // Property is NOT saved when it didn't actually pass.
    expect(parsed._propertySaved).toBeUndefined();
    // Passed count is zero (nothing ran).
    expect(parsed.passed).toBe(0);
  });
});

// Defensive: ensure vi.fn's presence is not a runtime import hazard.
describe("mock session sanity", () => {
  it("spy calls register correctly for property execute", async () => {
    const session = sessionReturningAmbiguousError();
    await handleQuickCheck(
      session,
      { property: "\\x -> x == x", incremental: true },
      undefined,
      "/tmp/doesnt-matter"
    );
    // Require at least one `execute` call for the property run.
    const spy = session.execute as unknown as ReturnType<typeof vi.fn>;
    expect(spy.mock.calls.length).toBeGreaterThan(0);
  });
});
