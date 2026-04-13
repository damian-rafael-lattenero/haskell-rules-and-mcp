import { describe, it, expect } from "vitest";
import {
  categorizeWarning,
  categorizeWarnings,
} from "../parsers/warning-categorizer.js";
import type { GhcError } from "../parsers/error-parser.js";

function makeWarning(
  overrides: Partial<GhcError> & { message: string; warningFlag: string }
): GhcError {
  return {
    file: "src/Test.hs",
    line: 1,
    column: 1,
    severity: "warning",
    ...overrides,
  };
}

describe("categorizeWarning", () => {
  it("categorizes -Wunused-imports with specific import name", () => {
    const w = makeWarning({
      warningFlag: "-Wunused-imports",
      message:
        "The import of \u2018Data.List\u2019 is redundant\n" +
        "  except perhaps to import instances from \u2018Data.List\u2019",
    });
    const action = categorizeWarning(w);
    expect(action).not.toBeNull();
    expect(action!.category).toBe("unused-import");
    expect(action!.suggestedAction).toContain("Data.List");
    expect(action!.confidence).toBe("high");
  });

  it("categorizes -Wunused-imports with ASCII quotes", () => {
    const w = makeWarning({
      warningFlag: "-Wunused-imports",
      message: "The import of 'sort' from module 'Data.List' is redundant",
    });
    const action = categorizeWarning(w);
    expect(action).not.toBeNull();
    expect(action!.category).toBe("unused-import");
    expect(action!.suggestedAction).toContain("sort");
  });

  it("categorizes -Wunused-imports with fallback (no regex match)", () => {
    const w = makeWarning({
      warningFlag: "-Wunused-imports",
      line: 5,
      message: "Some unrecognized format for unused import",
    });
    const action = categorizeWarning(w);
    expect(action).not.toBeNull();
    expect(action!.category).toBe("unused-import");
    expect(action!.suggestedAction).toContain("line 5");
    expect(action!.confidence).toBe("medium");
  });

  it("categorizes -Wmissing-signatures and extracts type", () => {
    const w = makeWarning({
      warningFlag: "-Wmissing-signatures",
      message:
        "Top-level binding with no type signature:\n" +
        "      foo :: forall {a}. Num a => a -> a\n" +
        "  |\n" +
        "3 | foo x = x + 1\n" +
        "  | ^^^",
    });
    const action = categorizeWarning(w);
    expect(action).not.toBeNull();
    expect(action!.category).toBe("missing-signature");
    expect(action!.suggestedAction).toContain(
      "foo :: forall {a}. Num a => a -> a"
    );
    expect(action!.confidence).toBe("high");
  });

  // Bug Fix 0B: GHC 9.12 single-line format for missing-signatures
  it("categorizes -Wmissing-signatures with single-line format (GHC 9.12)", () => {
    const w = makeWarning({
      warningFlag: "-Wmissing-signatures",
      line: 4,
      message:
        "Top-level binding with no type signature: double :: Num a => a -> a\n" +
        "  |\n" +
        "4 | double x = x + x\n" +
        "  | ^^^^^^",
    });
    const action = categorizeWarning(w);
    expect(action).not.toBeNull();
    expect(action!.category).toBe("missing-signature");
    expect(action!.suggestedAction).toContain("double :: Num a => a -> a");
  });

  it("categorizes -Wmissing-signatures even with unrecognized format", () => {
    const w = makeWarning({
      warningFlag: "-Wmissing-signatures",
      line: 7,
      message: "Top-level binding with no type signature (some future format)",
    });
    const action = categorizeWarning(w);
    expect(action).not.toBeNull();
    expect(action!.category).toBe("missing-signature");
    expect(action!.suggestedAction).toContain("line 7");
    expect(action!.confidence).toBe("medium");
  });

  it("categorizes -Wunused-matches with Unicode quotes", () => {
    const w = makeWarning({
      warningFlag: "-Wunused-matches",
      line: 5,
      message:
        "Defined but not used: \u2018x\u2019\n  |\n5 | bar x = 42\n  |     ^",
    });
    const action = categorizeWarning(w);
    expect(action).not.toBeNull();
    expect(action!.category).toBe("unused-binding");
    expect(action!.suggestedAction).toContain("_x");
  });

  it("categorizes -Wunused-matches with ASCII quotes", () => {
    const w = makeWarning({
      warningFlag: "-Wunused-matches",
      line: 5,
      message: "Defined but not used: 'x'",
    });
    const action = categorizeWarning(w);
    expect(action).not.toBeNull();
    expect(action!.category).toBe("unused-binding");
    expect(action!.suggestedAction).toContain("_x");
  });

  it("categorizes -Wunused-local-binds", () => {
    const w = makeWarning({
      warningFlag: "-Wunused-local-binds",
      line: 10,
      message: "Defined but not used: \u2018helper\u2019",
    });
    const action = categorizeWarning(w);
    expect(action).not.toBeNull();
    expect(action!.category).toBe("unused-binding");
    expect(action!.suggestedAction).toContain("_helper");
  });

  it("categorizes -Wincomplete-patterns with Unicode quotes", () => {
    const w = makeWarning({
      warningFlag: "-Wincomplete-patterns",
      message:
        "Pattern match(es) are non-exhaustive\n" +
        "    In an equation for \u2018baz\u2019:\n" +
        "        Patterns of type \u2018Maybe Int\u2019 not matched: Nothing\n" +
        "  |\n" +
        "7 | baz (Just n) = n\n" +
        "  | ^^^^^^^^^^^^^^^^",
    });
    const action = categorizeWarning(w);
    expect(action).not.toBeNull();
    expect(action!.category).toBe("incomplete-patterns");
    expect(action!.suggestedAction).toContain("Nothing");
  });

  it("categorizes -Wincomplete-patterns with ASCII quotes", () => {
    const w = makeWarning({
      warningFlag: "-Wincomplete-patterns",
      message:
        "Pattern match(es) are non-exhaustive\n" +
        "    Patterns of type 'Maybe Int' not matched: Nothing",
    });
    const action = categorizeWarning(w);
    expect(action).not.toBeNull();
    expect(action!.category).toBe("incomplete-patterns");
    expect(action!.suggestedAction).toContain("Nothing");
  });

  it("categorizes -Wname-shadowing", () => {
    const w = makeWarning({
      warningFlag: "-Wname-shadowing",
      line: 8,
      message:
        "This binding for \u2018x\u2019 shadows the existing binding",
    });
    const action = categorizeWarning(w);
    expect(action).not.toBeNull();
    expect(action!.category).toBe("name-shadowing");
    expect(action!.suggestedAction).toContain("x");
  });

  it("categorizes -Wredundant-constraints", () => {
    const w = makeWarning({
      warningFlag: "-Wredundant-constraints",
      line: 3,
      message: "Redundant constraint: Eq a",
    });
    const action = categorizeWarning(w);
    expect(action).not.toBeNull();
    expect(action!.category).toBe("redundant-constraint");
  });

  it("categorizes -Wunused-do-bind", () => {
    const w = makeWarning({
      warningFlag: "-Wunused-do-bind",
      line: 12,
      message: "A do-notation statement discarded a result",
    });
    const action = categorizeWarning(w);
    expect(action).not.toBeNull();
    expect(action!.category).toBe("unused-do-bind");
    expect(action!.suggestedAction).toContain("void");
  });

  it("categorizes -Wtyped-holes", () => {
    const w = makeWarning({
      warningFlag: "-Wtyped-holes",
      line: 9,
      message: "Found hole: _ :: String",
    });
    const action = categorizeWarning(w);
    expect(action).not.toBeNull();
    expect(action!.category).toBe("typed-hole");
    expect(action!.confidence).toBe("medium");
  });

  it("categorizes -Wtype-defaults", () => {
    const w = makeWarning({
      warningFlag: "-Wtype-defaults",
      line: 1,
      message: "Defaulting the type variable",
    });
    const action = categorizeWarning(w);
    expect(action).not.toBeNull();
    expect(action!.category).toBe("type-defaults");
  });

  it("categorizes -Wdeferred-type-errors with expected/actual", () => {
    const w = makeWarning({
      warningFlag: "-Wdeferred-type-errors",
      line: 12,
      message:
        "Couldn\u2019t match expected type \u2018[Int] -> Int\u2019\n" +
        "                with actual type \u2018Bool\u2019",
      expected: "[Int] -> Int",
      actual: "Bool",
    } as Partial<GhcError> & { message: string; warningFlag: string });
    const action = categorizeWarning(w);
    expect(action).not.toBeNull();
    expect(action!.category).toBe("deferred-type-error");
    expect(action!.suggestedAction).toContain("expected [Int] -> Int");
    expect(action!.suggestedAction).toContain("actual Bool");
    expect(action!.confidence).toBe("high");
  });

  it("categorizes -Wdeferred-type-errors without expected/actual", () => {
    const w = makeWarning({
      warningFlag: "-Wdeferred-type-errors",
      line: 5,
      message: "Some deferred type error",
    });
    const action = categorizeWarning(w);
    expect(action).not.toBeNull();
    expect(action!.category).toBe("deferred-type-error");
    expect(action!.suggestedAction).toContain("line 5");
    expect(action!.suggestedAction).not.toContain("expected");
  });

  it("categorizes -Wdeferred-out-of-scope-variables", () => {
    const w = makeWarning({
      warningFlag: "-Wdeferred-out-of-scope-variables",
      line: 17,
      message: "Variable not in scope: mySum :: [Int] -> Int",
    });
    const action = categorizeWarning(w);
    expect(action).not.toBeNull();
    expect(action!.category).toBe("deferred-type-error");
  });

  it("returns null for unknown warning flags", () => {
    const w = makeWarning({
      warningFlag: "-Wsome-unknown-flag",
      message: "Some unknown warning",
    });
    expect(categorizeWarning(w)).toBeNull();
  });

  it("returns null when warningFlag is empty", () => {
    const w = makeWarning({
      warningFlag: "",
      message: "No flag attached",
    });
    expect(categorizeWarning(w)).toBeNull();
  });
});

describe("categorizeWarnings", () => {
  it("splits warnings into actions and uncategorized", () => {
    const warnings: GhcError[] = [
      makeWarning({
        warningFlag: "-Wunused-imports",
        message: "The import of 'Data.List' is redundant",
      }),
      makeWarning({
        warningFlag: "-Wsome-unknown",
        message: "Unknown warning",
      }),
      makeWarning({
        warningFlag: "-Wtyped-holes",
        message: "Found hole",
      }),
    ];

    const { actions, uncategorized } = categorizeWarnings(warnings);
    expect(actions).toHaveLength(2);
    expect(uncategorized).toHaveLength(1);
    expect(actions[0]!.category).toBe("unused-import");
    expect(actions[1]!.category).toBe("typed-hole");
    expect(uncategorized[0]!.warningFlag).toBe("-Wsome-unknown");
  });

  it("returns empty arrays for empty input", () => {
    const { actions, uncategorized } = categorizeWarnings([]);
    expect(actions).toEqual([]);
    expect(uncategorized).toEqual([]);
  });
});
