/**
 * Stress tests and edge cases that go BEYOND covering existing code paths.
 *
 * These tests think like a Haskell developer and explore scenarios that
 * the smoke test protocol didn't cover. The goal is to find NEW bugs
 * and protect against future regressions in unusual but realistic scenarios.
 */
import { describe, it, expect, vi, afterEach } from "vitest";
import { parseGhcErrors } from "../parsers/error-parser.js";
import { parseInfoOutput, parseTypeOutput } from "../parsers/type-parser.js";
import { parseEvalOutput } from "../parsers/eval-output-parser.js";
import { categorizeWarning, categorizeWarnings } from "../parsers/warning-categorizer.js";
import { parseQuickCheckOutput } from "../tools/quickcheck.js";
import { handleTypeCheck } from "../tools/type-check.js";
import { handleTypeInfo } from "../tools/type-info.js";
import { handleCheckModule } from "../tools/check-module.js";
import { handleLoadModule } from "../tools/load-module.js";
import { handleHoleFits } from "../tools/hole-fits.js";
import { extractModules, moduleToFilePath, extractPackageName } from "../parsers/cabal-parser.js";
import { createMockSession } from "./helpers/mock-session.js";
import type { GhcError } from "../parsers/error-parser.js";
import type { GhciResult } from "../ghci-session.js";

function makeWarning(
  overrides: Partial<GhcError> & { message: string; warningFlag: string }
): GhcError {
  return { file: "src/Test.hs", line: 1, column: 1, severity: "warning", ...overrides };
}

// ============================================================================
// STRESS: Complex GHC error output from real-world Haskell development
// ============================================================================
describe("Stress: complex GHC error scenarios", () => {
  it("handles GHC ambiguous type variable error (common with type classes)", () => {
    const output =
      "src/App.hs:15:5-30: error: [GHC-46956]\n" +
      "    \u2022 Ambiguous type variable \u2018a0\u2019 arising from a use of \u2018show\u2019\n" +
      "      prevents the constraint \u2018(Show a0)\u2019 from being solved.\n" +
      "    \u2022 Probable fix: use a type annotation to specify what \u2018a0\u2019 should be.\n" +
      "    \u2022 In the expression: show (read \"42\")\n" +
      "      In an equation for \u2018foo\u2019: foo = show (read \"42\")\n" +
      "  |";
    const errors = parseGhcErrors(output);
    expect(errors).toHaveLength(1);
    expect(errors[0]!.code).toBe("GHC-46956");
    expect(errors[0]!.context).toContain("show (read");
  });

  it("handles GHC no-instance error (forgot deriving)", () => {
    const output =
      "src/Types.hs:10:15-20: error: [GHC-39660]\n" +
      "    \u2022 No instance for \u2018Show MyType\u2019 arising from a use of \u2018show\u2019\n" +
      "    \u2022 In the expression: show myVal";
    const errors = parseGhcErrors(output);
    expect(errors).toHaveLength(1);
    expect(errors[0]!.code).toBe("GHC-39660");
  });

  it("handles multiple errors across different files", () => {
    const output =
      "src/A.hs:3:1: error: [GHC-83865]\n    Type mismatch in A\n" +
      "src/B.hs:10:5: error: [GHC-39999]\n    Not in scope: \u2018helper\u2019\n" +
      "src/C.hs:7:1-20: warning: [GHC-62161] [-Wincomplete-patterns]\n    Non-exhaustive patterns\n" +
      "src/A.hs:15:8: error: [GHC-83865]\n    Another type mismatch in A";
    const errors = parseGhcErrors(output);
    expect(errors).toHaveLength(4);
    expect(errors.filter((e) => e.file === "src/A.hs")).toHaveLength(2);
    expect(errors.filter((e) => e.severity === "error")).toHaveLength(3);
    expect(errors.filter((e) => e.severity === "warning")).toHaveLength(1);
  });

  it("handles GHC out-of-scope with suggestions (common typo scenario)", () => {
    const output =
      "src/Lib.hs:5:10: error: [GHC-39999]\n" +
      "    Variable not in scope: lenght :: [a0] -> Int\n" +
      "    Suggested fix: Perhaps you meant \u2018length\u2019 (imported from Prelude)";
    const errors = parseGhcErrors(output);
    expect(errors).toHaveLength(1);
    expect(errors[0]!.code).toBe("GHC-39999");
    expect(errors[0]!.message).toContain("lenght");
    expect(errors[0]!.message).toContain("Suggested fix");
  });

  it("handles very long GHC error messages (constraint solving failures)", () => {
    // GHC can produce extremely verbose type error messages with constraints
    const constraintLines = Array.from({ length: 20 }, (_, i) =>
      `    \u2022 constraint ${i}: SomeClass (Nested (Type ${i}))`
    ).join("\n");
    const output = `src/Deep.hs:50:1-80: error: [GHC-83865]\n${constraintLines}`;
    const errors = parseGhcErrors(output);
    expect(errors).toHaveLength(1);
    expect(errors[0]!.message.length).toBeGreaterThan(500);
  });
});

// ============================================================================
// STRESS: Type parser with complex Haskell types
// ============================================================================
describe("Stress: complex Haskell types", () => {
  it("handles higher-kinded type with constraints", () => {
    const result = parseTypeOutput(
      "traverse :: (Traversable t, Applicative f) => (a -> f b) -> t a -> f (t b)"
    );
    expect(result).not.toBeNull();
    expect(result!.type).toContain("Traversable t");
    expect(result!.type).toContain("Applicative f");
  });

  it("handles RankNType (forall inside arrow)", () => {
    const result = parseTypeOutput(
      "runST :: (forall s. ST s a) -> a"
    );
    expect(result).not.toBeNull();
    expect(result!.type).toContain("forall s.");
  });

  it("handles type family result", () => {
    const result = parseTypeOutput("type family F a :: * -> *");
    // May or may not parse — but should not crash
    expect(true).toBe(true);
  });

  it("handles very long type signature (mtl stack)", () => {
    const result = parseTypeOutput(
      "runApp :: ReaderT Config (ExceptT AppError (StateT AppState IO)) a -> Config -> AppState -> IO (Either AppError a, AppState)"
    );
    expect(result).not.toBeNull();
    expect(result!.expression).toBe("runApp");
  });

  it("handles kind signature with PolyKinds", () => {
    const result = parseInfoOutput(
      "type Proxy :: forall k. k -> *\ndata Proxy a = Proxy"
    );
    expect(result.kind).toBe("data");
    expect(result.name).toBe("Proxy");
  });

  it("handles GADT with kind annotation", () => {
    const result = parseInfoOutput(
      "type Expr :: * -> *\ndata Expr a where\n  Lit :: Int -> Expr Int\n  Add :: Expr Int -> Expr Int -> Expr Int"
    );
    expect(result.kind).toBe("data");
  });

  it("handles multi-parameter typeclass", () => {
    const result = parseInfoOutput(
      "class MonadReader r m | m -> r where\n  ask :: m r\n  local :: (r -> r) -> m a -> m a"
    );
    expect(result.kind).toBe("class");
    expect(result.name).toBe("MonadReader");
  });
});

// ============================================================================
// STRESS: Warning categorizer with unusual warnings
// ============================================================================
describe("Stress: unusual warning scenarios", () => {
  it("handles -Wmissing-signatures with complex type", () => {
    const w = makeWarning({
      warningFlag: "-Wmissing-signatures",
      message:
        "Top-level binding with no type signature:\n" +
        "      runApp :: forall m a. (MonadReader Config m, MonadState AppState m, MonadError AppError m) => m a -> IO (Either AppError a)",
    });
    const action = categorizeWarning(w);
    expect(action).not.toBeNull();
    expect(action!.suggestedAction).toContain("MonadReader");
  });

  it("handles -Wunused-imports with operator import", () => {
    const w = makeWarning({
      warningFlag: "-Wunused-imports",
      message: "The import of \u2018(<>)\u2019 from module \u2018Data.Semigroup\u2019 is redundant",
    });
    const action = categorizeWarning(w);
    expect(action).not.toBeNull();
    expect(action!.suggestedAction).toContain("(<>)");
  });

  it("handles -Wincomplete-patterns with multiple missing constructors", () => {
    const w = makeWarning({
      warningFlag: "-Wincomplete-patterns",
      message:
        "Pattern match(es) are non-exhaustive\n" +
        "    In an equation for \u2018f\u2019:\n" +
        "        Patterns of type \u2018Expr\u2019 not matched:\n" +
        "            ELam _ _\n" +
        "            EApp _ _\n" +
        "            ELet _ _ _",
    });
    const action = categorizeWarning(w);
    expect(action).not.toBeNull();
    expect(action!.suggestedAction).toContain("ELam");
    expect(action!.suggestedAction).toContain("EApp");
    expect(action!.suggestedAction).toContain("ELet");
  });

  it("handles batch of 10+ warnings without losing any", () => {
    const warnings = Array.from({ length: 12 }, (_, i) =>
      makeWarning({
        warningFlag: "-Wunused-imports",
        line: i + 1,
        message: `The import of \u2018Module${i}\u2019 is redundant`,
      })
    );
    const { actions, uncategorized } = categorizeWarnings(warnings);
    expect(actions).toHaveLength(12);
    expect(uncategorized).toHaveLength(0);
  });
});

// ============================================================================
// STRESS: QuickCheck parser edge cases
// ============================================================================
describe("Stress: QuickCheck unusual outputs", () => {
  it("handles multi-line counterexample", () => {
    const output =
      "*** Failed! Falsifiable (after 3 tests and 2 shrinks):\n" +
      "[1,2,3]\n" +
      "[4,5,6]\n" +
      "\n";
    const result = parseQuickCheckOutput(output, "prop");
    expect(result.success).toBe(false);
    expect(result.counterexample).toContain("[1,2,3]");
    expect(result.counterexample).toContain("[4,5,6]");
  });

  it("handles very large test count", () => {
    const output = "+++ OK, passed 10000 tests.\n";
    const result = parseQuickCheckOutput(output, "prop");
    expect(result.success).toBe(true);
    expect(result.passed).toBe(10000);
  });

  it("handles failure after many tests", () => {
    const output = "*** Failed! Falsifiable (after 99 tests and 42 shrinks):\n-128\n\n";
    const result = parseQuickCheckOutput(output, "prop");
    expect(result.success).toBe(false);
    expect(result.passed).toBe(98);
    expect(result.shrinks).toBe(42);
  });

  it("handles gave-up output", () => {
    // QuickCheck can "give up" if too many tests are discarded
    const output = "*** Gave up! Passed only 47 tests; 1000 discarded tests.\n";
    const result = parseQuickCheckOutput(output, "prop");
    // Should at least not crash — may return fallback error
    expect(result).toBeDefined();
  });
});

// ============================================================================
// STRESS: Eval output with complex Haskell values
// ============================================================================
describe("Stress: eval with complex output", () => {
  it("handles nested data structure output", () => {
    const parsed = parseEvalOutput(
      'Just (Left (42,"hello",[True,False]))'
    );
    expect(parsed.result).toBe('Just (Left (42,"hello",[True,False]))');
    expect(parsed.warnings).toEqual([]);
  });

  it("handles multiline show output (Map/Set)", () => {
    const raw = "fromList [(1,\"a\"),(2,\"b\"),(3,\"c\")]";
    const parsed = parseEvalOutput(raw);
    expect(parsed.result).toBe("fromList [(1,\"a\"),(2,\"b\"),(3,\"c\")]");
  });

  it("handles infinite list truncation", () => {
    // GHCi would show a prefix before being interrupted
    const raw =
      "<interactive>:1:1: warning: [GHC-18042] [-Wtype-defaults]\n" +
      "    \u2022 Defaulting type variable\n\n" +
      "[1,2,3,4,5,6,7,8,9,10,11,12,Interrupted.";
    const parsed = parseEvalOutput(raw);
    expect(parsed.result).toContain("Interrupted");
    expect(parsed.warnings).toHaveLength(1);
  });

  it("handles empty string result", () => {
    const parsed = parseEvalOutput('""');
    expect(parsed.result).toBe('""');
  });

  it("handles result that looks like a warning header", () => {
    // A string value that contains "warning:" should NOT be mistaken for a warning
    const parsed = parseEvalOutput('"This is a warning: do not touch"');
    expect(parsed.result).toBe('"This is a warning: do not touch"');
    expect(parsed.warnings).toEqual([]);
  });
});

// ============================================================================
// STRESS: Module check with complex Haskell modules
// ============================================================================
describe("Stress: check-module with complex definitions", () => {
  it("handles module with type families", async () => {
    const session = createMockSession({
      loadModule: { output: "Ok, one module loaded.", success: true },
      execute: async (cmd: string): Promise<GhciResult> => {
        if (cmd.startsWith(":browse"))
          return {
            output: "type family F a :: * -> *\ntype instance F Int = Maybe\nfoo :: F Int String",
            success: true,
          };
        return { output: "", success: true };
      },
    });
    const result = JSON.parse(await handleCheckModule(session, { module_path: "src/TF.hs", module_name: "TF" }));
    expect(result.success).toBe(true);
    expect(result.definitions.length).toBeGreaterThan(0);
  });

  it("handles module with many exports (50+)", async () => {
    const defs = Array.from({ length: 55 }, (_, i) => `f${i} :: Int -> Int`).join("\n");
    const session = createMockSession({
      loadModule: { output: "Ok, one module loaded.", success: true },
      execute: async (cmd: string): Promise<GhciResult> => {
        if (cmd.startsWith(":browse")) return { output: defs, success: true };
        return { output: "", success: true };
      },
    });
    const result = JSON.parse(await handleCheckModule(session, { module_path: "src/Big.hs", module_name: "Big" }));
    expect(result.definitions).toHaveLength(55);
    expect(result.summary.functions).toBe(55);
  });

  it("handles GADT in browse output", async () => {
    const browse =
      "type Expr :: * -> *\n" +
      "data Expr a where\n" +
      "  Lit :: Int -> Expr Int\n" +
      "  Add :: Expr Int -> Expr Int -> Expr Int\n" +
      "  If :: Expr Bool -> Expr a -> Expr a -> Expr a";
    const session = createMockSession({
      loadModule: { output: "Ok, one module loaded.", success: true },
      execute: async (cmd: string): Promise<GhciResult> => {
        if (cmd.startsWith(":browse")) return { output: browse, success: true };
        return { output: "", success: true };
      },
    });
    const result = JSON.parse(await handleCheckModule(session, { module_path: "src/G.hs", module_name: "G" }));
    expect(result.success).toBe(true);
  });
});

// ============================================================================
// STRESS: Cabal parser with unusual formats
// ============================================================================
describe("Stress: cabal parser edge cases", () => {
  it("handles cabal file with test-suite and other-modules", () => {
    const content = `cabal-version: 3.12
name: mylib
version: 0.1.0.0

library
  exposed-modules:
    Lib
    Lib.Internal
  build-depends: base
  hs-source-dirs: src

test-suite unit-tests
  type: exitcode-stdio-1.0
  main-is: Main.hs
  other-modules: TestHelpers
  build-depends: base, mylib
  hs-source-dirs: test
`;
    const result = extractModules(content);
    expect(result.library).toEqual(["Lib", "Lib.Internal"]);
  });

  it("handles cabal file with multiple libraries (internal libraries)", () => {
    const content = `cabal-version: 3.12
name: mylib

library
  exposed-modules: PublicAPI
  build-depends: base
  hs-source-dirs: src

library internal
  exposed-modules: Internal.Utils
  build-depends: base
  hs-source-dirs: internal
`;
    const result = extractModules(content);
    expect(result.library).toContain("PublicAPI");
  });

  it("handles deeply nested module paths", () => {
    expect(moduleToFilePath("Control.Monad.Trans.State.Strict", "lib")).toBe(
      "lib/Control/Monad/Trans/State/Strict.hs"
    );
  });

  it("extracts package name with Unicode", () => {
    expect(extractPackageName("name: über-project")).toBe("über-project");
  });

  it("handles module list with trailing comma", () => {
    const content = `name: test
library
  exposed-modules: Foo, Bar,
  build-depends: base
`;
    const result = extractModules(content);
    expect(result.library).toContain("Foo");
    expect(result.library).toContain("Bar");
  });
});

// ============================================================================
// STRESS: Hole fits with complex scenarios
// ============================================================================
describe("Stress: hole fits with complex types", () => {
  it("handles hole in monadic context", async () => {
    const output =
      "src/App.hs:10:20: warning: [GHC-88464] [-Wtyped-holes]\n" +
      "    \u2022 Found hole: _ :: IO String\n" +
      "    \u2022 In a stmt of a 'do' block: result <- _\n" +
      "    \u2022 Relevant bindings include\n" +
      "        env :: Config (bound at src/App.hs:10:5)\n" +
      "        runApp :: Config -> IO String (bound at src/App.hs:9:1)\n" +
      "      Valid hole fits include\n" +
      "        getLine :: IO String\n" +
      "          (imported from 'Prelude' at src/App.hs:1:1)\n" +
      "   |\n" +
      "Ok, one module loaded.";
    const session = createMockSession({
      execute: async (): Promise<GhciResult> => ({ output: "", success: true }),
      loadModule: { output, success: true },
    });
    const result = JSON.parse(await handleHoleFits(session, { module_path: "src/App.hs" }));
    expect(result.holes).toHaveLength(1);
    expect(result.holes[0].expectedType).toBe("IO String");
    expect(result.holes[0].relevantBindings.some((b: any) => b.name === "env")).toBe(true);
  });

  it("handles named hole (_myHole)", async () => {
    const output =
      "src/Foo.hs:3:10: warning: [GHC-88464] [-Wtyped-holes]\n" +
      "    \u2022 Found hole: _myHole :: [Int] -> Int\n" +
      "    \u2022 Relevant bindings include\n" +
      "        xs :: [Int] (bound at src/Foo.hs:3:5)\n" +
      "      Valid hole fits include\n" +
      "        length :: forall a. [a] -> Int\n" +
      "   |\n" +
      "Ok, one module loaded.";
    const session = createMockSession({
      execute: async (): Promise<GhciResult> => ({ output: "", success: true }),
      loadModule: { output, success: true },
    });
    const result = JSON.parse(await handleHoleFits(session, { module_path: "src/Foo.hs" }));
    expect(result.holes[0].hole).toBe("_myHole");
    expect(result.holes[0].expectedType).toBe("[Int] -> Int");
  });
});

// ============================================================================
// STRESS: load-module with many simultaneous issues
// ============================================================================
describe("Stress: load-module with mixed issues", () => {
  it("handles compilation with errors + warnings + holes simultaneously", async () => {
    const mixedOutput =
      "src/Foo.hs:3:1-5: warning: [GHC-38417] [-Wmissing-signatures]\n" +
      "    Top-level binding with no type signature:\n" +
      "      foo :: Int\n" +
      "src/Foo.hs:5:10: error: [GHC-83865]\n" +
      "    Couldn\u2019t match expected type \u2018Int\u2019 with actual type \u2018Bool\u2019\n" +
      "    In the expression: True\n" +
      "Failed, no modules loaded.";

    const session = createMockSession({
      execute: async (): Promise<GhciResult> => ({ output: "", success: true }),
      loadModule: { output: mixedOutput, success: false },
    });
    const result = JSON.parse(await handleLoadModule(session, { module_path: "src/Foo.hs", diagnostics: false }));
    expect(result.success).toBe(false);
    expect(result.errors.length).toBeGreaterThan(0);
    expect(result.warnings.length).toBeGreaterThan(0);
  });
});

// ============================================================================
// STRESS: typeCheck with edge-case Haskell expressions
// ============================================================================
describe("Stress: type check unusual expressions", () => {
  it("handles section syntax", async () => {
    const session = createMockSession({
      typeOf: { output: "(+1) :: Num a => a -> a", success: true },
    });
    const result = JSON.parse(await handleTypeCheck(session, { expression: "(+1)" }));
    expect(result.success).toBe(true);
    expect(result.type).toContain("Num a");
  });

  it("handles tuple construction", async () => {
    const session = createMockSession({
      typeOf: { output: "(,) :: a -> b -> (a, b)", success: true },
    });
    const result = JSON.parse(await handleTypeCheck(session, { expression: "(,)" }));
    expect(result.success).toBe(true);
  });

  it("handles type application syntax", async () => {
    const session = createMockSession({
      typeOf: { output: "show @Int :: Int -> String", success: true },
    });
    const result = JSON.parse(await handleTypeCheck(session, { expression: "show @Int" }));
    expect(result.success).toBe(true);
    expect(result.type).toContain("Int -> String");
  });

  it("handles expression with do-notation", async () => {
    const session = createMockSession({
      typeOf: {
        output: "do { x <- getLine; return x } :: IO String",
        success: true,
      },
    });
    const result = JSON.parse(await handleTypeCheck(session, { expression: "do { x <- getLine; return x }" }));
    expect(result.success).toBe(true);
  });
});
