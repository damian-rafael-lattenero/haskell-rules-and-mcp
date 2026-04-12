/**
 * Bug hunting tests — written to FIND bugs, not just cover code.
 * Each test targets a specific scenario a Haskell developer would hit.
 * Tests that FAIL expose real bugs that need fixing.
 */
import { describe, it, expect } from "vitest";
import { parseGhcErrors } from "../parsers/error-parser.js";
import { parseInfoOutput } from "../parsers/type-parser.js";
import { parseEvalOutput } from "../parsers/eval-output-parser.js";
import { categorizeWarning } from "../parsers/warning-categorizer.js";
import { parseQuickCheckOutput } from "../tools/quickcheck.js";
import { handleTypeInfo } from "../tools/type-info.js";
import { handleCheckModule } from "../tools/check-module.js";
import { extractModules } from "../parsers/cabal-parser.js";
import { createMockSession } from "./helpers/mock-session.js";
import type { GhcError } from "../parsers/error-parser.js";
import type { GhciResult } from "../ghci-session.js";

function makeWarning(
  overrides: Partial<GhcError> & { message: string; warningFlag: string }
): GhcError {
  return { file: "src/Test.hs", line: 1, column: 1, severity: "warning", ...overrides };
}

// ============================================================================
// BUG HUNT 1: ghci_info has the same deferred-scope bug as ghci_type
//
// handleTypeInfo doesn't check for "Variable not in scope" — same oversight
// as the original ghci_type bug, but in :i instead of :t
// ============================================================================
describe("BUG HUNT: ghci_info deferred-out-of-scope", () => {
  it("should return success:false for out-of-scope name with deferred errors", async () => {
    const session = createMockSession({
      infoOf: {
        output:
          "<interactive>:1:1-15: warning: [GHC-88464] [-Wdeferred-out-of-scope-variables]\n" +
          "    Variable not in scope: nonExistent\n" +
          "nonExistent :: p",
        success: true,
      },
    });
    const result = JSON.parse(await handleTypeInfo(session, { name: "nonExistent" }));
    // This SHOULD be false — if it's true, we found a bug
    expect(result.success).toBe(false);
  });
});

// ============================================================================
// BUG HUNT 2: QuickCheck "Gave up!" is a real scenario
//
// When using ==> (implication) with restrictive preconditions, QuickCheck
// discards too many tests and gives up. A Haskell dev needs to know this.
// ============================================================================
describe("BUG HUNT: QuickCheck 'Gave up!' output", () => {
  it("should parse 'Gave up!' as a failure with meaningful message", () => {
    const output = "*** Gave up! Passed only 47 tests; 1000 discarded tests.\n";
    const result = parseQuickCheckOutput(output, "\\x -> x > 100 ==> x * 2 > 200");
    expect(result.success).toBe(false);
    // It should NOT say "Couldn't parse" — that's unhelpful
    // It should say something about giving up
    expect(result.error).not.toContain("Couldn't parse");
  });
});

// ============================================================================
// BUG HUNT 3: -Wmissing-signatures with multiline type
//
// Complex Haskell types often span multiple lines. The categorizer should
// extract the FULL type, not just the first line.
// ============================================================================
describe("BUG HUNT: missing-signature multiline type extraction", () => {
  it("should capture full multiline type signature", () => {
    const w = makeWarning({
      warningFlag: "-Wmissing-signatures",
      message:
        "Top-level binding with no type signature:\n" +
        "      runApp\n" +
        "        :: ReaderT Config (ExceptT AppError IO) a -> Config -> IO (Either AppError a)\n" +
        "  |\n" +
        "15 | runApp action cfg = runExceptT (runReaderT action cfg)\n" +
        "   | ^^^^^^",
    });
    const action = categorizeWarning(w);
    expect(action).not.toBeNull();
    expect(action!.category).toBe("missing-signature");
    // Should have the full type, not just "runApp"
    expect(action!.suggestedAction).toContain("ReaderT");
    expect(action!.suggestedAction).toContain("Config");
  });
});

// ============================================================================
// BUG HUNT 4: eval output with indented result (looks like warning continuation)
//
// Some Haskell values print with leading whitespace. The eval parser might
// mistake them for warning continuation lines.
// ============================================================================
describe("BUG HUNT: eval indented result confused with warning", () => {
  it("should handle result that starts with spaces after a warning", () => {
    // A Map or formatted data structure might have leading spaces
    const raw =
      "<interactive>:1:1: warning: [GHC-18042] [-Wtype-defaults]\n" +
      "    • Defaulting type variable\n" +
      "\n" +
      "  fromList [(1,2),(3,4)]";
    const parsed = parseEvalOutput(raw);
    // The indented result should NOT be swallowed into the warning
    expect(parsed.result).toContain("fromList");
  });
});

// ============================================================================
// BUG HUNT 5: check-module with operator class methods
//
// Haskell classes often have operator methods like (==), (>>=), (<*>).
// The :browse output shows them differently.
// ============================================================================
describe("BUG HUNT: check-module operator class methods", () => {
  it("should parse operator methods from class body", async () => {
    const browse =
      "class Eq a where\n" +
      "  (==) :: a -> a -> Bool\n" +
      "  (/=) :: a -> a -> Bool\n" +
      "  {-# MINIMAL (==) | (/=) #-}";
    const session = createMockSession({
      loadModule: { output: "Ok, one module loaded.", success: true },
      execute: async (cmd: string): Promise<GhciResult> => {
        if (cmd.startsWith(":browse")) return { output: browse, success: true };
        return { output: "", success: true };
      },
    });
    const result = JSON.parse(await handleCheckModule(session, { module_path: "src/E.hs", module_name: "E" }));
    const names = result.definitions.map((d: any) => d.name);
    expect(names).toContain("(==)");
    expect(names).toContain("(/=)");
  });
});

// ============================================================================
// BUG HUNT 6: error parser with multiline span (GHC 9.12 format)
//
// GHC 9.12 can report errors spanning multiple lines: file:line1:col1-line2:col2
// ============================================================================
describe("BUG HUNT: error parser multiline span format", () => {
  it("should parse error with line range", () => {
    // GHC 9.12 format for errors spanning multiple lines
    const output =
      "src/Foo.hs:5:1-7:15: error: [GHC-83865]\n" +
      "    Couldn't match expected type 'Int' with actual type 'String'";
    const errors = parseGhcErrors(output);
    // Should parse this — the column range is 1-7 (endColumn would be 15 in single-line view)
    // In multiline format, this is line 5 col 1 to line 7 col 15
    expect(errors.length).toBeGreaterThanOrEqual(1);
  });
});

// ============================================================================
// BUG HUNT 7: cabal parser with if/else conditional blocks
//
// Real-world cabal files use conditional blocks. Modules inside them
// should still be discovered.
// ============================================================================
describe("BUG HUNT: cabal parser conditional blocks", () => {
  it("should handle modules inside if-else blocks", () => {
    const content = `cabal-version: 3.12
name: mylib

library
  exposed-modules:
    Lib
  if os(windows)
    exposed-modules:
      Lib.Windows
  else
    exposed-modules:
      Lib.Unix
  build-depends: base
  hs-source-dirs: src
`;
    const result = extractModules(content);
    // At minimum, Lib should be present
    expect(result.library).toContain("Lib");
    // Ideally, platform-specific modules would also be found
    // (better to discover too many than miss them)
    expect(result.library.length).toBeGreaterThanOrEqual(1);
  });
});

// ============================================================================
// BUG HUNT 8: parseInfoOutput with instance that has constraints
//
// GHC often shows instances with complex constraints. The instance
// extraction should handle them correctly.
// ============================================================================
describe("BUG HUNT: info parser complex instances", () => {
  it("should extract instances with multi-param constraints", () => {
    const output =
      "class Monad m where\n" +
      "  (>>=) :: m a -> (a -> m b) -> m b\n" +
      "  return :: a -> m a\n" +
      "instance Monad IO\n" +
      "instance Monad []\n" +
      "instance Monad m => Monad (StateT s m)\n" +
      "instance (Monoid w, Monad m) => Monad (WriterT w m)";
    const result = parseInfoOutput(output);
    expect(result.kind).toBe("class");
    expect(result.instances).toBeDefined();
    expect(result.instances!.length).toBe(4);
    expect(result.instances!.some(i => i.includes("StateT"))).toBe(true);
    expect(result.instances!.some(i => i.includes("WriterT"))).toBe(true);
  });
});
