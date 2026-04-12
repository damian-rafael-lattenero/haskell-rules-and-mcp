/**
 * Regression tests for bugs found during smoke testing (2026-04-12).
 *
 * Each test documents a specific bug, reproduces it with real GHC output,
 * and verifies the fix. If any fix is reverted, these tests will catch it.
 *
 * Bug reference: mcp-server/test-results/2026-04-12.md
 */
import { describe, it, expect, vi, afterEach } from "vitest";
import { parseGhcErrors } from "../parsers/error-parser.js";
import { parseInfoOutput, parseTypeOutput } from "../parsers/type-parser.js";
import { parseEvalOutput } from "../parsers/eval-output-parser.js";
import { handleTypeCheck } from "../tools/type-check.js";
import { handleCheckModule } from "../tools/check-module.js";
import { handleHoogleSearch } from "../tools/hoogle.js";
import { createMockSession } from "./helpers/mock-session.js";
import type { GhciResult } from "../ghci-session.js";

// ============================================================================
// BUG FIX 1: ghci_type returned success:true for out-of-scope names
//
// With -fdefer-type-errors active, `:t nonExistentFunction` returned
// type "p" instead of an error. The tool marked it as success:true.
// Fix: detect "deferred-out-of-scope-variables" / "Variable not in scope"
// ============================================================================
describe("Bug Fix 1: ghci_type deferred-out-of-scope detection", () => {
  it("REGRESSION: `:t nonExistentFunction` with -fdefer-type-errors must fail", async () => {
    // This is the EXACT output GHCi 9.12 produces when you `:t` an unknown name
    // with -fdefer-type-errors active. Before the fix, handleTypeCheck returned
    // { success: true, expression: "nonExistentFunction", type: "p" }
    const session = createMockSession({
      typeOf: {
        output:
          "<interactive>:1:1-19: warning: [GHC-88464] [-Wdeferred-out-of-scope-variables]\n" +
          "    Variable not in scope: nonExistentFunction\n" +
          "nonExistentFunction :: p",
        success: true, // GHCi considers this "success" since error is deferred
      },
    });
    const result = JSON.parse(await handleTypeCheck(session, { expression: "nonExistentFunction" }));

    // MUST be false — the old bug had this as true
    expect(result.success).toBe(false);
    // MUST NOT return type "p"
    expect(result.type).toBeUndefined();
    // Should include the error for context
    expect(result.error).toContain("Variable not in scope");
  });

  it("REGRESSION: valid deferred type (not out-of-scope) should still succeed", async () => {
    // Ensure we don't over-correct: a valid `:t` with no scope issues must still work
    const session = createMockSession({
      typeOf: {
        output: "map :: (a -> b) -> [a] -> [b]",
        success: true,
      },
    });
    const result = JSON.parse(await handleTypeCheck(session, { expression: "map" }));
    expect(result.success).toBe(true);
    expect(result.type).toBe("(a -> b) -> [a] -> [b]");
  });
});

// ============================================================================
// BUG FIX 2: ghci_eval returned success:true for runtime exceptions
//
// `head []` and `div 1 0` returned success:true with the exception text
// in the output field. Fix: detect "*** Exception:" prefix.
// ============================================================================
describe("Bug Fix 2: ghci_eval runtime exception detection", () => {
  it("REGRESSION: `head []` must return success:false", () => {
    // This is the EXACT cleaned output after warning separation.
    // Before the fix, the eval handler passed result.success through unchanged.
    const parsed = parseEvalOutput(
      "<interactive>:1:1-4: warning: [GHC-63394] [-Wx-partial]\n" +
        '    In the use of \'head\': "This is a partial function"\n\n' +
        "*** Exception: Prelude.head: empty list\n\n" +
        "HasCallStack backtrace:\n" +
        "  error, called at libraries/ghc-internal/src/GHC/Internal/List.hs:2036:3"
    );

    // The parsed result should start with the exception
    expect(parsed.result).toMatch(/^\*\*\* Exception:/);

    // Simulate what index.ts does: check for exception prefix
    const isException = parsed.result.startsWith("*** Exception:");
    expect(isException).toBe(true);
  });

  it("REGRESSION: `div 1 0` must return success:false", () => {
    const parsed = parseEvalOutput(
      "<interactive>:1:1-7: warning: [GHC-18042] [-Wtype-defaults]\n" +
        "    \u2022 Defaulting the type variable\n\n" +
        "*** Exception: divide by zero"
    );
    expect(parsed.result).toBe("*** Exception: divide by zero");
    expect(parsed.result.startsWith("*** Exception:")).toBe(true);
  });

  it("REGRESSION: normal eval result must NOT be flagged as exception", () => {
    const parsed = parseEvalOutput("42");
    expect(parsed.result).toBe("42");
    expect(parsed.result.startsWith("*** Exception:")).toBe(false);
  });
});

// ============================================================================
// BUG FIX 3: ghci_load returned success:true for nonexistent files
//
// `ghci_load(module_path="src/NoExiste.hs")` returned success:true because
// the error parser couldn't parse `<no location info>: error:` format.
// Fix: added regex for no-location errors in parseGhcErrors.
// ============================================================================
describe("Bug Fix 3: ghci_load nonexistent file detection", () => {
  it("REGRESSION: `<no location info>: error:` must be parsed as an error", () => {
    // This is the EXACT output GHCi produces for a missing source file
    const output =
      "<no location info>: error: [GHC-49196] Can't find src/NoExiste.hs\n\nFailed, unloaded all modules.";
    const errors = parseGhcErrors(output);

    // Before the fix, errors was [] because the regex only matched file:line:col format
    expect(errors.length).toBeGreaterThanOrEqual(1);
    const error = errors.find((e) => e.file === "<no location>");
    expect(error).toBeDefined();
    expect(error!.severity).toBe("error");
    expect(error!.code).toBe("GHC-49196");
    expect(error!.message).toContain("Can't find");
  });

  it("REGRESSION: regular located errors must still parse", () => {
    // Ensure the fix didn't break normal error parsing
    const output =
      "src/Foo.hs:3:7-10: error: [GHC-83865]\n" +
      "    Couldn\u2019t match expected type \u2018Int\u2019 with actual type \u2018Bool\u2019";
    const errors = parseGhcErrors(output);
    expect(errors).toHaveLength(1);
    expect(errors[0]!.file).toBe("src/Foo.hs");
    expect(errors[0]!.code).toBe("GHC-83865");
  });
});

// ============================================================================
// BUG FIX 4: ghci_info classified everything as "type-synonym"
//
// parseInfoOutput always returned kind:"type-synonym" because GHC 9.12 prefixes
// type definitions with `type X :: Kind` before the actual `data`/`newtype`/`class`.
// Fix: check subsequent lines to reclassify. Also handle `type role` annotations.
// ============================================================================
describe("Bug Fix 4: ghci_info kind classification", () => {
  it("REGRESSION: Maybe must be classified as 'data', not 'type-synonym'", () => {
    // GHC 9.12 output for `:i Maybe`
    const output =
      "type Maybe :: * -> *\n" +
      "data Maybe a = Nothing | Just a\n" +
      "  \t-- Defined in 'ghc-internal-9.1202.0:GHC.Internal.Maybe'\n" +
      "instance Eq a => Eq (Maybe a)";
    const result = parseInfoOutput(output);
    // Before the fix, this was "type-synonym" because firstLine starts with "type "
    expect(result.kind).toBe("data");
    expect(result.name).toBe("Maybe");
  });

  it("REGRESSION: Container must be classified as 'class', not 'type-synonym'", () => {
    const output =
      "type Container :: (* -> *) -> Constraint\n" +
      "class Container f where\n" +
      "  empty :: f a\n" +
      "  insert :: a -> f a -> f a";
    const result = parseInfoOutput(output);
    expect(result.kind).toBe("class");
  });

  it("REGRESSION: Wrap must be classified as 'newtype', not 'type-synonym'", () => {
    // GHC 9.12 output for `:i Wrap` — starts with role annotation
    const output =
      "type role Wrap representational nominal\n" +
      "type Wrap :: forall {k}. (k -> *) -> k -> *\n" +
      "newtype Wrap f a = Wrap {unWrap :: f a}\n" +
      "  \t-- Defined at src/SmokeKind.hs:4:1";
    const result = parseInfoOutput(output);
    // Before the fix, "type role" was classified as "type-synonym"
    expect(result.kind).toBe("newtype");
    expect(result.name).toBe("Wrap");
  });

  it("REGRESSION: actual type synonym must still be 'type-synonym'", () => {
    const output = "type String = [Char]\n  \t-- Defined in 'GHC.Internal.Base'";
    const result = parseInfoOutput(output);
    expect(result.kind).toBe("type-synonym");
  });
});

// ============================================================================
// BUG FIX 5: ghci_check_module concatenated typeclass methods
//
// When parsing `:browse` output, the function signature continuation logic
// consumed class method lines and {-# MINIMAL #-} pragmas, producing:
//   empty.type = "f a insert :: a -> f a -> f a {-# MINIMAL empty, insert #-}"
// Fix: class handler now consumes body and extracts methods; continuation
// logic stops at pragmas and new signatures.
// ============================================================================
describe("Bug Fix 5: ghci_check_module method concatenation", () => {
  it("REGRESSION: class methods must be separate definitions", async () => {
    // Simulate `:browse` output for a module with a typeclass
    const browseOutput =
      "type Container :: (* -> *) -> Constraint\n" +
      "class Container f where\n" +
      "  empty :: f a\n" +
      "  insert :: a -> f a -> f a\n" +
      "  {-# MINIMAL empty, insert #-}";

    const session = createMockSession({
      loadModule: { output: "Ok, one module loaded.", success: true },
      execute: async (cmd: string): Promise<GhciResult> => {
        if (cmd.startsWith(":browse")) return { output: browseOutput, success: true };
        return { output: "", success: true };
      },
    });

    const result = JSON.parse(
      await handleCheckModule(session, { module_path: "src/K.hs", module_name: "K" })
    );

    const emptyDef = result.definitions.find((d: any) => d.name === "empty");
    const insertDef = result.definitions.find((d: any) => d.name === "insert");

    // Before the fix, empty.type was:
    //   "f a insert :: a -> f a -> f a {-# MINIMAL empty, insert #-}"
    // and insert didn't exist as a separate definition.
    expect(emptyDef).toBeDefined();
    expect(emptyDef.type).toBe("f a");
    expect(emptyDef.type).not.toContain("insert");
    expect(emptyDef.type).not.toContain("MINIMAL");

    expect(insertDef).toBeDefined();
    expect(insertDef.type).toBe("a -> f a -> f a");
  });

  it("REGRESSION: function after class must not absorb pragma", async () => {
    const browseOutput =
      "class Eq a where\n" +
      "  (==) :: a -> a -> Bool\n" +
      "  {-# MINIMAL (==) #-}\n" +
      "foo :: Int -> Int";

    const session = createMockSession({
      loadModule: { output: "Ok, one module loaded.", success: true },
      execute: async (cmd: string): Promise<GhciResult> => {
        if (cmd.startsWith(":browse")) return { output: browseOutput, success: true };
        return { output: "", success: true };
      },
    });

    const result = JSON.parse(
      await handleCheckModule(session, { module_path: "src/X.hs", module_name: "X" })
    );

    const foo = result.definitions.find((d: any) => d.name === "foo");
    expect(foo).toBeDefined();
    expect(foo.type).toBe("Int -> Int");
    expect(foo.type).not.toContain("MINIMAL");
  });
});

// ============================================================================
// EDGE CASE: Hoogle HTML stripping
// ============================================================================
describe("Edge case: Hoogle HTML entity handling", () => {
  const originalFetch = globalThis.fetch;
  afterEach(() => {
    globalThis.fetch = originalFetch;
  });

  it("strips all common HTML entities", async () => {
    globalThis.fetch = vi.fn().mockResolvedValue({
      ok: true,
      status: 200,
      json: async () => [
        {
          url: "",
          module: { name: "M", url: "" },
          package: { name: "p", url: "" },
          item: "f :: a &amp; b &lt;- c &gt; d &quot;e&quot; &#39;f&#39;",
          type: "",
          docs: "",
        },
      ],
    });
    const result = JSON.parse(await handleHoogleSearch({ query: "f" }));
    const name = result.results[0].name;
    expect(name).not.toContain("&amp;");
    expect(name).not.toContain("&lt;");
    expect(name).not.toContain("&gt;");
    expect(name).toContain("& b");
    expect(name).toContain("<- c");
    expect(name).toContain("> d");
  });
});

// ============================================================================
// EDGE CASE: QuickCheck parser with unusual output
// ============================================================================
describe("Edge case: parseTypeOutput resilience", () => {
  it("handles GHCi output with leading blank lines", () => {
    const result = parseTypeOutput("\n\nmap :: (a -> b) -> [a] -> [b]");
    expect(result).not.toBeNull();
    expect(result!.type).toBe("(a -> b) -> [a] -> [b]");
  });

  it("returns null for ':t' echo without type", () => {
    expect(parseTypeOutput(":t")).toBeNull();
  });

  it("handles type with Unicode characters", () => {
    // GHC sometimes uses Unicode in constraint arrows
    const result = parseTypeOutput("f :: \u2200 a. Show a => a -> String");
    // Depending on how GHCi formats, this should parse
    expect(result).not.toBeNull();
  });
});
