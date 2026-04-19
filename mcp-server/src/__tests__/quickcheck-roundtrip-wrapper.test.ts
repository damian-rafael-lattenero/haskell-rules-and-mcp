/**
 * Unit tests: the `roundtrip_wrapper` parameter of ghci_quickcheck generates
 * the correct property expression when the caller needs to scope the
 * Arbitrary instance to a newtype wrapper.
 *
 * These are string-shape tests — we don't need to actually run QuickCheck
 * because the generator logic is pure. We feed `handleQuickCheck` a mock
 * session that captures the property text and assert on it.
 */
import { describe, it, expect } from "vitest";
import { handleQuickCheck } from "../tools/quickcheck.js";

function makeCapturingSession() {
  const session = {
    execute: async (cmd: string) => {
      if (cmd.includes("import Test.QuickCheck")) {
        return { output: "", success: true };
      }
      if (cmd.includes("quickCheckWith") || cmd.includes("verboseCheckWith")) {
        return { output: "+++ OK, passed 100 tests.\n", success: true };
      }
      return { output: "", success: true };
    },
    typeOf: async () => ({ output: "", success: false }),
    loadModules: async () => {},
    isAlive: () => true,
  } as any;
  return { session };
}

describe("ghci_quickcheck — roundtrip shorthand (existing behaviour preserved)", () => {
  it("without wrapper: generates `\\e -> parse (pretty e) == Just e`", async () => {
    const { session } = makeCapturingSession();
    const raw = await handleQuickCheck(session, {
      property: "roundtrip",
      roundtrip: "pretty,parse",
    });
    const result = JSON.parse(raw);
    expect(result.generated_property).toBe("\\e -> parse (pretty e) == Just e");
  });

  it("with normalize: generates fmap-normalize form", async () => {
    const { session } = makeCapturingSession();
    const raw = await handleQuickCheck(session, {
      property: "roundtrip",
      roundtrip: "pretty,parse,normalize",
    });
    const result = JSON.parse(raw);
    expect(result.generated_property).toBe(
      "\\e -> fmap normalize (parse (pretty e)) == Just (normalize e)"
    );
  });
});

describe("ghci_quickcheck — roundtrip_wrapper (NEW)", () => {
  it("with wrapper alone: binds `w`, unwraps to `e` via the function", async () => {
    const { session } = makeCapturingSession();
    const raw = await handleQuickCheck(session, {
      property: "roundtrip",
      roundtrip: "pretty,parse",
      roundtrip_wrapper: "unPrettyable",
    });
    const result = JSON.parse(raw);
    expect(result.generated_property).toBe(
      "\\w -> let e = unPrettyable w in parse (pretty e) == Just e"
    );
  });

  it("with wrapper + normalize: combines both forms correctly", async () => {
    const { session } = makeCapturingSession();
    const raw = await handleQuickCheck(session, {
      property: "roundtrip",
      roundtrip: "pretty,parse,normalize",
      roundtrip_wrapper: "unPrettyable",
    });
    const result = JSON.parse(raw);
    expect(result.generated_property).toBe(
      "\\w -> let e = unPrettyable w in fmap normalize (parse (pretty e)) == Just (normalize e)"
    );
  });

  it("empty wrapper string is treated as absent (default behaviour kicks in)", async () => {
    const { session } = makeCapturingSession();
    const raw = await handleQuickCheck(session, {
      property: "roundtrip",
      roundtrip: "pretty,parse",
      roundtrip_wrapper: "   ", // whitespace only
    });
    const result = JSON.parse(raw);
    expect(result.generated_property).toBe("\\e -> parse (pretty e) == Just e");
  });

  it("wrapper name is interpolated verbatim (supports qualified names)", async () => {
    const { session } = makeCapturingSession();
    const raw = await handleQuickCheck(session, {
      property: "roundtrip",
      roundtrip: "pretty,parse",
      roundtrip_wrapper: "MyMod.unWrap",
    });
    const result = JSON.parse(raw);
    expect(result.generated_property).toBe(
      "\\w -> let e = MyMod.unWrap w in parse (pretty e) == Just e"
    );
  });
});
