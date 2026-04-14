import { describe, expect, it } from "vitest";
import { buildCabalTestArgs } from "../tools/test.js";
import { replaceModuleHeaderWithExportList } from "../tools/apply-exports.js";
import { detectGeneratorWarnings } from "../tools/arbitrary.js";
import { buildFuzzCorpus, escapeHaskellString } from "../tools/fuzz-parser.js";
import {
  createWorkflowState,
  deriveGuidance,
  moduleChecklist,
  setOptionalToolAvailability,
  updateModuleProgress,
} from "../workflow-state.js";

describe("workflow upgrades", () => {
  it("buildCabalTestArgs defaults to cabal test", () => {
    expect(buildCabalTestArgs()).toEqual(["test"]);
  });

  it("buildCabalTestArgs supports a specific component", () => {
    expect(buildCabalTestArgs("test:pkg-test")).toEqual(["test", "test:pkg-test"]);
  });

  it("replaceModuleHeaderWithExportList rewrites a plain module header", () => {
    const source = `module Foo where\n\nfoo :: Int\nfoo = 1\n`;
    const replaced = replaceModuleHeaderWithExportList(
      source,
      "module Foo\n  ( foo\n  ) where"
    );
    expect(replaced.replaced).toBe(true);
    expect(replaced.updated).toContain("( foo");
    expect(replaced.updated).toContain("foo = 1");
  });

  it("detectGeneratorWarnings flags listOf without resize", () => {
    const warnings = detectGeneratorWarnings(
      "instance Arbitrary Foo where\n  arbitrary = listOf arbitrary",
      false
    );
    expect(warnings.some((w) => w.includes("listOf"))).toBe(true);
  });

  it("escapeHaskellString preserves quotes and nul", () => {
    expect(escapeHaskellString("a\"b\0c")).toBe("a\\\"b\\0c");
  });

  it("buildFuzzCorpus keeps user inputs and deterministic malformed seeds", () => {
    const corpus = buildFuzzCorpus(["custom-input"], 4);
    expect(corpus).toContain("custom-input");
    expect(corpus).toContain("(");
    expect(corpus.some((item) => item.includes("prefix"))).toBe(true);
  });

  it("deriveGuidance downgrades lint/format when tooling is unavailable", () => {
    const state = createWorkflowState();
    state.activeModule = "src/Foo.hs";
    updateModuleProgress(state, "src/Foo.hs", {
      functionsImplemented: 1,
      functionsTotal: 1,
      arbitraryInstancesDefined: true,
      propertiesPassed: ["p1"],
      completionGates: { checkModule: true, lint: false, format: false },
    });
    setOptionalToolAvailability(state, "lint", "unavailable");
    setOptionalToolAvailability(state, "format", "unavailable");

    const guidance = deriveGuidance(state, "ghci_load");
    expect(guidance.some((g) => g.includes("recommended but not blocking"))).toBe(true);

    const checklist = moduleChecklist(state);
    expect(checklist.some((item) => item.includes("recommended (tool unavailable)"))).toBe(true);
  });
});
