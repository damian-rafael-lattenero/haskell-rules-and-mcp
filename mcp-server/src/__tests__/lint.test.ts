import { describe, it, expect, beforeAll, afterAll } from "vitest";
import { handleLint, handleLintBasic } from "../tools/lint.js";
import { mkdtemp, writeFile, rm, mkdir } from "node:fs/promises";
import { tmpdir } from "node:os";
import path from "node:path";
import {
  resetManifestCache,
  setManifestPathForTests,
} from "../vendor-tools/manifest.js";

// Same rationale as format.test.ts: swap in an empty-releases manifest so
// "hlint unavailable" returns fast instead of triggering a real ~136MB
// download. The test suite has never had hlint on PATH, and now that the
// release URL works we have to prevent the opportunistic download.
let emptyManifestDir: string;
beforeAll(async () => {
  emptyManifestDir = await mkdtemp(path.join(tmpdir(), "lint-manifest-"));
  const empty = {
    manifestVersion: 2,
    updatedAt: "test",
    releases: {
      hlint: { binaryName: "hlint", platforms: {} },
      fourmolu: { binaryName: "fourmolu", platforms: {} },
      ormolu: { binaryName: "ormolu", platforms: {} },
      hls: { binaryName: "haskell-language-server-wrapper", platforms: {} },
    },
    tools: [],
  };
  const manifestFile = path.join(emptyManifestDir, "manifest.json");
  await writeFile(manifestFile, JSON.stringify(empty), "utf-8");
  setManifestPathForTests(manifestFile);
  resetManifestCache();
});
afterAll(async () => {
  setManifestPathForTests(null);
  resetManifestCache();
  await rm(emptyManifestDir, { recursive: true, force: true });
});

describe("handleLint", () => {
  it("returns unavailable when hlint is not available", async () => {
    const result = JSON.parse(await handleLint("/tmp/fake", { module_path: "src/Test.hs" }));
    if (result.success) {
      expect(result.lint_tool).toBe("hlint");
    } else {
      expect(result.unavailable).toBe(true);
      expect(result.error).toMatch(/hlint|not available|not found/i);
    }
  });

  it("does not use fallback lint when hlint is unavailable even with session", async () => {
    const result = JSON.parse(await handleLint("/tmp/fake", { module_path: "src/Foo.hs" }, {}));
    if (!result.success) {
      expect(result.fallback).toBeUndefined();
      expect(result.unavailable).toBe(true);
    }
  });
});

describe("ghci_lint fallback contract (Plan B)", () => {
  // When hlint is unavailable, the ghci_lint tool wraps the response with
  // degraded=true, gateEligible=false, and a _primary_failure pointer to the
  // original hlint error. These two assertions pin the contract that the
  // degraded fallback surface continues to carry hints but does NOT satisfy
  // the module-complete lint gate.
  it("handleLint unavailable response carries enough info to construct the degraded envelope", async () => {
    const hlintResult = JSON.parse(await handleLint("/tmp/definitely-fake", { module_path: "src/Foo.hs" }));
    if (hlintResult.success) return; // hlint installed in CI — skip
    expect(hlintResult.unavailable).toBe(true);
    expect(hlintResult.reason).toBeDefined();
    expect(typeof hlintResult.error).toBe("string");
  });

  it("handleLintBasic result shape is compatible with the degraded envelope", async () => {
    const dir = await mkdtemp(path.join(tmpdir(), "lint-basic-envelope-"));
    try {
      const file = path.join(dir, "src/Foo.hs");
      await mkdir(path.dirname(file), { recursive: true });
      await writeFile(file, "foo = 1\n", "utf8");
      const basic = JSON.parse(await handleLintBasic(dir, { module_path: "src/Foo.hs" }));
      expect(basic.success).toBe(true);
      expect(basic.degraded).toBe(true);
      expect(basic.gateEligible).toBe(false);
      expect(basic.lint_tool).toBe("basic-lint-rules");
    } finally {
      await rm(dir, { recursive: true, force: true });
    }
  });
});

describe("handleLintBasic", () => {
  it("returns degraded=true and suggestions for partial-function use", async () => {
    const dir = await mkdtemp(path.join(tmpdir(), "lint-basic-"));
    try {
      const file = path.join(dir, "src/Foo.hs");
      await mkdir(path.dirname(file), { recursive: true });
      // `head xs` is one of the rules retained after the false-positive
      // cleanup. Any remaining rule should trigger here.
      await writeFile(file, "foo xs = head xs\n", "utf8");
      const result = JSON.parse(await handleLintBasic(dir, { module_path: "src/Foo.hs" }));
      expect(result.success).toBe(true);
      expect(result.degraded).toBe(true);
      expect(result.gateEligible).toBe(false);
      expect(result.count).toBeGreaterThan(0);
    } finally {
      await rm(dir, { recursive: true, force: true });
    }
  });

  it("does NOT flag legitimate patterns (module header, nested constructor apps)", async () => {
    // Regression guard for the false positives observed in the expr-evaluator
    // session (module header `module M (` and constructor application
    // `Lit (sign n)` both used to trigger redundant-parentheses).
    const dir = await mkdtemp(path.join(tmpdir(), "lint-basic-fp-"));
    try {
      const file = path.join(dir, "src/Expr/Pretty.hs");
      await mkdir(path.dirname(file), { recursive: true });
      await writeFile(
        file,
        [
          "module Expr.Pretty",
          "  ( pretty",
          "  , parse",
          "  ) where",
          "",
          "pretty = const \"\"",
          "",
          "litP = do",
          "  sign <- pure id",
          "  pure (Lit (sign 0))",
          "",
        ].join("\n"),
        "utf8"
      );
      const result = JSON.parse(await handleLintBasic(dir, { module_path: "src/Expr/Pretty.hs" }));
      expect(result.success).toBe(true);
      // The suggestions list should be empty (no FP rules triggered).
      expect(result.count).toBe(0);
    } finally {
      await rm(dir, { recursive: true, force: true });
    }
  });

  it("flags trailing whitespace (lexically safe rule)", async () => {
    const dir = await mkdtemp(path.join(tmpdir(), "lint-basic-ws-"));
    try {
      const file = path.join(dir, "src/Foo.hs");
      await mkdir(path.dirname(file), { recursive: true });
      await writeFile(file, "x = 1   \n", "utf8"); // trailing spaces
      const result = JSON.parse(await handleLintBasic(dir, { module_path: "src/Foo.hs" }));
      expect(result.suggestions.some((s: { hint: string }) => s.hint === "trailing-whitespace")).toBe(true);
    } finally {
      await rm(dir, { recursive: true, force: true });
    }
  });
});
