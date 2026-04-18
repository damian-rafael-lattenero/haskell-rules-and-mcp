import { describe, it, expect } from "vitest";
import { handleLint, handleLintBasic } from "../tools/lint.js";
import { mkdtemp, writeFile, rm, mkdir } from "node:fs/promises";
import { tmpdir } from "node:os";
import path from "node:path";

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
  it("returns degraded=true and suggestions for basic anti-patterns", async () => {
    const dir = await mkdtemp(path.join(tmpdir(), "lint-basic-"));
    try {
      const file = path.join(dir, "src/Foo.hs");
      await mkdir(path.dirname(file), { recursive: true });
      await writeFile(file, "foo xs = if null xs then True else False\n", "utf8");
      const result = JSON.parse(await handleLintBasic(dir, { module_path: "src/Foo.hs" }));
      expect(result.success).toBe(true);
      expect(result.degraded).toBe(true);
      expect(result.gateEligible).toBe(false);
      expect(result.count).toBeGreaterThan(0);
    } finally {
      await rm(dir, { recursive: true, force: true });
    }
  });
});
