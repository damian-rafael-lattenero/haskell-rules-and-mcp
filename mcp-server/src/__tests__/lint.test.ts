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
