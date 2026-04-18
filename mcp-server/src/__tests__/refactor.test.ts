import { describe, it, expect, beforeEach, afterEach } from "vitest";
import { handleRefactor } from "../tools/refactor.js";
import { mkdtemp, rm, readFile, writeFile } from "node:fs/promises";
import path from "node:path";
import os from "node:os";

const SAMPLE_MODULE = `module Sample where

helper :: Int -> Int
helper x = x + 1

compute :: Int -> Int
compute n = helper (helper n)

main :: IO ()
main = print (compute 5)
`;

describe("handleRefactor — rename_local", () => {
  let dir: string;

  beforeEach(async () => {
    dir = await mkdtemp(path.join(os.tmpdir(), "refactor-test-"));
    await writeFile(path.join(dir, "Sample.hs"), SAMPLE_MODULE, "utf-8");
  });

  afterEach(async () => {
    await rm(dir, { recursive: true, force: true });
  });

  it("renames all occurrences of a binding (apply=true)", async () => {
    const result = JSON.parse(
      await handleRefactor(dir, {
        action: "rename_local",
        module_path: "Sample.hs",
        old_name: "helper",
        new_name: "increment",
        apply: true,
      })
    );
    expect(result.success).toBe(true);
    expect(result.applied).toBe(true);
    expect(result.changed).toBeGreaterThan(0);

    const content = await readFile(path.join(dir, "Sample.hs"), "utf-8");
    expect(content).toContain("increment");
    expect(content).not.toContain("helper");
  });

  it("does not rename substrings (word-boundary aware)", async () => {
    await writeFile(
      path.join(dir, "Sample.hs"),
      "module Sample where\n\nfoo = 1\nfooBar = foo + 1\n",
      "utf-8"
    );
    const result = JSON.parse(
      await handleRefactor(dir, {
        action: "rename_local",
        module_path: "Sample.hs",
        old_name: "foo",
        new_name: "baz",
        apply: true,
      })
    );
    expect(result.success).toBe(true);
    expect(result.applied).toBe(true);
    const content = await readFile(path.join(dir, "Sample.hs"), "utf-8");
    expect(content).toContain("baz");
    // fooBar should NOT become bazBar
    expect(content).toContain("fooBar");
    expect(content).not.toContain("bazBar");
  });

  it("returns changed:0 when name not found", async () => {
    const result = JSON.parse(
      await handleRefactor(dir, {
        action: "rename_local",
        module_path: "Sample.hs",
        old_name: "nonexistent",
        new_name: "something",
      })
    );
    expect(result.success).toBe(true);
    expect(result.changed).toBe(0);
  });

  it("returns error when module_path missing", async () => {
    const result = JSON.parse(
      await handleRefactor(dir, {
        action: "rename_local",
        old_name: "helper",
        new_name: "increment",
      })
    );
    expect(result.success).toBe(false);
    expect(result.error).toBeDefined();
  });

  it("returns error when old_name or new_name missing", async () => {
    const result = JSON.parse(
      await handleRefactor(dir, {
        action: "rename_local",
        module_path: "Sample.hs",
      })
    );
    expect(result.success).toBe(false);
    expect(result.error).toBeDefined();
  });

  it("returns diff showing changed lines", async () => {
    const result = JSON.parse(
      await handleRefactor(dir, {
        action: "rename_local",
        module_path: "Sample.hs",
        old_name: "helper",
        new_name: "increment",
      })
    );
    expect(result.success).toBe(true);
    expect(Array.isArray(result.diff)).toBe(true);
    expect(result.diff.length).toBeGreaterThan(0);
  });
});

describe("handleRefactor — extract_binding", () => {
  let dir: string;

  beforeEach(async () => {
    dir = await mkdtemp(path.join(os.tmpdir(), "refactor-test-"));
  });

  afterEach(async () => {
    await rm(dir, { recursive: true, force: true });
  });

  it("extracts a range of lines to a new top-level function", async () => {
    await writeFile(
      path.join(dir, "Calc.hs"),
      `module Calc where

compute :: Int -> Int
compute n =
  let step1 = n + 1
      step2 = step1 * 2
  in step2
`,
      "utf-8"
    );

    const result = JSON.parse(
      await handleRefactor(dir, {
        action: "extract_binding",
        module_path: "Calc.hs",
        new_name: "doubleSucc",
        lines: [5, 6],
      })
    );
    expect(result.success).toBe(true);
    const content = await readFile(path.join(dir, "Calc.hs"), "utf-8");
    expect(content).toContain("doubleSucc");
  });

  it("returns error for invalid line range", async () => {
    await writeFile(path.join(dir, "X.hs"), "module X where\nx = 1\n", "utf-8");
    const result = JSON.parse(
      await handleRefactor(dir, {
        action: "extract_binding",
        module_path: "X.hs",
        new_name: "extracted",
        lines: [100, 200],
      })
    );
    expect(result.success).toBe(false);
    expect(result.error).toBeDefined();
  });

  it("returns error when new_name missing", async () => {
    await writeFile(path.join(dir, "X.hs"), "module X where\nx = 1\n", "utf-8");
    const result = JSON.parse(
      await handleRefactor(dir, {
        action: "extract_binding",
        module_path: "X.hs",
        lines: [1, 2],
      })
    );
    expect(result.success).toBe(false);
    expect(result.error).toBeDefined();
  });
});

describe("handleRefactor — unknown action", () => {
  let dir: string;

  beforeEach(async () => {
    dir = await mkdtemp(path.join(os.tmpdir(), "refactor-test-"));
  });

  afterEach(async () => {
    await rm(dir, { recursive: true, force: true });
  });

  it("returns error for unknown action", async () => {
    const result = JSON.parse(
      await handleRefactor(dir, { action: "teleport" })
    );
    expect(result.success).toBe(false);
    expect(result.error).toMatch(/unknown action/i);
  });
});
