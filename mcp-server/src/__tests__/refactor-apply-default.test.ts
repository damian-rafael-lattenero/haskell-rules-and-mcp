/**
 * Fase 4: `ghci_refactor(rename_local)` now defaults to preview (apply: false),
 * aligning with `ghci_rename`. Regression test that the file is NOT mutated
 * when `apply` is omitted.
 */
import { describe, it, expect, afterEach } from "vitest";
import { mkdtempSync, rmSync, writeFileSync, readFileSync, mkdirSync } from "node:fs";
import os from "node:os";
import path from "node:path";
import { handleRefactor } from "../tools/refactor.js";

describe("handleRefactor(rename_local) — apply default", () => {
  const tmpDirs: string[] = [];
  function mkProject(): { dir: string; modulePath: string; original: string } {
    const dir = mkdtempSync(path.join(os.tmpdir(), "refactor-apply-"));
    tmpDirs.push(dir);
    mkdirSync(path.join(dir, "src"), { recursive: true });
    const modulePath = "src/Foo.hs";
    const original =
      "module Foo where\n" +
      "\n" +
      "bar :: Int -> Int\n" +
      "bar x = x + 1\n" +
      "\n" +
      "baz :: Int -> Int\n" +
      "baz y = bar y + bar y\n";
    writeFileSync(path.join(dir, modulePath), original, "utf8");
    return { dir, modulePath, original };
  }

  afterEach(() => {
    for (const d of tmpDirs.splice(0)) {
      try { rmSync(d, { recursive: true, force: true }); } catch { /* ignore */ }
    }
  });

  it("DOES NOT mutate the file when apply is omitted", async () => {
    const { dir, modulePath, original } = mkProject();
    const result = JSON.parse(
      await handleRefactor(dir, {
        action: "rename_local",
        module_path: modulePath,
        old_name: "bar",
        new_name: "increment",
      })
    );
    expect(result.success).toBe(true);
    expect(result.applied).toBe(false);
    expect(result.changed).toBeGreaterThan(0);
    expect(result.diff.length).toBeGreaterThan(0);
    expect(result.message).toMatch(/Preview only/i);
    const after = readFileSync(path.join(dir, modulePath), "utf8");
    expect(after).toBe(original);
  });

  it("DOES mutate the file when apply=true", async () => {
    const { dir, modulePath } = mkProject();
    const result = JSON.parse(
      await handleRefactor(dir, {
        action: "rename_local",
        module_path: modulePath,
        old_name: "bar",
        new_name: "increment",
        apply: true,
      })
    );
    expect(result.success).toBe(true);
    expect(result.applied).toBe(true);
    const after = readFileSync(path.join(dir, modulePath), "utf8");
    expect(after).toContain("increment :: Int -> Int");
    expect(after).toContain("increment x = x + 1");
    expect(after).toContain("baz y = increment y + increment y");
    // No stragglers (old name fully replaced)
    expect(after).not.toMatch(/\bbar\b/);
  });

  it("reports zero-change when old_name is absent", async () => {
    const { dir, modulePath, original } = mkProject();
    const result = JSON.parse(
      await handleRefactor(dir, {
        action: "rename_local",
        module_path: modulePath,
        old_name: "nonexistent",
        new_name: "newname",
        apply: true,
      })
    );
    expect(result.success).toBe(true);
    expect(result.changed).toBe(0);
    expect(result.message).toMatch(/not found/i);
    const after = readFileSync(path.join(dir, modulePath), "utf8");
    expect(after).toBe(original);
  });
});
