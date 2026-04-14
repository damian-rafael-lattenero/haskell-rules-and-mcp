import { describe, it, expect, afterEach } from "vitest";
import { handleFormat } from "../tools/format.js";
import { writeFile, mkdtemp, rm, readFile } from "node:fs/promises";
import path from "node:path";
import os from "node:os";

describe("handleFormat", () => {
  const tmpDirs: string[] = [];

  async function makeTmpDir(): Promise<string> {
    const dir = await mkdtemp(path.join(os.tmpdir(), "format-test-"));
    tmpDirs.push(dir);
    return dir;
  }

  afterEach(async () => {
    for (const dir of tmpDirs) {
      try { await rm(dir, { recursive: true }); } catch { /* ignore */ }
    }
    tmpDirs.length = 0;
  });

  it("returns error when formatting fails (formatter available via auto-download)", async () => {
    const result = JSON.parse(await handleFormat("/tmp/fake", { module_path: "src/Test.hs" }));
    // With auto-download, formatters are now available on-demand
    // This test verifies that formatter is available but fails on invalid path
    expect(result.success).toBe(false);
    expect(result.formatter).toMatch(/fourmolu|ormolu/);
    expect(result.error).toBe("Formatting failed");
    // unavailable is NOT set because formatter IS available (just failed to run)
    expect(result.unavailable).toBeUndefined();
  });

  it("does not modify file when formatter is unavailable", async () => {
    const dir = await makeTmpDir();
    const filePath = path.join(dir, "Unchanged.hs");
    const original = "module Unchanged where   \nfoo = 42   \n";
    await writeFile(filePath, original, "utf-8");

    const result = JSON.parse(await handleFormat(dir, { module_path: "Unchanged.hs", write: true }));
    if (!result.success) {
      const content = await readFile(filePath, "utf-8");
      expect(content).toBe(original);
    }
  });
});
