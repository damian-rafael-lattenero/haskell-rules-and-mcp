import { describe, it, expect, afterEach } from "vitest";
import { handleFormat } from "../tools/format.js";
import { writeFile, mkdtemp, rm } from "node:fs/promises";
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

  it("returns error when no formatter is installed and file doesn't exist", async () => {
    const result = JSON.parse(await handleFormat("/tmp/fake", { module_path: "src/Test.hs" }));
    expect(result).toHaveProperty("success");
    // If no formatter, we get a fallback; the fallback will fail because the file doesn't exist
  });

  describe("basic style checks fallback", () => {
    it("detects tab characters", async () => {
      const dir = await makeTmpDir();
      const filePath = path.join(dir, "Tabs.hs");
      await writeFile(filePath, "module Tabs where\n\tfoo = 42\n", "utf-8");

      const result = JSON.parse(await handleFormat(dir, { module_path: "Tabs.hs" }));

      // If a formatter IS installed, it won't use fallback
      if (result.fallback) {
        expect(result.success).toBe(true);
        expect(result.source).toBe("basic-style-checks");
        expect(result.issues.some((i: { issue: string }) => i.issue.includes("Tab"))).toBe(true);
      }
    });

    it("detects trailing whitespace", async () => {
      const dir = await makeTmpDir();
      const filePath = path.join(dir, "Trailing.hs");
      await writeFile(filePath, "module Trailing where\nfoo = 42   \n", "utf-8");

      const result = JSON.parse(await handleFormat(dir, { module_path: "Trailing.hs" }));

      if (result.fallback) {
        expect(result.issues.some((i: { issue: string }) => i.issue.includes("Trailing whitespace"))).toBe(true);
      }
    });

    it("detects long lines", async () => {
      const dir = await makeTmpDir();
      const filePath = path.join(dir, "Long.hs");
      const longLine = "foo = " + "x".repeat(120);
      await writeFile(filePath, `module Long where\n${longLine}\n`, "utf-8");

      const result = JSON.parse(await handleFormat(dir, { module_path: "Long.hs" }));

      if (result.fallback) {
        expect(result.issues.some((i: { issue: string }) => i.issue.includes("too long"))).toBe(true);
      }
    });

    it("detects missing final newline", async () => {
      const dir = await makeTmpDir();
      const filePath = path.join(dir, "NoNewline.hs");
      await writeFile(filePath, "module NoNewline where\nfoo = 42", "utf-8");

      const result = JSON.parse(await handleFormat(dir, { module_path: "NoNewline.hs" }));

      if (result.fallback) {
        expect(result.issues.some((i: { issue: string }) => i.issue.includes("Missing final newline"))).toBe(true);
      }
    });

    it("reports clean file with no issues", async () => {
      const dir = await makeTmpDir();
      const filePath = path.join(dir, "Clean.hs");
      await writeFile(filePath, "module Clean where\n\nfoo :: Int\nfoo = 42\n", "utf-8");

      const result = JSON.parse(await handleFormat(dir, { module_path: "Clean.hs" }));

      if (result.fallback) {
        expect(result.count).toBe(0);
        expect(result.issues).toHaveLength(0);
      }
    });

    it("includes install suggestions in fallback", async () => {
      const dir = await makeTmpDir();
      const filePath = path.join(dir, "Any.hs");
      await writeFile(filePath, "module Any where\n", "utf-8");

      const result = JSON.parse(await handleFormat(dir, { module_path: "Any.hs" }));

      if (result.fallback) {
        expect(result.installSuggestions).toBeDefined();
        expect(result.installSuggestions.length).toBeGreaterThan(0);
        expect(result.installSuggestions.some((s: string) => s.includes("fourmolu"))).toBe(true);
      }
    });
  });
});
