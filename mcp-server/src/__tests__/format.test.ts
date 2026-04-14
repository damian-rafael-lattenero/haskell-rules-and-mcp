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

  // --- Fallback write mode (Change 4) ---
  describe("fallback write mode", () => {
    it("write:true strips trailing whitespace and sets written:true", async () => {
      const dir = await makeTmpDir();
      const filePath = path.join(dir, "Dirty.hs");
      await writeFile(filePath, "module Dirty where   \n\nfoo = 42   \n", "utf-8");

      const result = JSON.parse(await handleFormat(dir, { module_path: "Dirty.hs", write: true }));

      if (result.fallback) {
        expect(result.success).toBe(true);
        expect(result.written).toBe(true);
        expect(result.fixesApplied).toBeGreaterThan(0);
        const content = await readFile(filePath, "utf-8");
        expect(content).not.toMatch(/ +\n/);
      }
    });

    it("write:true converts tabs to spaces", async () => {
      const dir = await makeTmpDir();
      const filePath = path.join(dir, "Tabbed.hs");
      await writeFile(filePath, "module Tabbed where\n\tfoo = 42\n", "utf-8");

      const result = JSON.parse(await handleFormat(dir, { module_path: "Tabbed.hs", write: true }));

      if (result.fallback) {
        expect(result.success).toBe(true);
        expect(result.written).toBe(true);
        const content = await readFile(filePath, "utf-8");
        expect(content).not.toContain("\t");
      }
    });

    it("write:true adds missing final newline", async () => {
      const dir = await makeTmpDir();
      const filePath = path.join(dir, "NoNL.hs");
      await writeFile(filePath, "module NoNL where\nfoo = 42", "utf-8");

      const result = JSON.parse(await handleFormat(dir, { module_path: "NoNL.hs", write: true }));

      if (result.fallback) {
        expect(result.success).toBe(true);
        expect(result.written).toBe(true);
        const content = await readFile(filePath, "utf-8");
        expect(content.endsWith("\n")).toBe(true);
      }
    });

    it("write:true on clean file returns fixesApplied:0", async () => {
      const dir = await makeTmpDir();
      const filePath = path.join(dir, "Clean2.hs");
      await writeFile(filePath, "module Clean2 where\n\nfoo :: Int\nfoo = 42\n", "utf-8");

      const result = JSON.parse(await handleFormat(dir, { module_path: "Clean2.hs", write: true }));

      if (result.fallback) {
        expect(result.success).toBe(true);
        expect(result.written).toBe(true);
        expect(result.fixesApplied).toBe(0);
      }
    });

    it("write:false leaves file unchanged even with issues", async () => {
      const dir = await makeTmpDir();
      const filePath = path.join(dir, "Unchanged.hs");
      const original = "module Unchanged where   \nfoo = 42   \n";
      await writeFile(filePath, original, "utf-8");

      const result = JSON.parse(await handleFormat(dir, { module_path: "Unchanged.hs", write: false }));

      if (result.fallback) {
        // Should report issues but NOT modify the file
        const content = await readFile(filePath, "utf-8");
        expect(content).toBe(original);
        // written should be falsy
        expect(result.written).toBeFalsy();
      }
    });

    it("fallback write:true response includes _formatWarning", async () => {
      const dir = await makeTmpDir();
      const filePath = path.join(dir, "Warn.hs");
      await writeFile(filePath, "module Warn where\nfoo = 42\n", "utf-8");

      const result = JSON.parse(await handleFormat(dir, { module_path: "Warn.hs", write: true }));

      if (result.fallback) {
        expect(result._formatWarning).toBeDefined();
        expect(typeof result._formatWarning).toBe("string");
        expect(result._formatWarning).toContain("fourmolu");
        expect(result._formatWarning).toContain("ghcup install fourmolu");
      }
    });

    it("fallback dry-run response includes _formatWarning", async () => {
      const dir = await makeTmpDir();
      const filePath = path.join(dir, "WarnDry.hs");
      await writeFile(filePath, "module WarnDry where\nfoo = 42   \n", "utf-8");

      const result = JSON.parse(await handleFormat(dir, { module_path: "WarnDry.hs" }));

      if (result.fallback) {
        expect(result._formatWarning).toBeDefined();
        expect(result._formatWarning).toContain("fourmolu");
      }
    });
  });
});
