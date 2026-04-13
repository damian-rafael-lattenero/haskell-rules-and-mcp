import { describe, it, expect, afterEach } from "vitest";
import { handleSetup } from "../tools/setup.js";
import { readFile, rm, mkdir } from "node:fs/promises";
import path from "node:path";
import os from "node:os";

describe("handleSetup", () => {
  const tmpDirs: string[] = [];

  async function makeTmpDir(): Promise<string> {
    const dir = path.join(os.tmpdir(), `ghci-setup-test-${Date.now()}-${Math.random().toString(36).slice(2)}`);
    await mkdir(dir, { recursive: true });
    tmpDirs.push(dir);
    return dir;
  }

  afterEach(async () => {
    for (const dir of tmpDirs) {
      try { await rm(dir, { recursive: true }); } catch { /* ignore */ }
    }
    tmpDirs.length = 0;
  });

  // Use the actual MCP server base dir (2 levels up from src/)
  const BASE_DIR = path.resolve(import.meta.dirname, "..", "..", "..");

  it("installs rules to a new directory", async () => {
    const tmpDir = await makeTmpDir();
    const result = JSON.parse(await handleSetup(BASE_DIR, { target_dir: tmpDir }));

    expect(result.success).toBe(true);
    expect(result.installed.length).toBeGreaterThan(0);
    expect(result.installed).toContain("haskell-automation.md");
    expect(result.installed).toContain("haskell-development.md");

    // Verify files exist
    const automation = await readFile(path.join(tmpDir, ".claude", "rules", "haskell-automation.md"), "utf-8");
    expect(automation).toContain("Warning Action Table");

    const development = await readFile(path.join(tmpDir, ".claude", "rules", "haskell-development.md"), "utf-8");
    expect(development).toContain("Navigation & Discovery");
  });

  it("skips unchanged rules on second run", async () => {
    const tmpDir = await makeTmpDir();

    // First run: installs
    const r1 = JSON.parse(await handleSetup(BASE_DIR, { target_dir: tmpDir }));
    expect(r1.installed.length).toBeGreaterThan(0);

    // Second run: skips
    const r2 = JSON.parse(await handleSetup(BASE_DIR, { target_dir: tmpDir }));
    expect(r2.skipped.length).toBeGreaterThan(0);
    expect(r2.installed).toHaveLength(0);
    expect(r2.updated).toHaveLength(0);
  });

  it("force overwrites existing rules", async () => {
    const tmpDir = await makeTmpDir();

    await handleSetup(BASE_DIR, { target_dir: tmpDir });

    const result = JSON.parse(await handleSetup(BASE_DIR, { target_dir: tmpDir, force: true }));
    expect(result.success).toBe(true);
    expect(result.updated.length).toBeGreaterThan(0);
  });

  it("updates rules when content differs", async () => {
    const tmpDir = await makeTmpDir();
    const rulesDir = path.join(tmpDir, ".claude", "rules");
    await mkdir(rulesDir, { recursive: true });

    // Write an old version
    await import("node:fs/promises").then((fs) =>
      fs.writeFile(path.join(rulesDir, "haskell-automation.md"), "# Old content\n", "utf-8")
    );

    const result = JSON.parse(await handleSetup(BASE_DIR, { target_dir: tmpDir }));
    expect(result.success).toBe(true);
    expect(result.updated).toContain("haskell-automation.md");

    // Verify it was updated
    const content = await readFile(path.join(rulesDir, "haskell-automation.md"), "utf-8");
    expect(content).toContain("Warning Action Table");
  });
});
