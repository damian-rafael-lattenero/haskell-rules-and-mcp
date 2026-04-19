import { describe, it, expect, afterEach, beforeAll, afterAll } from "vitest";
import { handleFormat } from "../tools/format.js";
import { writeFile, mkdtemp, rm, readFile } from "node:fs/promises";
import path from "node:path";
import os from "node:os";
import {
  resetManifestCache,
  setManifestPathForTests,
} from "../vendor-tools/manifest.js";
import { _resetWarmupForTesting } from "../tools/toolchain-warmup.js";

// Point the tool installer at an empty releases manifest so it never attempts
// to auto-download the (now-working) 100MB+ formatter binaries. Each test
// just needs to exercise the "unavailable" envelope.
let emptyManifestDir: string;
beforeAll(async () => {
  emptyManifestDir = await mkdtemp(path.join(os.tmpdir(), "format-manifest-"));
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
  _resetWarmupForTesting();
});
afterAll(async () => {
  setManifestPathForTests(null);
  resetManifestCache();
  _resetWarmupForTesting();
  await rm(emptyManifestDir, { recursive: true, force: true });
});

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

  it("returns error envelope when formatter is not available or file does not exist", async () => {
    // Post-P0a, fourmolu may be genuinely available via auto-download cache.
    // In that case `handleFormat` still errors because the source file
    // `/tmp/fake/src/Test.hs` doesn't exist. Both paths MUST produce a
    // `success: false` envelope identifying the formatter name.
    const result = JSON.parse(await handleFormat("/tmp/fake", { module_path: "src/Test.hs" }));
    expect(result.success).toBe(false);
    expect(typeof (result.formatter ?? result.format_tool)).toBe("string");
    if (result.unavailable === true) {
      expect(result.error).toContain("No formatter available");
    } else {
      expect(typeof result.error).toBe("string");
    }
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
