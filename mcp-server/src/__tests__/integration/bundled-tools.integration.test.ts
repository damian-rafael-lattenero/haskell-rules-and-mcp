import { afterAll, beforeAll, describe, expect, it } from "vitest";
import { rm } from "node:fs/promises";
import path from "node:path";
import { handleLint } from "../../tools/lint.js";
import { handleFormat } from "../../tools/format.js";
import { handleHls } from "../../tools/hls.js";
import { resetBundledManifestCache } from "../../tools/tool-installer.js";

const fixtureDir = path.resolve(import.meta.dirname, "../fixtures/test-project");
import {
  TEST_PLATFORM,
  TEST_ARCH,
  acquireBundledToolsLock,
  bundledToolPath,
  readManifestRaw,
  restoreManifest,
  updateRuntimeManifestEntry,
  writeExecutable,
} from "../helpers/bundled-tools.js";

const lintBin = bundledToolPath("hlint");
const fmtBin = bundledToolPath("fourmolu");
const hlsBin = bundledToolPath("hls");

describe("bundled tools integration", () => {
  let manifestSnapshot = "";
  // Cross-worker mutex: both this file and bundled-tools-complete write to the
  // same shared `vendor-tools/` directory. Hold the lock for the entire
  // lifecycle of this test file so parallel workers serialize here.
  let releaseLock: (() => Promise<void>) | null = null;
  beforeAll(async () => {
    releaseLock = await acquireBundledToolsLock();
    manifestSnapshot = await readManifestRaw();
    if (!["darwin", "linux", "win32"].includes(TEST_PLATFORM)) return;
    if (!["x64", "arm64"].includes(TEST_ARCH)) return;

    if (TEST_PLATFORM === "win32") {
      await writeExecutable(lintBin, "@echo off\r\necho []\r\n");
      await writeExecutable(
        fmtBin,
        "@echo off\r\nif \"%1\"==\"--mode\" if \"%2\"==\"stdout\" type \"%3\"\r\nif \"%1\"==\"--mode\" if \"%2\"==\"inplace\" exit /b 0\r\n"
      );
      await writeExecutable(hlsBin, "@echo off\r\necho haskell-language-server-wrapper 2.9.0\r\n");
    } else {
      await writeExecutable(lintBin, "#!/usr/bin/env sh\necho '[]'\n");
      await writeExecutable(
        fmtBin,
        "#!/usr/bin/env sh\nif [ \"$1\" = \"--mode\" ] && [ \"$2\" = \"stdout\" ]; then cat \"$3\"; exit 0; fi\nif [ \"$1\" = \"--mode\" ] && [ \"$2\" = \"inplace\" ]; then exit 0; fi\nexit 1\n"
      );
      await writeExecutable(
        hlsBin,
        "#!/usr/bin/env sh\nif [ \"$1\" = \"--version\" ]; then echo 'haskell-language-server-wrapper 2.9.0'; exit 0; fi\nexit 1\n"
      );
    }
    await updateRuntimeManifestEntry("hlint");
    await updateRuntimeManifestEntry("fourmolu");
    await updateRuntimeManifestEntry("hls");
    resetBundledManifestCache();
  });

  afterAll(async () => {
    await rm(lintBin, { force: true });
    await rm(fmtBin, { force: true });
    await rm(hlsBin, { force: true });
    await restoreManifest(manifestSnapshot);
    resetBundledManifestCache();
    if (releaseLock) await releaseLock();
  });

  it("ghci_lint uses bundled hlint when present", async () => {
    const result = JSON.parse(await handleLint(fixtureDir, { module_path: "src/TestLib.hs" }));
    expect(result.success).toBe(true);
    if (!result.fallback) {
      expect(result.source).toBe("bundled");
      expect(result.binaryPath).toContain("vendor-tools");
    }
  });

  it("ghci_format uses bundled formatter when present", async () => {
    const result = JSON.parse(
      await handleFormat(fixtureDir, { module_path: "src/TestLib.hs", write: false })
    );
    expect(result.success).toBe(true);
    if (!result.fallback) {
      expect(result.source).toBe("bundled");
      expect(result.binaryPath).toContain("vendor-tools");
    }
  });

  it("ghci_hls available reports bundled source when wrapper exists", async () => {
    const result = JSON.parse(await handleHls(fixtureDir, { action: "available" }));
    expect(result.success).toBe(true);
    if (result.available) {
      expect(["host", "bundled"]).toContain(result.source);
      if (result.source === "bundled") {
        expect(result.binaryPath).toContain("vendor-tools");
      }
    }
  });
});
