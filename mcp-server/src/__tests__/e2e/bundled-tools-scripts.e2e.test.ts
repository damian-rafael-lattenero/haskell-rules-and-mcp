import { afterAll, beforeAll, describe, expect, it } from "vitest";
import { execFile } from "node:child_process";
import { promisify } from "node:util";
import { rm } from "node:fs/promises";
import path from "node:path";
import { resetBundledManifestCache } from "../../tools/tool-installer.js";
import {
  bundledToolPath,
  readManifestRaw,
  restoreManifest,
  updateRuntimeManifestEntry,
  writeExecutable,
  TEST_PLATFORM,
} from "../helpers/bundled-tools.js";

const execFileAsync = promisify(execFile);
const SERVER_ROOT = path.resolve(import.meta.dirname, "..", "..", "..");
const validateScript = path.join(SERVER_ROOT, "dist", "scripts", "validate-bundled-tools.js");
const smokeScript = path.join(SERVER_ROOT, "dist", "scripts", "test-bundled-tool.js");

const lintBin = bundledToolPath("hlint");
const fourmoluBin = bundledToolPath("fourmolu");
const ormoluBin = bundledToolPath("ormolu");
const hlsBin = bundledToolPath("hls");

describe("bundled tools scripts e2e", () => {
  let manifestSnapshot = "";

  beforeAll(async () => {
    manifestSnapshot = await readManifestRaw();
    if (TEST_PLATFORM === "win32") {
      await writeExecutable(lintBin, "@echo off\r\necho HLint 3.9\r\n");
      await writeExecutable(fourmoluBin, "@echo off\r\necho fourmolu 0.18.0.0\r\n");
      await writeExecutable(ormoluBin, "@echo off\r\necho ormolu 0.7.8.0\r\n");
      await writeExecutable(hlsBin, "@echo off\r\necho haskell-language-server-wrapper 2.9.0\r\n");
    } else {
      await writeExecutable(lintBin, "#!/usr/bin/env sh\necho 'HLint 3.9'\n");
      await writeExecutable(fourmoluBin, "#!/usr/bin/env sh\necho 'fourmolu 0.18.0.0'\n");
      await writeExecutable(ormoluBin, "#!/usr/bin/env sh\necho 'ormolu 0.7.8.0'\n");
      await writeExecutable(hlsBin, "#!/usr/bin/env sh\necho 'haskell-language-server-wrapper 2.9.0'\n");
    }

    await updateRuntimeManifestEntry("hlint");
    await updateRuntimeManifestEntry("fourmolu");
    await updateRuntimeManifestEntry("ormolu");
    await updateRuntimeManifestEntry("hls");
    resetBundledManifestCache();
  });

  afterAll(async () => {
    await rm(lintBin, { force: true });
    await rm(fourmoluBin, { force: true });
    await rm(ormoluBin, { force: true });
    await rm(hlsBin, { force: true });
    await restoreManifest(manifestSnapshot);
    resetBundledManifestCache();
  });

  it("validates bundled checksums through script", async () => {
    const { stdout } = await execFileAsync("node", [validateScript], { cwd: SERVER_ROOT });
    expect(stdout).toContain("hlint: available");
    expect(stdout).toContain("fourmolu: available");
    expect(stdout).toContain("ormolu: available");
  });

  it("smoke-tests bundled executable through script", async () => {
    const { stdout } = await execFileAsync("node", [smokeScript, "hlint"], { cwd: SERVER_ROOT });
    expect(stdout).toContain("hlint: ok");
  });
});
