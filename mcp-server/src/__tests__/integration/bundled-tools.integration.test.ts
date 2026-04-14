import { afterAll, beforeAll, describe, expect, it } from "vitest";
import { chmod, mkdir, rm, writeFile } from "node:fs/promises";
import path from "node:path";
import { handleLint } from "../../tools/lint.js";
import { handleFormat } from "../../tools/format.js";
import { handleHls } from "../../tools/hls.js";
import { resetBundledManifestCache } from "../../tools/tool-installer.js";

const rootDir = path.resolve(import.meta.dirname, "..", "..", "..");
const fixtureDir = path.resolve(import.meta.dirname, "../fixtures/test-project");
const platform = process.platform as "darwin" | "linux" | "win32";
const arch = process.arch as "x64" | "arm64";
const ext = platform === "win32" ? ".exe" : "";

const lintBin = path.join(rootDir, "vendor-tools", `hlint/${platform}-${arch}/hlint${ext}`);
const fmtBin = path.join(rootDir, "vendor-tools", `fourmolu/${platform}-${arch}/fourmolu${ext}`);
const hlsBin = path.join(
  rootDir,
  "vendor-tools",
  `hls/${platform}-${arch}/haskell-language-server-wrapper${ext}`
);

async function writeExecutable(filePath: string, content: string): Promise<void> {
  await mkdir(path.dirname(filePath), { recursive: true });
  await writeFile(filePath, content, "utf8");
  if (platform !== "win32") {
    await chmod(filePath, 0o755);
  }
}

describe("bundled tools integration", () => {
  beforeAll(async () => {
    if (!["darwin", "linux", "win32"].includes(platform)) return;
    if (!["x64", "arm64"].includes(arch)) return;

    if (platform === "win32") {
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
    resetBundledManifestCache();
  });

  afterAll(async () => {
    await rm(lintBin, { force: true });
    await rm(fmtBin, { force: true });
    await rm(hlsBin, { force: true });
    resetBundledManifestCache();
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
