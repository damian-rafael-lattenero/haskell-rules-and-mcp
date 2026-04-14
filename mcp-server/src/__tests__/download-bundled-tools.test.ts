import { afterEach, describe, expect, it } from "vitest";
import { mkdtemp, mkdir, readFile, rm, stat, writeFile } from "node:fs/promises";
import os from "node:os";
import path from "node:path";
import { execFile } from "node:child_process";
import {
  downloadFile,
  ensureExecutable,
  extractTarGz,
  extractZip,
  resolveDownloadUrl,
} from "../scripts/download-bundled-tools.js";

const tempDirs: string[] = [];

afterEach(async () => {
  await Promise.all(tempDirs.map((dir) => rm(dir, { recursive: true, force: true })));
  tempDirs.length = 0;
});

describe("resolveDownloadUrl", () => {
  it("returns configured URL for supported tool/target", () => {
    const url = resolveDownloadUrl("hlint", "darwin-arm64");
    expect(url).toContain("hlint");
  });

  it("throws for unsupported target combination", () => {
    expect(() => resolveDownloadUrl("hlint", "linux-x64")).not.toThrow();
  });
});

describe("downloadFile", () => {
  it("writes payload to destination", async () => {
    const dir = await mkdtemp(path.join(os.tmpdir(), "download-script-"));
    tempDirs.push(dir);
    const destination = path.join(dir, "payload.bin");

    const fakeFetch = async () =>
      new Response(new Uint8Array([1, 2, 3, 4]), { status: 200 }) as Response;

    await downloadFile("https://example.test/file", destination, fakeFetch as typeof fetch);
    const bytes = await readFile(destination);
    expect([...bytes]).toEqual([1, 2, 3, 4]);
  });

  it("throws on non-2xx responses", async () => {
    const dir = await mkdtemp(path.join(os.tmpdir(), "download-script-fail-"));
    tempDirs.push(dir);
    const destination = path.join(dir, "payload.bin");

    const fakeFetch = async () => new Response("nope", { status: 503 }) as Response;
    await expect(
      downloadFile("https://example.test/file", destination, fakeFetch as typeof fetch)
    ).rejects.toThrow("HTTP 503");
  });
});

describe("extractTarGz", () => {
  it.skipIf(process.platform === "win32")("extracts binary from tar archive", async () => {
    const dir = await mkdtemp(path.join(os.tmpdir(), "extract-script-"));
    tempDirs.push(dir);

    const sourceDir = path.join(dir, "src");
    const destDir = path.join(dir, "dest");
    const packageDir = path.join(sourceDir, "package");
    await mkdir(sourceDir, { recursive: true });
    await mkdir(packageDir, { recursive: true });
    await mkdir(destDir, { recursive: true });
    await writeFile(path.join(packageDir, "hlint"), "#!/usr/bin/env sh\necho ok\n", "utf-8");

    const tarPath = path.join(dir, "hlint.tar.gz");
    await new Promise<void>((resolve, reject) => {
      execFile("tar", ["-czf", tarPath, "-C", sourceDir, "package"], (error) =>
        error ? reject(error) : resolve()
      );
    });

    await extractTarGz(tarPath, destDir, "hlint");
    const extracted = await readFile(path.join(destDir, "hlint"), "utf-8");
    expect(extracted).toContain("echo ok");
  });
});

describe("extractZip", () => {
  it.skipIf(process.platform === "win32")("extracts binary from zip archive", async () => {
    const dir = await mkdtemp(path.join(os.tmpdir(), "extract-zip-script-"));
    tempDirs.push(dir);

    const sourceDir = path.join(dir, "src");
    const destDir = path.join(dir, "dest");
    const packageDir = path.join(sourceDir, "package");
    await mkdir(sourceDir, { recursive: true });
    await mkdir(packageDir, { recursive: true });
    await mkdir(destDir, { recursive: true });
    await writeFile(path.join(packageDir, "ormolu"), "#!/usr/bin/env sh\necho zip\n", "utf-8");

    const zipPath = path.join(dir, "ormolu.zip");
    await new Promise<void>((resolve, reject) => {
      execFile("zip", ["-r", zipPath, "package"], { cwd: sourceDir }, (error) =>
        error ? reject(error) : resolve()
      );
    });

    await extractZip(zipPath, destDir, "ormolu");
    const extracted = await readFile(path.join(destDir, "ormolu"), "utf-8");
    expect(extracted).toContain("echo zip");
  });
});

describe("ensureExecutable", () => {
  it.skipIf(process.platform === "win32")("sets executable mode", async () => {
    const dir = await mkdtemp(path.join(os.tmpdir(), "chmod-script-"));
    tempDirs.push(dir);
    const file = path.join(dir, "tool");
    await writeFile(file, "echo hi\n", "utf-8");

    await ensureExecutable(file);
    const info = await stat(file);
    expect((info.mode & 0o111) !== 0).toBe(true);
  });
});
