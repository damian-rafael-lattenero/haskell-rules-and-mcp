import { chmod, mkdir, readFile, rm, writeFile } from "node:fs/promises";
import { createHash } from "node:crypto";
import path from "node:path";

export const TEST_ROOT_DIR = path.resolve(import.meta.dirname, "..", "..", "..");

/**
 * Inter-worker mutex for `vendor-tools/` mutations.
 *
 * Both `bundled-tools.integration.test.ts` and
 * `bundled-tools-complete.integration.test.ts` mutate the shared
 * `vendor-tools/bundled-tools-manifest.json` and related binaries. When
 * vitest runs files in parallel (fileParallelism: true), these mutations
 * race across workers. `mkdir` with a sentinel path is atomic at the
 * filesystem layer (EEXIST if another worker holds the lock), so we use it
 * as a portable cross-process mutex. Each caller must invoke the returned
 * release function — we wrap it in try/finally at the call site.
 */
const LOCK_DIR = path.join(TEST_ROOT_DIR, "vendor-tools", ".test-bundled-lock");

export async function acquireBundledToolsLock(
  timeoutMs = 60_000,
  pollMs = 100
): Promise<() => Promise<void>> {
  const start = Date.now();
  while (Date.now() - start < timeoutMs) {
    try {
      // `recursive: false` + "wx" style semantics: fails if the dir exists.
      await mkdir(LOCK_DIR);
      return async () => {
        await rm(LOCK_DIR, { recursive: true, force: true });
      };
    } catch {
      // Lock held by another worker — back off and retry.
      await new Promise((resolve) => setTimeout(resolve, pollMs));
    }
  }
  throw new Error(
    `acquireBundledToolsLock: timed out after ${timeoutMs}ms waiting for ${LOCK_DIR}. ` +
      "If a previous test crashed mid-lock, delete this directory manually."
  );
}
export const TEST_PLATFORM = process.platform as "darwin" | "linux" | "win32";
export const TEST_ARCH = process.arch as "x64" | "arm64";
export const TEST_EXT = TEST_PLATFORM === "win32" ? ".exe" : "";
export const MANIFEST_PATH = path.join(TEST_ROOT_DIR, "vendor-tools", "bundled-tools-manifest.json");

export function bundledToolPath(tool: "hlint" | "fourmolu" | "ormolu" | "hls"): string {
  const binaryName =
    tool === "hls" ? `haskell-language-server-wrapper${TEST_EXT}` : `${tool}${TEST_EXT}`;
  return path.join(TEST_ROOT_DIR, "vendor-tools", `${tool}/${TEST_PLATFORM}-${TEST_ARCH}/${binaryName}`);
}

export async function writeExecutable(filePath: string, content: string): Promise<void> {
  await mkdir(path.dirname(filePath), { recursive: true });
  await writeFile(filePath, content, "utf8");
  if (TEST_PLATFORM !== "win32") {
    await chmod(filePath, 0o755);
  }
}

export async function readManifestRaw(): Promise<string> {
  return await readFile(MANIFEST_PATH, "utf8");
}

export async function restoreManifest(raw: string): Promise<void> {
  await writeFile(MANIFEST_PATH, raw, "utf8");
}

export async function updateRuntimeManifestEntry(
  tool: "hlint" | "fourmolu" | "ormolu" | "hls",
  opts: { version?: string; provenance?: string } = {}
): Promise<void> {
  const raw = await readManifestRaw();
  const manifest = JSON.parse(raw) as {
    manifestVersion: number;
    updatedAt: string;
    tools: Array<{
      tool: string;
      version: string;
      platform: string;
      arch: string;
      filename: string;
      sha256?: string;
      provenance?: string;
    }>;
  };
  const absPath = bundledToolPath(tool);
  const relPath = path.relative(path.join(TEST_ROOT_DIR, "vendor-tools"), absPath).replace(/\\/g, "/");
  const entry = manifest.tools.find(
    (item) => item.tool === tool && item.platform === TEST_PLATFORM && item.arch === TEST_ARCH
  );
  if (!entry) {
    throw new Error(`Missing manifest entry for ${tool} ${TEST_PLATFORM}-${TEST_ARCH}`);
  }
  const digest = createHash("sha256").update(await readFile(absPath)).digest("hex");
  entry.filename = relPath;
  entry.sha256 = digest;
  entry.version = opts.version ?? "test-bundled-1.0.0";
  entry.provenance = opts.provenance ?? "test://bundled-tools";
  manifest.updatedAt = new Date().toISOString();
  await writeFile(MANIFEST_PATH, JSON.stringify(manifest, null, 2) + "\n", "utf8");
}
