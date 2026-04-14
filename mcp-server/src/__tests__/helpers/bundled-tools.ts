import { chmod, mkdir, readFile, writeFile } from "node:fs/promises";
import { createHash } from "node:crypto";
import path from "node:path";

export const TEST_ROOT_DIR = path.resolve(import.meta.dirname, "..", "..", "..");
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
