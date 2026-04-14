import { chmod, mkdir, readdir, rename, rm, stat, writeFile } from "node:fs/promises";
import path from "node:path";
import { fileURLToPath } from "node:url";
import { execFile } from "node:child_process";

type SupportedTool = "hlint" | "fourmolu" | "ormolu" | "hls";
type SupportedTarget =
  | "darwin-arm64"
  | "darwin-x64"
  | "linux-arm64"
  | "linux-x64";

export const TOOL_URLS: Record<SupportedTool, Partial<Record<SupportedTarget, string>>> = {
  hlint: {
    "darwin-arm64": "https://github.com/ndmitchell/hlint/releases/download/v3.10/hlint-3.10-x86_64-osx.tar.gz",
    "darwin-x64": "https://github.com/ndmitchell/hlint/releases/download/v3.10/hlint-3.10-x86_64-osx.tar.gz",
    "linux-x64": "https://github.com/ndmitchell/hlint/releases/download/v3.10/hlint-3.10-x86_64-linux.tar.gz",
  },
  fourmolu: {
    "darwin-arm64": "https://github.com/fourmolu/fourmolu/releases/download/v0.19.0.1/fourmolu-0.19.0.1-osx-arm64",
    "darwin-x64": "https://github.com/fourmolu/fourmolu/releases/download/v0.19.0.1/fourmolu-0.19.0.1-osx-x86_64",
    "linux-x64": "https://github.com/fourmolu/fourmolu/releases/download/v0.19.0.1/fourmolu-0.19.0.1-linux-x86_64",
  },
  ormolu: {
    "darwin-arm64": "https://github.com/tweag/ormolu/releases/download/0.7.7.0/ormolu-aarch64-darwin.zip",
    "darwin-x64": "https://github.com/tweag/ormolu/releases/download/0.7.7.0/ormolu-x86_64-darwin.zip",
    "linux-x64": "https://github.com/tweag/ormolu/releases/download/0.7.7.0/ormolu-x86_64-linux.zip",
  },
  hls: {
    "darwin-arm64":
      "https://github.com/haskell/haskell-language-server/releases/download/2.13.0.0/haskell-language-server-2.13.0.0-aarch64-apple-darwin.tar.xz",
    "darwin-x64":
      "https://github.com/haskell/haskell-language-server/releases/download/2.13.0.0/haskell-language-server-2.13.0.0-x86_64-apple-darwin.tar.xz",
    "linux-arm64":
      "https://github.com/haskell/haskell-language-server/releases/download/2.13.0.0/haskell-language-server-2.13.0.0-aarch64-linux-ubuntu2204.tar.xz",
    "linux-x64":
      "https://github.com/haskell/haskell-language-server/releases/download/2.13.0.0/haskell-language-server-2.13.0.0-x86_64-linux-ubuntu2204.tar.xz",
  },
};

function usage(): string {
  return "Usage: npm run tools:download -- <tool> <platform-arch> (e.g. hlint darwin-arm64)";
}

function isSupportedTool(tool: string): tool is SupportedTool {
  return tool === "hlint" || tool === "fourmolu" || tool === "ormolu" || tool === "hls";
}

function isSupportedTarget(target: string): target is SupportedTarget {
  return (
    target === "darwin-arm64" ||
    target === "darwin-x64" ||
    target === "linux-arm64" ||
    target === "linux-x64"
  );
}

export function getBinaryName(tool: SupportedTool, target: SupportedTarget): string {
  const ext = target.startsWith("win32") ? ".exe" : "";
  if (tool === "hls") return `haskell-language-server-wrapper${ext}`;
  return `${tool}${ext}`;
}

export function resolveDownloadUrl(tool: SupportedTool, target: SupportedTarget): string {
  const url = TOOL_URLS[tool][target];
  if (!url) throw new Error(`No download URL configured for ${tool} ${target}`);
  return url;
}

export async function downloadFile(
  url: string,
  destinationPath: string,
  fetchImpl: typeof fetch = fetch
): Promise<void> {
  const response = await fetchImpl(url);
  if (!response.ok) {
    throw new Error(`Failed to download ${url}: HTTP ${response.status}`);
  }

  const bytes = Buffer.from(await response.arrayBuffer());
  await writeFile(destinationPath, bytes);
}

export async function extractTarGz(
  archivePath: string,
  destinationDir: string,
  binaryName: string
): Promise<void> {
  await new Promise<void>((resolve, reject) => {
    execFile("tar", ["-xzf", archivePath, "-C", destinationDir], (error) =>
      error ? reject(error) : resolve()
    );
  });

  const located = await findFileRecursive(destinationDir, binaryName);
  if (!located) {
    throw new Error(`Could not locate ${binaryName} after extracting ${archivePath}`);
  }

  const targetPath = path.join(destinationDir, binaryName);
  if (located !== targetPath) {
    await rename(located, targetPath);
  }
}

export async function extractZip(
  archivePath: string,
  destinationDir: string,
  binaryName: string
): Promise<void> {
  await new Promise<void>((resolve, reject) => {
    execFile("unzip", ["-o", archivePath, "-d", destinationDir], (error) =>
      error ? reject(error) : resolve()
    );
  });

  const located = await findFileRecursive(destinationDir, binaryName);
  if (!located) {
    throw new Error(`Could not locate ${binaryName} after extracting ${archivePath}`);
  }

  const targetPath = path.join(destinationDir, binaryName);
  if (located !== targetPath) {
    await rename(located, targetPath);
  }
}

export async function extractTarXz(
  archivePath: string,
  destinationDir: string,
  binaryName: string
): Promise<void> {
  await new Promise<void>((resolve, reject) => {
    execFile("tar", ["-xJf", archivePath, "-C", destinationDir], (error) =>
      error ? reject(error) : resolve()
    );
  });

  const located = await findFileRecursive(destinationDir, binaryName);
  if (!located) {
    throw new Error(`Could not locate ${binaryName} after extracting ${archivePath}`);
  }

  const targetPath = path.join(destinationDir, binaryName);
  if (located !== targetPath) {
    await rename(located, targetPath);
  }
}

export async function ensureExecutable(binaryPath: string): Promise<void> {
  await chmod(binaryPath, 0o755);
}

async function findFileRecursive(root: string, fileName: string): Promise<string | null> {
  const entries = await readdir(root);
  for (const entry of entries) {
    const fullPath = path.join(root, entry);
    const details = await stat(fullPath);
    if (details.isDirectory()) {
      const nested = await findFileRecursive(fullPath, fileName);
      if (nested) return nested;
      continue;
    }
    if (entry === fileName) return fullPath;
  }
  return null;
}

export async function downloadBundledTool(tool: SupportedTool, target: SupportedTarget): Promise<string> {
  const rootDir = path.resolve(path.dirname(fileURLToPath(import.meta.url)), "..", "..");
  const url = resolveDownloadUrl(tool, target);
  const binaryName = getBinaryName(tool, target);
  const outputDir = path.join(rootDir, "vendor-tools", tool, target);
  const outputBinary = path.join(outputDir, binaryName);
  const isArchive = url.endsWith(".tar.gz");
  const isTarXz = url.endsWith(".tar.xz");
  const isZip = url.endsWith(".zip");
  const tempArchive = path.join(outputDir, `${tool}.tar.gz`);
  const tempTarXz = path.join(outputDir, `${tool}.tar.xz`);
  const tempZip = path.join(outputDir, `${tool}.zip`);

  await mkdir(outputDir, { recursive: true });
  if (isArchive) {
    await downloadFile(url, tempArchive);
    await extractTarGz(tempArchive, outputDir, binaryName);
    await rm(tempArchive, { force: true });
  } else if (isTarXz) {
    await downloadFile(url, tempTarXz);
    await extractTarXz(tempTarXz, outputDir, binaryName);
    await rm(tempTarXz, { force: true });
  } else if (isZip) {
    await downloadFile(url, tempZip);
    await extractZip(tempZip, outputDir, binaryName);
    await rm(tempZip, { force: true });
  } else {
    await downloadFile(url, outputBinary);
  }

  await ensureExecutable(outputBinary);
  return outputBinary;
}

async function main(): Promise<void> {
  const toolArg = process.argv[2];
  const targetArg = process.argv[3];
  if (!toolArg || !targetArg || !isSupportedTool(toolArg) || !isSupportedTarget(targetArg)) {
    throw new Error(usage());
  }

  const binary = await downloadBundledTool(toolArg, targetArg);
  process.stdout.write(`Downloaded ${toolArg} for ${targetArg} -> ${binary}\n`);
}

const invokedPath = process.argv[1] ? path.resolve(process.argv[1]) : "";
const currentPath = fileURLToPath(import.meta.url);
if (invokedPath === currentPath) {
  main().catch((err) => {
    process.stderr.write(`download-bundled-tools failed: ${String(err)}\n`);
    process.exit(1);
  });
}
