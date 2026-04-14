import { createHash } from "node:crypto";
import { readFile, writeFile } from "node:fs/promises";
import path from "node:path";
import { fileURLToPath } from "node:url";

interface BundledToolEntry {
  tool: string;
  version: string;
  platform: "darwin" | "linux" | "win32";
  arch: "x64" | "arm64";
  filename: string;
  sha256?: string;
  provenance?: string;
}

interface BundledToolsManifest {
  manifestVersion: number;
  updatedAt: string;
  tools: BundledToolEntry[];
}

function getArg(name: string): string | undefined {
  const idx = process.argv.indexOf(`--${name}`);
  if (idx === -1) return undefined;
  return process.argv[idx + 1];
}

async function sha256File(filePath: string): Promise<string> {
  const raw = await readFile(filePath);
  return createHash("sha256").update(raw).digest("hex");
}

async function main(): Promise<void> {
  const rootDir = path.resolve(path.dirname(fileURLToPath(import.meta.url)), "..", "..");
  const manifestPath = path.join(rootDir, "vendor-tools", "bundled-tools-manifest.json");

  const tool = getArg("tool");
  const platform = getArg("platform") as BundledToolEntry["platform"] | undefined;
  const arch = getArg("arch") as BundledToolEntry["arch"] | undefined;
  const version = getArg("version");
  const provenance = getArg("provenance");

  if (!tool || !platform || !arch) {
    throw new Error(
      "Missing required args. Usage: npm run tools:update-manifest -- --tool hlint --platform darwin --arch arm64 --version 3.9 --provenance https://..."
    );
  }

  const manifest = JSON.parse(
    await readFile(manifestPath, "utf8")
  ) as BundledToolsManifest;

  const entry = manifest.tools.find(
    (item) => item.tool === tool && item.platform === platform && item.arch === arch
  );
  if (!entry) {
    throw new Error(`No manifest entry for ${tool} ${platform}-${arch}`);
  }

  const binaryAbs = path.join(rootDir, "vendor-tools", entry.filename);
  entry.sha256 = await sha256File(binaryAbs);
  if (version) entry.version = version;
  if (provenance) entry.provenance = provenance;

  manifest.updatedAt = new Date().toISOString();
  await writeFile(manifestPath, JSON.stringify(manifest, null, 2) + "\n", "utf8");

  process.stdout.write(
    `Updated ${tool} ${platform}-${arch}\nsha256=${entry.sha256}\nversion=${entry.version}\n`
  );
}

main().catch((err) => {
  process.stderr.write(`update-bundled-tools failed: ${String(err)}\n`);
  process.exit(1);
});
