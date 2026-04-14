import { getBundledToolStatus, resetBundledManifestCache } from "../tools/tool-installer.js";

type ValidatedTool = "hlint" | "fourmolu" | "ormolu" | "hls";

function getValidatedTools(): ValidatedTool[] {
  return process.argv.includes("--include-hls")
    ? ["hlint", "fourmolu", "ormolu", "hls"]
    : ["hlint", "fourmolu", "ormolu"];
}

async function main(): Promise<void> {
  resetBundledManifestCache();
  const failures: string[] = [];

  for (const tool of getValidatedTools()) {
    const status = await getBundledToolStatus(tool);
    if (!status.available) {
      failures.push(`${tool}: ${status.message}`);
      continue;
    }
    process.stdout.write(
      `${tool}: available source=${status.source} version=${status.version ?? "unknown"} binary=${status.binaryPath ?? "n/a"}\n`
    );
  }

  if (failures.length > 0) {
    process.stderr.write(`Bundled tool validation failed:\n- ${failures.join("\n- ")}\n`);
    process.exit(1);
  }
}

main().catch((err) => {
  process.stderr.write(`validate-bundled-tools failed: ${String(err)}\n`);
  process.exit(1);
});
