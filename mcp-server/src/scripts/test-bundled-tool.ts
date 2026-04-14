import { execFile } from "node:child_process";
import { promisify } from "node:util";
import { ensureTool } from "../tools/tool-installer.js";

const execFileAsync = promisify(execFile);

async function main(): Promise<void> {
  const tool = process.argv[2];
  if (!tool) {
    throw new Error("Usage: npm run tools:test -- <hlint|fourmolu|ormolu|hls>");
  }

  const ensured = await ensureTool(tool);
  if (!ensured.available || !ensured.binaryPath) {
    throw new Error(`Tool ${tool} unavailable: ${ensured.message}`);
  }

  const versionArgs =
    tool === "hlint"
      ? ["--version"]
      : tool === "hls"
        ? ["--version"]
        : ["--version"];

  const { stdout, stderr } = await execFileAsync(ensured.binaryPath, versionArgs, {
    env: { ...process.env },
  });

  process.stdout.write(
    `${tool}: ok source=${ensured.source ?? "unknown"} binary=${ensured.binaryPath}\n${stdout || stderr}`
  );
}

main().catch((err) => {
  process.stderr.write(`test-bundled-tool failed: ${String(err)}\n`);
  process.exit(1);
});
