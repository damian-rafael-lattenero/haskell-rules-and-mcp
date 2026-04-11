import { execFile } from "node:child_process";
import path from "node:path";
import { parseGhcErrors } from "../parsers/error-parser.js";

export const buildTool = {
  name: "cabal_build",
  description:
    "Run 'cabal build' to compile the project. " +
    "Returns parsed GHC errors and warnings in structured format. " +
    "Use this for full compilation checks. For quick type-checks, prefer ghci_load.",
  inputSchema: {
    type: "object" as const,
    properties: {
      component: {
        type: "string",
        description:
          'Optional component to build. Examples: "lib:haskell-rules-and-mcp", "exe:haskell-rules-and-mcp", "all". Defaults to building everything.',
      },
    },
    required: [],
  },
};

export async function handleBuild(
  projectDir: string,
  args: { component?: string }
): Promise<string> {
  const ghcupBin = path.join(process.env.HOME ?? "/Users", ".ghcup", "bin");
  const cabalBin = path.join(process.env.HOME ?? "/Users", ".cabal", "bin");
  const env = {
    ...process.env,
    PATH: `${ghcupBin}:${cabalBin}:${process.env.PATH}`,
  };

  const cabalArgs = ["build"];
  if (args.component) {
    cabalArgs.push(args.component);
  }

  return new Promise<string>((resolve) => {
    execFile(
      "cabal",
      cabalArgs,
      { cwd: projectDir, env, timeout: 120_000 },
      (error, stdout, stderr) => {
        const fullOutput = `${stdout}\n${stderr}`;
        const errors = parseGhcErrors(fullOutput);
        const errorCount = errors.filter((e) => e.severity === "error").length;
        const warningCount = errors.filter((e) => e.severity === "warning").length;

        const isSuccess = !error || errorCount === 0;

        resolve(
          JSON.stringify({
            success: isSuccess,
            errors: errors.filter((e) => e.severity === "error"),
            warnings: errors.filter((e) => e.severity === "warning"),
            summary: isSuccess
              ? `Build successful${warningCount > 0 ? ` with ${warningCount} warning(s)` : ""}`
              : `Build failed: ${errorCount} error(s), ${warningCount} warning(s)`,
            raw: fullOutput.trim(),
          })
        );
      }
    );
  });
}
