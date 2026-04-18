import type { McpServer } from "@modelcontextprotocol/sdk/server/mcp.js";
import { z } from "zod";
import { execFile } from "node:child_process";
import path from "node:path";
import { parseGhcErrors } from "../parsers/error-parser.js";
import { type ToolContext, registerStrictTool } from "./registry.js";

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
          'Optional component to build. Examples: "lib:my-project", "exe:my-project", "all". Defaults to building everything.',
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

export function register(server: McpServer, ctx: ToolContext): void {
  registerStrictTool(server, ctx, 
    "cabal_build",
    "Run 'cabal build' to compile the project. Returns parsed GHC errors/warnings. Use for full compilation checks.",
    {
      component: z.string().optional().describe(
        'Component to build. Examples: "lib:my-package", "exe:my-package". Defaults to all.'
      ),
    },
    async ({ component }) => {
      const result = await handleBuild(ctx.getProjectDir(), { component });
      return { content: [{ type: "text" as const, text: result }] };
    }
  );
}
