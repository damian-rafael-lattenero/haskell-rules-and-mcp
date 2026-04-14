import type { McpServer } from "@modelcontextprotocol/sdk/server/mcp.js";
import { z } from "zod";
import { execFile } from "node:child_process";
import path from "node:path";
import { parseGhcErrors } from "../parsers/error-parser.js";
import type { ToolContext } from "./registry.js";

export async function handleCabalTest(
  projectDir: string,
  args: { component?: string }
): Promise<string> {
  const ghcupBin = path.join(process.env.HOME ?? "/Users", ".ghcup", "bin");
  const cabalBin = path.join(process.env.HOME ?? "/Users", ".cabal", "bin");
  const env = {
    ...process.env,
    PATH: `${ghcupBin}:${cabalBin}:${process.env.PATH}`,
  };

  const cabalArgs = buildCabalTestArgs(args.component);

  return new Promise<string>((resolve) => {
    execFile(
      "cabal",
      cabalArgs,
      { cwd: projectDir, env, timeout: 180_000 },
      (error, stdout, stderr) => {
        const fullOutput = `${stdout}\n${stderr}`.trim();
        const diagnostics = parseGhcErrors(fullOutput);
        const errors = diagnostics.filter((e) => e.severity === "error");
        const warnings = diagnostics.filter((e) => e.severity === "warning");
        const isSuccess = !error || errors.length === 0;

        resolve(
          JSON.stringify({
            success: isSuccess,
            command: ["cabal", ...cabalArgs].join(" "),
            errors,
            warnings,
            summary: isSuccess
              ? `Tests passed${warnings.length > 0 ? ` with ${warnings.length} warning(s)` : ""}`
              : `Tests failed: ${errors.length} error(s), ${warnings.length} warning(s)`,
            raw: fullOutput,
          })
        );
      }
    );
  });
}

export function buildCabalTestArgs(component?: string): string[] {
  return component ? ["test", component] : ["test"];
}

export function register(server: McpServer, ctx: ToolContext): void {
  server.tool(
    "cabal_test",
    "Run 'cabal test' to execute the package test-suite. Returns parsed GHC errors/warnings and raw test output.",
    {
      component: z.string().optional().describe(
        'Optional test component to run. Examples: "test:my-package-test", "all". Defaults to cabal test.'
      ),
    },
    async ({ component }) => {
      const result = await handleCabalTest(ctx.getProjectDir(), { component });
      return { content: [{ type: "text" as const, text: result }] };
    }
  );
}
