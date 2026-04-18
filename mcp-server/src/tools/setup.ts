import type { McpServer } from "@modelcontextprotocol/sdk/server/mcp.js";
import { z } from "zod";
import { readFile, writeFile, mkdir, readdir } from "node:fs/promises";
import path from "node:path";
import { RULES_REGISTRY, loadRule } from "../resources/rules.js";
import { type ToolContext, registerStrictTool, zBool } from "./registry.js";

/**
 * Install or update Claude Code rules in the target project's .claude/rules/ directory.
 * This is the onboarding tool: new users call it once to get the automation rules,
 * warning tables, and development workflow docs that make the MCP tools effective.
 *
 * Rules source of truth: mcp-server/rules/*.md
 * Target: <projectDir>/.claude/rules/ (or any specified directory)
 */
export async function handleSetup(
  baseDir: string,
  args: { target_dir?: string; force?: boolean }
): Promise<string> {
  // Determine where to install rules
  // Default: the project root's .claude/rules/ directory
  // baseDir is the repo root (parent of mcp-server/)
  const targetDir = args.target_dir
    ? path.resolve(args.target_dir, ".claude", "rules")
    : path.resolve(baseDir, ".claude", "rules");

  // Create directory if it doesn't exist
  await mkdir(targetDir, { recursive: true });

  // Check what's already there
  let existingFiles: string[] = [];
  try {
    existingFiles = await readdir(targetDir);
  } catch {
    // Directory didn't exist, we just created it
  }

  const installed: string[] = [];
  const updated: string[] = [];
  const skipped: string[] = [];

  // Map of rule filenames to install (single consolidated workflow file)
  const ruleFiles: Record<string, string> = {
    "haskell-mcp-workflow.md": "haskell-mcp-workflow",
  };

  for (const [fileName, ruleName] of Object.entries(ruleFiles)) {
    const targetPath = path.join(targetDir, fileName);
    const rule = RULES_REGISTRY.find((r) => r.name === ruleName);
    if (!rule) continue;

    // Load the full rule content (from disk or fallback)
    const content = await loadRule(rule);

    // Check if file already exists with same content
    const exists = existingFiles.includes(fileName);
    if (exists && !args.force) {
      try {
        const existing = await readFile(targetPath, "utf-8");
        if (existing === content) {
          skipped.push(fileName);
          continue;
        }
        // Content differs — update
        await writeFile(targetPath, content, "utf-8");
        updated.push(fileName);
      } catch {
        await writeFile(targetPath, content, "utf-8");
        installed.push(fileName);
      }
    } else {
      await writeFile(targetPath, content, "utf-8");
      if (exists) {
        updated.push(fileName);
      } else {
        installed.push(fileName);
      }
    }
  }

  const parts: string[] = [];
  if (installed.length > 0) parts.push(`Installed: ${installed.join(", ")}`);
  if (updated.length > 0) parts.push(`Updated: ${updated.join(", ")}`);
  if (skipped.length > 0) parts.push(`Already up to date: ${skipped.join(", ")}`);

  return JSON.stringify({
    success: true,
    targetDir,
    installed,
    updated,
    skipped,
    summary: parts.join(". ") || "No changes needed",
    hint: installed.length > 0
      ? "Rules installed. Claude Code will load them automatically in your next conversation."
      : updated.length > 0
        ? "Rules updated. Changes take effect in your next conversation."
        : "All rules are up to date.",
  });
}

export function register(server: McpServer, ctx: ToolContext): void {
  registerStrictTool(server, ctx, 
    "ghci_setup",
    "Install or update Haskell development rules in your project's .claude/rules/ directory. " +
      "These rules tell Claude Code how to use the MCP tools effectively: the automation loop, " +
      "warning action table, error resolution, navigation, and code quality workflows. " +
      "Run this once when setting up a new project, or after updating the MCP server.",
    {
      target_dir: z.string().optional().describe(
        "Project directory to install rules in. Defaults to the repository root."
      ),
      force: zBool().optional().describe(
        "If true, overwrite existing rules even if they haven't changed. Default: false."
      ),
    },
    async ({ target_dir, force }) => {
      const result = await handleSetup(ctx.getBaseDir(), { target_dir, force });
      ctx.resetRulesCache();
      return { content: [{ type: "text" as const, text: result }] };
    }
  );
}
