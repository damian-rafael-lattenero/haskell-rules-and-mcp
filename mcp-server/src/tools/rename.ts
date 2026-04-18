import type { McpServer } from "@modelcontextprotocol/sdk/server/mcp.js";
import { z } from "zod";
import { readFile, writeFile } from "node:fs/promises";
import path from "node:path";
import { handleReferences } from "./references.js";
import { type ToolContext, registerStrictTool } from "./registry.js";

export interface RenameChange {
  file: string;
  line: number;
  column: number;
  oldText: string;
  newText: string;
}

/**
 * Preview a rename operation across the project.
 * Finds all references and shows what would change.
 * Does NOT edit files — returns the changes for the LLM agent to apply.
 */
export async function handleRename(
  projectDir: string,
  args: { oldName: string; newName: string; apply?: boolean }
): Promise<string> {
  const { oldName, newName, apply } = args;

  // Validate new name
  if (!/^[a-zA-Z_][a-zA-Z0-9_']*$/.test(newName)) {
    return JSON.stringify({
      success: false,
      error: `Invalid Haskell identifier: '${newName}'`,
    });
  }

  // Find all references
  const refsResult = JSON.parse(await handleReferences(projectDir, { name: oldName }));
  if (!refsResult.success) {
    return JSON.stringify(refsResult);
  }

  if (refsResult.count === 0) {
    return JSON.stringify({
      success: false,
      oldName,
      newName,
      error: `No references found for '${oldName}'`,
    });
  }

  // Group by file and build change preview
  const changesByFile = new Map<string, RenameChange[]>();

  for (const ref of refsResult.references) {
    const file = ref.file;
    if (!changesByFile.has(file)) {
      changesByFile.set(file, []);
    }
    changesByFile.get(file)!.push({
      file,
      line: ref.line,
      column: ref.column,
      oldText: oldName,
      newText: newName,
    });
  }

  const files = Array.from(changesByFile.entries()).map(([file, changes]) => ({
    file,
    changes: changes.length,
    lines: changes.map((c) => c.line),
  }));

  // Apply the rename directly if requested
  if (apply) {
    const wordBoundaryRegex = new RegExp(`\\b${oldName.replace(/[.*+?^${}()|[\]\\]/g, '\\$&')}\\b`, 'g');
    const filesModified: string[] = [];

    for (const [file] of changesByFile) {
      const fullPath = path.resolve(projectDir, file);
      const content = await readFile(fullPath, "utf-8");
      const newContent = content.replace(wordBoundaryRegex, newName);
      if (content !== newContent) {
        await writeFile(fullPath, newContent, "utf-8");
        filesModified.push(file);
      }
    }

    return JSON.stringify({
      success: true,
      oldName,
      newName,
      applied: true,
      totalReferences: refsResult.count,
      filesModified,
      message: `Renamed '${oldName}' to '${newName}' in ${filesModified.length} file(s). Run ghci_load to verify.`,
    });
  }

  return JSON.stringify({
    success: true,
    oldName,
    newName,
    totalReferences: refsResult.count,
    files,
    message: `Found ${refsResult.count} reference(s) across ${files.length} file(s). ` +
      `Use Edit tool to replace '${oldName}' with '${newName}' in each file, or re-run with apply=true.`,
  });
}

export function register(server: McpServer, ctx: ToolContext): void {
  registerStrictTool(server, ctx, 
    "ghci_rename",
    "Preview a rename operation for a Haskell identifier across the project. " +
      "Finds all references and shows which files and lines would change. " +
      "With apply=true, performs the rename directly in all files.",
    {
      old_name: z.string().describe("The current name to rename"),
      new_name: z.string().describe("The new name to use"),
      apply: z.boolean().optional().describe("If true, apply the rename directly to files. Default: false (preview only)."),
    },
    async ({ old_name, new_name, apply }) => {
      const result = await handleRename(ctx.getProjectDir(), {
        oldName: old_name,
        newName: new_name,
        apply,
      });
      return { content: [{ type: "text" as const, text: result }] };
    }
  );
}
