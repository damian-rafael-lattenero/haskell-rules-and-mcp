import type { McpServer } from "@modelcontextprotocol/sdk/server/mcp.js";
import { z } from "zod";
import { readFile } from "node:fs/promises";
import path from "node:path";
import { handleReferences } from "./references.js";
import type { ToolContext } from "./registry.js";

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
  args: { oldName: string; newName: string }
): Promise<string> {
  const { oldName, newName } = args;

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

  return JSON.stringify({
    success: true,
    oldName,
    newName,
    totalReferences: refsResult.count,
    files,
    message: `Found ${refsResult.count} reference(s) across ${files.length} file(s). ` +
      `Use Edit tool to replace '${oldName}' with '${newName}' in each file.`,
  });
}

export function register(server: McpServer, ctx: ToolContext): void {
  server.tool(
    "ghci_rename",
    "Preview a rename operation for a Haskell identifier across the project. " +
      "Finds all references and shows which files and lines would change. " +
      "Does NOT edit files — returns a change plan for the agent to apply with the Edit tool.",
    {
      old_name: z.string().describe("The current name to rename"),
      new_name: z.string().describe("The new name to use"),
    },
    async ({ old_name, new_name }) => {
      const result = await handleRename(ctx.getProjectDir(), {
        oldName: old_name,
        newName: new_name,
      });
      return { content: [{ type: "text" as const, text: result }] };
    }
  );
}
