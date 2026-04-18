/**
 * ghci_refactor — Text-based refactoring for Haskell source files.
 *
 * Actions:
 *   rename_local  — rename a binding everywhere in the module (word-boundary aware)
 *   extract_binding — extract a range of lines to a new top-level function
 *
 * These are text transformations only (no AST). They work reliably for simple
 * cases. For complex refactoring, use an HLS-backed tool.
 */
import type { McpServer } from "@modelcontextprotocol/sdk/server/mcp.js";
import { z } from "zod";
import { readFile, writeFile } from "node:fs/promises";
import path from "node:path";
import { type ToolContext, registerStrictTool } from "./registry.js";

export async function handleRefactor(
  projectDir: string,
  args: {
    action: string;
    module_path?: string;
    old_name?: string;
    new_name?: string;
    lines?: number[];
  }
): Promise<string> {
  const { action } = args;

  if (action === "rename_local") {
    return handleRenameLocal(projectDir, args);
  }

  if (action === "extract_binding") {
    return handleExtractBinding(projectDir, args);
  }

  return JSON.stringify({
    success: false,
    error: `Unknown action '${action}'. Valid actions: rename_local, extract_binding`,
  });
}

async function handleRenameLocal(
  projectDir: string,
  args: { module_path?: string; old_name?: string; new_name?: string }
): Promise<string> {
  if (!args.module_path) {
    return JSON.stringify({ success: false, error: "module_path is required for rename_local" });
  }
  if (!args.old_name || !args.new_name) {
    return JSON.stringify({ success: false, error: "old_name and new_name are required for rename_local" });
  }

  const absPath = path.resolve(projectDir, args.module_path);
  let content: string;
  try {
    content = await readFile(absPath, "utf-8");
  } catch {
    return JSON.stringify({ success: false, error: `File not found: ${args.module_path}` });
  }

  const { old_name, new_name } = args;

  // Word-boundary regex to avoid matching substrings
  // e.g. renaming "foo" should not affect "fooBar" or "barfoo"
  const wordBoundaryRegex = new RegExp(`\\b${escapeRegex(old_name)}\\b`, "g");

  let changed = 0;
  const lines = content.split("\n");
  const diff: Array<{ line: number; before: string; after: string }> = [];

  const newLines = lines.map((line, idx) => {
    const newLine = line.replace(wordBoundaryRegex, () => {
      changed++;
      return new_name;
    });
    if (newLine !== line) {
      diff.push({ line: idx + 1, before: line, after: newLine });
    }
    return newLine;
  });

  if (changed > 0) {
    await writeFile(absPath, newLines.join("\n"), "utf-8");
  }

  return JSON.stringify({
    success: true,
    action: "rename_local",
    module_path: args.module_path,
    old_name,
    new_name,
    changed,
    diff,
    message: changed > 0
      ? `Renamed '${old_name}' → '${new_name}' in ${changed} location(s). Run ghci_load to verify.`
      : `'${old_name}' not found in ${args.module_path}. No changes made.`,
  });
}

async function handleExtractBinding(
  projectDir: string,
  args: { module_path?: string; new_name?: string; lines?: number[] }
): Promise<string> {
  if (!args.module_path) {
    return JSON.stringify({ success: false, error: "module_path is required for extract_binding" });
  }
  if (!args.new_name) {
    return JSON.stringify({ success: false, error: "new_name is required for extract_binding" });
  }
  if (!args.lines || args.lines.length < 2) {
    return JSON.stringify({ success: false, error: "lines [start, end] is required for extract_binding" });
  }

  const absPath = path.resolve(projectDir, args.module_path);
  let content: string;
  try {
    content = await readFile(absPath, "utf-8");
  } catch {
    return JSON.stringify({ success: false, error: `File not found: ${args.module_path}` });
  }

  const lines = content.split("\n");
  const [startLine, endLine] = [args.lines[0]! - 1, args.lines[1]! - 1]; // 0-indexed

  if (startLine < 0 || endLine >= lines.length || startLine > endLine) {
    return JSON.stringify({
      success: false,
      error: `Invalid line range [${args.lines[0]}, ${args.lines[1]}]. File has ${lines.length} lines.`,
    });
  }

  // Extract the selected lines
  const extracted = lines.slice(startLine, endLine + 1);
  const extractedContent = extracted.join("\n").trimEnd();

  // Build new binding: new_name = \n  extracted lines
  const indent = "  ";
  const newBinding = `${args.new_name} =\n${extracted.map((l) => `${indent}${l.trim()}`).join("\n")}`;

  // Replace extracted lines with a call to the new binding
  const replacement = `${indent}${args.new_name}`;
  const newLines = [
    ...lines.slice(0, startLine),
    replacement,
    ...lines.slice(endLine + 1),
    "",
    newBinding,
  ];

  await writeFile(absPath, newLines.join("\n"), "utf-8");

  return JSON.stringify({
    success: true,
    action: "extract_binding",
    module_path: args.module_path,
    new_name: args.new_name,
    extracted_lines: endLine - startLine + 1,
    extracted_content: extractedContent,
    message: `Extracted lines ${args.lines[0]}–${args.lines[1]} to new binding '${args.new_name}'. Run ghci_load to verify.`,
  });
}

function escapeRegex(s: string): string {
  return s.replace(/[.*+?^${}()|[\]\\]/g, "\\$&");
}

export function register(server: McpServer, ctx: ToolContext): void {
  registerStrictTool(server, ctx, 
    "ghci_refactor",
    "Text-based refactoring for Haskell source files. " +
      "Actions: 'rename_local' renames a binding everywhere in a module (word-boundary safe, no substring matches). " +
      "'extract_binding' extracts a range of lines to a new top-level function. " +
      "These are text transformations — run ghci_load after to verify the result compiles.",
    {
      action: z.enum(["rename_local", "extract_binding"]).describe(
        '"rename_local": rename a binding in a module. "extract_binding": extract lines to a new function.'
      ),
      module_path: z.string().optional().describe(
        'Path to the module to refactor. Examples: "src/MyModule.hs"'
      ),
      old_name: z.string().optional().describe(
        'Current name to rename. Required for rename_local. Examples: "helper", "myFn"'
      ),
      new_name: z.string().optional().describe(
        'New name. Required for rename_local and extract_binding. Examples: "increment", "extracted"'
      ),
      lines: z.array(z.number()).optional().describe(
        'Line range [start, end] (1-indexed, inclusive). Required for extract_binding. Example: [5, 8]'
      ),
    },
    async ({ action, module_path, old_name, new_name, lines }) => {
      const result = await handleRefactor(ctx.getProjectDir(), {
        action,
        module_path,
        old_name,
        new_name,
        lines,
      });
      return { content: [{ type: "text" as const, text: result }] };
    }
  );
}
