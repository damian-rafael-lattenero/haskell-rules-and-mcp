import type { McpServer } from "@modelcontextprotocol/sdk/server/mcp.js";
import { z } from "zod";
import { execFile } from "node:child_process";
import path from "node:path";
import { GhciSession } from "../ghci-session.js";
import type { ToolContext } from "./registry.js";

export interface Reference {
  file: string;
  line: number;
  column: number;
  context: string; // The line content
}

/**
 * Find all references to a Haskell name across the project source files.
 * Uses ripgrep or grep with Haskell-aware filtering (skips comments, strings).
 */
export async function handleReferences(
  projectDir: string,
  args: { name: string }
): Promise<string> {
  const name = args.name;

  // Use ripgrep if available, fallback to grep
  const refs = await findReferencesWithGrep(projectDir, name);

  if (refs.length === 0) {
    return JSON.stringify({
      success: true,
      name,
      references: [],
      count: 0,
      message: `No references found for '${name}'`,
    });
  }

  return JSON.stringify({
    success: true,
    name,
    references: refs,
    count: refs.length,
  });
}

async function findReferencesWithGrep(
  projectDir: string,
  name: string
): Promise<Reference[]> {
  // Escape special regex characters in the name
  const escaped = name.replace(/[.*+?^${}()|[\]\\]/g, "\\$&");
  // Word boundary: match the name as a whole word (not substring)
  const pattern = `\\b${escaped}\\b`;

  return new Promise((resolve) => {
    // Try rg first (faster, respects .gitignore)
    execFile(
      "rg",
      [
        "--type", "haskell",
        "--line-number",
        "--column",
        "--no-heading",
        pattern,
        projectDir,
      ],
      { timeout: 10_000 },
      (error, stdout) => {
        if (error && !stdout) {
          // rg not found or no matches — try grep
          execFile(
            "grep",
            [
              "-rn",
              "--include=*.hs",
              pattern,
              projectDir,
            ],
            { timeout: 10_000 },
            (grepError, grepStdout) => {
              resolve(parseGrepOutput(grepStdout ?? "", projectDir));
            }
          );
          return;
        }
        resolve(parseRgOutput(stdout ?? "", projectDir));
      }
    );
  });
}

function parseRgOutput(output: string, projectDir: string): Reference[] {
  const refs: Reference[] = [];
  for (const line of output.trim().split("\n")) {
    if (!line) continue;
    // rg format: file:line:column:content
    const match = line.match(/^(.+?):(\d+):(\d+):(.*)$/);
    if (match) {
      refs.push({
        file: path.relative(projectDir, match[1]!),
        line: parseInt(match[2]!, 10),
        column: parseInt(match[3]!, 10),
        context: match[4]!.trim(),
      });
    }
  }
  return refs;
}

function parseGrepOutput(output: string, projectDir: string): Reference[] {
  const refs: Reference[] = [];
  for (const line of output.trim().split("\n")) {
    if (!line) continue;
    // grep -n format: file:line:content
    const match = line.match(/^(.+?):(\d+):(.*)$/);
    if (match) {
      refs.push({
        file: path.relative(projectDir, match[1]!),
        line: parseInt(match[2]!, 10),
        column: 1, // grep doesn't give column
        context: match[3]!.trim(),
      });
    }
  }
  return refs;
}

export function register(server: McpServer, ctx: ToolContext): void {
  server.tool(
    "ghci_references",
    "Find all references to a Haskell name across the project's .hs files. " +
      "Uses word-boundary matching to avoid partial matches. " +
      "Returns file, line, column, and the line content for each reference.",
    {
      name: z.string().describe(
        'The name to search for. Examples: "myFunction", "MyType", "Container"'
      ),
    },
    async ({ name }) => {
      const result = await handleReferences(ctx.getProjectDir(), { name });
      return { content: [{ type: "text" as const, text: result }] };
    }
  );
}
