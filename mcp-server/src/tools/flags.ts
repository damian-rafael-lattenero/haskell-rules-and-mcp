/**
 * ghci_flags — Manage GHCi language flags and extensions in the active session.
 * Actions: set, unset, list
 */
import type { McpServer } from "@modelcontextprotocol/sdk/server/mcp.js";
import { z } from "zod";
import type { GhciSession } from "../ghci-session.js";
import type { ToolContext } from "./registry.js";

export async function handleFlags(
  session: GhciSession,
  args: { action: string; flags?: string }
): Promise<string> {
  const { action } = args;

  if (action === "set") {
    if (!args.flags) {
      return JSON.stringify({
        success: false,
        error: "flags parameter is required for action 'set'. Example: { flags: '-XOverloadedStrings' }",
      });
    }
    const result = await session.execute(`:set ${args.flags}`);
    return JSON.stringify({
      success: result.success,
      action: "set",
      flags: args.flags,
      message: result.success
        ? `Applied: :set ${args.flags}`
        : `Failed to apply flags: ${result.output}`,
      ...(result.output ? { output: result.output } : {}),
    });
  }

  if (action === "unset") {
    if (!args.flags) {
      return JSON.stringify({
        success: false,
        error: "flags parameter is required for action 'unset'. Example: { flags: '-XOverloadedStrings' }",
      });
    }
    const result = await session.execute(`:unset ${args.flags}`);
    return JSON.stringify({
      success: result.success,
      action: "unset",
      flags: args.flags,
      message: result.success
        ? `Removed: :unset ${args.flags}`
        : `Failed to unset flags: ${result.output}`,
      ...(result.output ? { output: result.output } : {}),
    });
  }

  if (action === "list") {
    const result = await session.execute(":show language");
    const flags = parseLanguageOutput(result.output);
    return JSON.stringify({
      success: true,
      action: "list",
      flags,
      raw: result.output,
    });
  }

  return JSON.stringify({
    success: false,
    error: `Unknown action '${action}'. Valid actions: set, unset, list`,
  });
}

/** Parse :show language output into a list of active language flags. */
function parseLanguageOutput(output: string): string[] {
  const flags: string[] = [];
  const lines = output.split("\n");

  // First line: "base language is X" or "The Haskell language is X"
  const baseLine = lines[0] ?? "";
  const baseMatch = baseLine.match(/(?:base language is|language is)\s+(\S+)/i);
  if (baseMatch) {
    flags.push(baseMatch[1]!);
  }

  // Modifier lines: "  -XFoo" or "  -XNoBar"
  for (const line of lines.slice(1)) {
    const trimmed = line.trim();
    if (trimmed.startsWith("-X") || trimmed.startsWith("-W")) {
      flags.push(trimmed);
    }
  }

  return flags;
}

export function register(server: McpServer, ctx: ToolContext): void {
  server.tool(
    "ghci_flags",
    "Manage GHCi language extensions and flags in the active session. " +
      "Actions: 'set' to enable flags/extensions, 'unset' to disable them, 'list' to see active language settings. " +
      "Changes apply only to the current GHCi session (not persisted to .cabal). " +
      "To persist, add the extension to default-extensions in your .cabal file.",
    {
      action: z.enum(["set", "unset", "list"]).describe(
        '"set": enable flags (e.g. -XOverloadedStrings). "unset": disable flags. "list": show active language settings.'
      ),
      flags: z.string().optional().describe(
        'Flags to set or unset. Examples: "-XOverloadedStrings", "-Wall -Werror", "-XTupleSections". Required for set/unset.'
      ),
    },
    async ({ action, flags }) => {
      const session = await ctx.getSession();
      const result = await handleFlags(session, { action, flags });
      return { content: [{ type: "text" as const, text: result }] };
    }
  );
}
