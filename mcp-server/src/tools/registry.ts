import type { McpServer } from "@modelcontextprotocol/sdk/server/mcp.js";
import type { GhciSession } from "../ghci-session.js";
import { access } from "node:fs/promises";
import path from "node:path";

/**
 * Context passed to each tool's register function.
 * Provides access to session, project directory, and other shared state.
 */
export interface ToolContext {
  getSession: () => Promise<GhciSession>;
  getProjectDir: () => string;
  getBaseDir: () => string;
  resetQuickCheckState: () => void;
  /** Returns a notice string if rules are not installed, or null if OK. Cached. */
  getRulesNotice: () => Promise<string | null>;
  /** Reset the rules notice cache (call after ghci_setup installs rules). */
  resetRulesCache: () => void;
}

export type RegisterFn = (server: McpServer, ctx: ToolContext) => void;

/**
 * Create a cached rules-check function.
 * Checks once, then caches the result for the session lifetime.
 * Reset by calling the returned resetFn.
 */
export function createRulesChecker(
  getProjectDir: () => string,
  getBaseDir?: () => string
): {
  check: () => Promise<string | null>;
  reset: () => void;
} {
  let cached: string | null | undefined = undefined;

  return {
    check: async () => {
      if (cached !== undefined) return cached;

      // Check project dir first, then walk up to repo root (base dir)
      const dirsToCheck = [getProjectDir()];
      if (getBaseDir) {
        const baseDir = getBaseDir();
        if (baseDir !== getProjectDir()) {
          dirsToCheck.push(baseDir);
        }
      }

      for (const dir of dirsToCheck) {
        const automationPath = path.join(dir, ".claude", "rules", "haskell-automation.md");
        try {
          await access(automationPath);
          cached = null; // Rules exist
          return cached;
        } catch {
          // Not found here, try next directory
        }
      }

      cached =
        "Haskell development rules not installed. Run ghci_setup() to enable the automation loop, " +
        "warning action table, and development workflow.";
      return cached;
    },
    reset: () => {
      cached = undefined;
    },
  };
}
