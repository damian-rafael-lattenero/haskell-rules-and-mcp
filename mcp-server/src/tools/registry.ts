import type { McpServer } from "@modelcontextprotocol/sdk/server/mcp.js";
import type { GhciSession } from "../ghci-session.js";

/**
 * Context passed to each tool's register function.
 * Provides access to session, project directory, and other shared state.
 */
export interface ToolContext {
  getSession: () => Promise<GhciSession>;
  getProjectDir: () => string;
  getBaseDir: () => string;
  resetQuickCheckState: () => void;
}

export type RegisterFn = (server: McpServer, ctx: ToolContext) => void;
