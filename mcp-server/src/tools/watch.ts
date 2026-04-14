import type { McpServer } from "@modelcontextprotocol/sdk/server/mcp.js";
import { z } from "zod";
import path from "node:path";
import { watch, type FSWatcher } from "node:fs";
import type { ToolContext } from "./registry.js";

interface WatchState {
  active: boolean;
  watchPaths: string[];
  autoActions: Array<"load" | "quickcheck">;
  lastEvent: { eventType: string; file: string; at: string } | null;
  watchers: FSWatcher[];
  eventsSeen: number;
}

const state: WatchState = {
  active: false,
  watchPaths: [],
  autoActions: [],
  lastEvent: null,
  watchers: [],
  eventsSeen: 0,
};

function stopWatchers(): void {
  for (const w of state.watchers) {
    try { w.close(); } catch { /* ignore */ }
  }
  state.watchers = [];
  state.active = false;
}

export function register(server: McpServer, ctx: ToolContext): void {
  server.tool(
    "ghci_watch",
    "Watch files for changes and optionally auto-run ghci_load/quickcheck.",
    {
      action: z.enum(["start", "stop", "status"]).describe("start|stop|status"),
      paths: z.array(z.string()).optional().describe("Paths to watch, default ['src']"),
      auto_actions: z.array(z.enum(["load", "quickcheck"])).optional().describe("Auto actions on change"),
    },
    async ({ action, paths, auto_actions }) => {
      if (action === "status") {
        return {
          content: [{
            type: "text" as const,
            text: JSON.stringify({
              success: true,
              active: state.active,
              watchPaths: state.watchPaths,
              autoActions: state.autoActions,
              eventsSeen: state.eventsSeen,
              lastEvent: state.lastEvent,
            }),
          }],
        };
      }

      if (action === "stop") {
        stopWatchers();
        state.watchPaths = [];
        state.autoActions = [];
        state.lastEvent = null;
        return {
          content: [{
            type: "text" as const,
            text: JSON.stringify({
              success: true,
              active: false,
              message: "Watch mode stopped",
            }),
          }],
        };
      }

      // action=start
      stopWatchers();
      state.watchPaths = (paths && paths.length > 0) ? paths : ["src"];
      state.autoActions = auto_actions ?? [];
      state.eventsSeen = 0;
      state.lastEvent = null;

      for (const p of state.watchPaths) {
        const abs = path.resolve(ctx.getProjectDir(), p);
        try {
          const watcher = watch(abs, { recursive: true }, async (eventType, fileName) => {
            const file = String(fileName ?? "");
            state.eventsSeen += 1;
            state.lastEvent = { eventType, file, at: new Date().toISOString() };

            if (state.autoActions.includes("load") && file.endsWith(".hs")) {
              try {
                const session = await ctx.getSession();
                const rel = path.relative(ctx.getProjectDir(), path.join(abs, file));
                await session.loadModule(rel);
              } catch {
                // Non-fatal: watch mode should continue even if load fails
              }
            }
          });
          state.watchers.push(watcher);
        } catch {
          // Skip paths that cannot be watched
        }
      }

      state.active = state.watchers.length > 0;

      return {
        content: [{
          type: "text" as const,
          text: JSON.stringify({
            success: true,
            active: state.active,
            watchPaths: state.watchPaths,
            autoActions: state.autoActions,
            message: state.active
              ? "Watch mode started"
              : "No valid paths to watch",
          }),
        }],
      };
    }
  );
}
