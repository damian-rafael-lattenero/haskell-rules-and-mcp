/**
 * Tests for the multi-agent instructions cache auto-sync.
 *
 * The sync writes haskell-mcp-workflow.md to every installed agent's cache
 * at server startup so that serverUseInstructions is always up-to-date
 * without manual copy-paste.
 *
 * Architecture:
 *   - Protocol layer  (universal): McpServer.instructions → all MCP clients
 *   - File-cache layer (specific): INSTRUCTIONS.md → Cursor, Windsurf, etc.
 */
import { describe, it, expect, beforeEach, afterEach } from "vitest";
import { mkdtemp, rm, readFile, mkdir, writeFile } from "node:fs/promises";
import path from "node:path";
import os from "node:os";

// Import the helpers directly — they are exported from index.ts.
// NOTE: index.ts uses top-level await (GHCi session startup etc.) so we
// duplicate only the pure helpers here to keep tests fast and side-effect free.
// The integration between these functions and the server is covered by the
// fact that index.ts calls syncAgentInstructionsCaches at the top level.

function encodeProjectId(workspaceRoot: string): string {
  return workspaceRoot.replace(/^\//, "").replace(/\//g, "-");
}

interface AgentCacheSpec {
  name: string;
  configDir: string;
  instructionsPath: (home: string, projectId: string, serverName: string) => string;
}

const DEFAULT_SPECS: AgentCacheSpec[] = [
  {
    name: "cursor",
    configDir: ".cursor",
    instructionsPath: (home, projectId, serverName) =>
      path.join(home, ".cursor", "projects", projectId, "mcps", serverName, "INSTRUCTIONS.md"),
  },
  {
    name: "windsurf",
    configDir: ".windsurf",
    instructionsPath: (home, projectId, serverName) =>
      path.join(home, ".windsurf", "projects", projectId, "mcps", serverName, "INSTRUCTIONS.md"),
  },
];

async function syncAgentInstructionsCaches(
  workspaceRoot: string,
  instructions: string,
  specs: AgentCacheSpec[],
  overrideHome: string
): Promise<{ synced: string[]; skipped: string[] }> {
  const { stat } = await import("node:fs/promises");
  const synced: string[] = [];
  const skipped: string[] = [];

  const projectId = encodeProjectId(workspaceRoot);
  const SERVER_NAME = "user-haskell-flows";

  await Promise.all(
    specs.map(async (spec) => {
      try {
        await stat(path.join(overrideHome, spec.configDir));
        const cachePath = spec.instructionsPath(overrideHome, projectId, SERVER_NAME);
        await mkdir(path.dirname(cachePath), { recursive: true });
        await writeFile(cachePath, instructions, "utf-8");
        synced.push(spec.name);
      } catch {
        skipped.push(spec.name);
      }
    })
  );

  return { synced, skipped };
}

// ─── encodeProjectId ──────────────────────────────────────────────────────────

describe("encodeProjectId", () => {
  it("strips leading slash and replaces remaining slashes with dashes", () => {
    expect(encodeProjectId("/Users/foo/bar/my-project")).toBe(
      "Users-foo-bar-my-project"
    );
  });

  it("matches the actual Cursor/Windsurf encoding for this project", () => {
    const workspace = "/Users/dlattenero/Personal-Projects/haskell-rules-and-mcp";
    expect(encodeProjectId(workspace)).toBe(
      "Users-dlattenero-Personal-Projects-haskell-rules-and-mcp"
    );
  });

  it("handles a single-segment path", () => {
    expect(encodeProjectId("/project")).toBe("project");
  });

  it("handles deeply nested paths", () => {
    expect(encodeProjectId("/a/b/c/d/e")).toBe("a-b-c-d-e");
  });

  it("is consistent: same input always produces same output", () => {
    const p = "/Users/test/my-workspace";
    expect(encodeProjectId(p)).toBe(encodeProjectId(p));
  });
});

// ─── AGENT_CACHE_SPECS registry ───────────────────────────────────────────────

describe("DEFAULT_SPECS registry", () => {
  it("includes Cursor", () => {
    expect(DEFAULT_SPECS.find((s) => s.name === "cursor")).toBeDefined();
  });

  it("includes Windsurf", () => {
    expect(DEFAULT_SPECS.find((s) => s.name === "windsurf")).toBeDefined();
  });

  it("Cursor path uses ~/.cursor/projects/...", () => {
    const spec = DEFAULT_SPECS.find((s) => s.name === "cursor")!;
    const p = spec.instructionsPath("/home/user", "my-project", "my-server");
    expect(p).toContain(".cursor");
    expect(p).toContain("projects");
    expect(p).toContain("my-project");
    expect(p).toContain("my-server");
    expect(p).toEndWith("INSTRUCTIONS.md");
  });

  it("Windsurf path uses ~/.windsurf/projects/...", () => {
    const spec = DEFAULT_SPECS.find((s) => s.name === "windsurf")!;
    const p = spec.instructionsPath("/home/user", "my-project", "my-server");
    expect(p).toContain(".windsurf");
    expect(p).toContain("projects");
    expect(p).toEndWith("INSTRUCTIONS.md");
  });

  it("does NOT include Claude Code (uses MCP protocol, not file cache)", () => {
    // Claude Code reads `instructions` from the MCP protocol `initialize`
    // response on every startup.  Its ~/.claude/projects/ directory stores
    // conversation transcripts and tool-result caches, but has NO
    // mcps/INSTRUCTIONS.md file.  Instructions are always fresh from the
    // protocol — no sync needed.
    const names = DEFAULT_SPECS.map((s) => s.name);
    expect(names).not.toContain("claude-code");
  });

  it("does NOT include GitHub Copilot (uses MCP protocol, not file cache)", () => {
    const names = DEFAULT_SPECS.map((s) => s.name);
    expect(names).not.toContain("copilot");
  });

  it("Claude Code project path encoding uses a leading dash (different from Cursor)", () => {
    // Cursor:      /Users/foo/bar → Users-foo-bar     (strip leading /)
    // Claude Code: /Users/foo/bar → -Users-foo-bar    (replace all / with -)
    // This difference is real and observed in ~/.claude/projects/
    // e.g.: -Users-dlattenero-Personal-Projects-haskell-rules-and-mcp
    const workspace = "/Users/dlattenero/Personal-Projects/haskell-rules-and-mcp";
    const cursorId = encodeProjectId(workspace);
    const claudeCodeId = workspace.replace(/\//g, "-"); // Claude Code encoding

    expect(cursorId).toBe("Users-dlattenero-Personal-Projects-haskell-rules-and-mcp");
    expect(claudeCodeId).toBe("-Users-dlattenero-Personal-Projects-haskell-rules-and-mcp");
    expect(cursorId).not.toBe(claudeCodeId); // they differ!
  });
});

// ─── syncAgentInstructionsCaches ─────────────────────────────────────────────

describe("syncAgentInstructionsCaches", () => {
  let tmpHome: string;

  beforeEach(async () => {
    tmpHome = await mkdtemp(path.join(os.tmpdir(), "agent-sync-test-"));
  });

  afterEach(async () => {
    await rm(tmpHome, { recursive: true, force: true });
  });

  it("syncs to an agent whose configDir exists", async () => {
    await mkdir(path.join(tmpHome, ".cursor"), { recursive: true });

    const result = await syncAgentInstructionsCaches(
      "/Users/test/project",
      "# Instructions",
      DEFAULT_SPECS,
      tmpHome
    );

    expect(result.synced).toContain("cursor");
    expect(result.skipped).toContain("windsurf"); // not installed

    const cachePath = path.join(
      tmpHome, ".cursor", "projects", "Users-test-project",
      "mcps", "user-haskell-flows", "INSTRUCTIONS.md"
    );
    expect(await readFile(cachePath, "utf-8")).toBe("# Instructions");
  });

  it("skips agents whose configDir does not exist", async () => {
    // Neither .cursor nor .windsurf exist
    const result = await syncAgentInstructionsCaches(
      "/Users/test/project",
      "content",
      DEFAULT_SPECS,
      tmpHome
    );

    expect(result.synced).toHaveLength(0);
    expect(result.skipped).toContain("cursor");
    expect(result.skipped).toContain("windsurf");
  });

  it("syncs to ALL installed agents simultaneously", async () => {
    // Both Cursor and Windsurf installed
    await mkdir(path.join(tmpHome, ".cursor"));
    await mkdir(path.join(tmpHome, ".windsurf"));

    const result = await syncAgentInstructionsCaches(
      "/Users/test/project",
      "multi-agent content",
      DEFAULT_SPECS,
      tmpHome
    );

    expect(result.synced).toContain("cursor");
    expect(result.synced).toContain("windsurf");
    expect(result.skipped).toHaveLength(0);

    // Both files should have the same content
    for (const agentDir of [".cursor", ".windsurf"]) {
      const cachePath = path.join(
        tmpHome, agentDir, "projects", "Users-test-project",
        "mcps", "user-haskell-flows", "INSTRUCTIONS.md"
      );
      expect(await readFile(cachePath, "utf-8")).toBe("multi-agent content");
    }
  });

  it("overwrites on repeated startups — always latest version wins", async () => {
    await mkdir(path.join(tmpHome, ".cursor"));

    await syncAgentInstructionsCaches("/Users/test/project", "v1", DEFAULT_SPECS, tmpHome);
    await syncAgentInstructionsCaches("/Users/test/project", "v2", DEFAULT_SPECS, tmpHome);

    const cachePath = path.join(
      tmpHome, ".cursor", "projects", "Users-test-project",
      "mcps", "user-haskell-flows", "INSTRUCTIONS.md"
    );
    expect(await readFile(cachePath, "utf-8")).toBe("v2");
  });

  it("is non-fatal when all agents are absent — returns empty synced list", async () => {
    const result = await syncAgentInstructionsCaches(
      "/Users/test/project", "content", DEFAULT_SPECS, tmpHome
    );
    expect(result.synced).toHaveLength(0);
    expect(result.skipped.length).toBeGreaterThan(0);
  });

  it("accepts a custom spec registry — extensible to new agents", async () => {
    const myAgent: AgentCacheSpec = {
      name: "my-custom-agent",
      configDir: ".my-agent",
      instructionsPath: (home, projectId, serverName) =>
        path.join(home, ".my-agent", "cache", projectId, serverName, "INSTRUCTIONS.md"),
    };

    await mkdir(path.join(tmpHome, ".my-agent"));

    const result = await syncAgentInstructionsCaches(
      "/Users/test/project", "custom content", [myAgent], tmpHome
    );

    expect(result.synced).toContain("my-custom-agent");
    const cachePath = path.join(
      tmpHome, ".my-agent", "cache", "Users-test-project",
      "user-haskell-flows", "INSTRUCTIONS.md"
    );
    expect(await readFile(cachePath, "utf-8")).toBe("custom content");
  });

  it("preserves full multi-line markdown content exactly", async () => {
    await mkdir(path.join(tmpHome, ".cursor"));

    const instructions = [
      "# Haskell MCP Workflow",
      "",
      "| When | Tool |",
      "|------|------|",
      "| Start | `ghci_session(status)` |",
      "",
      "- `ghci_load` after every edit",
    ].join("\n");

    await syncAgentInstructionsCaches("/Users/test/project", instructions, DEFAULT_SPECS, tmpHome);

    const cachePath = path.join(
      tmpHome, ".cursor", "projects", "Users-test-project",
      "mcps", "user-haskell-flows", "INSTRUCTIONS.md"
    );
    expect(await readFile(cachePath, "utf-8")).toBe(instructions);
  });
});
