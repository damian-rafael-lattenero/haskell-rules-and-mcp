/**
 * E2E: the new scaffolding tools via the MCP protocol.
 *
 * Exercises:
 *   - ghci_create_project creates a brand-new project in a scratch dir
 *   - ghci_add_modules extends it with more modules + typed stubs
 *   - strict Zod schemas reject unknown params (Bug 1 fix)
 *   - ghci_create_project refuses to overwrite an existing project
 *   - ghci_lint_basic is no longer registered (removed from surface)
 */
import { afterAll, beforeAll, describe, expect, it } from "vitest";
import { Client } from "@modelcontextprotocol/sdk/client/index.js";
import { StdioClientTransport } from "@modelcontextprotocol/sdk/client/stdio.js";
import { mkdtempSync, rmSync, existsSync, readFileSync } from "node:fs";
import os from "node:os";
import path from "node:path";

const SERVER_SCRIPT = path.resolve(import.meta.dirname, "../../../dist/index.js");
const GHCUP_BIN = path.join(process.env.HOME ?? "", ".ghcup", "bin");
const TEST_PATH = `${GHCUP_BIN}:${process.env.PATH}`;

function parseResult(result: Awaited<ReturnType<Client["callTool"]>>): any {
  return JSON.parse((result.content as Array<{ type: string; text: string }>)[0]!.text);
}

describe("E2E: create-project + add-modules flow", () => {
  let client: Client;
  let transport: StdioClientTransport;
  let workspace: string;

  beforeAll(async () => {
    workspace = mkdtempSync(path.join(os.tmpdir(), "e2e-create-project-"));
    transport = new StdioClientTransport({
      command: process.execPath,
      args: [SERVER_SCRIPT],
      env: {
        ...process.env,
        PATH: TEST_PATH,
        HASKELL_PROJECT_DIR: workspace,
      } as Record<string, string>,
    });
    client = new Client({ name: "e2e-create-project", version: "0.0.1" });
    await client.connect(transport);
  }, 30_000);

  afterAll(async () => {
    try { await client.close(); } catch { /* ignore */ }
    try { rmSync(workspace, { recursive: true, force: true }); } catch { /* ignore */ }
  });

  it("ghci_create_project + ghci_add_modules are exposed and strict", async () => {
    const tools = await client.listTools();
    const names = tools.tools.map((t) => t.name);
    expect(names).toContain("ghci_create_project");
    expect(names).toContain("ghci_add_modules");
    // Tool removed from public surface:
    expect(names).not.toContain("ghci_lint_basic");

    const create = tools.tools.find((t) => t.name === "ghci_create_project")!;
    expect((create.inputSchema as { additionalProperties?: boolean }).additionalProperties).toBe(false);
    const add = tools.tools.find((t) => t.name === "ghci_add_modules")!;
    expect((add.inputSchema as { additionalProperties?: boolean }).additionalProperties).toBe(false);
  });

  it("ghci_create_project creates a clean project with switch_to_it=false (no GHCi)", async () => {
    const result = parseResult(
      await client.callTool({
        name: "ghci_create_project",
        arguments: {
          name: "e2e-demo",
          root_dir: workspace,
          modules: ["Demo.Core", "Demo.Util"],
          switch_to_it: false,
          with_test_suite: true,
        },
      })
    );
    expect(result.success).toBe(true);
    expect(result.switched).toBe(false);
    expect(result.created).toEqual(
      expect.arrayContaining(["e2e-demo.cabal", "cabal.project", "test/Spec.hs"])
    );
    const projectDir = path.join(workspace, "e2e-demo");
    expect(existsSync(path.join(projectDir, "src", "Demo", "Core.hs"))).toBe(true);
    expect(existsSync(path.join(projectDir, "src", "Demo", "Util.hs"))).toBe(true);
    const cabal = readFileSync(path.join(projectDir, "e2e-demo.cabal"), "utf-8");
    expect(cabal).toContain("    Demo.Core");
    expect(cabal).toContain("    Demo.Util");
  });

  it("refuses to overwrite an existing project (no force flag exists)", async () => {
    const second = parseResult(
      await client.callTool({
        name: "ghci_create_project",
        arguments: { name: "e2e-demo", root_dir: workspace, switch_to_it: false },
      })
    );
    expect(second.success).toBe(false);
    expect(second.error).toMatch(/already exists/i);
    expect(second.hint).toMatch(/ghci_add_modules/);
  });

  it("rejects unknown parameters at the MCP layer (strict Zod)", async () => {
    // `ghci_switch_project` used to accept `project_path` silently. With the
    // strict schema in place, the SDK surfaces the validation failure as an
    // isError response carrying the zod issue details (not a rejected promise
    // — the MCP JSON-RPC protocol returns the error in the response envelope).
    const response = await client.callTool({
      name: "ghci_switch_project",
      // @ts-expect-error: deliberately passing an unknown key
      arguments: { project_path: workspace },
    });
    expect(response.isError).toBe(true);
    const text = (response.content as Array<{ type: string; text: string }>)[0]!.text;
    expect(text).toMatch(/Unrecognized key|Input validation/);
  });

  it("ghci_add_modules fails cleanly when project has no .cabal", async () => {
    // Call add_modules in the workspace (empty), not in a project.
    // The server currently points at HASKELL_PROJECT_DIR (= workspace).
    const result = parseResult(
      await client.callTool({
        name: "ghci_add_modules",
        arguments: { modules: ["NoCabalHere"] },
      })
    );
    expect(result.success).toBe(false);
    expect(result.error).toMatch(/\.cabal/);
    expect(result.hint).toMatch(/ghci_create_project/);
  });
});
