import { afterAll, beforeAll, describe, expect, it } from "vitest";
import { Client } from "@modelcontextprotocol/sdk/client/index.js";
import { StdioClientTransport } from "@modelcontextprotocol/sdk/client/stdio.js";
import { rm } from "node:fs/promises";
import path from "node:path";
import { resetBundledManifestCache } from "../../tools/tool-installer.js";
import {
  TEST_PLATFORM,
  bundledToolPath,
  readManifestRaw,
  restoreManifest,
  updateRuntimeManifestEntry,
  writeExecutable,
} from "../helpers/bundled-tools.js";

const fixtureDir = path.resolve(import.meta.dirname, "../fixtures/test-project");
const serverScript = path.resolve(import.meta.dirname, "../../../dist/index.js");
const fmtBin = bundledToolPath("fourmolu");
const hlsBin = bundledToolPath("hls");

describe("bundled tools e2e", () => {
  let client: Client;
  let transport: StdioClientTransport;
  let manifestSnapshot = "";

  beforeAll(async () => {
    manifestSnapshot = await readManifestRaw();
    if (TEST_PLATFORM === "win32") {
      await writeExecutable(
        fmtBin,
        "@echo off\r\nif \"%1\"==\"--mode\" if \"%2\"==\"stdout\" type \"%3\"\r\nif \"%1\"==\"--mode\" if \"%2\"==\"inplace\" exit /b 0\r\n"
      );
      await writeExecutable(hlsBin, "@echo off\r\necho haskell-language-server-wrapper 2.9.0\r\n");
    } else {
      await writeExecutable(
        fmtBin,
        "#!/usr/bin/env sh\nif [ \"$1\" = \"--mode\" ] && [ \"$2\" = \"stdout\" ]; then cat \"$3\"; exit 0; fi\nif [ \"$1\" = \"--mode\" ] && [ \"$2\" = \"inplace\" ]; then exit 0; fi\nexit 1\n"
      );
      await writeExecutable(
        hlsBin,
        "#!/usr/bin/env sh\nif [ \"$1\" = \"--version\" ]; then echo 'haskell-language-server-wrapper 2.9.0'; exit 0; fi\nexit 1\n"
      );
    }
    await updateRuntimeManifestEntry("fourmolu");
    await updateRuntimeManifestEntry("hls");
    resetBundledManifestCache();

    transport = new StdioClientTransport({
      command: "node",
      args: [serverScript],
      env: {
        ...process.env,
        HASKELL_PROJECT_DIR: fixtureDir,
        HASKELL_LIBRARY_TARGET: "lib:test-project",
      },
    });
    client = new Client({ name: "bundled-e2e-client", version: "0.1.0" }, { capabilities: {} });
    await client.connect(transport);
  }, 60_000);

  afterAll(async () => {
    try {
      await client.close();
    } catch {
      // ignore close errors
    }
    await rm(fmtBin, { force: true });
    await rm(hlsBin, { force: true });
    await restoreManifest(manifestSnapshot);
    resetBundledManifestCache();
  });

  it("ghci_format reports bundled source through MCP", async () => {
    const result = await client.callTool({
      name: "ghci_format",
      arguments: { module_path: "src/TestLib.hs", write: false },
    });
    const parsed = JSON.parse((result.content as Array<{ type: string; text: string }>)[0]!.text);
    expect(parsed.success).toBe(true);
    if (!parsed.fallback) {
      expect(parsed.source).toBe("bundled");
      expect(parsed.binaryPath).toContain("vendor-tools");
    }
  });

  it("ghci_hls available reports bundled source through MCP", async () => {
    const result = await client.callTool({
      name: "ghci_hls",
      arguments: { action: "available" },
    });
    const parsed = JSON.parse((result.content as Array<{ type: string; text: string }>)[0]!.text);
    expect(parsed.success).toBe(true);
    if (parsed.available) {
      expect(["host", "bundled"]).toContain(parsed.source);
      if (parsed.source === "bundled") {
        expect(parsed.binaryPath).toContain("vendor-tools");
      }
    }
  });

  it("ghci_watch start/status/stop lifecycle works", async () => {
    const startResult = await client.callTool({
      name: "ghci_watch",
      arguments: { action: "start", paths: ["src"], auto_actions: ["load"] },
    });
    const startParsed = JSON.parse((startResult.content as Array<{ type: string; text: string }>)[0]!.text);
    expect(startParsed.success).toBe(true);

    const statusResult = await client.callTool({
      name: "ghci_watch",
      arguments: { action: "status" },
    });
    const statusParsed = JSON.parse((statusResult.content as Array<{ type: string; text: string }>)[0]!.text);
    expect(statusParsed.success).toBe(true);
    expect(statusParsed.active).toBeTypeOf("boolean");

    const stopResult = await client.callTool({
      name: "ghci_watch",
      arguments: { action: "stop" },
    });
    const stopParsed = JSON.parse((stopResult.content as Array<{ type: string; text: string }>)[0]!.text);
    expect(stopParsed.success).toBe(true);
    expect(stopParsed.active).toBe(false);
  });
});
