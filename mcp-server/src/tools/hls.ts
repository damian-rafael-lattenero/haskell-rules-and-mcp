/**
 * ghci_hls — Haskell Language Server integration.
 *
 * Actions:
 *   available   — detect if haskell-language-server-wrapper is installed
 *   hover       — get type info at a position via LSP textDocument/hover
 *   diagnostics — get diagnostics for a file via LSP
 *
 * Communicates via JSON-RPC 2.0 over stdin/stdout of the HLS process.
 * No external LSP library needed: uses raw Content-Length framing.
 */
import type { McpServer } from "@modelcontextprotocol/sdk/server/mcp.js";
import { z } from "zod";
import { execFile, spawn } from "node:child_process";
import { readFile } from "node:fs/promises";
import path from "node:path";
import { pathToFileURL } from "node:url";
import type { ToolContext } from "./registry.js";
import { resolveToolBinary, ensureTool, TOOL_PATH } from "./tool-installer.js";

// ─── LSP wire protocol helpers ────────────────────────────────────────────────

export interface LspMessage {
  jsonrpc: string;
  id?: number;
  method?: string;
  params?: unknown;
  result?: any;
  error?: { code: number; message: string };
}

/** Parse a raw LSP framed message. Returns null if not parseable. */
export function parseLspMessage(raw: string): LspMessage | null {
  if (!raw.includes("Content-Length:")) return null;
  const headerEnd = raw.indexOf("\r\n\r\n");
  if (headerEnd === -1) return null;
  const body = raw.slice(headerEnd + 4);
  try {
    return JSON.parse(body) as LspMessage;
  } catch {
    return null;
  }
}

/** Serialize an LSP message with Content-Length framing. */
export function serializeLspMessage(msg: LspMessage): string {
  const body = JSON.stringify(msg);
  return `Content-Length: ${Buffer.byteLength(body, "utf8")}\r\n\r\n${body}`;
}

/** Build a JSON-RPC 2.0 request. */
export function buildLspRequest(id: number, method: string, params: unknown): LspMessage {
  return { jsonrpc: "2.0", id, method, params };
}

/** Build an LSP initialize request. */
export function buildInitializeRequest(rootPath: string): LspMessage {
  return buildLspRequest(1, "initialize", {
    processId: process.pid,
    rootUri: pathToFileURL(rootPath).toString(),
    rootPath,
    capabilities: {
      textDocument: {
        hover: { contentFormat: ["plaintext", "markdown"] },
        publishDiagnostics: {},
      },
      workspace: { applyEdit: false },
    },
    initializationOptions: {},
  });
}

/** Build a textDocument/didOpen notification. */
export function buildDidOpenRequest(fileUri: string, text: string): LspMessage {
  return {
    jsonrpc: "2.0",
    method: "textDocument/didOpen",
    params: {
      textDocument: {
        uri: fileUri,
        languageId: "haskell",
        version: 1,
        text,
      },
    },
  };
}

/** Build a textDocument/hover request. */
export function buildHoverRequest(fileUri: string, line: number, character: number): LspMessage {
  return buildLspRequest(3, "textDocument/hover", {
    textDocument: { uri: fileUri },
    position: { line, character },
  });
}

// ─── HLS availability check ───────────────────────────────────────────────────

function hlsVersion(binaryPath = "haskell-language-server-wrapper"): Promise<string | undefined> {
  return new Promise((resolve) => {
    execFile(
      binaryPath,
      ["--version"],
      { env: { ...process.env, PATH: TOOL_PATH }, timeout: 10_000 },
      (err, stdout) => resolve(err ? undefined : stdout.trim().split("\n")[0])
    );
  });
}

// ─── HLS hover via LSP ────────────────────────────────────────────────────────

async function hlsHover(
  projectDir: string,
  filePath: string,
  line: number,
  character: number,
  binaryPath = "haskell-language-server-wrapper",
  timeout = 30_000
): Promise<string> {
  const absFile = path.resolve(projectDir, filePath);
  const fileUri = pathToFileURL(absFile).toString();

  let fileContent: string;
  try {
    fileContent = await readFile(absFile, "utf-8");
  } catch {
    return JSON.stringify({ success: false, error: `File not found: ${filePath}` });
  }

  return new Promise((resolve) => {
    const hls = spawn(binaryPath, ["--lsp"], {
      cwd: projectDir,
      env: { ...process.env, PATH: TOOL_PATH },
      stdio: ["pipe", "pipe", "pipe"],
    });

    let buffer = "";
    let settled = false;
    const timer = setTimeout(() => {
      if (!settled) {
        settled = true;
        hls.kill();
        resolve(JSON.stringify({ success: false, error: "HLS timeout: no hover response received in time" }));
      }
    }, timeout);

    // Send messages after process starts
    const send = (msg: LspMessage) => {
      if (hls.stdin.writable) {
        hls.stdin.write(serializeLspMessage(msg));
      }
    };

    let initialized = false;
    let didOpen = false;

    hls.stdout.on("data", (chunk: Buffer) => {
      buffer += chunk.toString("utf8");

      // Process all complete messages in buffer
      while (buffer.includes("\r\n\r\n")) {
        const headerEnd = buffer.indexOf("\r\n\r\n");
        const headerPart = buffer.slice(0, headerEnd);
        const lenMatch = headerPart.match(/Content-Length:\s*(\d+)/);
        if (!lenMatch) break;

        const contentLen = parseInt(lenMatch[1]!, 10);
        const bodyStart = headerEnd + 4;
        if (buffer.length < bodyStart + contentLen) break; // incomplete

        const body = buffer.slice(bodyStart, bodyStart + contentLen);
        buffer = buffer.slice(bodyStart + contentLen);

        let msg: LspMessage;
        try { msg = JSON.parse(body); } catch { continue; }

        // Initialize response
        if (msg.id === 1 && msg.result !== undefined && !initialized) {
          initialized = true;
          send({ jsonrpc: "2.0", method: "initialized", params: {} });

          if (!didOpen) {
            didOpen = true;
            send(buildDidOpenRequest(fileUri, fileContent));
            setTimeout(() => send(buildHoverRequest(fileUri, line, character)), 2000);
          }
        }

        // Hover response
        if (msg.id === 3) {
          if (!settled) {
            settled = true;
            clearTimeout(timer);
            hls.kill();

            const hoverResult = msg.result;
            if (!hoverResult) {
              resolve(JSON.stringify({ success: true, hover: null, message: "No hover info at this position" }));
            } else {
              const contents = hoverResult.contents;
              const text = typeof contents === "string"
                ? contents
                : contents?.value ?? JSON.stringify(contents);
              resolve(JSON.stringify({
                success: true,
                hover: { text, range: hoverResult.range },
              }));
            }
          }
        }
      }
    });

    hls.on("error", (err) => {
      if (!settled) {
        settled = true;
        clearTimeout(timer);
        resolve(JSON.stringify({ success: false, error: `HLS process error: ${err.message}` }));
      }
    });

    // Send initialize
    send(buildInitializeRequest(projectDir));
  });
}

// ─── Main handler ─────────────────────────────────────────────────────────────

export async function handleHls(
  projectDir: string,
  args: { action: string; module_path?: string; line?: number; character?: number }
): Promise<string> {
  if (args.action === "available") {
    const resolved = await ensureTool("hls");
    if (resolved.available) {
      const version = await hlsVersion(resolved.binaryPath ?? "haskell-language-server-wrapper");
      return JSON.stringify({
        success: true,
        action: "available",
        available: true,
        binaryResolved: true,
        versionProbeOk: Boolean(version),
        ...(version ? { version } : {}),
        source: resolved?.source ?? "host",
        binaryPath: resolved?.binaryPath,
        ...(resolved.checksumVerified !== undefined
          ? { checksumVerified: resolved.checksumVerified }
          : {}),
        _hint: "HLS is available. Use action='hover' to get type info at a position.",
      });
    }
    return JSON.stringify({
      success: true,
      action: "available",
      available: false,
      binaryResolved: false,
      versionProbeOk: false,
      source: "none",
      ...(resolved.error ? { error: resolved.error } : {}),
      _hint:
        "HLS is not available (not found in host PATH or bundled toolchain).",
    });
  }

  if (args.action === "hover") {
    if (!args.module_path) {
      return JSON.stringify({ success: false, error: "module_path is required for action 'hover'" });
    }
    const resolved = await ensureTool("hls");
    if (!resolved.available) {
      return JSON.stringify({
        success: false,
        unavailable: true,
        error:
          "HLS is not available (not found in host PATH or bundled toolchain).",
      });
    }

    const hover = await hlsHover(
      projectDir,
      args.module_path,
      args.line ?? 0,
      args.character ?? 0,
      resolved.binaryPath
    );
    const parsed = JSON.parse(hover) as Record<string, unknown>;
    return JSON.stringify({
      ...parsed,
      source: resolved.source,
      version: resolved.version,
      binaryPath: resolved.binaryPath,
    });
  }

  if (args.action === "diagnostics") {
    return JSON.stringify({
      success: false,
      error: "diagnostics action requires an active HLS session. Use ghci_load for compilation diagnostics instead.",
      _hint: "For real-time diagnostics, use ghci_load(diagnostics=true) which provides structured GHC errors and warnings.",
    });
  }

  return JSON.stringify({
    success: false,
    error: `Unknown action '${args.action}'. Valid actions: available, hover, diagnostics`,
  });
}

export function register(server: McpServer, ctx: ToolContext): void {
  server.tool(
    "ghci_hls",
    "Haskell Language Server (HLS) integration. " +
      "Actions: 'available' to check if HLS is installed; " +
      "'hover' to get type information at a specific position in a file (requires HLS); " +
      "'diagnostics' provides guidance on using ghci_load for diagnostics. " +
      "Note: hover requires HLS to be installed (ghcup install hls) and may take 10-30s on first use.",
    {
      action: z.enum(["available", "hover", "diagnostics"]).describe(
        '"available": check if HLS is installed. "hover": type info at position. "diagnostics": guidance on diagnostics.'
      ),
      module_path: z.string().optional().describe(
        'Path to the module (required for hover). Example: "src/MyModule.hs"'
      ),
      line: z.number().optional().describe(
        "0-indexed line number for hover. Default: 0"
      ),
      character: z.number().optional().describe(
        "0-indexed character position for hover. Default: 0"
      ),
    },
    async ({ action, module_path, line, character }) => {
      const result = await handleHls(ctx.getProjectDir(), { action, module_path, line, character });
      try {
        const parsed = JSON.parse(result);
        if (action === "available") {
          ctx.setOptionalToolAvailability("hls", parsed.available ? "available" : "unavailable");
        } else if (action === "hover" && parsed.unavailable) {
          ctx.setOptionalToolAvailability("hls", "unavailable");
        }
      } catch {
        // non-fatal
      }
      return { content: [{ type: "text" as const, text: result }] };
    }
  );
}
