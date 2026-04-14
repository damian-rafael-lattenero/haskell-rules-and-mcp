/**
 * Auto-installer for optional Haskell development tools.
 *
 * When a tool (hlint, fourmolu, hls) is called and the binary is not present,
 * this module starts installation in the background and returns a "installing"
 * response so the LLM can retry after a short wait.
 *
 * Installation state is kept in module-level Maps so repeated calls within the
 * same MCP server process don't re-trigger installs.
 */
import { execFile } from "node:child_process";
import path from "node:path";

const GHCUP_BIN = path.join(process.env.HOME ?? "/Users", ".ghcup", "bin");
const CABAL_BIN = path.join(process.env.HOME ?? "/Users", ".cabal", "bin");
export const TOOL_PATH = `${GHCUP_BIN}:${CABAL_BIN}:${process.env.PATH}`;

type InstallStatus = "installing" | "done" | "failed";

// Module-level state — survives across tool calls in the same process.
const installState = new Map<string, InstallStatus>();
const installErrors = new Map<string, string>();

export interface ToolSpec {
  /** Binary name used to check availability with `which`. */
  checkCmd: string;
  /**
   * Primary install command (argv[0] = executable, rest = args).
   * Prefers ghcup (pre-built) over cabal (compiles from source).
   */
  installCmd: string[];
  /**
   * Fallback install command when the primary fails (e.g. ghcup not found).
   */
  fallbackInstallCmd?: string[];
  /** Max milliseconds to wait for the install process. Default: 360_000 (6 min). */
  installTimeout?: number;
  /** Optional command to run after successful install (e.g. hoogle generate). */
  postInstallCmd?: string[];
  /** Human-readable install hint shown in error messages. */
  manualInstallHint: string;
}

/**
 * Tool registry: the set of optional tools the MCP can auto-install.
 * hoogle is intentionally absent — it uses the Hoogle web API.
 */
export const TOOL_SPECS: Record<string, ToolSpec> = {
  hlint: {
    checkCmd: "hlint",
    installCmd: ["cabal", "install", "hlint", "--overwrite-policy=always"],
    installTimeout: 360_000,
    manualInstallHint: "cabal install hlint  OR  ghcup install hlint",
  },
  fourmolu: {
    checkCmd: "fourmolu",
    // ghcup is faster (pre-built), cabal is the fallback
    installCmd: ["ghcup", "install", "fourmolu", "latest"],
    fallbackInstallCmd: ["cabal", "install", "fourmolu", "--overwrite-policy=always"],
    installTimeout: 180_000,
    manualInstallHint: "ghcup install fourmolu  OR  cabal install fourmolu",
  },
  ormolu: {
    checkCmd: "ormolu",
    installCmd: ["cabal", "install", "ormolu", "--overwrite-policy=always"],
    installTimeout: 360_000,
    manualInstallHint: "cabal install ormolu",
  },
  hls: {
    checkCmd: "haskell-language-server-wrapper",
    installCmd: ["ghcup", "install", "hls", "latest"],
    installTimeout: 360_000,
    manualInstallHint: "ghcup install hls",
  },
};

// ─── Public API ───────────────────────────────────────────────────────────────

/** Check whether a tool binary exists in the MCP PATH. */
export function toolAvailable(toolName: string): Promise<boolean> {
  const checkCmd = TOOL_SPECS[toolName]?.checkCmd ?? toolName;
  return new Promise((resolve) => {
    execFile(
      "which",
      [checkCmd],
      { env: { ...process.env, PATH: TOOL_PATH } },
      (err) => resolve(!err)
    );
  });
}

export interface EnsureResult {
  /** True only when the tool is confirmed available right now. */
  available: boolean;
  installing?: boolean;
  justInstalled?: boolean;
  failed?: boolean;
  error?: string;
  /** Human-readable status message suitable for including in a tool response. */
  message: string;
}

/**
 * Ensure a tool is available, auto-installing it in the background if needed.
 *
 * Callers should check `result.available` before proceeding.  When
 * `result.installing` is true the LLM should retry in 30–120 seconds.
 */
export async function ensureTool(toolName: string): Promise<EnsureResult> {
  if (await toolAvailable(toolName)) {
    return { available: true, message: `${toolName} is available` };
  }

  const spec = TOOL_SPECS[toolName];
  if (!spec) {
    return {
      available: false,
      message: `${toolName} not found and no install spec registered. Install manually.`,
    };
  }

  const status = installState.get(toolName);

  if (status === "installing") {
    return {
      available: false,
      installing: true,
      message:
        `${toolName} is being installed in the background. ` +
        `Retry in 30–120 seconds.`,
    };
  }

  if (status === "failed") {
    const err = installErrors.get(toolName) ?? "unknown error";
    return {
      available: false,
      failed: true,
      error: err,
      message:
        `${toolName} auto-installation failed: ${err}. ` +
        `Install manually: ${spec.manualInstallHint}`,
    };
  }

  if (status === "done") {
    // Installation finished but the binary still isn't found — something went
    // wrong (wrong PATH, install to unexpected location, etc.).
    installState.delete(toolName);
    return {
      available: false,
      failed: true,
      message:
        `${toolName} was installed but is still not found in PATH. ` +
        `Install manually: ${spec.manualInstallHint}`,
    };
  }

  // First call: kick off background installation.
  installState.set(toolName, "installing");
  void installInBackground(toolName, spec);

  return {
    available: false,
    installing: true,
    message:
      `${toolName} not found — starting auto-installation ` +
      `(${spec.installCmd.join(" ")}). This may take 1–5 minutes. ` +
      `Retry this tool call after waiting. ` +
      `Manual alternative: ${spec.manualInstallHint}`,
  };
}

/** Reset install state for a tool (useful for testing or manual retry). */
export function resetInstallState(toolName: string): void {
  installState.delete(toolName);
  installErrors.delete(toolName);
}

/** Expose state for testing. */
export function getInstallStatus(toolName: string): InstallStatus | undefined {
  return installState.get(toolName);
}

// ─── Private helpers ──────────────────────────────────────────────────────────

async function installInBackground(toolName: string, spec: ToolSpec): Promise<void> {
  try {
    const ok = await runCommand(
      spec.installCmd[0]!,
      spec.installCmd.slice(1),
      spec.installTimeout ?? 360_000
    );

    if (!ok && spec.fallbackInstallCmd) {
      const fallbackOk = await runCommand(
        spec.fallbackInstallCmd[0]!,
        spec.fallbackInstallCmd.slice(1),
        spec.installTimeout ?? 360_000
      );
      if (!fallbackOk) {
        throw new Error(
          `Both primary (${spec.installCmd[0]}) and fallback (${spec.fallbackInstallCmd[0]}) installs failed`
        );
      }
    } else if (!ok) {
      throw new Error(`${spec.installCmd[0]} exited with non-zero code`);
    }

    if (spec.postInstallCmd) {
      // Post-install failure is non-fatal (e.g. hoogle generate may fail without network)
      await runCommand(
        spec.postInstallCmd[0]!,
        spec.postInstallCmd.slice(1),
        120_000
      ).catch(() => {});
    }

    installState.set(toolName, "done");
  } catch (err) {
    installState.set(toolName, "failed");
    installErrors.set(toolName, err instanceof Error ? err.message : String(err));
  }
}

/** Returns true on exit code 0, false on non-zero. Rejects only on spawn errors. */
function runCommand(cmd: string, args: string[], timeout: number): Promise<boolean> {
  return new Promise((resolve, reject) => {
    execFile(
      cmd,
      args,
      { env: { ...process.env, PATH: TOOL_PATH }, timeout },
      (error) => {
        if (error && (error as NodeJS.ErrnoException).code === "ENOENT") {
          // Executable not found — not a transient error
          reject(new Error(`Command not found: ${cmd}`));
        } else {
          resolve(!error);
        }
      }
    );
  });
}
