import { spawn, ChildProcess } from "node:child_process";
import { EventEmitter } from "node:events";
import path from "node:path";
import { parseCabalPackageName } from "./parsers/cabal-parser.js";

const SENTINEL = "<<<GHCi-DONE-7f3a2b>>>";
const DEFAULT_TIMEOUT_MS = 30_000;
const STARTUP_TIMEOUT_MS = 90_000;

export interface GhciResult {
  output: string;
  success: boolean;
}

export type SessionHealth = 'healthy' | 'degraded' | 'corrupted';

export interface SessionHealthStatus {
  status: SessionHealth;
  lastCommand?: string;
  bufferSize: number;
}

export class GhciSession extends EventEmitter {
  private process: ChildProcess | null = null;
  private buffer: string = "";
  private pendingResolve: ((result: GhciResult) => void) | null = null;
  private pendingReject: ((error: Error) => void) | null = null;
  private pendingTimer: NodeJS.Timeout | null = null;
  private commandQueue: Promise<void> = Promise.resolve();
  private ready: boolean = false;
  private projectDir: string;
  private libraryTarget: string | undefined;
  private ghcupBin: string;
  /** Tracked imports that persist across :r/:l reloads. */
  private persistentImports: Set<string> = new Set();
  /** Session health status for monitoring and recovery. */
  private sessionHealth: SessionHealth = 'healthy';
  /** Last executed command for debugging. */
  private lastExecutedCommand?: string;

  constructor(projectDir: string, libraryTarget?: string) {
    super();
    this.projectDir = projectDir;
    this.libraryTarget = libraryTarget;
    this.ghcupBin = path.join(
      process.env.HOME ?? "/Users",
      ".ghcup",
      "bin"
    );
  }

  async start(): Promise<void> {
    if (this.process) {
      throw new Error("GHCi session already running");
    }

    const env = {
      ...process.env,
      PATH: `${this.ghcupBin}:${process.env.HOME}/.cabal/bin:${process.env.PATH}`,
    };

    // Auto-detect library target from .cabal if not provided
    const target = this.libraryTarget ?? `lib:${await parseCabalPackageName(this.projectDir)}`;

    return new Promise<void>((resolve, reject) => {
      this.process = spawn("cabal", ["repl", target], {
        cwd: this.projectDir,
        env,
        stdio: ["pipe", "pipe", "pipe"],
      });

      let startupBuffer = "";
      let startupStderr = "";
      let settled = false;

      const onStartupData = (data: Buffer) => {
        startupBuffer += data.toString();
        // GHCi is ready when we see the prompt after loading
        if (startupBuffer.includes("Ok,") || startupBuffer.includes("Ok.") || startupBuffer.includes("Loaded GHCi")) {
          this.process!.stdout!.removeListener("data", onStartupData);
          this.process!.stderr!.removeListener("data", onStartupStderr);
          // Remove the startup-only exit handler before installing the permanent one
          this.process!.removeListener("exit", onStartupExit);
          this.setupHandlers();
          this.initSession().then(() => {
            this.ready = true;
            settled = true;
            resolve();
          }).catch((err) => {
            settled = true;
            reject(err);
          });
        }
      };

      const onStartupStderr = (data: Buffer) => {
        startupStderr += data.toString();
      };

      const onStartupExit = (code: number | null) => {
        this.ready = false;
        this.process = null;
        if (!settled) {
          settled = true;
          // Detect common failure patterns and provide helpful hints
          let hint = "";
          if (startupStderr.includes("multiple") && startupStderr.includes(".cabal")) {
            hint = " Hint: Multiple .cabal files found. Did ghci_init create inside an existing project?";
          } else if (startupStderr.includes("can't find source")) {
            hint = " Hint: Source files missing. Run ghci_scaffold to create stubs.";
          }
          reject(
            new Error(
              `GHCi exited during startup with code ${code}.${hint} stderr: ${startupStderr}`
            )
          );
        }
        this.emit("exit", code);
      };

      this.process.stdout!.on("data", onStartupData);
      this.process.stderr!.on("data", onStartupStderr);

      this.process.on("error", (err) => {
        this.ready = false;
        if (!settled) {
          settled = true;
          reject(new Error(`Failed to start GHCi: ${err.message}`));
        }
      });

      this.process.on("exit", onStartupExit);

      // Timeout for startup — 90s to allow first-time cabal dependency resolution
      setTimeout(() => {
        if (!settled) {
          settled = true;
          this.kill();
          reject(
            new Error(
              `GHCi startup timed out after ${STARTUP_TIMEOUT_MS / 1000}s. stdout: ${startupBuffer}, stderr: ${startupStderr}`
            )
          );
        }
      }, STARTUP_TIMEOUT_MS);
    });
  }

  private setupHandlers(): void {
    if (!this.process) return;

    this.process.stdout!.on("data", (data: Buffer) => {
      this.buffer += data.toString();
      this.checkForSentinel();
    });

    this.process.stderr!.on("data", (data: Buffer) => {
      // GHC often writes warnings/errors to stderr, capture them too
      this.buffer += data.toString();
      this.checkForSentinel();
    });

    this.process.on("exit", (code) => {
      this.ready = false;
      this.process = null;
      if (this.pendingReject) {
        this.pendingReject(new Error(`GHCi exited unexpectedly with code ${code}`));
        this.clearPending();
      }
      this.emit("exit", code);
    });
  }

  private checkForSentinel(): void {
    const sentinelIndex = this.buffer.indexOf(SENTINEL);
    if (sentinelIndex !== -1 && this.pendingResolve) {
      const output = this.buffer.substring(0, sentinelIndex)
        .replace(/^\n+/, "")     // strip leading newlines (sentinel protocol artifact)
        .replace(/\s+$/, "");    // strip trailing whitespace
      this.buffer = this.buffer.substring(sentinelIndex + SENTINEL.length);
      const resolve = this.pendingResolve;
      this.clearPending();
      // Determine success: if output contains "error:" it's a failure
      const success = !output.toLowerCase().includes("error:");
      resolve({ output, success });
    }
    // NOTE: We intentionally do NOT drain orphan sentinels here.
    // waitForSentinel() also checks the buffer with pendingResolve=null,
    // and draining here would steal sentinels from it.
    // Instead, drainStaleSentinels() is called in executeInternal()
    // as defense-in-depth against any accumulated orphans.
  }

  private clearPending(): void {
    if (this.pendingTimer) {
      clearTimeout(this.pendingTimer);
      this.pendingTimer = null;
    }
    this.pendingResolve = null;
    this.pendingReject = null;
  }

  private async initSession(): Promise<void> {
    // Set up the sentinel-based prompt so we can detect command completion
    await this.rawSend(`:set prompt "\\n${SENTINEL}\\n"`);
    await this.rawSend(`:set prompt-cont ""`);
    // Each :set command triggers GHCi to display the new prompt (sentinel),
    // so we must consume both sentinels to avoid an off-by-one in execute().
    await this.waitForSentinel();
    await this.waitForSentinel();
    // Drain any extra sentinels and verify synchronization.
    await this.drainAndSync();

    // Enable commonly-needed GHC extensions so users don't have to add
    // them manually. ScopedTypeVariables is critical for QuickCheck
    // properties with type annotations like (\(x :: Int) -> ...).
    // Use executeInternal since the session isn't marked "ready" yet.
    await this.executeInternal(":set -XScopedTypeVariables");
    await this.executeInternal(":set -XTypeApplications");
    await this.executeInternal(":set -XOverloadedStrings");
  }

  /**
   * Drain stale sentinels from the buffer and send a sync handshake
   * to verify the command/output pipeline is properly aligned.
   */
  private async drainAndSync(): Promise<void> {
    // Give GHCi a moment to flush any pending output
    await new Promise((r) => setTimeout(r, 50));

    // Drain any leftover sentinels
    this.drainStaleSentinels();

    // Send a sync command and verify the response
    const syncToken = `<<<SYNC-${Date.now()}>>>`;
    const result = await this.executeInternal(
      `putStrLn "${syncToken}"`,
      DEFAULT_TIMEOUT_MS
    );

    if (result.output.includes(syncToken)) {
      return; // Synchronized
    }

    // Retry: drain again and send another sync
    this.drainStaleSentinels();
    const retry = await this.executeInternal(
      `putStrLn "${syncToken}"`,
      DEFAULT_TIMEOUT_MS
    );

    if (!retry.output.includes(syncToken)) {
      throw new Error(
        `GHCi sentinel sync failed after retry. Expected "${syncToken}" but got: "${retry.output.slice(0, 200)}"`
      );
    }
  }

  /**
   * Remove all sentinel occurrences currently sitting in the buffer.
   */
  private drainStaleSentinels(): void {
    while (this.buffer.includes(SENTINEL)) {
      const idx = this.buffer.indexOf(SENTINEL);
      this.buffer = this.buffer.substring(idx + SENTINEL.length);
    }
  }

  /**
   * Send a raw string to GHCi stdin without waiting for sentinel.
   */
  private rawSend(command: string): Promise<void> {
    return new Promise((resolve, reject) => {
      if (!this.process?.stdin?.writable) {
        reject(new Error("GHCi stdin not writable"));
        return;
      }
      this.process.stdin.write(command + "\n", (err) => {
        if (err) reject(err);
        else resolve();
      });
    });
  }

  /**
   * Wait for the sentinel to appear in the buffer.
   */
  private waitForSentinel(): Promise<string> {
    return new Promise((resolve) => {
      const check = () => {
        const sentinelIndex = this.buffer.indexOf(SENTINEL);
        if (sentinelIndex !== -1) {
          const output = this.buffer.substring(0, sentinelIndex)
            .replace(/^\n+/, "")     // strip leading newlines (sentinel protocol artifact)
            .replace(/\s+$/, "");    // strip trailing whitespace
          this.buffer = this.buffer.substring(sentinelIndex + SENTINEL.length);
          resolve(output);
        } else {
          setTimeout(check, 50);
        }
      };
      check();
    });
  }

  /**
   * Get the current health status of the session.
   */
  getHealth(): SessionHealthStatus {
    return {
      status: this.sessionHealth,
      lastCommand: this.lastExecutedCommand,
      bufferSize: this.buffer.length
    };
  }

  /**
   * Execute a GHCi command and return the output.
   * Concurrent calls are automatically serialized via an internal queue.
   */
  async execute(
    command: string,
    timeoutMs: number = DEFAULT_TIMEOUT_MS
  ): Promise<GhciResult> {
    if (!this.process || !this.ready) {
      throw new Error("GHCi session not running. Call start() first.");
    }

    // Check session health before executing
    if (this.sessionHealth === 'corrupted') {
      throw new Error('GHCi session is corrupted. Call restart() to recover.');
    }

    return new Promise<GhciResult>((outerResolve, outerReject) => {
      this.commandQueue = this.commandQueue.then(async () => {
        try {
          this.lastExecutedCommand = command;
          const result = await this.executeInternal(command, timeoutMs);
          outerResolve(result);
        } catch (err) {
          outerReject(err);
        }
      });
    });
  }

  /**
   * Internal: execute a single GHCi command (must not be called concurrently).
   */
  private executeInternal(
    command: string,
    timeoutMs: number = DEFAULT_TIMEOUT_MS
  ): Promise<GhciResult> {
    // Defense-in-depth: drain any stale sentinels that accumulated in the buffer
    // from previous operations (e.g., GHCi producing extra prompts during reload).
    // This is safe because the queue guarantees no concurrent execution, and
    // the previous command has already resolved and consumed its sentinel.
    this.drainStaleSentinels();

    if (this.pendingResolve) {
      // Defensive assertion — the queue should prevent this
      throw new Error("Another command is already in progress");
    }

    return new Promise<GhciResult>((resolve, reject) => {
      this.pendingResolve = resolve;
      this.pendingReject = reject;

      this.pendingTimer = setTimeout(() => {
        // Mark session as corrupted on timeout
        this.sessionHealth = 'corrupted';
        this.clearPending();
        
        // Auto-kill the process to prevent zombie state
        if (this.process) {
          this.process.kill('SIGTERM');
        }
        
        reject(
          new Error(
            `GHCi command timed out after ${timeoutMs}ms: ${command}`
          )
        );
      }, timeoutMs);

      this.rawSend(command).catch((err) => {
        this.clearPending();
        reject(err);
      });
    });
  }

  /**
   * Execute a block of statements using GHCi's :{ / :} multi-line syntax.
   * Useful for multi-line definitions or sequences of let bindings.
   * Note: GHCi commands (starting with :) are NOT supported inside blocks.
   */
  async executeBlock(lines: string[], timeoutMs?: number): Promise<GhciResult> {
    const block = ":{\n" + lines.map((l) => l + "\n").join("") + ":}\n";
    return this.execute(block, timeoutMs);
  }

  /**
   * Execute with automatic retry on "already in progress" errors.
   * Useful when concurrent tool calls may overlap.
   */
  async executeWithRetry(
    command: string,
    retries: number = 2,
    timeoutMs?: number
  ): Promise<GhciResult> {
    for (let i = 0; i < retries; i++) {
      try {
        return await this.execute(command, timeoutMs);
      } catch (err) {
        if (
          err instanceof Error &&
          err.message.includes("already in progress") &&
          i < retries - 1
        ) {
          await new Promise((r) => setTimeout(r, 100 * (i + 1)));
          continue;
        }
        throw err;
      }
    }
    throw new Error("executeWithRetry: unreachable");
  }

  /**
   * Reload modules first, then execute a command.
   * This ensures GHCi always sees the latest source code.
   */
  private async reloadThenExecute(command: string): Promise<GhciResult> {
    await this.execute(":r");
    // Re-apply persistent imports after reload
    if (this.persistentImports.size > 0) {
      await this.reapplyImports();
    }
    return this.execute(command);
  }

  /**
   * Get the type of an expression. Auto-reloads modules first
   * so you always get types based on the latest source code.
   */
  async typeOf(expression: string): Promise<GhciResult> {
    return this.reloadThenExecute(`:t ${expression}`);
  }

  /**
   * Get info about a name (type, typeclass, etc). Auto-reloads first.
   */
  async infoOf(name: string): Promise<GhciResult> {
    return this.reloadThenExecute(`:i ${name}`);
  }

  /**
   * Get the kind of a type. Auto-reloads first.
   */
  async kindOf(typeExpr: string): Promise<GhciResult> {
    return this.reloadThenExecute(`:k ${typeExpr}`);
  }

  /**
   * Get documentation for a name. Auto-reloads first.
   */
  async docOf(name: string): Promise<GhciResult> {
    return this.reloadThenExecute(`:doc ${name}`);
  }

  /**
   * Get completions for a prefix from GHCi's :complete command.
   * Does NOT auto-reload (operates on in-scope names).
   */
  async completionsOf(prefix: string): Promise<GhciResult> {
    return this.execute(`:complete repl "${prefix}"`);
  }

  /**
   * Show currently loaded imports.
   */
  async showImports(): Promise<GhciResult> {
    return this.execute(":show imports");
  }

  /**
   * Show modules currently loaded in GHCi.
   */
  async showModules(): Promise<GhciResult> {
    return this.execute(":show modules");
  }

  /**
   * Load a single module and re-apply persistent imports.
   */
  async loadModule(modulePath: string): Promise<GhciResult> {
    const result = await this.execute(`:l ${modulePath}`);
    if (this.persistentImports.size > 0) {
      await this.reapplyImports();
    }
    return result;
  }

  /**
   * Load multiple modules at once and bring them all into scope.
   * moduleNames are in Haskell dotted form (e.g. "HM.Syntax").
   */
  async loadModules(
    modulePaths: string[],
    moduleNames: string[]
  ): Promise<GhciResult> {
    const loadResult = await this.execute(`:l ${modulePaths.join(" ")}`);
    if (!loadResult.success) {
      return loadResult;
    }
    // Bring all modules into scope with full access (using * prefix)
    const starNames = moduleNames.map((n) => `*${n}`).join(" ");
    await this.execute(`:m + ${starNames}`);
    return loadResult;
  }

  /**
   * Add a persistent import that survives :r/:l reloads.
   * The import is executed immediately and re-applied after each reload.
   */
  async addPersistentImport(importCmd: string): Promise<GhciResult> {
    const result = await this.execute(importCmd);
    if (result.success) {
      this.persistentImports.add(importCmd);
    }
    return result;
  }

  /**
   * Re-apply all persistent imports. Called internally after :r/:l.
   */
  private async reapplyImports(): Promise<void> {
    for (const imp of this.persistentImports) {
      await this.execute(imp);
    }
  }

  /**
   * Reload all modules and re-apply persistent imports.
   */
  async reload(): Promise<GhciResult> {
    const result = await this.execute(":r");
    if (this.persistentImports.size > 0) {
      await this.reapplyImports();
    }
    return result;
  }

  /**
   * Execute multiple commands sequentially and return all results.
   * Stops early if stopOnError is true and a command fails.
   */
  async executeBatch(
    commands: string[],
    options?: { stopOnError?: boolean; reload?: boolean }
  ): Promise<{ results: GhciResult[]; allSuccess: boolean }> {
    const results: GhciResult[] = [];
    const stopOnError = options?.stopOnError ?? false;

    // Validate commands for dangerous patterns that break sentinel protocol
    for (const cmd of commands) {
      if (cmd.includes(':set +m') || cmd.includes(':set prompt')) {
        throw new Error('Dangerous GHCi command in batch: ' + cmd);
      }
    }

    if (options?.reload) {
      await this.execute(":r");
    }

    for (const cmd of commands) {
      const result = await this.execute(cmd);
      results.push(result);
      if (stopOnError && !result.success) {
        break;
      }
    }

    return {
      results,
      allSuccess: results.every((r) => r.success),
    };
  }

  /**
   * Check if the session is alive.
   */
  isAlive(): boolean {
    return this.ready && this.process !== null;
  }

  /**
   * Kill the GHCi process and wait for it to fully exit.
   */
  async kill(): Promise<void> {
    if (!this.process) return;

    this.ready = false;
    const proc = this.process;
    this.process = null;
    this.clearPending();
    this.commandQueue = Promise.resolve();

    return new Promise<void>((resolve) => {
      // If the process is already dead, resolve immediately
      if (proc.exitCode !== null || proc.killed) {
        resolve();
        return;
      }
      proc.once("exit", () => resolve());
      proc.kill("SIGTERM");
    });
  }

  /**
   * Restart the session (kill + start).
   */
  async restart(): Promise<void> {
    await this.kill();
    // Reset health status on restart
    this.sessionHealth = 'healthy';
    this.lastExecutedCommand = undefined;
    await this.start();
  }
}
