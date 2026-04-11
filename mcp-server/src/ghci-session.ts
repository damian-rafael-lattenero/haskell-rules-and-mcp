import { spawn, ChildProcess } from "node:child_process";
import { EventEmitter } from "node:events";
import path from "node:path";

const SENTINEL = "<<<GHCi-DONE-7f3a2b>>>";
const DEFAULT_TIMEOUT_MS = 30_000;

export interface GhciResult {
  output: string;
  success: boolean;
}

export class GhciSession extends EventEmitter {
  private process: ChildProcess | null = null;
  private buffer: string = "";
  private pendingResolve: ((result: GhciResult) => void) | null = null;
  private pendingReject: ((error: Error) => void) | null = null;
  private pendingTimer: NodeJS.Timeout | null = null;
  private ready: boolean = false;
  private projectDir: string;
  private ghcupBin: string;

  constructor(projectDir: string) {
    super();
    this.projectDir = projectDir;
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

    return new Promise<void>((resolve, reject) => {
      this.process = spawn("cabal", ["repl", "lib:haskell-rules-and-mcp"], {
        cwd: this.projectDir,
        env,
        stdio: ["pipe", "pipe", "pipe"],
      });

      let startupBuffer = "";
      let startupStderr = "";

      const onStartupData = (data: Buffer) => {
        startupBuffer += data.toString();
        // GHCi is ready when we see the prompt after loading
        if (startupBuffer.includes("Ok,") || startupBuffer.includes("Ok.") || startupBuffer.includes("Loaded GHCi")) {
          this.process!.stdout!.removeListener("data", onStartupData);
          this.process!.stderr!.removeListener("data", onStartupStderr);
          this.setupHandlers();
          this.initSession().then(() => {
            this.ready = true;
            resolve();
          }).catch(reject);
        }
      };

      const onStartupStderr = (data: Buffer) => {
        startupStderr += data.toString();
      };

      this.process.stdout!.on("data", onStartupData);
      this.process.stderr!.on("data", onStartupStderr);

      this.process.on("error", (err) => {
        this.ready = false;
        reject(new Error(`Failed to start GHCi: ${err.message}`));
      });

      this.process.on("exit", (code) => {
        this.ready = false;
        this.process = null;
        if (!this.ready) {
          reject(
            new Error(
              `GHCi exited during startup with code ${code}. stderr: ${startupStderr}`
            )
          );
        }
        this.emit("exit", code);
      });

      // Timeout for startup
      setTimeout(() => {
        if (!this.ready) {
          this.kill();
          reject(
            new Error(
              `GHCi startup timed out after 30s. stdout: ${startupBuffer}, stderr: ${startupStderr}`
            )
          );
        }
      }, 30_000);
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
      const output = this.buffer.substring(0, sentinelIndex).trim();
      this.buffer = this.buffer.substring(sentinelIndex + SENTINEL.length);
      const resolve = this.pendingResolve;
      this.clearPending();
      // Determine success: if output contains "error:" it's a failure
      const success = !output.toLowerCase().includes("error:");
      resolve({ output, success });
    }
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
          const output = this.buffer.substring(0, sentinelIndex).trim();
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
   * Execute a GHCi command and return the output.
   */
  async execute(
    command: string,
    timeoutMs: number = DEFAULT_TIMEOUT_MS
  ): Promise<GhciResult> {
    if (!this.process || !this.ready) {
      throw new Error("GHCi session not running. Call start() first.");
    }

    if (this.pendingResolve) {
      throw new Error("Another command is already in progress");
    }

    return new Promise<GhciResult>((resolve, reject) => {
      this.pendingResolve = resolve;
      this.pendingReject = reject;

      this.pendingTimer = setTimeout(() => {
        this.clearPending();
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
   * Reload modules first, then execute a command.
   * This ensures GHCi always sees the latest source code.
   */
  private async reloadThenExecute(command: string): Promise<GhciResult> {
    const reloadResult = await this.execute(":r");
    // If reload has errors, we still proceed with the command —
    // the user wants to know the type even if there are warnings,
    // and if there are errors the :t will fail with a clear message.
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
   * Load a single module.
   */
  async loadModule(modulePath: string): Promise<GhciResult> {
    return this.execute(`:l ${modulePath}`);
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
   * Reload all modules.
   */
  async reload(): Promise<GhciResult> {
    return this.execute(":r");
  }

  /**
   * Check if the session is alive.
   */
  isAlive(): boolean {
    return this.ready && this.process !== null;
  }

  /**
   * Kill the GHCi process.
   */
  kill(): void {
    if (this.process) {
      this.ready = false;
      this.process.kill("SIGTERM");
      this.process = null;
      this.clearPending();
    }
  }

  /**
   * Restart the session (kill + start).
   */
  async restart(): Promise<void> {
    this.kill();
    await this.start();
  }
}
