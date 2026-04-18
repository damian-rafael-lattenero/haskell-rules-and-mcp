/**
 * Integration tests for ghci_hls. Exercises the three actions against a real
 * fixture project so hover and diagnostics have something to attach to.
 *
 * HLS is slow to start (seconds) and optional. Each test has an explicit
 * timeout; the whole suite skips cleanly when HLS is not available. We
 * validate shape, not specific payload text (HLS output varies across
 * versions).
 */
import { describe, it, expect, beforeAll, afterAll } from "vitest";
import { execSync, execFileSync } from "node:child_process";
import path from "node:path";
import { handleHls } from "../../tools/hls.js";
import { setupIsolatedFixture, type IsolatedFixture } from "../helpers/isolated-fixture.js";

const GHCUP_BIN = path.join(process.env.HOME ?? "", ".ghcup", "bin");
const TEST_PATH = `${GHCUP_BIN}:${process.env.PATH}`;

const GHC_AVAILABLE = (() => {
  try {
    execSync("ghc --version", { stdio: "pipe", env: { ...process.env, PATH: TEST_PATH } });
    return true;
  } catch {
    return false;
  }
})();

const HLS_AVAILABLE = (() => {
  try {
    execFileSync("haskell-language-server-wrapper", ["--version"], {
      stdio: "pipe",
      env: { ...process.env, PATH: TEST_PATH },
    });
    return true;
  } catch {
    return false;
  }
})();

describe.runIf(GHC_AVAILABLE && HLS_AVAILABLE)("ghci_hls integration", () => {
  let fixture: IsolatedFixture;
  let FIXTURE_DIR: string;

  beforeAll(async () => {
    fixture = await setupIsolatedFixture("test-project", "hls-integ");
    FIXTURE_DIR = fixture.dir;
  }, 60_000);

  afterAll(async () => {
    await fixture.cleanup();
  });

  it("action='available' reports HLS present with a binaryPath", async () => {
    const result = JSON.parse(
      await handleHls(FIXTURE_DIR, { action: "available" })
    );
    expect(result.available).toBe(true);
    expect(typeof result.binaryPath).toBe("string");
    expect(result.source).toMatch(/host|bundled|installed/);
  }, 60_000);

  it("action='hover' returns a hover envelope (success or unreachable)", async () => {
    const result = JSON.parse(
      await handleHls(FIXTURE_DIR, {
        action: "hover",
        module_path: "src/Lib.hs",
        line: 1,
        character: 0,
      })
    );
    // HLS can answer "no hover at this position" (success:true, contents empty)
    // or "session initialization failed" (success:false). Shape checks only.
    expect(typeof result.success).toBe("boolean");
    if (result.success) {
      expect(result).toHaveProperty("contents");
    } else {
      expect(typeof result.error).toBe("string");
    }
  }, 120_000);

  it("action='diagnostics' returns an array (possibly empty) without throwing", async () => {
    const result = JSON.parse(
      await handleHls(FIXTURE_DIR, {
        action: "diagnostics",
        module_path: "src/Lib.hs",
      })
    );
    expect(typeof result.success).toBe("boolean");
    if (result.success) {
      expect(Array.isArray(result.diagnostics)).toBe(true);
    } else {
      expect(typeof result.error).toBe("string");
    }
  }, 120_000);
});
