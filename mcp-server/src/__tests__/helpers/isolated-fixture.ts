/**
 * Test fixture isolation helper.
 *
 * The `test-project`, `hm-project`, and `parser-project` fixtures under
 * `src/__tests__/fixtures/` are shared by ~40 tests each. As soon as the
 * vitest configs enable `fileParallelism: true`, multiple test files start
 * racing against the same fixture directory — writing to the same
 * `TempExports.hs`, `.haskell-flows/properties.json`, `<name>.cabal`, or
 * `dist-newstyle/`. That corrupts fixture state and causes flaky runs.
 *
 * This helper creates a per-describe-block copy of a fixture in a fresh
 * tmpdir and returns the path + a cleanup function. Each test file that
 * MUTATES fixture state should call `setupIsolatedFixture(name)` inside its
 * `beforeAll` and `cleanup()` inside `afterAll`.
 *
 * Read-only tests (most of them) don't need this — they can keep using the
 * shared fixture directly.
 */
import { cp, mkdtemp, rm } from "node:fs/promises";
import os from "node:os";
import path from "node:path";

const FIXTURES_DIR = path.resolve(import.meta.dirname, "..", "fixtures");

export interface IsolatedFixture {
  /** Absolute path to the per-test copy of the fixture. */
  dir: string;
  /** Delete the tmp copy. Call from `afterAll`. Idempotent and best-effort. */
  cleanup: () => Promise<void>;
}

/**
 * Copy one of the known fixtures (`test-project`, `hm-project`, `parser-project`)
 * into a fresh tmpdir and return a handle with cleanup.
 *
 * The tmpdir name includes a prefix so parallel workers never collide on the
 * same directory even when the same fixture is used by many test files.
 *
 * Security: the copy is into `os.tmpdir()` with default user permissions.
 * The fixture contents are test sources that already live in the repo — no
 * secrets are duplicated, and the cleanup in `afterAll` removes the tmp
 * directory so it doesn't accumulate on developer machines.
 */
export async function setupIsolatedFixture(
  fixtureName: "test-project" | "hm-project" | "parser-project",
  prefix?: string
): Promise<IsolatedFixture> {
  const source = path.join(FIXTURES_DIR, fixtureName);
  const tmpPrefix = `${prefix ?? "iso"}-${fixtureName}-`;
  const dir = await mkdtemp(path.join(os.tmpdir(), tmpPrefix));

  // `cp` with recursive: true preserves file mtimes and directory structure.
  // We copy EVERYTHING including `dist-newstyle/` so subsequent cabal
  // invocations reuse the pre-built artefacts — the big win vs re-compiling
  // from scratch in every worker.
  await cp(source, dir, { recursive: true, force: true });

  return {
    dir,
    cleanup: async () => {
      try {
        await rm(dir, { recursive: true, force: true });
      } catch {
        /* best-effort — don't fail the test on cleanup errors */
      }
    },
  };
}
