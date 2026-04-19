import { defineConfig } from "vitest/config";

export default defineConfig({
  test: {
    include: ["src/__tests__/e2e/**/*.test.ts"],
    // 120s per test absorbs the first-time cabal build on a cold GitHub
    // runner (no cabal store cache between jobs). On macOS with a warm
    // store the same tests finish in 2-3s; the higher ceiling only costs
    // CI wall clock on the pathological "all 100 tests hit a cold build"
    // day — that has already been traded for determinism.
    testTimeout: 120_000,
    hookTimeout: 120_000,
    // Each e2e file spawns a full MCP server (node dist/index.js) plus cabal
    // and GHCi. Use `forks` for isolation between files.
    pool: "forks",
    // Parallelize across e2e files, but conservatively. Each worker owns a
    // full Node + GHCi + cabal stack, which is heavier than integration.
    // 2 workers is the conservative default; fixture isolation (Fase B of
    // the parallelization plan) prevents cross-worker writes to the shared
    // test-project fixture.
    fileParallelism: true,
    maxWorkers: 6,
    minWorkers: 1,
  },
});
