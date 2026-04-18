import { defineConfig } from "vitest/config";

export default defineConfig({
  test: {
    include: ["src/__tests__/integration/**/*.test.ts"],
    testTimeout: 30_000,
    hookTimeout: 60_000,
    // `forks` gives each test file its own process → safe for GHCi sessions
    // and isolated workflow-state mutations.
    pool: "forks",
    // Parallelize ACROSS files. Cap at 4 workers because multiple simultaneous
    // cabal/GHCi processes contend for `dist-newstyle/`; 4 is the empirical
    // sweet spot on typical dev boxes (see plan-file benchmark notes).
    fileParallelism: true,
    maxWorkers: 10,
    minWorkers: 1,
  },
});
