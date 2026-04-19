import { defineConfig } from "vitest/config";

export default defineConfig({
  test: {
    include: ["src/__tests__/integration/**/*.test.ts"],
    // First `cabal repl` on a cold GitHub runner has to resolve + compile
    // base, containers, QuickCheck from scratch — regularly hits 60-90s
    // before GHCi emits its first prompt. Locally on macOS this is 2s
    // because `~/.cabal/store` is warm. Bumped to match the e2e ceiling
    // so `beforeEach` GHCi spawns don't time out on cold CI.
    testTimeout: 90_000,
    hookTimeout: 120_000,
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
