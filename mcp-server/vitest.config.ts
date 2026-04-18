import { defineConfig } from "vitest/config";

export default defineConfig({
  test: {
    include: ["src/__tests__/*.test.ts"],
    exclude: [
      "src/__tests__/integration/**",
      "src/__tests__/e2e/**",
      "node_modules/**",
    ],
    globals: true,
    testTimeout: 30000,
    // Unit tests are pure code (parsers, coercers, law engines). No GHCi,
    // no dist-newstyle contention. Let vitest parallelize across all CPUs.
    fileParallelism: true,
    maxWorkers: 12,
    minWorkers: 1,
    // `sequence.concurrent` lets tests inside a single file run in parallel
    // when they are declared via `it.concurrent`. We leave the default off
    // (=false) so existing sequential assumptions inside a file keep holding;
    // the big win comes from file-level parallelism above.
    coverage: {
      provider: 'v8',
      reporter: ['text', 'json', 'html'],
      exclude: [
        'src/__tests__/**',
        'src/scripts/**',
        'dist/**',
        'node_modules/**',
        '**/*.d.ts',
        '**/*.config.ts',
      ],
      thresholds: {
        lines: 80,
        functions: 80,
        branches: 75,
        statements: 80,
      },
    },
  },
});
