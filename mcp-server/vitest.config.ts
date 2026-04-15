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
    // Run tests with limited concurrency to reduce dist-newstyle conflicts
    maxConcurrency: 1,
    fileParallelism: false,
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
