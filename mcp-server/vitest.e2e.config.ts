import { defineConfig } from "vitest/config";

export default defineConfig({
  test: {
    include: ["src/__tests__/e2e/**/*.test.ts"],
    testTimeout: 30_000,
    hookTimeout: 60_000,
    pool: "forks",
    fileParallelism: false,
  },
});
