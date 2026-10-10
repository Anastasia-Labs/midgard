import { defineConfig } from "vitest/config";

export default defineConfig({
  test: {
    include: ["./tests/**/*.test.ts"],
    // Builds the sidecar and the mock node once per run.
    globalSetup: ["./tests/global-setup.ts"],
    testTimeout: 60_000,
    hookTimeout: 120_000,
  },
});
