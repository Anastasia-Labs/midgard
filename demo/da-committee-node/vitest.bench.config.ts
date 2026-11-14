import { midgardSourceEnvironments } from "@al-ft/midgard-test-support/vitest";
import { defineConfig } from "vitest/config";

// Benchmarks (plan §16.2 B5). Not part of `test`: they preload a
// thousand-node state queue on both store dialects.
export default defineConfig({
  test: {
    environment: "node",
    include: ["bench/**/*.bench.test.ts"],
    testTimeout: 3_600_000,
    hookTimeout: 600_000,
    fileParallelism: false,
  },
  environments: midgardSourceEnvironments(),
});
