import { midgardSourceEnvironments } from "@al-ft/midgard-test-support/vitest";
import { defineConfig } from "vitest/config";

// Benchmarks (plan §16.2 B3, B8). Not part of `test`: they preload 10^6
// outputs or replay tens of thousands of blocks.
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
