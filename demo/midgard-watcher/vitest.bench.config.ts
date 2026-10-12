import { midgardSourceEnvironments } from "@al-ft/midgard-test-support/vitest";
import { defineConfig } from "vitest/config";

// Benchmarks (plan §16.2 B6). Not part of `test`: they preload 10^5 journal
// rows and time single-row commits.
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
