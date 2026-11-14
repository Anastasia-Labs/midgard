import {
  blueprintStampGlobalSetup,
  midgardSourceEnvironments,
  rawSqlLoaderPlugin,
} from "@al-ft/midgard-test-support/vitest";
import { defineConfig } from "vitest/config";

// Wall-clock benchmarks. Not part of `test` and not run by CI: a replay over
// a hundred thousand ledger entries takes minutes, and its time depends on
// the machine and its load.
export default defineConfig({
  plugins: [rawSqlLoaderPlugin()],
  test: {
    globalSetup: [blueprintStampGlobalSetup],
    reporters: "verbose",
    include: ["bench/**/*.bench.test.ts"],
    testTimeout: 3_600_000,
    hookTimeout: 600_000,
    fileParallelism: false,
  },
  environments: midgardSourceEnvironments(),
});
