import {
  blueprintStampGlobalSetup,
  midgardSourceEnvironments,
} from "@al-ft/midgard-test-support/vitest";
import { defineConfig } from "vitest/config";

// Wall-clock benchmarks. Not part of `test`: their bounds are milliseconds,
// which depend on the machine and its load. Node CI runs them in a step that
// does not fail the run.
export default defineConfig({
  test: {
    globalSetup: [blueprintStampGlobalSetup],
    reporters: "verbose",
    include: ["bench/**/*.bench.test.ts"],
    fileParallelism: false,
  },
  environments: midgardSourceEnvironments(),
});
