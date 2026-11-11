import {
  isolatedForksPool,
  midgardSourceEnvironments,
  rawSqlLoaderPlugin,
} from "@al-ft/midgard-test-support/vitest";
import { defineConfig } from "vitest/config";

export default defineConfig({
  plugins: [rawSqlLoaderPlugin()],
  test: {
    ...isolatedForksPool({ maxForks: 1 }),
    include: ["devnet/watcher-journeys/**/*.test.ts"],
    testTimeout: 3_600_000,
    environment: "node",
    // A live journey reports each stage as it starts. Left to choose, Vitest
    // may pick its minimal reporter from the environment it detects, and
    // that reporter holds a running test's console output back until the
    // test fails: a run in progress would show nothing past its banner.
    reporters: ["default"],
  },
  environments: midgardSourceEnvironments(),
});
