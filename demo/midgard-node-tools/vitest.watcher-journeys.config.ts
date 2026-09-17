import {
  isolatedForksPool,
  midgardSourceSsr,
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
  },
  ssr: midgardSourceSsr(),
});
