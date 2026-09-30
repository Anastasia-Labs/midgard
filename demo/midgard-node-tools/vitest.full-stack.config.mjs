import {
  midgardSourceSsr,
  rawSqlLoaderPlugin,
} from "@al-ft/midgard-test-support/vitest";
import { defineConfig } from "vitest/config";

// File journal and pure verifier tests need no database shards or native owner.
export default defineConfig({
  plugins: [rawSqlLoaderPlugin()],
  ssr: midgardSourceSsr(),
  test: {
    include: ["tests/full-stack*.test.ts"],
    environment: "node",
    testTimeout: 10_000,
  },
});
