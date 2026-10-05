import { fileURLToPath } from "node:url";

import {
  isolatedForksPool,
  midgardSourceSsr,
  rawSqlLoaderPlugin,
  workspaceBundleProjects,
} from "@al-ft/midgard-test-support/vitest";
import { configDefaults, defineConfig } from "vitest/config";

// The shard vocabulary is midgard-node's: tooling tests reuse its per-worker
// Postgres sharding and its migration runner. This is a config-time import, so
// it reaches the file directly; test-time code imports the same helpers by
// package name (`midgard-node/tests/test-env`).
import {
  defaultTestDatabasePrefix,
  parsePositiveInteger,
  testMaxForks,
} from "../midgard-node/tests/test-env.js";

// Own database shards, so a tooling run can never share a database with a
// concurrently running midgard-node suite (tests/test-env.ts explains why two
// database-touching files must never share one). A linked git worktree gets a
// worktree-specific family; the main checkout keeps `midgard_tools_test`.
process.env.MIDGARD_TEST_DATABASE_PREFIX ??=
  defaultTestDatabasePrefix("midgard_tools_test");

const bail = parsePositiveInteger(process.env.MIDGARD_NODE_TEST_BAIL);

export default defineConfig({
  plugins: [rawSqlLoaderPlugin()],
  test: {
    ...isolatedForksPool({ maxForks: testMaxForks() }),
    reporters: [["default", { summary: false }]],
    // Workspace packages load from a per-run source bundle; files that need
    // them module-by-module run in `midgard-node-tools:source`.
    workspace: workspaceBundleProjects(
      {
        extends: true,
        test: {
          name: "midgard-node-tools",
          // On the project, so the bundle serves the midgard-node helpers it
          // imports (see `workspaceBundleProjects`).
          globalSetup: ["./tests/global-setup.ts"],
          include: ["./tests/**/*.test.ts"],
          exclude: [
            ...configDefaults.exclude,
            "./tests/phase4-pipelined-process-summary-verifier.test.mjs",
          ],
        },
      },
      { packageDirectory: fileURLToPath(new URL(".", import.meta.url)) },
    ),
    testTimeout: 420_000,
    ...(bail === undefined ? {} : { bail }),
    environment: "node",
  },
  ssr: midgardSourceSsr(),
  esbuild: {
    target: "es2020",
  },
});
