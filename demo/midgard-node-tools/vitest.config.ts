import { fileURLToPath } from "node:url";

import {
  isolatedForksPool,
  midgardSourceEnvironments,
  preprodPublicBlueprint,
  preprodPublicPlugin,
  preprodPublicSetup,
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

// Sign a public release and open it as public listen does, or run the node
// command line a public build compiles, so they compile the preprod-public
// profile against a blueprint built for it.
const preprodPublicTests = [
  "./tests/sign-deployment-release.public.test.ts",
  "./tests/full-stack-public-release.test.ts",
  "./tests/public-submit-deposit.test.ts",
];
// Load a preprod-public stack configuration, which needs only the compiled
// profile, with the fixture's wallets on its operator allow-list.
const preprodPublicStackTests = ["./tests/full-stack-public-config.test.ts"];

/**
 * Adds the stack configuration fixture's operator and merge wallets (seeds
 * from entropy 0 and 1, full-stack-config.fixtures.ts) to the preprod-public
 * operator allow-list, before the generated module freezes its profiles, so
 * a fixture can pass every public input check.
 */
const fixtureOperatorsPlugin = () => ({
  name: "midgard-fixture-public-operators",
  transform(code: string, id: string) {
    if (!id.endsWith("/demo/midgard-core/src/generated-deployment-profiles.ts"))
      return null;
    const anchor = "for (const profile of Object.values(DEPLOYMENT_PROFILES))";
    if (!code.includes(anchor))
      throw new Error("Missing the generated profile freeze");
    return {
      code: code.replace(
        anchor,
        `DEPLOYMENT_PROFILES["preprod-public"].allow_lists.operators.push(${JSON.stringify(
          [
            "0fdc780023d8be7c9ff3a6bdc0d8d3b263bd0cc12448c40948efbf42",
            "fa6407a107929f517bbd7ac465b7654809bc5b0f55c4bc7dac09b5e0",
          ],
        ).slice(1, -1)});\n${anchor}`,
      ),
      map: null,
    };
  },
});

export default defineConfig({
  plugins: [rawSqlLoaderPlugin()],
  test: {
    ...isolatedForksPool({ maxForks: testMaxForks() }),
    reporters: [["default", { summary: false }]],
    // Workspace packages load from a per-run source bundle; files that need
    // them module-by-module run in `midgard-node-tools:source`.
    projects: [
      ...workspaceBundleProjects(
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
              "./tests/phase4-journal-kill-recovery-summary-verifier.test.mjs",
              ...preprodPublicTests,
              ...preprodPublicStackTests,
            ],
          },
        },
        { packageDirectory: fileURLToPath(new URL(".", import.meta.url)) },
      ),
      // Not bundled: its plugin rewrites a midgard-core module.
      {
        extends: true,
        plugins: [preprodPublicPlugin()],
        test: {
          name: "preprod-public-release",
          include: preprodPublicTests,
          globalSetup: [preprodPublicSetup],
          env: {
            MIDGARD_DEPLOYMENT_PROFILE: "preprod-public",
            MIDGARD_REAL_BLUEPRINT_PATH: preprodPublicBlueprint,
          },
        },
      },
      // Not bundled: its plugins rewrite a midgard-core module.
      {
        extends: true,
        plugins: [preprodPublicPlugin(), fixtureOperatorsPlugin()],
        test: {
          name: "preprod-public-stack",
          include: preprodPublicStackTests,
          env: { MIDGARD_DEPLOYMENT_PROFILE: "preprod-public" },
        },
      },
    ],
    testTimeout: 420_000,
    ...(bail === undefined ? {} : { bail }),
    environment: "node",
  },
  environments: midgardSourceEnvironments(),
  esbuild: {
    target: "es2020",
  },
});
