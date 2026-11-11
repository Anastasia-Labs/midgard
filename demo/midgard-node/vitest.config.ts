import { fileURLToPath } from "node:url";

import { durationShards } from "@al-ft/midgard-test-support/duration-shards";
import {
  blueprintStampGlobalSetup,
  interactiveEmulatorBlueprint,
  interactiveEmulatorPlugin,
  interactiveEmulatorSetup,
  isolatedForksPool,
  midgardSourceEnvironments,
  rawSqlLoaderPlugin,
  runSharedFixturesGlobalSetup,
  workspaceBundleProjects,
} from "@al-ft/midgard-test-support/vitest";
import { configDefaults, defineConfig } from "vitest/config";

import { parsePositiveInteger, testMaxForks } from "./tests/test-env.js";

/**
 * The files the interactive-emulator project runs on the 72 h emulator
 * profile: every suite that exercises the execution dispute (00000006), which
 * the 900 s testing profiles exclude because their maturity cannot hold it.
 */
const interactiveTests = [
  "./tests/validation-trace-defense-end-to-end-emulator.test.ts",
  "./tests/validation-trace-defense-fiber-emulator.test.ts",
  "./tests/validation-trace-defense-funding-emulator.test.ts",
  "./tests/validation-trace-defense-max-shape-emulator.test.ts",
  "./tests/validation-trace-defense-race-emulator.test.ts",
  "./tests/validation-trace-defense-references-emulator.test.ts",
  "./tests/validation-trace-defense-replacement-emulator.test.ts",
  "./tests/validation-trace-defense-restart-emulator.test.ts",
  "./tests/validation-trace-defense-survey-emulator.test.ts",
];

// A committed `bail` makes the suite's cost and its result set unreproducible:
// with `bail: 3` a run with three early failures silently skips most of the
// files, so a green-looking summary and a red one describe different suites. It
// is opt-in per run instead — `MIDGARD_NODE_TEST_BAIL=3 pnpm test`.
const bail = parsePositiveInteger(process.env.MIDGARD_NODE_TEST_BAIL);

export default defineConfig({
  plugins: [rawSqlLoaderPlugin()],
  test: {
    // The one-process-per-file requirement is stated once in
    // `isolatedForksPool`. This suite's own choice is the scheduling cap:
    // every file runs on its worker's own database shard, so file parallelism
    // is safe. A 2-core runner sets `MIDGARD_NODE_TEST_FORKS=1` rather than
    // forcing every machine down to one file at a time.
    ...isolatedForksPool({ maxForks: testMaxForks() }),
    // Pins each file to its worker's shard before the file loads, in every
    // project (tests/test-env.ts states the invariant).
    setupFiles: ["./tests/pin-database-shard.ts"],
    // Creates and migrates one database per worker shard before any file runs,
    // after refusing a stale onchain/aiken/plutus.json, and gives the run its
    // shared fixture directory.
    globalSetup: [
      blueprintStampGlobalSetup,
      runSharedFixturesGlobalSetup,
      "./tests/global-setup.ts",
    ],
    // Packs Node CI's `--shard=i/3` by the CI seconds each file took and starts
    // the longest files first: a few database and emulator files run two to
    // three minutes each while most files take seconds, so a giant that starts
    // last sets a fork's wall time by itself.
    // The table is keyed by package-relative path, so a file weighs the same in
    // `midgard-node` and `midgard-node:source`. Each CI shard is its own job
    // with its own Postgres service, so the per-worker database scheme above
    // holds inside every shard unchanged. The table refreshes itself from CI;
    // see `@al-ft/midgard-test-support/duration-shards`.
    ...durationShards({
      tablePath: fileURLToPath(
        new URL("./tests/support/ci-file-durations.json", import.meta.url),
      ),
      reporters: [["default", { summary: false }]],
    }),
    // Workspace packages load from a per-run source bundle; files that need
    // them module-by-module run in the `midgard-node:source` project. The
    // projects inherit everything else here.
    projects: [
      ...workspaceBundleProjects(
        {
          extends: true,
          test: {
            name: "midgard-node",
            // Vitest 3's filter resolution does not reliably match the
            // eight-way extension brace used here previously, so keep the
            // overwhelmingly common TypeScript lane explicit. Otherwise a
            // focused `*.test.ts` invocation can report "No test files found"
            // and never exercise a release gate.
            include: [
              "./tests/**/*.test.ts",
              "./tests/**/*.test.{js,mjs,cjs,mts,cts,jsx,tsx}",
            ],
            // The explicit public framing config requires retained genuine release inputs.
            exclude: [
              ...configDefaults.exclude,
              "./tests/public-http-release-framing.acceptance.test.ts",
              ...interactiveTests,
            ],
          },
        },
        { packageDirectory: fileURLToPath(new URL(".", import.meta.url)) },
      ),
      // Not bundled: its plugin rewrites a midgard-core module.
      {
        extends: true,
        plugins: [interactiveEmulatorPlugin()],
        test: {
          name: "interactive-emulator",
          include: interactiveTests,
          globalSetup: [interactiveEmulatorSetup],
          env: {
            // The suite's env defaults name the testing profile; this project
            // compiles the emulator profile instead.
            MIDGARD_DEPLOYMENT_PROFILE: "preprod-emulator-testing",
            MIDGARD_REAL_BLUEPRINT_PATH: interactiveEmulatorBlueprint,
          },
        },
      },
    ],
    testTimeout: 420_000,
    // Suites drop their test databases in `afterAll`; each `DROP DATABASE …
    // WITH (FORCE)` can outlast the 10 s hook default on a loaded CI runner.
    hookTimeout: 120_000,
    ...(bail === undefined ? {} : { bail }),
    environment: "node",
  },
  environments: midgardSourceEnvironments(),
  esbuild: {
    target: "es2020",
  },
});
