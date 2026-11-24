import { fileURLToPath } from "node:url";

import { durationShards } from "@al-ft/midgard-test-support/duration-shards";
import {
  blueprintStampGlobalSetup,
  curatedEmulatorBlueprint,
  curatedEmulatorPlugin,
  curatedEmulatorSetup,
  interactiveEmulatorBlueprint,
  interactiveEmulatorPlugin,
  interactiveEmulatorSetup,
  interactiveEmulatorSourceTransform,
  isolatedForksPool,
  midgardSourceEnvironments,
  rawSqlLoaderPlugin,
  withDurationTier,
  workspaceBundleProjects,
} from "@al-ft/midgard-test-support/vitest";
import { defineConfig } from "vitest/config";

import { curatedTests, interactiveTests } from "./vitest.interactive-tests.mjs";

/**
 * How many test files may run at once. Overridable with
 * `MIDGARD_FAULT_PROOF_FORKS`; anything that is not a positive integer falls
 * back to the default. Each fork is its own process, so lower it on smaller
 * machines. See docs/fault-proofs/testing-status.md for the gate commands.
 */
const DEFAULT_MAX_FORKS = 8;

const parseMaxForks = (raw: string | undefined): number => {
  if (raw === undefined) {
    return DEFAULT_MAX_FORKS;
  }
  const parsed = Number(raw.trim());
  if (!Number.isInteger(parsed) || parsed < 1) {
    return DEFAULT_MAX_FORKS;
  }
  return parsed;
};

const maxForks = parseMaxForks(process.env.MIDGARD_FAULT_PROOF_FORKS);

/** CI seconds per file: shards are packed by them, and they split the
 * suite into its fast and slow tiers (`MIDGARD_TEST_TIER=fast|slow`; unset,
 * as in CI, runs everything). */
const durationTablePath = fileURLToPath(
  new URL("./tests/support/ci-file-durations.json", import.meta.url),
);

const config = defineConfig({
  plugins: [rawSqlLoaderPlugin()],
  test: {
    // Every project extends this config, so every emulator submission in the
    // suite is held to the Van Rossem limits and the publication target.
    setupFiles: ["./tests/support/emulator/van-rossem-submit-guard.ts"],
    // Refuses the run when onchain/aiken/plutus.json is stale.
    projects: [
      // Workspace packages load from a per-run source bundle; files that need
      // them module-by-module run in `testing-profile:source`.
      ...workspaceBundleProjects(
        {
          extends: true,
          test: {
            name: "testing-profile",
            include: ["./tests/**/*.test.{ts,tsx}"],
            exclude: [...interactiveTests, ...curatedTests],
            globalSetup: [blueprintStampGlobalSetup],
          },
        },
        { packageDirectory: fileURLToPath(new URL(".", import.meta.url)) },
      ),
      // Bundled too: its plugin selects the emulator profile in a midgard-core
      // module, and the bundle applies the same rewrite to the source it
      // bundles. Files that need module-by-module loading run in
      // `interactive-emulator:source`.
      ...workspaceBundleProjects(
        {
          extends: true,
          plugins: [interactiveEmulatorPlugin()],
          test: {
            name: "interactive-emulator",
            include: interactiveTests,
            globalSetup: [interactiveEmulatorSetup],
            env: {
              // scripts/run-traced-refusals.mjs swaps in a blueprint whose
              // pinned validators carry their traces.
              MIDGARD_REAL_BLUEPRINT_PATH:
                process.env.MIDGARD_TRACED_INTERACTIVE_BLUEPRINT ??
                interactiveEmulatorBlueprint,
            },
          },
        },
        {
          packageDirectory: fileURLToPath(new URL(".", import.meta.url)),
          sourceTransforms: [interactiveEmulatorSourceTransform],
        },
      ),
      // Not bundled: wrapping it would need its plugin's rewrite as a source
      // transform, as the interactive project passes.
      {
        extends: true,
        plugins: [curatedEmulatorPlugin()],
        test: {
          name: "curated-emulator",
          include: curatedTests,
          globalSetup: [curatedEmulatorSetup],
          env: { MIDGARD_REAL_BLUEPRINT_PATH: curatedEmulatorBlueprint },
        },
      },
    ],
    // Packs `--shard=i/n` by the CI seconds each file took and starts the
    // longest files first: a few emulator files run for many minutes while
    // most take seconds, Vitest's default hash sharding put 75 minutes of them
    // in one CI shard and 41 in another, and a whale that starts late sets a
    // fork's wall time by itself. The table refreshes itself from CI; see
    // `@al-ft/midgard-test-support/duration-shards`.
    ...durationShards({
      tablePath: durationTablePath,
      reporters: ["verbose"],
    }),
    // The one-process-per-file requirement, and why `isolate` must stay
    // `true`, are stated once in `isolatedForksPool`; 7c7162cb, which moved
    // this suite off one shared fork for every file, is the same story.
    //
    // This suite's own choice is only the scheduling cap. 5b9982a8 serialized
    // it outright for a 2-core CI runner; that is now expressed as a cap
    // rather than as `--no-file-parallelism`, so such a runner pins
    // `MIDGARD_FAULT_PROOF_FORKS=1` (or 2) instead of forcing every machine
    // down to one file at a time.
    ...isolatedForksPool({ maxForks }),
  },
  environments: midgardSourceEnvironments(),
});

export default withDurationTier(config, {
  tablePath: durationTablePath,
  packageDirectory: fileURLToPath(new URL(".", import.meta.url)),
});
