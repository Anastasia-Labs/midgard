import {
  blueprintStampGlobalSetup,
  interactiveEmulatorBlueprint,
  interactiveEmulatorPlugin,
  interactiveEmulatorSetup,
  isolatedForksPool,
  midgardSourceSsr,
  rawSqlLoaderPlugin,
} from "@al-ft/midgard-test-support/vitest";
import { defineConfig } from "vitest/config";

import { EmulatorSequencer } from "./tests/support/emulator-sequencer.js";

/**
 * How many test files may run at once. Overridable with
 * `MIDGARD_FAULT_PROOF_FORKS`; anything that is not a positive integer falls
 * back to the default.
 *
 * The 434-case emulator stage on a 32-CPU/61-GiB host took 23.1 minutes at
 * four forks and 21.2 at eight, with peak aggregate RSS of 6.5 and 10.1 GiB.
 * See docs/fault-proofs/testing-status.md for scope and measurements.
 *
 * On the same host the whole suite (392 files, 4,466 cases) took 20.5
 * minutes at eight forks on 2026-09-21. Its tail is one case, the 1,295-asset
 * depth-64 deep-deposit transition trace, which alone spends about eleven
 * minutes inside genuine UPLC evaluation of near-budget scripts; more forks
 * cannot shorten that, only a smaller shape or splitting its hops can.
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

// Interactive journeys need enough protocol time to finish. Their isolated
// project uses a separately stamped blueprint; emulator clock jumps are instant.
const interactiveTests = [
  "./tests/*validation-dispute*.test.ts",
  "./tests/validation-trace-dispute-installed-lifecycle.test.ts",
  "./tests/cek-*-lifecycle.test.ts",
  "./tests/value-and-mint-asset-yield-lifecycle.test.ts",
  "./tests/ledger-output-value-permutation-lifecycle.test.ts",
  "./tests/forced-submission-lifecycle.test.ts",
  "./tests/submit-init-emulator-cek-value-and-mint.test.ts",
  "./tests/submit-init-emulator-value-and-mint.test.ts",
  "./tests/submit-init-emulator-min-ada.test.ts",
  "./tests/submit-init-emulator-option-b-*.test.ts",
  "./tests/submit-init-emulator-route-freedom-*.test.ts",
  "./tests/submit-init-emulator-soundness*.test.ts",
  "./tests/submit-init-emulator-transition-trace-final-deep-deposit.test.ts",
];

export default defineConfig({
  plugins: [rawSqlLoaderPlugin()],
  test: {
    // Refuses the run when onchain/aiken/plutus.json is stale.
    workspace: [
      {
        extends: true,
        test: {
          name: "testing-profile",
          include: ["./tests/**/*.test.{ts,tsx}"],
          exclude: interactiveTests,
          globalSetup: [blueprintStampGlobalSetup],
        },
      },
      {
        extends: true,
        plugins: [interactiveEmulatorPlugin()],
        test: {
          name: "interactive-emulator",
          include: interactiveTests,
          globalSetup: [interactiveEmulatorSetup],
          env: { MIDGARD_REAL_BLUEPRINT_PATH: interactiveEmulatorBlueprint },
        },
      },
    ],
    reporters: "verbose",
    sequence: { sequencer: EmulatorSequencer },
    // The one-process-per-file requirement, and why `isolate` must stay
    // `true`, are stated once in `isolatedForksPool`; 7c7162cb reverting
    // `singleFork` here is the same story.
    //
    // This suite's own choice is only the scheduling cap. 5b9982a8 serialized
    // it outright for a 2-core CI runner; that is now expressed as a cap
    // rather than as `--no-file-parallelism`, so such a runner pins
    // `MIDGARD_FAULT_PROOF_FORKS=1` (or 2) instead of forcing every machine
    // down to one file at a time.
    ...isolatedForksPool({ maxForks }),
  },
  ssr: midgardSourceSsr(),
});
