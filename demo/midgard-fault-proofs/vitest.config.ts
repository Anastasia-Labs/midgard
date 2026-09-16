import {
  isolatedForksPool,
  midgardSourceSsr,
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

export default defineConfig({
  test: {
    reporters: "verbose",
    include: ["./tests/**/*.test.{ts,tsx}"],
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
