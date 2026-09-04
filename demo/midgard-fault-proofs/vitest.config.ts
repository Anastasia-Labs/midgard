import {
  isolatedForksPool,
  midgardSourceSsr,
} from "@al-ft/midgard-test-support/vitest";
import { defineConfig } from "vitest/config";

/**
 * How many test files may run at once. Overridable with
 * `MIDGARD_FAULT_PROOF_FORKS`; anything that is not a positive integer falls
 * back to the default.
 *
 * 8 was measured on a 32-core box at ~1 GB resident per fork: 292 s at 6
 * forks, 253 s at 8, and 16 forks only added CPU contention (tests' summed CPU
 * time rose 1.7× for a 6% wall-clock gain).
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
