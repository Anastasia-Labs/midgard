import { defineConfig } from "vitest/config";

/**
 * How many test files may run at once. Overridable with
 * `MIDGARD_FAULT_PROOF_FORKS`; anything that is not a positive integer falls
 * back to the default.
 */
const DEFAULT_MAX_FORKS = 6;

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
    // Fails a file that is approaching the wasm32 ceiling with a message that
    // names the cause, so a leaked-heap trap is never re-diagnosed as an
    // on-chain rejection. See tests/support/uplc-heap-guard.ts.
    setupFiles: ["./tests/support/uplc-heap-guard.ts"],
    // One fresh process per test FILE is a correctness requirement, not a
    // performance knob. `@lucid-evolution/uplc` grows wasm linear memory on
    // every `eval_phase_two_raw` (~1.5-3.7 MB) and never reclaims it, so a
    // long-lived worker eventually exhausts the ~4 GiB wasm32 ceiling and the
    // next evaluation surfaces as `EvaluatorError: unreachable` — a
    // WebAssembly abort wearing a validator rejection's clothes. That is why
    // 7c7162cb reverted `singleFork` and why `isolate` must stay `true`:
    // disabling it re-shares one heap across every emulator journey in the
    // run. See tests/support/uplc-heap-guard.ts.
    //
    // `maxForks` is the separate, purely-scheduling bound. 5b9982a8
    // serialized the suite outright for a 2-core CI runner; that is now
    // expressed as a cap rather than as `--no-file-parallelism`, so such a
    // runner pins `MIDGARD_FAULT_PROOF_FORKS=1` (or 2) instead of forcing
    // every machine down to one file at a time.
    //
    // If a run dies under the 12G memory scope, LOWER `maxForks` — each fork
    // carries its own multi-GB uplc heap. Raising the memory ceiling only
    // moves the wall.
    pool: "forks",
    poolOptions: {
      forks: {
        isolate: true,
        singleFork: false,
        minForks: 1,
        maxForks,
      },
    },
  },
});
