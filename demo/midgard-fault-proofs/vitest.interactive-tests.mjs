/**
 * The test files the interactive-emulator Vitest project runs; the
 * testing-profile project runs every other file. Interactive journeys need
 * enough protocol time to finish, so their project uses a separately stamped
 * blueprint whose emulator clock jumps are instant.
 *
 * Shared by vitest.config.ts and scripts/run-traced-refusals.mjs, which runs
 * each pinned negative against its own project's blueprint.
 */
export const interactiveTests = [
  "./tests/submit-init-emulator-transition-trace-final-deep-deposit.test.ts",
  "./tests/validation-trace-dispute-bisection.test.ts",
  "./tests/validation-trace-dispute-conviction.test.ts",
  "./tests/validation-trace-dispute-one-step-context-assemble.test.ts",
  "./tests/validation-trace-dispute-one-step-context-finalize.test.ts",
  "./tests/validation-trace-dispute-one-step-context-item-107.test.ts",
  "./tests/validation-trace-dispute-one-step-context-item-108.test.ts",
  "./tests/validation-trace-dispute-one-step-context-item-109.test.ts",
  "./tests/validation-trace-dispute-one-step-context-item-110.test.ts",
  "./tests/validation-trace-dispute-one-step-context-item-111.test.ts",
  "./tests/validation-trace-dispute-one-step-context-item-95.test.ts",
  "./tests/validation-trace-dispute-one-step-context-item-96.test.ts",
  "./tests/validation-trace-dispute-one-step-context-mint-init.test.ts",
  "./tests/validation-trace-dispute-one-step-context-observer.test.ts",
  "./tests/validation-trace-dispute-one-step-context-output.test.ts",
  "./tests/validation-trace-dispute-one-step-context-output-101.test.ts",
  "./tests/validation-trace-dispute-one-step-context-redeemer-begin.test.ts",
  "./tests/validation-trace-dispute-one-step-context-redeemer-select.test.ts",
  "./tests/validation-trace-dispute-one-step-context-reference.test.ts",
  "./tests/validation-trace-dispute-one-step-context-seed.test.ts",
  "./tests/validation-trace-dispute-one-step-context-signer.test.ts",
  "./tests/validation-trace-dispute-one-step-context-signer-103.test.ts",
  "./tests/validation-trace-dispute-one-step-context-spend.test.ts",
  "./tests/validation-trace-dispute-one-step-context-spend-99.test.ts",
  "./tests/validation-trace-dispute-one-step-context-tx-info.test.ts",
  "./tests/validation-trace-dispute-one-step-conviction.test.ts",
  "./tests/validation-trace-dispute-one-step-defender-won-award.test.ts",
  "./tests/validation-trace-dispute-one-step-defender-won.test.ts",
  "./tests/validation-trace-dispute-one-step-move-walk.test.ts",
  "./tests/validation-trace-dispute-one-step-selection-drive.test.ts",
  "./tests/validation-trace-dispute-one-step-selection.test.ts",
  "./tests/validation-trace-dispute-one-step.test.ts",
  "./tests/validation-trace-dispute-timeout.test.ts",
];

/**
 * The test files the curated-emulator Vitest project runs, against a blueprint
 * compiled for `preprod-emulator-curated` (stage-0 allow-lists and the P1
 * fraud-proof catalogue). No other project runs them.
 */
export const curatedTests = ["./tests/curated-emulator-*.test.ts"];
