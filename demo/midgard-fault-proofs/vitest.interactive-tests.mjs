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
