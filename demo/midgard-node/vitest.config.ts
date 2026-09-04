import {
  isolatedForksPool,
  midgardSourceSsr,
  rawSqlLoaderPlugin,
} from "@al-ft/midgard-test-support/vitest";
import { configDefaults, defineConfig } from "vitest/config";

import { parsePositiveInteger, testMaxForks } from "./tests/test-env.js";

/**
 * Files that talk to Postgres.
 *
 * Invariant: a database-touching file must not run concurrently with another
 * database-touching file on the same database — several of these clear whole
 * tables, so a neighbour's rows would vanish mid-assertion. The suite holds the
 * invariant by sharding rather than by serializing: `tests/test-env.ts` pins
 * each worker to `midgard_test_w${VITEST_POOL_ID}` and a fork-pool worker only
 * ever runs one file at a time, so no two files share a database. Keep this
 * inventory current — it is the list `tests/global-setup.ts` provisions shards
 * for, and the reason the shard scheme exists.
 *
 *   tests/admission-writer.test.ts
 *   tests/da-publication-reconciler-e2e.test.ts        (opt-in)
 *   tests/database.test.ts
 *   tests/deposit-flow-emulator-commit-selection.test.ts
 *   tests/deposit-flow-emulator-confirmation-journal.test.ts
 *   tests/deposit-flow-emulator-merge-payout.test.ts
 *   tests/deposit-flow-emulator-recovery-invalidation.test.ts
 *   tests/deposit-flow-emulator-submission.test.ts
 *   tests/migration-locking.test.ts
 *   tests/phase1-admission-acceptance.operator.test.ts (opt-in)
 *   tests/phase1-exact-crash.operator.test.ts          (opt-in)
 *   tests/retention-enforcement.test.ts
 *   tests/tx-admissions-claim-load.test.ts
 *   tests/tx-admissions-monotone-timestamps.test.ts
 *   tests/tx-order-carriage-l1-observation.test.ts
 */

// A committed `bail` makes the suite's cost and its result set unreproducible:
// with `bail: 3` a run with three early failures silently skips most of the
// files, so a green-looking summary and a red one describe different suites. It
// is opt-in per run instead — `MIDGARD_NODE_TEST_BAIL=3 pnpm test`.
const bail = parsePositiveInteger(process.env.MIDGARD_NODE_TEST_BAIL);

export default defineConfig({
  plugins: [rawSqlLoaderPlugin()],
  test: {
    // The one-process-per-file requirement is stated once in
    // `isolatedForksPool`. This suite's own choice is the scheduling cap: only
    // ~15 of the ~155 files touch Postgres and each of those is pinned to its
    // worker's own database shard, so file parallelism is safe. A 2-core
    // runner sets `MIDGARD_NODE_TEST_FORKS=1` rather than forcing every
    // machine down to one file at a time.
    ...isolatedForksPool({ maxForks: testMaxForks() }),
    // Creates and migrates one database per worker shard before any file runs.
    globalSetup: ["./tests/global-setup.ts"],
    reporters: [["default", { summary: false }]],
    // Vitest 3's filter resolution does not reliably match the eight-way
    // extension brace used here previously, so keep the overwhelmingly common
    // TypeScript lane explicit. Otherwise a focused `*.test.ts` invocation can
    // report "No test files found" and never exercise a release gate.
    include: [
      "./tests/**/*.test.ts",
      "./tests/**/*.test.{js,mjs,cjs,mts,cts,jsx,tsx}",
    ],
    exclude: [
      ...configDefaults.exclude,
      "./tests/phase4-pipelined-process-summary-verifier.test.mjs",
    ],
    testTimeout: 420_000,
    ...(bail === undefined ? {} : { bail }),
    environment: "node",
  },
  ssr: midgardSourceSsr(),
  esbuild: {
    target: "es2020",
  },
});
