import fs from "node:fs";

import path from "path";
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
 *   tests/retention-enforcement-v1.test.ts
 *   tests/tx-admissions-claim-load.test.ts
 *   tests/tx-admissions-monotone-timestamps.test.ts
 *   tests/tx-order-carriage-l1-observation-v1.test.ts
 */

// A committed `bail` makes the suite's cost and its result set unreproducible:
// with `bail: 3` a run with three early failures silently skips most of the
// files, so a green-looking summary and a red one describe different suites. It
// is opt-in per run instead — `MIDGARD_NODE_TEST_BAIL=3 pnpm test`.
const bail = parsePositiveInteger(process.env.MIDGARD_NODE_TEST_BAIL);

export default defineConfig({
  plugins: [
    {
      name: "raw-sql-loader",
      load(id) {
        if (!id.endsWith(".sql")) {
          return null;
        }
        return `export default ${JSON.stringify(fs.readFileSync(id, "utf8"))};`;
      },
    },
  ],
  test: {
    // One fresh process per test FILE is a correctness requirement, not a
    // performance knob. `@lucid-evolution/uplc` grows wasm linear memory on
    // every evaluation and never reclaims it, so a long-lived worker
    // eventually exhausts the wasm32 ceiling and the next evaluation surfaces
    // as `EvaluatorError: unreachable` — a WebAssembly abort wearing a
    // validator rejection's clothes. `isolate` must stay `true`.
    //
    // `maxForks` is the separate, purely-scheduling bound: only ~15 of the
    // ~155 files touch Postgres and each of those is pinned to its worker's
    // own database shard, so file parallelism is safe. A 2-core runner sets
    // `MIDGARD_NODE_TEST_FORKS=1` rather than forcing every machine down to
    // one file at a time. If a run dies on memory, LOWER the fork count — each
    // fork carries its own multi-GB emulator heap.
    pool: "forks",
    poolOptions: {
      forks: {
        isolate: true,
        singleFork: false,
        minForks: 1,
        maxForks: testMaxForks(),
        // Bound each worker's V8 heap here rather than exporting a blanket
        // NODE_OPTIONS from the lane runner, which would also hit pnpm, vitest's
        // own main process and every unrelated tool in the lane. This bounds the
        // JS heap only: the emulator's wasm evaluator allocates outside it,
        // which is why the fork count — not this number — is the knob that
        // actually caps a run's footprint.
        execArgv: ["--max-old-space-size=4096"],
      },
    },
    // Creates and migrates one database per worker shard before any file runs.
    globalSetup: ["./tests/global-setup.ts"],
    reporters: [["default", { summary: false }]],
    include: ["./tests/**/*.test.{js,mjs,cjs,ts,mts,cts,jsx,tsx}"],
    exclude: [
      ...configDefaults.exclude,
      "./tests/phase4-pipelined-process-summary-verifier.test.mjs",
    ],
    testTimeout: 420_000,
    ...(bail === undefined ? {} : { bail }),
    environment: "node",
  },
  resolve: {
    alias: {
      "@": path.resolve(__dirname, "./src"),
    },
  },
  esbuild: {
    target: "es2020",
  },
});
