#!/usr/bin/env node

/**
 * Pins the `committed-field-shape` family's §12.8 cost against a fresh
 * `aiken check` measurement at the GOAL_SPEC §3.3 13.2M-mem basis — issue #601.
 *
 * **Why this exists.** §12.8 publishes a cost claim for the family's step 01:
 * one `blake2b_256` over the committed preimage, §12.7's total §5.1 walk as the
 * envelope guard, and then this section's own two questions — §5.4's byte bound
 * and §7.4's stride arithmetic, both `O(1)` over the header the walk already
 * read. Every published cost claim in this tree carries a ledger, and the
 * #519/#523 gate-that-cannot-fail doctrine says the same of a new one: a cost
 * claim nothing can falsify is not a measurement. It matters here for §12.7's
 * reason one section over — this family is the *only* adjudication of a
 * committed envelope its own slot refuses, so if its step does not fit an L1
 * transaction the two stalls §12.8 was written to close are still open, and
 * nothing else in the suite would notice.
 *
 * Aiken tests cannot observe their own execution units — the units are the check
 * *report's* reading of the test, not a value in scope — so the pin lives one
 * level up: a ledger of expected readings next to this verifier, taken through
 * `run-focused-check.mjs` (so a row for a test that did not run, or one that did
 * not pass, cannot be published) and compared.
 *
 * **What is checked** is the within-basis shape defined by
 * `exec-ledger-within-basis-v1.mjs`, which is where the checking lives. Nothing
 * about that shape is specific to this lane, so nothing about it is spelled out
 * again here — this file is the lane's three facts: which ledger, which label,
 * and which basis its judgements are made at.
 *
 * **What this lane's rows are.** The claim splits the way the verdict's work
 * splits, and the split is the same one §12.7's ledger makes because this
 * family's dominant term *is* §12.7's walk, reused as the envelope guard:
 *
 *   * the **byte** term at §5.4's per-field bound — 32,768 committed bytes in
 *     one item, carried on §8.4's chunked route at §8.3's maximum chunk count,
 *     which is the largest committed preimage any carriage this family accepts
 *     can reach; and
 *   * the **item** term at two cardinalities either side of nothing and both
 *     inside the basis.
 *
 * Each row has a fixture-only control beside it, for §12.7's ledger's reason:
 * rows are whole-test readings and this lane's fixture is `encode_field_preimage`
 * over a list whose length is the very quantity being priced, so differencing the
 * two item rows alone would charge the walk for the producer as well.
 *
 * **One thing this lane's rows are not.** They are not a measurement of the
 * *carriage* ceiling. §12.8's byte-bound shape is convictable by this family's
 * rule at any length, and the §8 ladder cannot deliver a preimage above §5.4's
 * bound to a step at all (`certified_chunks` and the §8.6 certificate mint both
 * refuse `total_length > max_transaction_aggregate_field_bytes`). That is a
 * carriage fact rather than a cost fact, it is recorded in the ledger's note and
 * in §12.8, and no row here can move it.
 *
 * Usage, from `onchain/aiken/`:
 *
 *   MIDGARD_AIKEN_BIN=<fork> node scripts/verify-committed-field-shape-exec-ledger-v1.mjs
 *   MIDGARD_AIKEN_BIN=<fork> node scripts/verify-committed-field-shape-exec-ledger-v1.mjs --update
 *
 * `--update` rewrites the raw rows from the measurement and is the only way to
 * record a legitimate re-take. It absorbs measurement drift and nothing else: a
 * selector that did not run, a row that did not run, or a fresh reading that
 * contradicts a `within` judgement fails in update mode too, and the ledger is
 * not rewritten when it does.
 */

import { dirname, resolve } from "node:path";
import { fileURLToPath } from "node:url";

import {
  GOAL_SPEC_EXECUTION_BASIS_V1,
  checkWithinBasisExecLedger,
} from "./exec-ledger-within-basis-v1.mjs";

const scriptDirectory = dirname(fileURLToPath(import.meta.url));

// Set rather than `process.exit(code)`: the checker is synchronous and has
// nothing pending, so letting the process end on its own cannot truncate the
// report it just wrote.
//
// The basis is named rather than spelled: it is a constant of the *program* —
// GOAL_SPEC §3.3, exported once from the shared module — not a number read out
// of the file being judged, and not a literal retyped per lane. The ledger's own
// `basis` block is checked against it.
process.exitCode = checkWithinBasisExecLedger({
  ledgerPath: resolve(
    scriptDirectory,
    "committed-field-shape-exec-ledger-v1.json",
  ),
  lane: "committed-field-shape §12.8 (field_index, preimage) shape verdict",
  declaredBasis: GOAL_SPEC_EXECUTION_BASIS_V1,
  update: process.argv.includes("--update"),
});
