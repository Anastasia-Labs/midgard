#!/usr/bin/env node

/**
 * Pins the `canonical-decodability` family's §12.7 cost against a fresh
 * `aiken check` measurement at the GOAL_SPEC §3.3 13.2M-mem basis — issue #596.
 *
 * **Why this exists.** §12.7 publishes a cost claim for the family's step 01:
 * one `blake2b_256` over the committed preimage plus a total §5.1 walk that
 * reads one item head per declared item. Every published cost claim in this
 * tree carries a ledger — §8.10's carriage rows, the Q1x family, Q21, Q31, and
 * the §8.11 tx-order mint — and the #519/#523 gate-that-cannot-fail doctrine
 * says the same of a new one: a cost claim nothing can falsify is not a
 * measurement. It matters more here than for most families, because this one is
 * the *only* adjudication of a committed non-envelope: if its step does not fit
 * an L1 transaction, the operator escape hatch §12.7 was written to close is
 * still open, and nothing else would notice.
 *
 * Aiken tests cannot observe their own execution units — the units are the
 * check *report's* reading of the test, not a value in scope — so the pin lives
 * one level up: a ledger of expected readings next to this verifier, taken
 * through `run-focused-check.mjs` (so a row for a test that did not run, or one
 * that did not pass, cannot be published) and compared.
 *
 * **What is checked** is the within-basis shape defined by
 * `exec-ledger-within-basis-v1.mjs`, which is where the checking lives: the
 * declared basis, the raw rows, the `within` judgements against fresh readings,
 * the neutralisation selectors, and that anything was measured at all. Nothing
 * about that shape is specific to this lane, so nothing about it is spelled out
 * again here — this file is the lane's three facts: which ledger, which label,
 * and which basis its judgements are made at.
 *
 * **What this lane's rows are.** §12.7's claim splits the way the verdict's own
 * work splits, into a byte term and an item term, and the rows measure each
 * separately with a control beside it:
 *
 *   * the **byte** term at §5.4's per-field bound — 32,768 committed bytes in
 *     one item, carried on §8.4's chunked route at §8.3's maximum chunk count,
 *     which is the largest committed preimage any carriage this family accepts
 *     can reach; and
 *   * the **item** term at two cardinalities either side of nothing and both
 *     inside the basis, so a walk that gets dearer per item moves a row here
 *     rather than surfacing in a later wave.
 *
 * Each row has a fixture-only control, and that is this ledger's one addition to
 * §8.11's precedent. Rows are whole-test readings, and this lane's fixture is
 * `encode_field_preimage` over a list whose length is the very quantity being
 * priced — so differencing the two item rows alone would charge the walk for the
 * producer as well. The controls are what make the published per-item figure a
 * measurement of the walk rather than of the test harness.
 *
 * The **over-basis** shape is recorded in the ledger's note and on #596 rather
 * than as a row, for the reason §8.11's ledger records its own: the within-basis
 * shape admits only `within` rows, and the q1x-style `basisFit:"exceeds"`
 * vocabulary demands an `errata` cross-reference into a normative erratum this
 * section does not yet carry.
 *
 * Usage, from `onchain/aiken/`:
 *
 *   MIDGARD_AIKEN_BIN=<fork> node scripts/verify-canonical-decodability-exec-ledger-v1.mjs
 *   MIDGARD_AIKEN_BIN=<fork> node scripts/verify-canonical-decodability-exec-ledger-v1.mjs --update
 *
 * `--update` rewrites the raw rows from the measurement and is the only way to
 * record a legitimate re-take. It absorbs measurement drift and nothing else:
 * a selector that did not run, a row that did not run, or a fresh reading that
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
// of the file being judged, and not a literal retyped per lane. The ledger's
// own `basis` block is checked against it.
process.exitCode = checkWithinBasisExecLedger({
  ledgerPath: resolve(
    scriptDirectory,
    "canonical-decodability-exec-ledger-v1.json",
  ),
  lane: "canonical-decodability §12.7 committed-preimage verdict",
  declaredBasis: GOAL_SPEC_EXECUTION_BASIS_V1,
  update: process.argv.includes("--update"),
});
