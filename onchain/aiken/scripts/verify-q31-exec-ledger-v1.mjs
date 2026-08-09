#!/usr/bin/env node

/**
 * Pins the #576 lane-B (Q31 `reference-input-no-idx`) family-rebind execution
 * figures against a fresh `aiken check` measurement at the GOAL_SPEC §3.3
 * 13.2M-mem basis.
 *
 * **Why this exists.** The Q31 rebind moves the family off the counted
 * `field_commitment_from_items` re-hash of a reproduced item list and onto the
 * §8.8 door: step-02 reads field 1 (reference inputs) by arithmetic at field
 * 0's shared 38-byte stride (§5.3/§10.5), and step-04 reads field 2's
 * authenticated item count (§5.2). Neither is a walk whose cost grows with the
 * disputed transaction, so the lane introduces no new unbounded walk and the
 * ledger carries no `basisFit:"exceeds"` row. What still has to be asserted is
 * that the two door-open steps *fit* the basis with margin, and that a re-take
 * cannot silently turn a `within` row into one that does not — a cost claim
 * nothing can falsify is not a measurement.
 *
 * Aiken tests cannot assert their own execution units, so the pin lives one
 * level up: a ledger of expected readings next to this verifier, which takes
 * the readings through `run-focused-check.mjs` (so a row for a test that did
 * not run, or one that did not pass, cannot be published) and compares.
 *
 * **What is checked** is the within-basis shape defined by
 * `exec-ledger-within-basis-v1.mjs`, which is where the checking lives: the
 * declared basis, the raw rows, the `within` judgements against fresh readings,
 * the neutralisation selectors, and that anything was measured at all. Nothing
 * about that shape is Q31-specific, so nothing about it is spelled out again
 * here — this file is the lane's three facts: which ledger, which label, and
 * which basis the lane's judgements are made at.
 *
 * **Why it is a lane file rather than a second copy.** It was the second copy,
 * and it carried the two defects the shared module was written to close: it
 * read `memoryUnits`/`cpuUnits` out of `native-tx-q31-exec-ledger-v1.json` and
 * then judged that file's rows against them — so raising the number in the
 * ledger made every `within` verdict vacuous while this gate stayed green — and
 * it had no guard against measuring nothing at all, so a ledger whose `modules`
 * array was emptied printed `"rows": 0` and exited 0. Both are the
 * gate-that-cannot-fail shape of #519/#523. #577 moved this file onto the
 * shared checker rather than patching two copies of one judgement, and its
 * ledger's rows are byte-identical across the move.
 *
 * Usage, from `onchain/aiken/`:
 *
 *   MIDGARD_AIKEN_BIN=<fork> node scripts/verify-q31-exec-ledger-v1.mjs
 *   MIDGARD_AIKEN_BIN=<fork> node scripts/verify-q31-exec-ledger-v1.mjs --update
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
process.exitCode = checkWithinBasisExecLedger({
  ledgerPath: resolve(scriptDirectory, "native-tx-q31-exec-ledger-v1.json"),
  lane: "Q31 reference-input-no-idx",
  declaredBasis: GOAL_SPEC_EXECUTION_BASIS_V1,
  update: process.argv.includes("--update"),
});
