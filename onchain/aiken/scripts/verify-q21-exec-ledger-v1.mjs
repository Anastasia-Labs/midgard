#!/usr/bin/env node

/**
 * Pins the #577 lane-C (Q21 `transition-trace`) family-rebind execution figures
 * against a fresh `aiken check` measurement at the GOAL_SPEC §3.3 13.2M-mem
 * basis.
 *
 * **Why this exists.** The Q21 rebind moves the family off the retired
 * proof-source idiom and onto the §8.8 door:
 * `validate_l2_transaction_transition` opens field 0 and field 2 through the
 * #575 `FieldOpeningV1` bridge instead of decoding each preimage and re-hashing
 * the reproduced item list, and `tx_order_compact_body` re-derives the compact
 * body from the transaction id instead of verifying a proof-source triple.
 * Neither introduces a walk whose cost grows faster than the one it replaced,
 * so the ledger carries no `basisFit:"exceeds"` row.
 *
 * The two call sites moved in **opposite** directions — the forced-order path
 * got cheaper, the two-field L2 path got dearer, because opening two fields
 * re-derives the transaction id twice over. Both are pinned for that reason. A
 * lane that publishes only the row that improved has not measured itself, and
 * an unpinned regression is the one that grows.
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
 * about that shape is Q21-specific, so nothing about it is spelled out again
 * here — this file is the lane's three facts: which ledger, which label, and
 * which basis the lane's judgements are made at.
 *
 * The one Q21-specific note is on the selectors. Check 4 asks that the rows
 * measure a validator that discriminates rather than one that returns `True`
 * for anything, and *which* selectors establish that is a per-lane judgement:
 * each of this lane's five is verified by a source mutation that turns it red,
 * recorded vector by vector in the ledger's `neutralisationNote`.
 *
 * Usage, from `onchain/aiken/`:
 *
 *   MIDGARD_AIKEN_BIN=<fork> node scripts/verify-q21-exec-ledger-v1.mjs
 *   MIDGARD_AIKEN_BIN=<fork> node scripts/verify-q21-exec-ledger-v1.mjs --update
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
// AC3 pins this lane's rows "at the 13.2M basis", and the basis is named rather
// than spelled: it is a constant of the *program* — GOAL_SPEC §3.3, exported
// once from the shared module — not a number read out of the file being judged,
// and not a literal retyped per lane. The ledger's own `basis` block is checked
// against it.
process.exitCode = checkWithinBasisExecLedger({
  ledgerPath: resolve(scriptDirectory, "native-tx-q21-exec-ledger-v1.json"),
  lane: "Q21 transition-trace",
  declaredBasis: GOAL_SPEC_EXECUTION_BASIS_V1,
  update: process.argv.includes("--update"),
});
