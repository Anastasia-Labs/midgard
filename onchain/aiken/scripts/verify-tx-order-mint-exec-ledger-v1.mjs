#!/usr/bin/env node

/**
 * Pins the tx-order **mint**'s §8 material-carriage cost against a fresh
 * `aiken check` measurement at the GOAL_SPEC §3.3 13.2M-mem basis — issue #594.
 *
 * **Why this exists.** `docs/spec/midgard-tx.md` §8.11 publishes a cost claim
 * for this mint: the whole-materialising door entry point's cost is "§12.5's
 * tier-2 per-step full-preimage re-hash, bounded by §5.4's 32,768-byte
 * aggregate over all nine fields". Nothing measured it. Every other published
 * cost claim in this tree carries a ledger — §8.10's carriage rows, the Q1x
 * family, Q21, Q31 — and the §8.10 precedent plus the gate-that-cannot-fail
 * doctrine of #519/#523 say the same of this one: a cost claim nothing can
 * falsify is not a measurement, and the mint is the *only* on-chain reader of
 * an order's material, so if its cost is wrong the material never reaches L1 at
 * all.
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
 * **What this lane's rows are, and the one thing they refuse to launder.** §8.11's
 * claim splits along `whole_view`'s own split: §5.1 count consistency is settled
 * by arithmetic for the five fixed-stride fields (0, 1, 3, 4, 7) and by a full
 * `walk_to_end` over every item for the four variable-width ones (2, 5, 6, 8).
 * On the arithmetic half the claim holds as published and the ledger's
 * aggregate-bound row measures it. On the walked half it does not: the walk
 * costs a measured ~21,062 mem per item, so the basis is reached at roughly 537
 * items — three orders of magnitude below the item count §5.4's byte bound
 * admits — and the worst shape the mint accepts at that bound measures ~26x the
 * basis. That row is therefore **not** in this ledger: the within-basis shape
 * records only `within` rows, and a q1x-style `basisFit:"exceeds"` row demands
 * an `errata` cross-reference into a normative §8.11 erratum that does not exist
 * yet. What this ledger does instead is pin the *rate*, at two cardinalities
 * either side of nothing and both inside the basis, so a walk that gets dearer
 * per item moves a row here rather than surfacing years later. The over-basis
 * reading, its reproduction, and the residual are recorded on #594 for #580's
 * re-measurement pass.
 *
 * Usage, from `onchain/aiken/`:
 *
 *   MIDGARD_AIKEN_BIN=<fork> node scripts/verify-tx-order-mint-exec-ledger-v1.mjs
 *   MIDGARD_AIKEN_BIN=<fork> node scripts/verify-tx-order-mint-exec-ledger-v1.mjs --update
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
  ledgerPath: resolve(scriptDirectory, "tx-order-mint-exec-ledger-v1.json"),
  lane: "tx-order mint §8.11 material carriage",
  declaredBasis: GOAL_SPEC_EXECUTION_BASIS_V1,
  update: process.argv.includes("--update"),
});
