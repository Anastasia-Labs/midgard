#!/usr/bin/env node

/**
 * Pins the native-script decoding-fault thread's execution cost against a
 * fresh `aiken check` measurement at the GOAL_SPEC §3.3 13.2M-mem basis —
 * issue #635 (originating divergence #633).
 *
 * **Why this exists.** The family's design (§6 of
 * `docs/fault-proofs/native-script-decoding-fault-thread-design-v1.md`)
 * publishes a per-node throughput and a worst-case transaction count that
 * were *derived* from the scan ledger's pinned one-shot rates, with an
 * explicit caveat that nothing had measured the staged fold itself. Every
 * published cost claim in this tree carries a ledger, and the #519/#523
 * gate-that-cannot-fail doctrine says the same of a new one: a cost claim
 * nothing can falsify is not a measurement. This lane's first measurement
 * moved the number — the realized fold rate is ≈3.5× the derived one (the
 * ledger's note carries the arithmetic and the surviving conclusions) — which
 * is precisely the kind of fact a derivation left unpinned would have hidden.
 *
 * Aiken tests cannot observe their own execution units, so the pin lives one
 * level up: a ledger of expected readings next to this verifier, taken
 * through `run-focused-check.mjs` (so a row for a test that did not run, or
 * one that did not pass, cannot be published) and compared.
 *
 * **What is checked** is the within-basis shape defined by
 * `exec-ledger-within-basis-v1.mjs`, which is where the checking lives: the
 * declared basis, the raw rows, the `within` judgements against fresh
 * readings, the neutralisation selectors, and that anything was measured at
 * all. This file is the lane's three facts: which ledger, which label, and
 * which basis its judgements are made at.
 *
 * **What this lane's rows are.** Two groups: the engine-fold slope rows
 * (9- and 17-node worst-shape vectors, each with a fixture-only control) that
 * price the scan machine per node, and one whole-L1-step row per validator
 * arm of the thread that prices the step envelope the fold rows exclude. The
 * over-basis 65-node readings are recorded in the ledger's note rather than
 * as rows, because the within-basis shape admits only `within` judgements —
 * and a big scan not fitting one transaction is the family's premise, not a
 * regression.
 *
 * Usage, from `onchain/aiken/`:
 *
 *   MIDGARD_AIKEN_BIN=<fork> node scripts/verify-native-script-decoding-engine-exec-ledger-v1.mjs
 *   MIDGARD_AIKEN_BIN=<fork> node scripts/verify-native-script-decoding-engine-exec-ledger-v1.mjs --update
 *
 * `--update` rewrites the raw rows from the measurement and is the only way
 * to record a legitimate re-take. It absorbs measurement drift and nothing
 * else: a selector that did not run, a row that did not run, or a fresh
 * reading that contradicts a `within` judgement fails in update mode too,
 * and the ledger is not rewritten when it does.
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
  ledgerPath: resolve(
    scriptDirectory,
    "native-script-decoding-engine-exec-ledger-v1.json",
  ),
  lane: "native-script decoding-fault thread (#635) engine fold and step envelope",
  declaredBasis: GOAL_SPEC_EXECUTION_BASIS_V1,
  update: process.argv.includes("--update"),
});
