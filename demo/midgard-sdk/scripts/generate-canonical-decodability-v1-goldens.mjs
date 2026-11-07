#!/usr/bin/env node

/**
 * Produces the cross-language golden vectors for the `canonical-decodability`
 * family of `docs/spec/midgard-tx.md` §12.7 — the total §5.1 envelope verdict
 * and the two wire types the family's two steps carry.
 *
 * **Why this channel exists at all.** §12.7's verdict is the one predicate in
 * this document that both language sides must agree on *without* either of them
 * being able to fail: on chain it decides whether a block is slashed, off chain
 * it decides whether a prover builds a step that would be refused. Two
 * independent implementations of a decision procedure are exactly the shape
 * that drifts, and the drift would be silent in both directions — a TypeScript
 * twin that called a preimage grammatical would leave a genuine fault
 * unfiled, and one that called it ungrammatical would have a prover burn a
 * transaction on a step step 02 refuses.
 *
 * **Why it lives in the SDK.** Two of the three artifacts it pins are Plutus
 * **Data** encodings, whose off-chain producer is `Data.to` against the schemas
 * in `src/fraud-proof/canonical-decodability.ts`; the verdict twin is in the
 * same module. A vector is only worth having if it is emitted by the thing that
 * will really emit it, so the generator sits beside the producer. The shared
 * channel plumbing is imported from midgard-core rather than copied, so the
 * `--check` contract is one implementation.
 *
 * Two artifacts, the pair every channel emits:
 *
 *   * `demo/midgard-sdk/tests/fixtures/canonical-decodability-v1.generated.json`
 *     — recomputed by `tests/canonical-decodability.test.ts`, so a drifting
 *     TypeScript twin fails on the TypeScript side; and
 *   * `onchain/aiken/lib/midgard/fraud-proofs/canonical-decodability/rule-golden.test.ak`
 *     — recomputed by the Aiken producers under the fork runner, so a
 *     divergence between the two verdicts fails on the Aiken side.
 *
 * The verdict vector set covers **every one of the eleven codes**, both
 * §5.1 width boundaries (23/24 and 255/256) on both the array head and the item
 * head, the empty field `80`, and the declared-count mismatch in both
 * directions. Coverage is asserted rather than eyeballed: the JSON carries the
 * code each vector earns and both suites check the set of codes reached is the
 * whole code space.
 *
 * Vectors are regenerated, never hand-edited. Run with `--check` to assert the
 * checked-in artifacts are exactly what the producers emit today.
 *
 * usage: node scripts/generate-canonical-decodability-v1-goldens.mjs [--check]
 */

import "node:path";
import "node:url";
import "@lucid-evolution/lucid";
import "@al-ft/midgard-core/scripts/golden-channel.mjs";
import "../dist/index.js";
import "./generate-canonical-decodability-v1-goldens.verdict-vectors.mjs";
import "./generate-canonical-decodability-v1-goldens.render-aiken.mjs";
