#!/usr/bin/env node

/**
 * Produces the cross-language golden vectors for the `committed-field-shape`
 * family of `docs/spec/midgard-tx.md` §12.8 — the `(field_index, preimage)`
 * shape verdict and the wire type its two steps carry.
 *
 * **Why this channel exists at all.** §12.8's verdict decides whether a block is
 * slashed, and it decides it from two arguments rather than one. Two independent
 * implementations of a two-argument decision procedure drift in a way a
 * one-argument one cannot: a twin that transposes the slot, or reads §5.3's
 * stride table one row differently, agrees with its partner on most inputs and
 * disagrees on exactly the ones the fault kind exists for. Every vector below is
 * therefore a `(slot, bytes)` pair and both sides recompute the verdict from it.
 *
 * **The channel also enforces the partition against §12.7.** Each vector carries
 * the §12.7 envelope verdict alongside this section's, and the generator refuses
 * to emit a set in which any vector is convicted by both fault kinds or by
 * neither-when-the-door-refuses. A cross-section boundary that lives only in
 * prose is a boundary one side can drift out of silently.
 *
 * **Why vectors are built rather than transcribed.** Two of the shapes this
 * family adjudicates are 32,768 and 32,769 bytes long. A hex literal for those
 * would be a 65 KB string in two files that nobody can read and neither side
 * could check for transposition. Each vector therefore carries a *construction*
 * — a literal for the small ones, `sizedFieldEnvelope(totalLength, fill)` for the
 * large — and the byte-level agreement is proved by a `blake2b_256` commitment
 * over the built bytes, which both sides recompute. That is §4's own hash, so
 * the check is the one the door itself would make.
 *
 * Two artifacts, the pair every channel emits:
 *
 *   * `demo/midgard-sdk/tests/fixtures/committed-field-shape-v1.generated.json`
 *     — recomputed by `tests/committed-field-shape.test.ts`, so a drifting
 *     TypeScript twin fails on the TypeScript side; and
 *   * `onchain/aiken/lib/midgard/fraud-proofs/committed-field-shape/rule-golden.test.ak`
 *     — recomputed by the Aiken producers under the fork runner, so a divergence
 *     between the two verdicts fails on the Aiken side.
 *
 * Vectors are regenerated, never hand-edited. Run with `--check` to assert the
 * checked-in artifacts are exactly what the producers emit today.
 *
 * usage: node scripts/generate-committed-field-shape-v1-goldens.mjs [--check]
 */

import "node:path";
import "node:url";
import "@lucid-evolution/lucid";
import "@al-ft/midgard-core/scripts/golden-channel.mjs";
import "@al-ft/midgard-core";
import "../dist/index.js";
import "./generate-committed-field-shape-v1-goldens.verdict-vectors.mjs";
import "./generate-committed-field-shape-v1-goldens.build-golden.mjs";
