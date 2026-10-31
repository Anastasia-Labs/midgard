#!/usr/bin/env node

/**
 * Produces the **per-field** cross-language golden vectors for the nine item
 * encodings of `docs/spec/midgard-tx.md` §5.3 (with §5.5 for outputs and §5.6
 * for mint), the §5.1 envelope each field wears, the §4 flat commitment each
 * field commits under, and the §8 carriage each field's preimage selects.
 *
 * This is the fan-out half of the channel #568 opened. That generator owns what
 * all nine fields *share* — envelope widths, strides, chunk split, certificate
 * asset names — and deliberately carries no per-field item bytes. This one owns
 * the other half: for every field index 0..8, what `enc_i` actually is.
 *
 * Every value is computed by the TypeScript twins
 * (`demo/midgard-core/src/codec/native-tx-field-items.ts`, plus the reused
 * canonical encoders for fields 2 and 6) and written to two places:
 *
 *   * `demo/midgard-core/tests/fixtures/native-tx-field-items-v1.generated.json`
 *     — recomputed by `tests/native-tx-field-items-goldens.test.ts`, so a
 *     drifting twin fails on the TypeScript side; and
 *   * `onchain/aiken/lib/midgard/native-tx-field-items-v1-golden.test.ak`
 *     — recomputed by the Aiken producers under the fork runner, so a
 *     divergence between the two encoders fails on the Aiken side.
 *
 * The Aiken side proves item-encoding agreement two ways, because neither alone
 * is enough:
 *
 *   1. **Directly**, where the item's Aiken value is cheap to write down
 *      (fields 0/1, 3/4, 6, 7, 8): `encode_midgard_tx_input(...)` and friends
 *      are called on structured literals and compared to the TypeScript bytes.
 *   2. **By producer round-trip**, for all nine including the two whose Aiken
 *      values are whole records (`MidgardTxOutput`, the mint `Data` map):
 *      decoding the TypeScript preimage and re-encoding it with the field's own
 *      Aiken producer must return the identical bytes. A decoder that accepted
 *      the bytes but an encoder that spelled them differently fails here.
 *
 * Vectors are regenerated, never hand-edited. Run with `--check` to assert the
 * checked-in artifacts are exactly what the twins produce today. That contract
 * — argument parsing, the trip through `aiken fmt`, and the check-or-write
 * emission — is one implementation shared with the sibling generator, in
 * `scripts/golden-channel.mjs`; only what is computed differs between them.
 *
 * usage: node scripts/generate-native-tx-field-items-v1-goldens.mjs [--check]
 */

import "node:path";
import "node:url";
import "./golden-channel.mjs";
import "../dist/codec/native-tx-field-access.js";
import "../dist/codec/native-tx-field-items.js";
import "../dist/codec/native.js";
import "../dist/codec/native-script.js";
import "../dist/codec/versioned-script.js";
import "../tests/fixtures/native-tx-field-items-v1.vectors.mjs";
import "./generate-native-tx-field-items-v1-goldens.build-straddle.mjs";
import "./generate-native-tx-field-items-v1-goldens.build-golden.mjs";
import "./generate-native-tx-field-items-v1-goldens.render-aiken.mjs";
import "./generate-native-tx-field-items-v1-goldens.registration.mjs";
