#!/usr/bin/env node

/**
 * Produces the cross-language golden vectors for the shared field-access
 * surface of `docs/spec/midgard-tx.md` — §4's flat commitment, §5.1's enveloped
 * preimage grammar, §5.3's stride table, §8.3's carriage constants, §8.4's
 * deterministic chunk split and §8.6's certificate asset name.
 *
 * Every value is computed by the TypeScript twin
 * (`demo/midgard-core/src/codec/native-tx-field-access.ts`) from the inputs in
 * `tests/fixtures/native-tx-field-access-v1.vectors.mjs` and written to
 * `onchain/aiken/lib/midgard/native-tx-field-access-v1-golden.test.ak`, where
 * the Aiken producers recompute it under the fork runner, so a divergence
 * between the two encoders fails on the Aiken side. Nothing here is per-field;
 * the per-field item encodings of §5.3 are the sibling channel (issue #569).
 *
 * Vectors are regenerated, never hand-edited. Run with `--check` to assert the
 * checked-in artifacts are exactly what the twin produces today. That contract
 * — argument parsing, the trip through `aiken fmt`, and the check-or-write
 * emission — is one implementation shared with the sibling generator, in
 * `scripts/golden-channel.mjs`; only what is computed differs between them.
 *
 * usage: node scripts/generate-native-tx-field-access-v1-goldens.mjs [--check]
 */

import "node:path";
import "node:url";
import "./generate-native-tx-field-access-v1-goldens.build-golden.mjs";
import "./generate-native-tx-field-access-v1-goldens.render-aiken.mjs";
