#!/usr/bin/env node

/**
 * Produces the cross-language golden vectors for the **wire encodings** of
 * `docs/spec/midgard-tx.md` §8.6 and §8.8 — the frozen `FieldCarriageV1` /
 * `FieldViewV1` sum types, the `FieldPreimageCertificateV1` manifest datum, and
 * the `FieldPreimageCertificateMintRedeemerV1` an off-chain minter emits.
 *
 * **Why this channel lives in the SDK and not in midgard-core.** The other two
 * channels (#568, #569) pin *byte-level derivations* — preimages, commitments,
 * chunk splits — and their producer is the CML-free codec twin in
 * midgard-core. What this channel pins is Plutus **Data** encoding, and the
 * off-chain producer of that is `Data.to` against the schemas in
 * `src/native-tx-field-access.ts`. A vector is only worth having if it is
 * emitted by the thing that will really emit it in production, so the generator
 * sits beside that producer. The shared channel plumbing is imported from
 * midgard-core rather than copied, so the `--check` contract is the same one
 * implementation.
 *
 * Two artifacts, the same pair every channel emits:
 *
 *   * `demo/midgard-sdk/tests/fixtures/native-tx-carriage-wire-v1.generated.json`
 *     — recomputed by `tests/native-tx-carriage-wire-goldens.test.ts`, so a
 *     drifting schema fails on the TypeScript side; and
 *   * `onchain/aiken/lib/midgard/native-tx-carriage-wire-v1-golden.test.ak`
 *     — which both **decodes** each vector into the Aiken type and re-**serialises**
 *     the reconstructed value back to the same bytes, so a divergence fails on
 *     the Aiken side in whichever direction it appears. Decoding alone would
 *     miss an Aiken encoder that emitted a shape it could still read; serialising
 *     alone would miss a decoder that accepted something the producer never emits.
 *
 * The set **straddles the 64-byte Plutus Data chunking boundary from both
 * sides**, which is not decoration: Data serialisation keeps a byte string
 * definite up to and including 64 bytes and switches to an indefinite-length
 * string of 64-byte definite chunks strictly above it, so the disagreement to
 * fear is a `>=` where the rule says `>`. Three vectors
 * (`carriage_inline_chunked_preimage`, `view_chunked_three_chunk_corner`,
 * `mint_redeemer_certify_chunked_arguments`) sit above the boundary and encode
 * as `5f 5840 … ff`; `carriage_inline_63_byte_preimage` (`583f…`) and
 * `carriage_inline_64_byte_preimage` (`5840…`) sit at and just below it and
 * must stay definite. The remaining vectors carry only short byte strings and
 * are here for their shapes, not their widths. #568's channel pins no
 * Data-encoded value at all, so this is the first vector set that exercises any
 * of it.
 *
 * A second, smaller set — `negativeVectors` — carries payloads that a
 * conforming decoder must **refuse** (§9 clause 2), one trailing-bytes case and
 * several wrong-shape cases per wire type. Each declares the layer that refuses
 * it, so the generated Aiken asserts the refusal where it actually happens
 * rather than accepting any error at all as proof.
 *
 * Vectors are regenerated, never hand-edited. Run with `--check` to assert the
 * checked-in artifacts are exactly what the producers emit today.
 *
 * usage: node scripts/generate-native-tx-carriage-wire-v1-goldens.mjs [--check]
 */

import "node:path";
import "node:url";
import "@lucid-evolution/lucid";
import "@al-ft/midgard-core/scripts/golden-channel.mjs";
import "../dist/index.js";
import "./generate-native-tx-carriage-wire-v1-goldens.vectors.mjs";
import "./generate-native-tx-carriage-wire-v1-goldens.negative-vectors.mjs";
import "./generate-native-tx-carriage-wire-v1-goldens.render-aiken.mjs";
