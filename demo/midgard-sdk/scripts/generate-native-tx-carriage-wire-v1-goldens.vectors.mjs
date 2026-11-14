import { dirname, join, resolve } from "node:path";
import { fileURLToPath } from "node:url";

import {
  aikenBytes,
  goldenChannelEmitter,
  hex,
  parseGoldenChannelArguments,
} from "@al-ft/midgard-core/scripts/golden-channel.mjs";
import { Data } from "@lucid-evolution/lucid";

import {
  FieldCarriage,
  FieldPreimageCertificate,
  FieldPreimageCertificateMintRedeemer,
  FieldView,
} from "../dist/index.js";

const scriptDirectory = dirname(fileURLToPath(import.meta.url));

const packageRoot = resolve(scriptDirectory, "..");

export const repositoryRoot = resolve(packageRoot, "../..");

export const generatedJsonPath = join(
  packageRoot,
  "tests/fixtures/native-tx-carriage-wire-v1.generated.json",
);

export const generatedAikenPath = join(
  repositoryRoot,
  "onchain/aiken/lib/midgard/native-tx-carriage-wire-v1-golden.test.ak",
);

const { checkOnly } = parseGoldenChannelArguments(
  "usage: node scripts/generate-native-tx-carriage-wire-v1-goldens.mjs [--check]",
);

export const writeOrCheck = goldenChannelEmitter({ repositoryRoot, checkOnly });

// ---------------------------------------------------------------------------
// Vector inputs
// ---------------------------------------------------------------------------

/**
 * `length` deterministic bytes. An affine pattern rather than a repeated byte,
 * so a chunk boundary that lands in the wrong place changes the bytes either
 * side of it and the vector notices.
 */
const patternBytes = (seed, length) =>
  Buffer.from(
    Array.from({ length }, (_, index) => (seed + index * 7 + 3) & 0xff),
  );

const repeatedByte = (byte, length) => Buffer.alloc(length, byte);

/** Just over the 64-byte Plutus Data chunking boundary. */
const CHUNKED_BYTES = 100;

/**
 * The boundary itself, from both sides. A Plutus Data byte string stays a
 * single definite string at 64 bytes (`5840 …`) and only becomes an
 * indefinite-length string of 64-byte chunks strictly above it (`5f 5840 …`),
 * so 63 and 64 are where a `>=` written for a `>` shows up — and 64 is a width
 * production reaches, being both the digest-pair size and a round chunk.
 */
const BOUNDARY_BYTES = 64;

const compactCbor = patternBytes(0x11, CHUNKED_BYTES);

const witnessSetCompactCbor = patternBytes(0x40, 70);

export const smallCompactCbor = Buffer.from([0xa1, 0x02, 0x03]);

export const smallWitnessSetCompactCbor = Buffer.from([0xb2, 0x04]);

const inlinePreimage = patternBytes(0x90, 80);

const inlinePreimageAtBoundary = patternBytes(0x70, BOUNDARY_BYTES);

const inlinePreimageBelowBoundary = patternBytes(0x60, BOUNDARY_BYTES - 1);

export const emptyFieldPreimage = Buffer.from([0x80]);

const chunkedViewChunkA = patternBytes(0x20, 65);

export const chunkedViewChunkB = Buffer.from([0x01, 0x02, 0x03]);

export const digestA = repeatedByte(0x33, 32);

export const digestB = repeatedByte(0x44, 32);

export const digestC = repeatedByte(0x55, 32);

export const certificateOwner = repeatedByte(0x11, 28);

export const certificateTxId = repeatedByte(0x22, 32);

// The #606 mint-welded datum commitment slot. A distinct repeated byte, not a
// recomputed hash: what this channel pins is the *wire* shape — field order
// and encoding — and the semantic weld (`field_hash == hash(concat(chunks))`)
// is pinned by the field-access golden channel and the .ak selectors instead.
export const certificateFieldHash = repeatedByte(0x66, 32);

/**
 * Each vector carries three things that have to stay in step: the value the
 * TypeScript producer encodes, the CBOR it produces, and the Aiken expression
 * that reconstructs the same value. The Aiken expression is *rendered from the
 * same inputs* as the value — never transcribed from the hex — so the two sides
 * are independent constructions of one vector rather than one construction and
 * a copy of its output.
 */
export const vectors = [
  {
    label: "carriage_inline_chunked_preimage",
    aikenType: "FieldCarriageV1",
    schema: FieldCarriage,
    value: { Inline: { preimage: hex(inlinePreimage) } },
    aiken: `Inline { preimage: ${aikenBytes(hex(inlinePreimage))} }`,
  },
  {
    // 63 bytes: one below the boundary, still a definite `583f …` string.
    label: "carriage_inline_63_byte_preimage",
    aikenType: "FieldCarriageV1",
    schema: FieldCarriage,
    value: { Inline: { preimage: hex(inlinePreimageBelowBoundary) } },
    aiken: `Inline { preimage: ${aikenBytes(hex(inlinePreimageBelowBoundary))} }`,
  },
  {
    // 64 bytes exactly: the last width that is still a single definite
    // `5840 …` string. An encoder that chunks at `>= 64` rather than `> 64`
    // diverges here and nowhere else.
    label: "carriage_inline_64_byte_preimage",
    aikenType: "FieldCarriageV1",
    schema: FieldCarriage,
    value: { Inline: { preimage: hex(inlinePreimageAtBoundary) } },
    aiken: `Inline { preimage: ${aikenBytes(hex(inlinePreimageAtBoundary))} }`,
  },
  {
    label: "carriage_inline_empty_field",
    aikenType: "FieldCarriageV1",
    schema: FieldCarriage,
    value: { Inline: { preimage: hex(emptyFieldPreimage) } },
    aiken: `Inline { preimage: ${aikenBytes(hex(emptyFieldPreimage))} }`,
  },
  {
    label: "carriage_raw_utxo",
    aikenType: "FieldCarriageV1",
    schema: FieldCarriage,
    value: { RawUtxo: { ref_input_index: 3n } },
    aiken: "RawUtxo { ref_input_index: 3 }",
  },
  {
    label: "carriage_certified_three_chunks",
    aikenType: "FieldCarriageV1",
    schema: FieldCarriage,
    value: {
      Certified: {
        cert_ref_input_index: 0n,
        chunk_ref_input_indices: [1n, 2n, 3n],
      },
    },
    aiken:
      "Certified { cert_ref_input_index: 0, chunk_ref_input_indices: [1, 2, 3] }",
  },
  {
    label: "view_whole_empty_field",
    aikenType: "FieldViewV1",
    schema: FieldView,
    value: {
      Whole: { bytes: hex(emptyFieldPreimage), count: 0n, stride: 40n },
    },
    aiken: `Whole { bytes: ${aikenBytes(hex(emptyFieldPreimage))}, count: 0, stride: 40 }`,
  },
  {
    label: "view_chunked_three_chunk_corner",
    aikenType: "FieldViewV1",
    schema: FieldView,
    value: {
      Chunked: {
        chunks: [hex(chunkedViewChunkA), hex(chunkedViewChunkB)],
        chunk_digests: [hex(digestA), hex(digestB)],
        count: 819n,
        stride: 40n,
      },
    },
    aiken: [
      "Chunked {",
      `  chunks: [${aikenBytes(hex(chunkedViewChunkA))}, ${aikenBytes(hex(chunkedViewChunkB))}],`,
      `  chunk_digests: [${aikenBytes(hex(digestA))}, ${aikenBytes(hex(digestB))}],`,
      "  count: 819,",
      "  stride: 40,",
      "}",
    ].join("\n"),
  },
  {
    label: "certificate_three_chunk_corner",
    aikenType: "FieldPreimageCertificateV1",
    schema: FieldPreimageCertificate,
    value: {
      owner: hex(certificateOwner),
      tx_id: hex(certificateTxId),
      field_index: 5n,
      field_hash: hex(certificateFieldHash),
      total_length: 32_763n,
      chunk_digests: [hex(digestA), hex(digestB), hex(digestC)],
    },
    aiken: [
      "FieldPreimageCertificateV1 {",
      `  owner: ${aikenBytes(hex(certificateOwner))},`,
      `  tx_id: ${aikenBytes(hex(certificateTxId))},`,
      "  field_index: 5,",
      `  field_hash: ${aikenBytes(hex(certificateFieldHash))},`,
      "  total_length: 32763,",
      `  chunk_digests: [${aikenBytes(hex(digestA))}, ${aikenBytes(hex(digestB))}, ${aikenBytes(hex(digestC))}],`,
      "}",
    ].join("\n"),
  },
  {
    label: "mint_redeemer_certify_chunked_arguments",
    aikenType: "FieldPreimageCertificateMintRedeemerV1",
    schema: FieldPreimageCertificateMintRedeemer,
    value: {
      Certify: {
        source_kind: 0n,
        compact_cbor: hex(compactCbor),
        witness_set_compact_cbor: hex(witnessSetCompactCbor),
        chunk_ref_input_indices: [1n, 2n, 3n],
        output_index: 0n,
      },
    },
    aiken: [
      "Certify {",
      "  source_kind: 0,",
      `  compact_cbor: ${aikenBytes(hex(compactCbor))},`,
      `  witness_set_compact_cbor: ${aikenBytes(hex(witnessSetCompactCbor))},`,
      "  chunk_ref_input_indices: [1, 2, 3],",
      "  output_index: 0,",
      "}",
    ].join("\n"),
  },
  {
    label: "mint_redeemer_certify_short_arguments",
    aikenType: "FieldPreimageCertificateMintRedeemerV1",
    schema: FieldPreimageCertificateMintRedeemer,
    value: {
      Certify: {
        source_kind: 0n,
        compact_cbor: hex(smallCompactCbor),
        witness_set_compact_cbor: hex(smallWitnessSetCompactCbor),
        chunk_ref_input_indices: [0n, 1n],
        output_index: 2n,
      },
    },
    aiken: [
      "Certify {",
      "  source_kind: 0,",
      `  compact_cbor: ${aikenBytes(hex(smallCompactCbor))},`,
      `  witness_set_compact_cbor: ${aikenBytes(hex(smallWitnessSetCompactCbor))},`,
      "  chunk_ref_input_indices: [0, 1],",
      "  output_index: 2,",
      "}",
    ].join("\n"),
  },
  {
    label: "mint_redeemer_retire",
    aikenType: "FieldPreimageCertificateMintRedeemerV1",
    schema: FieldPreimageCertificateMintRedeemer,
    value: "Retire",
    aiken: "Retire",
  },
];

// ---------------------------------------------------------------------------
// Negative vector inputs
// ---------------------------------------------------------------------------

/**
 * §9 clause 2 requires the decoders to be fail-closed, and a vector set made
 * only of things that must be accepted says nothing about what must be refused:
 * a decoder that accepted every byte string it was handed would pass every
 * positive vector above.
 *
 * These are built the same way the positive ones are — from inputs, never
 * transcribed. The trailing-bytes cases append a complete extra CBOR item to
 * whatever the producer really emits for a named positive vector, so they stay
 * correct if that vector's bytes ever change; the wrong-shape cases are
 * `Data.to` over an explicitly malformed `Constr`, which is the same encoder
 * emitting a value the schema would never build.
 */
const encodedVector = (label) => {
  const vector = vectors.find((entry) => entry.label === label);
  if (vector === undefined) {
    throw new Error(`no positive vector named ${label}`);
  }
  return Data.to(vector.value, vector.schema);
};

/**
 * A valid encoding with one further **complete** CBOR item after it (`00`, the
 * unsigned integer zero). A complete item rather than a truncated one, so what
 * the vector tests is "the payload did not end where the value did" and not
 * "the bytes ran out".
 */
export const withTrailingItem = (label) => `${encodedVector(label)}00`;
