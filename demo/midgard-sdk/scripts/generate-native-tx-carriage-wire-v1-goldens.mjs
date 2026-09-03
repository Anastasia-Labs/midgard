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

import { dirname, join, resolve } from "node:path";
import { fileURLToPath } from "node:url";

import { Constr, Data } from "@lucid-evolution/lucid";

// The channel plumbing is midgard-core's and is imported rather than
// duplicated: `--check` is the contract that makes a golden channel mean
// anything, and two copies of it is two chances for one to soften. It is
// addressed through midgard-core's `exports` map, not by a relative path across
// the package boundary, so the dependency is one this package declares.
import {
  aikenBytes,
  formatAikenSource,
  goldenChannelEmitter,
  hex,
  parseGoldenChannelArguments,
} from "@al-ft/midgard-core/scripts/golden-channel.mjs";
import {
  FIELD_CARRIAGE_CONSTRUCTOR_INDEXES,
  FIELD_PREIMAGE_CERTIFICATE_MINT_REDEEMER_CONSTRUCTOR_INDEXES,
  FIELD_VIEW_CONSTRUCTOR_INDEXES,
  FieldCarriage,
  FieldPreimageCertificateMintRedeemer,
  FieldPreimageCertificate,
  FieldView,
} from "../dist/index.js";

const scriptDirectory = dirname(fileURLToPath(import.meta.url));
const packageRoot = resolve(scriptDirectory, "..");
const repositoryRoot = resolve(packageRoot, "../..");
const generatedJsonPath = join(
  packageRoot,
  "tests/fixtures/native-tx-carriage-wire-v1.generated.json",
);
const generatedAikenPath = join(
  repositoryRoot,
  "onchain/aiken/lib/midgard/native-tx-carriage-wire-v1-golden.test.ak",
);

const { checkOnly } = parseGoldenChannelArguments(
  "usage: node scripts/generate-native-tx-carriage-wire-v1-goldens.mjs [--check]",
);
const writeOrCheck = goldenChannelEmitter({ repositoryRoot, checkOnly });

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
const smallCompactCbor = Buffer.from([0xa1, 0x02, 0x03]);
const smallWitnessSetCompactCbor = Buffer.from([0xb2, 0x04]);
const inlinePreimage = patternBytes(0x90, 80);
const inlinePreimageAtBoundary = patternBytes(0x70, BOUNDARY_BYTES);
const inlinePreimageBelowBoundary = patternBytes(0x60, BOUNDARY_BYTES - 1);
const emptyFieldPreimage = Buffer.from([0x80]);
const chunkedViewChunkA = patternBytes(0x20, 65);
const chunkedViewChunkB = Buffer.from([0x01, 0x02, 0x03]);
const digestA = repeatedByte(0x33, 32);
const digestB = repeatedByte(0x44, 32);
const digestC = repeatedByte(0x55, 32);
const certificateOwner = repeatedByte(0x11, 28);
const certificateTxId = repeatedByte(0x22, 32);
// The #606 mint-welded datum commitment slot. A distinct repeated byte, not a
// recomputed hash: what this channel pins is the *wire* shape — field order
// and encoding — and the semantic weld (`field_hash == hash(concat(chunks))`)
// is pinned by the field-access golden channel and the .ak selectors instead.
const certificateFieldHash = repeatedByte(0x66, 32);

/**
 * Each vector carries three things that have to stay in step: the value the
 * TypeScript producer encodes, the CBOR it produces, and the Aiken expression
 * that reconstructs the same value. The Aiken expression is *rendered from the
 * same inputs* as the value — never transcribed from the hex — so the two sides
 * are independent constructions of one vector rather than one construction and
 * a copy of its output.
 */
const vectors = [
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
        compact_cbor: hex(compactCbor),
        witness_set_compact_cbor: hex(witnessSetCompactCbor),
        chunk_ref_input_indices: [1n, 2n, 3n],
        output_index: 0n,
      },
    },
    aiken: [
      "Certify {",
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
        compact_cbor: hex(smallCompactCbor),
        witness_set_compact_cbor: hex(smallWitnessSetCompactCbor),
        chunk_ref_input_indices: [0n, 1n],
        output_index: 2n,
      },
    },
    aiken: [
      "Certify {",
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
const withTrailingItem = (label) => `${encodedVector(label)}00`;

/**
 * Each negative vector names the layer that must refuse it, because "something
 * threw" is not evidence that the right thing threw. `cbor-parse` means the
 * bytes are not one well-formed CBOR item at all and `cbor.deserialise` must
 * answer `None`; `data-cast` means the bytes parse cleanly and it is the cast
 * into the Aiken type that must fail — for those the generated module also
 * asserts the parse *succeeds*, so the `fail` test cannot pass because the
 * vector happened to be malformed CBOR too.
 *
 * `typescript` records what `Data.from` really does rather than what one would
 * like it to do. Every wrong-shape vector throws. The trailing-bytes vectors do
 * **not**: `Data.from` decodes the leading item and discards the rest, so the
 * off-chain decoder is tolerant exactly where the on-chain one is strict. That
 * asymmetry is the finding; the suite pins it as `tolerates-trailing-bytes` and
 * asserts the re-encoding is shorter than the vector, which is the property
 * that still makes the vector detectably not producer output.
 */
const negativeVectors = [
  {
    label: "carriage_trailing_bytes",
    aikenType: "FieldCarriageV1",
    cborHex: withTrailingItem("carriage_inline_empty_field"),
    reason:
      "a valid `Inline` encoding followed by a complete extra CBOR item; a fail-closed decoder consumes the whole payload or refuses it",
    rejectedBy: { aiken: "cbor-parse", typescript: "tolerates-trailing-bytes" },
  },
  {
    label: "carriage_constructor_index_out_of_range",
    aikenType: "FieldCarriageV1",
    cborHex: Data.to(new Constr(3, [hex(emptyFieldPreimage)])),
    reason:
      "constructor index 3; FieldCarriageV1 declares exactly Inline/RawUtxo/Certified at 0/1/2",
    rejectedBy: { aiken: "data-cast", typescript: "throws" },
  },
  {
    label: "carriage_inline_preimage_as_integer",
    aikenType: "FieldCarriageV1",
    cborHex: Data.to(new Constr(0, [5n])),
    reason:
      "`Inline.preimage` is a ByteArray; this vector puts an Integer there",
    rejectedBy: { aiken: "data-cast", typescript: "throws" },
  },
  {
    label: "carriage_inline_extra_field",
    aikenType: "FieldCarriageV1",
    cborHex: Data.to(
      new Constr(0, [hex(emptyFieldPreimage), hex(chunkedViewChunkB)]),
    ),
    reason:
      "`Inline` has arity 1; a decoder that reads the first field and ignores the rest accepts this",
    rejectedBy: { aiken: "data-cast", typescript: "throws" },
  },
  {
    label: "view_trailing_bytes",
    aikenType: "FieldViewV1",
    cborHex: withTrailingItem("view_whole_empty_field"),
    reason: "a valid `Whole` encoding followed by a complete extra CBOR item",
    rejectedBy: { aiken: "cbor-parse", typescript: "tolerates-trailing-bytes" },
  },
  {
    label: "view_constructor_index_out_of_range",
    aikenType: "FieldViewV1",
    cborHex: Data.to(new Constr(3, [hex(emptyFieldPreimage), 0n, 40n])),
    reason:
      "constructor index 3; FieldViewV1 declares exactly Whole/Chunked/ProvisionalWhole at 0/1/2",
    rejectedBy: { aiken: "data-cast", typescript: "throws" },
  },
  {
    label: "view_whole_missing_stride",
    aikenType: "FieldViewV1",
    cborHex: Data.to(new Constr(0, [hex(emptyFieldPreimage), 0n])),
    reason: "`Whole` has arity 3 (bytes, count, stride); this vector carries 2",
    rejectedBy: { aiken: "data-cast", typescript: "throws" },
  },
  {
    label: "view_whole_count_as_bytes",
    aikenType: "FieldViewV1",
    cborHex: Data.to(
      new Constr(0, [hex(emptyFieldPreimage), hex(chunkedViewChunkB), 40n]),
    ),
    reason: "`Whole.count` is an Integer; this vector puts a ByteArray there",
    rejectedBy: { aiken: "data-cast", typescript: "throws" },
  },
  {
    label: "view_chunked_chunks_as_bytes",
    aikenType: "FieldViewV1",
    cborHex: Data.to(
      new Constr(1, [hex(chunkedViewChunkB), [hex(digestA)], 1n, 40n]),
    ),
    reason:
      "`Chunked.chunks` is a List<ByteArray>; this vector puts a bare ByteArray there",
    rejectedBy: { aiken: "data-cast", typescript: "throws" },
  },
  {
    label: "certificate_trailing_bytes",
    aikenType: "FieldPreimageCertificateV1",
    cborHex: withTrailingItem("certificate_three_chunk_corner"),
    reason:
      "a valid certificate datum followed by a complete extra CBOR item; a datum is one value and a manifest that decodes past its own end is not one",
    rejectedBy: { aiken: "cbor-parse", typescript: "tolerates-trailing-bytes" },
  },
  {
    label: "certificate_missing_chunk_digests",
    aikenType: "FieldPreimageCertificateV1",
    cborHex: Data.to(
      new Constr(0, [
        hex(certificateOwner),
        hex(certificateTxId),
        5n,
        hex(certificateFieldHash),
        32_763n,
      ]),
    ),
    reason:
      "the certificate record has 6 fields; dropping `chunk_digests` is the shape that would let a manifest certify nothing",
    rejectedBy: { aiken: "data-cast", typescript: "throws" },
  },
  {
    label: "certificate_missing_field_hash",
    aikenType: "FieldPreimageCertificateV1",
    cborHex: Data.to(
      new Constr(0, [
        hex(certificateOwner),
        hex(certificateTxId),
        5n,
        32_763n,
        [hex(digestA), hex(digestB), hex(digestC)],
      ]),
    ),
    reason:
      "the pre-#606 5-field shape — a datum without the mint-welded `field_hash` is a certificate the door has no anchored equality to hold, and the frozen wire format refuses it at decode",
    rejectedBy: { aiken: "data-cast", typescript: "throws" },
  },
  {
    label: "certificate_field_index_as_bytes",
    aikenType: "FieldPreimageCertificateV1",
    cborHex: Data.to(
      new Constr(0, [
        hex(certificateOwner),
        hex(certificateTxId),
        "05",
        hex(certificateFieldHash),
        32_763n,
        [hex(digestA), hex(digestB), hex(digestC)],
      ]),
    ),
    reason:
      "`field_index` is an Integer; a one-byte ByteArray with the same value must not pass for it",
    rejectedBy: { aiken: "data-cast", typescript: "throws" },
  },
  {
    label: "mint_redeemer_trailing_bytes",
    aikenType: "FieldPreimageCertificateMintRedeemerV1",
    cborHex: withTrailingItem("mint_redeemer_retire"),
    reason: "a valid `Retire` encoding followed by a complete extra CBOR item",
    rejectedBy: { aiken: "cbor-parse", typescript: "tolerates-trailing-bytes" },
  },
  {
    label: "mint_redeemer_constructor_index_out_of_range",
    aikenType: "FieldPreimageCertificateMintRedeemerV1",
    cborHex: Data.to(new Constr(2, [])),
    reason:
      "constructor index 2; the mint redeemer declares exactly Certify/Retire at 0/1, and a third arm is how a burn-only policy would be talked into minting",
    rejectedBy: { aiken: "data-cast", typescript: "throws" },
  },
  {
    label: "mint_redeemer_certify_indices_as_bytes",
    aikenType: "FieldPreimageCertificateMintRedeemerV1",
    cborHex: Data.to(
      new Constr(0, [
        hex(smallCompactCbor),
        hex(smallWitnessSetCompactCbor),
        hex(chunkedViewChunkB),
        2n,
      ]),
    ),
    reason:
      "`Certify.chunk_ref_input_indices` is a List<Int>; this vector puts a ByteArray there",
    rejectedBy: { aiken: "data-cast", typescript: "throws" },
  },
  {
    label: "mint_redeemer_certify_missing_output_index",
    aikenType: "FieldPreimageCertificateMintRedeemerV1",
    cborHex: Data.to(
      new Constr(0, [
        hex(smallCompactCbor),
        hex(smallWitnessSetCompactCbor),
        [0n, 1n],
      ]),
    ),
    reason:
      "`Certify` has arity 4; without `output_index` the policy has no named output to check",
    rejectedBy: { aiken: "data-cast", typescript: "throws" },
  },
  {
    label: "mint_redeemer_retire_extra_field",
    aikenType: "FieldPreimageCertificateMintRedeemerV1",
    cborHex: Data.to(new Constr(1, [0n])),
    reason:
      "`Retire` has arity 0; a decoder that only checks the constructor index accepts this",
    rejectedBy: { aiken: "data-cast", typescript: "throws" },
  },
];

// ---------------------------------------------------------------------------
// The golden
// ---------------------------------------------------------------------------

const buildGolden = () => ({
  schema: "midgard-native-tx-carriage-wire-golden",
  version: 1,
  specDocument: "docs/spec/midgard-tx.md",
  generator:
    "demo/midgard-sdk/scripts/generate-native-tx-carriage-wire-v1-goldens.mjs",
  constructorIndexes: {
    FieldCarriageV1: FIELD_CARRIAGE_CONSTRUCTOR_INDEXES,
    FieldViewV1: FIELD_VIEW_CONSTRUCTOR_INDEXES,
    FieldPreimageCertificateMintRedeemerV1:
      FIELD_PREIMAGE_CERTIFICATE_MINT_REDEEMER_CONSTRUCTOR_INDEXES,
  },
  vectors: vectors.map((vector) => ({
    label: vector.label,
    aikenType: vector.aikenType,
    value: JSON.parse(
      JSON.stringify(vector.value, (_key, entry) =>
        typeof entry === "bigint" ? `${entry}n` : entry,
      ),
    ),
    cborHex: Data.to(vector.value, vector.schema),
  })),
  negativeVectors: negativeVectors.map((vector) => ({
    label: vector.label,
    aikenType: vector.aikenType,
    cborHex: vector.cborHex,
    reason: vector.reason,
    rejectedBy: vector.rejectedBy,
  })),
});

// ---------------------------------------------------------------------------
// Aiken rendering
// ---------------------------------------------------------------------------

/**
 * `///` doc lines wrapped to the same width the rest of the tree reads at.
 * `aiken fmt` reflows code but never comments, so a generator that emits a
 * paragraph on one line leaves one on one line forever.
 */
const docComment = (text, width = 74) => {
  const lines = [];
  let current = "";
  for (const word of text.split(/\s+/u)) {
    const candidate = current === "" ? word : `${current} ${word}`;
    if (candidate.length + "/// ".length > width && current !== "") {
      lines.push(`/// ${current}`);
      current = word;
    } else {
      current = candidate;
    }
  }
  if (current !== "") {
    lines.push(`/// ${current}`);
  }
  return lines;
};

const section = (title) => [
  "// ---------------------------------------------------------------------------",
  `// ${title}`,
  "// ---------------------------------------------------------------------------",
  "",
];

const renderAiken = (golden) =>
  [
    "//// Generated by",
    `//// ${golden.generator}.`,
    "//// Do not edit; regenerate from the TypeScript twins.",
    "////",
    "//// The §8.6 and §8.8 **wire** goldens: every vector is encoded off-chain by",
    "//// `Data.to` against the SDK schemas, and each test here proves the Aiken",
    "//// side agrees in both directions — it decodes the vector's bytes into the",
    "//// Aiken type and checks the fields, then re-serialises the value it",
    "//// reconstructed and checks the bytes come back identical. A decoder that",
    "//// accepted a shape the producer never emits fails the second half; an",
    "//// encoder that emitted a shape the decoder tolerates fails it too.",
    "////",
    "//// Constructor order is frozen consensus wire format. These vectors are what",
    "//// make that concrete: a reordered `pub type` re-tags every vector at once.",
    "////",
    "//// **The 64-byte boundary.** A Plutus Data byte string stays one definite",
    "//// string up to and including 64 bytes and becomes an indefinite-length",
    "//// string of 64-byte definite chunks strictly above it, so the divergence to",
    "//// fear is a `>=` written where the rule says `>`. Every type whose shape can",
    "//// reach that width carries a crossing vector — `FieldCarriageV1` via",
    "//// `carriage_inline_chunked_preimage`, `FieldViewV1` via",
    "//// `view_chunked_three_chunk_corner`, and",
    "//// `FieldPreimageCertificateMintRedeemerV1` via",
    "//// `mint_redeemer_certify_chunked_arguments` — and `FieldCarriageV1` also",
    "//// pins the boundary itself from below and at it, with the 63-byte (`583f…`)",
    "//// and 64-byte (`5840…`) vectors that must stay definite.",
    "//// `FieldPreimageCertificateV1` is the one exception and is so structurally:",
    "//// every field it declares is fixed-width and at most 32 bytes (owner 28,",
    "//// tx-id 32, each digest 32), so no value of that type can carry a byte",
    "//// string wide enough to chunk and there is no vector to write.",
    "////",
    "//// **Refusals.** The `_rejects` tests below are §9 clause 2's half: payloads a",
    "//// fail-closed decoder must not accept. Each names the layer that refuses it.",
    "//// Trailing-bytes vectors are refused by `cbor.deserialise` itself, which",
    "//// answers `None` unless the payload is exactly one CBOR item, so those are",
    "//// asserted positively as `== None` rather than as a `fail` test that any",
    "//// error would satisfy. Wrong-shape vectors parse cleanly and are refused by",
    "//// the cast into the Aiken type; each of those is a `fail` test paired with a",
    "//// companion test asserting the bytes *do* parse, so the `fail` cannot be",
    "//// passing because the vector was malformed CBOR as well.",
    "",
    "use aiken/cbor",
    "use midgard/native_tx_carriage_v1.{",
    "  Certify, FieldPreimageCertificateMintRedeemerV1, Retire,",
    "}",
    "use midgard/native_tx_field_access_v1.{",
    "  Certified, FieldCarriageV1, FieldPreimageCertificateV1, FieldViewV1, Inline,",
    "  RawUtxo, Whole, Chunked,",
    "}",
    "",
    ...section("Vectors"),
    ...golden.vectors.flatMap((vector, index) => {
      const source = vectors[index];
      return [
        `const ${vector.label}_cbor: ByteArray =`,
        `  ${aikenBytes(vector.cborHex)}`,
        "",
        `fn ${vector.label}_value() -> ${vector.aikenType} {`,
        ...source.aiken.split("\n").map((line) => `  ${line}`),
        "}",
        "",
        `/// ${vector.aikenType} / \`${vector.label}\`: decode the TypeScript`,
        "/// producer's bytes, then re-serialise what came back.",
        `test golden_${vector.label}_round_trips() {`,
        `  expect Some(decoded_data) = cbor.deserialise(${vector.label}_cbor)`,
        `  expect decoded: ${vector.aikenType} = decoded_data`,
        `  let rebuilt: Data = ${vector.label}_value()`,
        "  and {",
        `    decoded == ${vector.label}_value(),`,
        `    cbor.serialise(rebuilt) == ${vector.label}_cbor,`,
        "  }",
        "}",
        "",
      ];
    }),
    ...section("Negative vectors — §9 clause 2"),
    ...golden.negativeVectors.flatMap((vector) => {
      const constant = [
        `const ${vector.label}_cbor: ByteArray =`,
        `  ${aikenBytes(vector.cborHex)}`,
        "",
      ];
      if (vector.rejectedBy.aiken === "cbor-parse") {
        return [
          ...constant,
          ...docComment(
            `${vector.aikenType} / \`${vector.label}\`: ${vector.reason}. Refused before any typing happens — \`cbor.deserialise\` answers \`None\` unless the payload is exactly one CBOR item.`,
          ),
          `test golden_${vector.label}_rejects() {`,
          `  cbor.deserialise(${vector.label}_cbor) == None`,
          "}",
          "",
        ];
      }
      return [
        ...constant,
        ...docComment(
          `\`${vector.label}\` is well-formed CBOR. This is what keeps the \`fail\` test below honest: its refusal has to come from the cast into ${vector.aikenType}, not from the parse.`,
        ),
        `test golden_${vector.label}_parses_as_cbor() {`,
        `  cbor.deserialise(${vector.label}_cbor) != None`,
        "}",
        "",
        ...docComment(
          `${vector.aikenType} / \`${vector.label}\`: ${vector.reason}.`,
        ),
        `test golden_${vector.label}_rejects() fail {`,
        `  expect Some(decoded_data) = cbor.deserialise(${vector.label}_cbor)`,
        `  expect _decoded: ${vector.aikenType} = decoded_data`,
        "  True",
        "}",
        "",
      ];
    }),
  ].join("\n");

// ---------------------------------------------------------------------------
// Emission
// ---------------------------------------------------------------------------

const golden = buildGolden();
writeOrCheck(generatedJsonPath, `${JSON.stringify(golden, null, 2)}\n`);
writeOrCheck(
  generatedAikenPath,
  formatAikenSource({
    source: renderAiken(golden),
    fileName: "native-tx-carriage-wire-v1-golden.test.ak",
    repositoryRoot,
    tmpPrefix: "midgard-574-aiken-format-",
  }),
);
