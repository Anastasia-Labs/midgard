#!/usr/bin/env node

/**
 * Produces the cross-language golden vectors for the shared field-access
 * surface of `docs/spec/midgard-tx.md` — §4's flat commitment, §5.1's enveloped
 * preimage grammar, §5.3's stride table, §8.3's carriage constants, §8.4's
 * deterministic chunk split and §8.6's certificate asset name.
 *
 * Every value is computed by the TypeScript twin
 * (`demo/midgard-core/src/codec/native-tx-field-access-v1.ts`) and written to
 * two places:
 *
 *   * `demo/midgard-core/tests/fixtures/native-tx-field-access-v1.generated.json`
 *     — recomputed by `tests/native-tx-field-access-v1-goldens.test.ts`, so a
 *     drifting twin fails on the TypeScript side; and
 *   * `onchain/aiken/lib/midgard/native-tx-field-access-v1-golden.test.ak`
 *     — recomputed by the Aiken producers under the fork runner, so a
 *     divergence between the two encoders fails on the Aiken side.
 *
 * That pair is the channel: one generator, one JSON fixture, one Aiken golden
 * module. The per-field item encodings of §5.3 fan out over the same three
 * artifacts (issue #569); nothing here is per-field.
 *
 * Vectors are regenerated, never hand-edited. Run with `--check` to assert the
 * checked-in artifacts are exactly what the twin produces today.
 *
 * usage: node scripts/generate-native-tx-field-access-v1-goldens.mjs [--check]
 */

import { spawnSync } from "node:child_process";
import {
  mkdirSync,
  mkdtempSync,
  readFileSync,
  rmSync,
  writeFileSync,
} from "node:fs";
import { tmpdir } from "node:os";
import { dirname, join, relative, resolve } from "node:path";
import { fileURLToPath } from "node:url";

import {
  decodeMidgardFieldArrayHeaderV1,
  deriveMidgardFieldPreimageCertificateV1,
  encodeMidgardDefiniteBytesV1,
  encodeMidgardFieldArrayHeaderV1,
  encodeMidgardFieldPreimageV1,
  MIDGARD_ADDRESS_WITNESS_ITEM_BYTES_V1,
  MIDGARD_CHUNK_BYTES_K_V1,
  MIDGARD_EMPTY_FIELD_COMMITMENT_V1,
  MIDGARD_FIELD_CARRIAGE_CONSTRUCTORS_V1,
  MIDGARD_FIELD_COUNT_V1,
  MIDGARD_FIELD_VIEW_CONSTRUCTORS_V1,
  MIDGARD_HASH28_ITEM_BYTES_V1,
  MIDGARD_MAX_FIELD_ITEM_COUNT_V1,
  MIDGARD_MAX_SPEND_INPUTS_PREIMAGE_BYTES_V1,
  MIDGARD_MAX_TIER1_REDEEMER_PREIMAGE_BYTES_V1,
  MIDGARD_MAX_TIER3_CHUNK_COUNT_V1,
  MIDGARD_MAX_TRANSACTION_AGGREGATE_FIELD_BYTES_V1,
  MIDGARD_MAXIMUM_CARDANO_SPEND_REDEEMER_COUNT_V1,
  MIDGARD_SPEND_INPUT_ITEM_BYTES_V1,
  midgardExpectedChunkCountV1,
  midgardFieldCommitmentFromItemsV1,
  midgardFieldCommitmentV1,
  midgardFieldItemExtentV1,
  midgardFieldPreimageCertificateAssetNameV1,
  midgardFieldStrideV1,
  buildMidgardWholeFieldViewV1,
  splitMidgardFieldPreimageIntoChunksV1,
} from "../dist/codec/native-tx-field-access-v1.js";

const scriptDirectory = dirname(fileURLToPath(import.meta.url));
const packageRoot = resolve(scriptDirectory, "..");
const repositoryRoot = resolve(packageRoot, "../..");
const generatedJsonPath = join(
  packageRoot,
  "tests/fixtures/native-tx-field-access-v1.generated.json",
);
const generatedAikenPath = join(
  repositoryRoot,
  "onchain/aiken/lib/midgard/native-tx-field-access-v1-golden.test.ak",
);

const commandArguments = process.argv.slice(2);
if (
  commandArguments.length > 1 ||
  (commandArguments.length === 1 && commandArguments[0] !== "--check")
) {
  console.error(
    "usage: node scripts/generate-native-tx-field-access-v1-goldens.mjs [--check]",
  );
  process.exit(2);
}
const checkOnly = commandArguments.length === 1;

const hex = (value) => Buffer.from(value).toString("hex");
const aikenBytes = (hexValue) => `#"${hexValue}"`;

/**
 * A deterministic filler so a vector's payload bytes are reproducible from its
 * declared length alone. Byte `i` is `(i * 7 + 3) mod 256`: never all-zero,
 * never a repeating single byte, so a length/offset slip cannot pass unnoticed.
 */
const filler = (length, seed = 0) =>
  Buffer.from(
    Array.from({ length }, (_, index) => (index * 7 + 3 + seed * 31) % 256),
  );

/**
 * Rebuilds `length` bytes by repeating `block`.
 *
 * `filler` has period 256 — byte `i` is `(7i + 3 + 31·seed) mod 256` and 7 is a
 * unit mod 256 — so repeating `filler(256, seed)` reproduces `filler(length,
 * seed)` byte for byte. That identity is what lets the §8.4 tier-3 vector pin a
 * 16 KiB payload in 256 bytes: both languages rebuild the payload from the
 * block and hash the chunks for themselves, rather than each trusting a digest
 * only the other side computed.
 */
const repeatToLength = (block, length) => {
  const whole = Math.floor(length / block.length);
  return Buffer.concat([
    ...Array.from({ length: whole }, () => block),
    block.subarray(0, length - whole * block.length),
  ]);
};

// ---------------------------------------------------------------------------
// Vectors
// ---------------------------------------------------------------------------

/** §5.1 `definite_array_header(N)` at every width boundary the grammar admits. */
const ARRAY_HEADER_COUNTS = [0, 1, 23, 24, 255, 256, 65535];

/** §5.1 `definite_bytes_header(L)` at every width boundary. */
const ITEM_WRAPPER_PAYLOAD_LENGTHS = [0, 1, 23, 24, 255, 256];

/**
 * §5.3 fixed-stride item shapes. The item *bytes* are supplied here as vector
 * data rather than produced by a per-field encoder — the per-field encoders and
 * their vectors are #569's fan-out. What these vectors pin is the shared
 * machinery: the §5.1 envelope over fixed-width items, the resulting stride,
 * and the wrapper each accessor must read back (§7.2).
 */
const spendInputItem = (txIdSeed, outputIndex) => {
  const head = Buffer.from([0x82, 0x58, 0x20]);
  const txId = filler(32, txIdSeed);
  const index = Buffer.alloc(3);
  index[0] = 0x19;
  index.writeUInt16BE(outputIndex, 1);
  return Buffer.concat([head, txId, index]);
};

const addressWitnessItem = (seed) =>
  Buffer.concat([
    Buffer.from([0x82, 0x58, 0x20]),
    filler(32, seed),
    Buffer.from([0x58, 0x40]),
    filler(64, seed + 1),
  ]);

const PREIMAGE_VECTORS = [
  // `empty_envelope`, not `empty_field`: the standalone
  // `golden_empty_field_commitment` constant already owns that name in the
  // generated Aiken module, and two `const`s of one name do not compile.
  { label: "empty_envelope", fieldIndex: 2, items: [] },
  ...ITEM_WRAPPER_PAYLOAD_LENGTHS.map((length) => ({
    label: `single_item_payload_${length}`,
    fieldIndex: 2,
    items: [filler(length, length)],
  })),
  {
    label: "variable_width_walk",
    fieldIndex: 2,
    items: [filler(1, 1), filler(24, 2), filler(300, 3)],
  },
  {
    // §5.3 fields 0/1: the fixed 3-byte index makes every item 38 bytes at
    // stride 40. Indices 0, 23 and 65,535 are the §9 boundary values.
    label: "spend_inputs_stride_40",
    fieldIndex: 0,
    items: [
      spendInputItem(1, 0),
      spendInputItem(2, 23),
      spendInputItem(3, 65535),
    ],
  },
  {
    // §5.3 fields 3/4: raw 28-byte hashes, stride 30.
    label: "hash28_stride_30",
    fieldIndex: 3,
    items: [filler(28, 4), filler(28, 5)],
  },
  {
    // §5.3 field 7: 101-byte items, stride 103.
    label: "address_witness_stride_103",
    fieldIndex: 7,
    items: [addressWitnessItem(6), addressWitnessItem(8)],
  },
  {
    // The §5.1 two-byte header boundary reached with real items: 24 items of
    // 28 bytes at stride 30 gives `98 18 ‖ 24·(58 1c ‖ 28 B)`.
    label: "hash28_two_byte_header",
    fieldIndex: 3,
    items: Array.from({ length: 24 }, (_, index) => filler(28, index + 9)),
  },
];

/** §8.4's split rule at and around the tier boundary, plus the §5.4 cap. */
const CHUNK_COUNT_TOTAL_LENGTHS = [
  MIDGARD_CHUNK_BYTES_K_V1,
  MIDGARD_CHUNK_BYTES_K_V1 + 1,
  2 * MIDGARD_CHUNK_BYTES_K_V1,
  2 * MIDGARD_CHUNK_BYTES_K_V1 + 1,
  MIDGARD_MAX_TRANSACTION_AGGREGATE_FIELD_BYTES_V1,
];

/** §8.6. One fixed transaction id across all nine field indices. */
const CERTIFICATE_TX_ID = filler(32, 42);

/**
 * §8.4's tier-3 vector: a real preimage of one full chunk plus a ragged tail.
 * Its payload is `TIER3_PREIMAGE_BLOCK` repeated to `TIER3_TOTAL_LENGTH`, and
 * the block — not the 16 KiB it expands to — is what the artifacts carry, so
 * both languages can expand it and recompute `blake2b_256(chunk_j)` for
 * themselves.
 */
const TIER3_TOTAL_LENGTH = MIDGARD_CHUNK_BYTES_K_V1 + 517;
const TIER3_PREIMAGE_BLOCK = filler(256, 77);

const buildGolden = () => {
  const preimages = PREIMAGE_VECTORS.map((vector) => {
    const preimage = encodeMidgardFieldPreimageV1(vector.items);
    const commitment = midgardFieldCommitmentV1(preimage);
    const view = buildMidgardWholeFieldViewV1({
      fieldIndex: vector.fieldIndex,
      preimage,
      expectedCommitment: commitment,
    });
    return {
      label: vector.label,
      fieldIndex: vector.fieldIndex,
      stride: midgardFieldStrideV1(vector.fieldIndex),
      itemCount: vector.items.length,
      itemsHex: vector.items.map(hex),
      preimageHex: hex(preimage),
      commitmentHex: hex(commitment),
      itemExtents: vector.items.map((_, index) => {
        const extent = midgardFieldItemExtentV1(view, index);
        return { offset: extent.offset, length: extent.length };
      }),
    };
  });

  // A real tier-3 preimage: one full chunk plus a ragged tail. What the
  // artifacts carry is the 256-byte period the payload repeats, not the 16 KiB
  // itself — so each side rebuilds the payload, splits it by its own §8.4 rule
  // and recomputes every chunk digest, instead of one side pinning digests the
  // other only counts.
  const tier3Preimage = repeatToLength(TIER3_PREIMAGE_BLOCK, TIER3_TOTAL_LENGTH);
  // The block is a period of `filler`, so the compaction is lossless: these are
  // the same bytes `filler(TIER3_TOTAL_LENGTH, 77)` has always produced.
  if (!tier3Preimage.equals(filler(TIER3_TOTAL_LENGTH, 77))) {
    throw new Error(
      "tier-3 payload block is not a period of the filler it stands in for",
    );
  }
  const tier3Chunks = splitMidgardFieldPreimageIntoChunksV1(tier3Preimage);
  const tier3Certificate = deriveMidgardFieldPreimageCertificateV1({
    owner: filler(28, 11),
    txId: CERTIFICATE_TX_ID,
    fieldIndex: 0,
    preimage: tier3Preimage,
  });

  return {
    schema: "midgard-native-tx-field-access-v1-golden",
    version: 1,
    specDocument: "docs/spec/midgard-tx.md",
    generator:
      "demo/midgard-core/scripts/generate-native-tx-field-access-v1-goldens.mjs",
    constants: {
      fieldCount: MIDGARD_FIELD_COUNT_V1,
      maxFieldItemCount: MIDGARD_MAX_FIELD_ITEM_COUNT_V1,
      maxTransactionAggregateFieldBytes:
        MIDGARD_MAX_TRANSACTION_AGGREGATE_FIELD_BYTES_V1,
      maxSpendInputsPreimageBytes: MIDGARD_MAX_SPEND_INPUTS_PREIMAGE_BYTES_V1,
      maximumCardanoSpendRedeemerCount:
        MIDGARD_MAXIMUM_CARDANO_SPEND_REDEEMER_COUNT_V1,
      chunkBytesK: MIDGARD_CHUNK_BYTES_K_V1,
      maxTier1RedeemerPreimageBytes:
        MIDGARD_MAX_TIER1_REDEEMER_PREIMAGE_BYTES_V1,
      maxTier3ChunkCount: MIDGARD_MAX_TIER3_CHUNK_COUNT_V1,
      // §5.3 item widths. The *strides* are what the encoders spend, and they
      // are pinned by the stride table below; these are the payload widths the
      // strides are built from, twinned constant-for-constant with the Aiken
      // module and otherwise referenced nowhere — exactly the shape a value can
      // drift in unnoticed, which is why they belong in the channel too.
      spendInputItemBytes: MIDGARD_SPEND_INPUT_ITEM_BYTES_V1,
      hash28ItemBytes: MIDGARD_HASH28_ITEM_BYTES_V1,
      addressWitnessItemBytes: MIDGARD_ADDRESS_WITNESS_ITEM_BYTES_V1,
      carriageConstructors: [...MIDGARD_FIELD_CARRIAGE_CONSTRUCTORS_V1],
      viewConstructors: [...MIDGARD_FIELD_VIEW_CONSTRUCTORS_V1],
    },
    emptyFieldCommitmentHex: hex(MIDGARD_EMPTY_FIELD_COMMITMENT_V1),
    strides: Array.from({ length: MIDGARD_FIELD_COUNT_V1 }, (_, fieldIndex) =>
      midgardFieldStrideV1(fieldIndex),
    ),
    arrayHeaders: ARRAY_HEADER_COUNTS.map((count) => {
      const header = encodeMidgardFieldArrayHeaderV1(count);
      const decoded = decodeMidgardFieldArrayHeaderV1(header);
      return {
        count,
        headerHex: hex(header),
        headerLength: decoded.nextOffset,
      };
    }),
    itemWrappers: ITEM_WRAPPER_PAYLOAD_LENGTHS.map((payloadLength) => ({
      payloadLength,
      wrapperHex: hex(
        encodeMidgardDefiniteBytesV1(filler(payloadLength, payloadLength)),
      ).slice(0, payloadLength <= 23 ? 2 : payloadLength <= 255 ? 4 : 6),
    })),
    preimages,
    chunkCounts: CHUNK_COUNT_TOTAL_LENGTHS.map((totalLength) => ({
      totalLength,
      chunkCount: midgardExpectedChunkCountV1(totalLength),
    })),
    tier3Certificate: {
      txIdHex: hex(tier3Certificate.txId),
      ownerHex: hex(tier3Certificate.owner),
      fieldIndex: tier3Certificate.fieldIndex,
      totalLength: tier3Certificate.totalLength,
      // The payload's 256-byte period. Both sides expand it to `totalLength`
      // and hash the resulting chunks, so `chunkDigestsHex` below is a
      // recomputed value on either side of the channel, never a bare literal.
      preimageBlockHex: hex(TIER3_PREIMAGE_BLOCK),
      chunkLengths: tier3Chunks.map((chunk) => chunk.length),
      chunkDigestsHex: tier3Certificate.chunkDigests.map(hex),
      assetNameHex: hex(
        midgardFieldPreimageCertificateAssetNameV1({
          txId: tier3Certificate.txId,
          fieldIndex: tier3Certificate.fieldIndex,
        }),
      ),
    },
    certificateAssetNames: Array.from(
      { length: MIDGARD_FIELD_COUNT_V1 },
      (_, fieldIndex) => ({
        fieldIndex,
        txIdHex: hex(CERTIFICATE_TX_ID),
        assetNameHex: hex(
          midgardFieldPreimageCertificateAssetNameV1({
            txId: CERTIFICATE_TX_ID,
            fieldIndex,
          }),
        ),
      }),
    ),
  };
};

// ---------------------------------------------------------------------------
// Aiken rendering
// ---------------------------------------------------------------------------

const section = (title) => [
  "// ---------------------------------------------------------------------------",
  `// ${title}`,
  "// ---------------------------------------------------------------------------",
  "",
];

const headerLengthForCount = (count) =>
  count <= 23 ? 1 : count <= 255 ? 2 : 3;

const renderAiken = (golden) => {
  const fillerOf = (payloadLength) =>
    golden.preimages.find(
      (vector) => vector.label === `single_item_payload_${payloadLength}`,
    ).itemsHex[0];

  return [
    `// Generated by ${golden.generator}.`,
    "// Do not edit; regenerate from the TypeScript twin.",
    "//",
    "// Cross-language golden vectors for the shared field-access surface of",
    `// \`${golden.specDocument}\` — §4's flat commitment, §5.1's enveloped preimage`,
    "// grammar, §5.3's stride table, §8.3's carriage constants, §8.4's deterministic",
    "// chunk split and §8.6's certificate asset name.",
    "//",
    "// Every constant below was produced by the TypeScript twin",
    "// (`demo/midgard-core/src/codec/native-tx-field-access-v1.ts`) and is recomputed",
    "// here by the Aiken producers, so the two encoders cannot silently diverge. The",
    "// nine per-field item encodings of §5.3 fan out over this same module in #569.",
    "",
    "use aiken/collection/list",
    "use aiken/crypto.{blake2b_256}",
    "use aiken/primitive/bytearray",
    "use midgard/fraud_proofs/native_tx/codec.{encode_definite_bytes}",
    "use midgard/native_tx_field_access_v1.{",
    "  Whole, address_witness_item_bytes, chunk_bytes_k,",
    "  decode_field_array_header, empty_field_commitment,",
    "  encode_field_array_header, encode_field_preimage, expected_chunk_count,",
    "  field_commitment, field_commitment_from_items, field_item_at,",
    "  field_item_extent, field_preimage_certificate_asset_name, field_stride,",
    "  hash28_item_bytes, max_field_item_count, max_spend_inputs_preimage_bytes,",
    "  max_tier1_redeemer_preimage_bytes, max_tier3_chunk_count,",
    "  max_transaction_aggregate_field_bytes,",
    "  maximum_cardano_spend_redeemer_count, spend_input_item_bytes,",
    "}",
    "",
    ...section("Generated constants"),
    `const golden_empty_field_commitment = ${aikenBytes(golden.emptyFieldCommitmentHex)}`,
    "",
    `const golden_certificate_tx_id = ${aikenBytes(golden.certificateAssetNames[0].txIdHex)}`,
    "",
    ...golden.itemWrappers.flatMap((entry) => [
      `const golden_filler_${entry.payloadLength} = ${aikenBytes(fillerOf(entry.payloadLength))}`,
      "",
    ]),
    `const golden_tier3_preimage_block = ${aikenBytes(golden.tier3Certificate.preimageBlockHex)}`,
    "",
    `const golden_tier3_total_length: Int = ${golden.tier3Certificate.totalLength}`,
    "",
    "const golden_tier3_chunk_digests: List<ByteArray> = [",
    ...golden.tier3Certificate.chunkDigestsHex.map(
      (digest) => `  ${aikenBytes(digest)},`,
    ),
    "]",
    "",
    ...golden.preimages.flatMap((vector) => {
      const prefix = `golden_${vector.label}`;
      return [
        `const ${prefix}_items: List<ByteArray> = [`,
        ...vector.itemsHex.map((item) => `  ${aikenBytes(item)},`),
        "]",
        "",
        `const ${prefix}_preimage = ${aikenBytes(vector.preimageHex)}`,
        "",
        `const ${prefix}_commitment = ${aikenBytes(vector.commitmentHex)}`,
        "",
        // A vector with no items has no extent or read assertions, so emitting a
        // view for it would leave an unused private constant behind — an Aiken
        // warning, and this module's warning count is part of its evidence.
        ...(vector.itemCount === 0
          ? []
          : [
              `const ${prefix}_view = Whole {`,
              `  bytes: ${prefix}_preimage,`,
              `  count: ${vector.itemCount},`,
              `  stride: ${vector.stride},`,
              "}",
              "",
            ]),
      ];
    }),
    ...section("§5.3/§5.4/§8.3 carriage, bound and item-width constants"),
    "test golden_carriage_constants_match_typescript() {",
    "  and {",
    `    max_field_item_count == ${golden.constants.maxFieldItemCount},`,
    `    max_transaction_aggregate_field_bytes == ${golden.constants.maxTransactionAggregateFieldBytes},`,
    `    max_spend_inputs_preimage_bytes == ${golden.constants.maxSpendInputsPreimageBytes},`,
    `    maximum_cardano_spend_redeemer_count == ${golden.constants.maximumCardanoSpendRedeemerCount},`,
    `    chunk_bytes_k == ${golden.constants.chunkBytesK},`,
    `    max_tier1_redeemer_preimage_bytes == ${golden.constants.maxTier1RedeemerPreimageBytes},`,
    `    max_tier3_chunk_count == ${golden.constants.maxTier3ChunkCount},`,
    `    spend_input_item_bytes == ${golden.constants.spendInputItemBytes},`,
    `    hash28_item_bytes == ${golden.constants.hash28ItemBytes},`,
    `    address_witness_item_bytes == ${golden.constants.addressWitnessItemBytes},`,
    "  }",
    "}",
    "",
    ...section("§4 the empty-field commitment — field-independent, plain hashing"),
    "test golden_empty_field_commitment_matches_typescript() {",
    "  and {",
    '    encode_field_preimage([]) == #"80",',
    '    field_commitment(#"80") == golden_empty_field_commitment,',
    "    field_commitment_from_items([]) == golden_empty_field_commitment,",
    "    empty_field_commitment == golden_empty_field_commitment,",
    "  }",
    "}",
    "",
    ...section("§5.3 stride table"),
    "test golden_field_strides_match_typescript() {",
    "  and {",
    ...golden.strides.map(
      (stride, fieldIndex) => `    field_stride(${fieldIndex}) == ${stride},`,
    ),
    "  }",
    "}",
    "",
    ...section("§5.1 array headers — minimal width, capped at `99 NNNN`"),
    "test golden_field_array_headers_match_typescript() {",
    "  and {",
    ...golden.arrayHeaders.map(
      (entry) =>
        `    encode_field_array_header(${entry.count}) == ${aikenBytes(entry.headerHex)},`,
    ),
    "  }",
    "}",
    "",
    "test golden_field_array_header_decode_matches_typescript() {",
    "  and {",
    ...golden.arrayHeaders.map(
      (entry) =>
        `    decode_field_array_header(${aikenBytes(entry.headerHex)}) == Pair(${entry.headerLength}, ${entry.count}),`,
    ),
    "  }",
    "}",
    "",
    ...section(
      "§5.1 item wrappers — the two bytes every accessor must read back (§7.2)",
    ),
    "test golden_item_wrappers_match_typescript() {",
    "  and {",
    ...golden.itemWrappers.map(
      (entry) =>
        `    encode_definite_bytes(golden_filler_${entry.payloadLength}) == bytearray.concat(${aikenBytes(entry.wrapperHex)}, golden_filler_${entry.payloadLength}),`,
    ),
    "  }",
    "}",
    "",
    ...section("§8.4 deterministic split — `ceil(total_length / K)`"),
    "test golden_expected_chunk_counts_match_typescript() {",
    "  and {",
    ...golden.chunkCounts.map(
      (entry) =>
        `    expected_chunk_count(${entry.totalLength}) == ${entry.chunkCount},`,
    ),
    "  }",
    "}",
    "",
    ...section("§8.4 tier-3 payload — rebuilt here, not read out of the vector"),
    "/// The tier-3 vector's payload: `golden_tier3_preimage_block` repeated to",
    "/// `golden_tier3_total_length` bytes. The artifacts carry the 256-byte period,",
    "/// not the 16 KiB it expands to, and each language expands it for itself — so",
    "/// the §8.6 digests below are recomputed on both sides of the channel rather",
    "/// than pinned on one and counted on the other.",
    "fn golden_tier3_preimage() -> ByteArray {",
    "  let block_length = bytearray.length(golden_tier3_preimage_block)",
    "  let whole = golden_tier3_total_length / block_length",
    "  bytearray.concat(",
    '    golden_repeat_block(golden_tier3_preimage_block, whole, #""),',
    "    bytearray.take(",
    "      golden_tier3_preimage_block,",
    "      golden_tier3_total_length - whole * block_length,",
    "    ),",
    "  )",
    "}",
    "",
    "fn golden_repeat_block(",
    "  block: ByteArray,",
    "  times: Int,",
    "  acc: ByteArray,",
    ") -> ByteArray {",
    "  if times <= 0 {",
    "    acc",
    "  } else {",
    "    golden_repeat_block(block, times - 1, bytearray.concat(acc, block))",
    "  }",
    "}",
    "",
    "/// §8.4's split rule applied by this module rather than copied from the",
    "/// vector: chunk `j` is bytes `[j·K, (j+1)·K)`, with a ragged last chunk.",
    "fn golden_tier3_chunks(preimage: ByteArray) -> List<ByteArray> {",
    "  let count = expected_chunk_count(bytearray.length(preimage))",
    "  list.map(",
    "    list.range(0, count - 1),",
    "    fn(index) {",
    "      preimage",
    "        |> bytearray.drop(index * chunk_bytes_k)",
    "        |> bytearray.take(chunk_bytes_k)",
    "    },",
    "  )",
    "}",
    "",
    "test golden_tier3_chunk_digests_match_typescript() {",
    "  let preimage = golden_tier3_preimage()",
    "  let chunks = golden_tier3_chunks(preimage)",
    "  and {",
    `    bytearray.length(preimage) == ${golden.tier3Certificate.totalLength},`,
    `    list.map(chunks, bytearray.length) == [${golden.tier3Certificate.chunkLengths.join(", ")}],`,
    "    list.map(chunks, blake2b_256) == golden_tier3_chunk_digests,",
    "  }",
    "}",
    "",
    ...section("§8.6 certificate asset name — blake2b_256(field_index ‖ tx_id)"),
    "test golden_certificate_asset_names_match_typescript() {",
    "  and {",
    ...golden.certificateAssetNames.map(
      (entry) =>
        `    field_preimage_certificate_asset_name(golden_certificate_tx_id, ${entry.fieldIndex}) == ${aikenBytes(entry.assetNameHex)},`,
    ),
    "  }",
    "}",
    "",
    "/// The tier-3 manifest a TypeScript publisher derives for a preimage of",
    `/// ${golden.tier3Certificate.totalLength} bytes: chunk lengths ${golden.tier3Certificate.chunkLengths.join(" + ")}, one`,
    "/// digest per chunk, and the §8.6 token name. This test pins the manifest's",
    "/// shape; the digest *values* are recomputed from the rebuilt payload by",
    "/// `golden_tier3_chunk_digests_match_typescript` above.",
    "test golden_tier3_certificate_shape_matches_typescript() {",
    "  and {",
    `    expected_chunk_count(${golden.tier3Certificate.totalLength}) == ${golden.tier3Certificate.chunkLengths.length},`,
    `    list.length(golden_tier3_chunk_digests) == ${golden.tier3Certificate.chunkLengths.length},`,
    `    field_preimage_certificate_asset_name(golden_certificate_tx_id, ${golden.tier3Certificate.fieldIndex}) == ${aikenBytes(golden.tier3Certificate.assetNameHex)},`,
    "  }",
    "}",
    "",
    ...section("§5.1 preimages, §4 commitments and §7.2 item extents"),
    ...golden.preimages.flatMap((vector) => {
      const prefix = `golden_${vector.label}`;
      return [
        `test ${prefix}_matches_typescript() {`,
        "  and {",
        `    encode_field_preimage(${prefix}_items) == ${prefix}_preimage,`,
        `    field_commitment(${prefix}_preimage) == ${prefix}_commitment,`,
        `    field_commitment_from_items(${prefix}_items) == ${prefix}_commitment,`,
        `    decode_field_array_header(${prefix}_preimage) == Pair(${headerLengthForCount(vector.itemCount)}, ${vector.itemCount}),`,
        ...vector.itemExtents.map(
          (extent, index) =>
            `    field_item_extent(${prefix}_view, ${index}) == Pair(${extent.offset}, ${extent.length}),`,
        ),
        ...vector.itemsHex.map(
          (item, index) =>
            `    field_item_at(${prefix}_view, ${index}) == ${aikenBytes(item)},`,
        ),
        "  }",
        "}",
        "",
      ];
    }),
  ].join("\n");
};

// ---------------------------------------------------------------------------
// Emission
// ---------------------------------------------------------------------------

const formatAiken = (source) => {
  const directory = mkdtempSync(join(tmpdir(), "midgard-568-aiken-format-"));
  const target = join(directory, "native-tx-field-access-v1-golden.test.ak");
  const aikenBinary = process.env.MIDGARD_AIKEN_BIN ?? "aiken";
  try {
    writeFileSync(target, source, "utf8");
    const result = spawnSync(aikenBinary, ["fmt", target], {
      cwd: join(repositoryRoot, "onchain/aiken"),
      encoding: "utf8",
    });
    if (result.status !== 0) {
      throw new Error(
        `Aiken formatter (${aikenBinary}) failed: ${result.error?.message ?? result.stderr.trim()}`,
      );
    }
    return readFileSync(target, "utf8");
  } finally {
    rmSync(directory, { force: true, recursive: true });
  }
};

const writeOrCheck = (target, expected) => {
  const relativePath = relative(repositoryRoot, target);
  if (checkOnly) {
    let actual;
    try {
      actual = readFileSync(target, "utf8");
    } catch {
      throw new Error(`missing generated artifact: ${relativePath}`);
    }
    if (actual !== expected) {
      throw new Error(`stale generated artifact: ${relativePath}`);
    }
    console.log(`checked ${relativePath}`);
    return;
  }
  mkdirSync(dirname(target), { recursive: true });
  writeFileSync(target, expected, "utf8");
  console.log(`wrote ${relativePath}`);
};

const golden = buildGolden();
writeOrCheck(generatedJsonPath, `${JSON.stringify(golden, null, 2)}\n`);
writeOrCheck(generatedAikenPath, formatAiken(renderAiken(golden)));
