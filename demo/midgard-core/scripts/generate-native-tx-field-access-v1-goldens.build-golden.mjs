import { dirname, join, resolve } from "node:path";
import { fileURLToPath } from "node:url";

import {
  buildMidgardWholeFieldView,
  decodeMidgardFieldArrayHeader,
  deriveMidgardFieldPreimageCertificate,
  encodeMidgardDefiniteBytes,
  encodeMidgardFieldArrayHeader,
  encodeMidgardFieldPreimage,
  MIDGARD_ADDRESS_WITNESS_ITEM_BYTES,
  MIDGARD_CHUNK_BYTES_K,
  MIDGARD_EMPTY_FIELD_COMMITMENT,
  MIDGARD_FIELD_CARRIAGE_CONSTRUCTORS,
  MIDGARD_FIELD_COUNT,
  MIDGARD_FIELD_PREIMAGE_CERTIFICATE_ASSET_NAME,
  MIDGARD_FIELD_VIEW_CONSTRUCTORS,
  MIDGARD_HASH28_ITEM_BYTES,
  MIDGARD_MAX_FIELD_ITEM_COUNT,
  MIDGARD_MAX_SPEND_INPUTS_PREIMAGE_BYTES,
  MIDGARD_MAX_TIER1_REDEEMER_PREIMAGE_BYTES,
  MIDGARD_MAX_TIER3_CHUNK_COUNT,
  MIDGARD_MAX_TRANSACTION_AGGREGATE_FIELD_BYTES,
  MIDGARD_MAXIMUM_CARDANO_SPEND_REDEEMER_COUNT,
  MIDGARD_SPEND_INPUT_ITEM_BYTES,
  midgardExpectedChunkCount,
  midgardFieldCommitment,
  midgardFieldItemExtent,
  midgardFieldStride,
  splitMidgardFieldPreimageIntoChunks,
} from "../dist/codec/native-tx-field-access.js";
import {
  goldenChannelEmitter,
  hex,
  parseGoldenChannelArguments,
} from "./golden-channel.mjs";

const scriptDirectory = dirname(fileURLToPath(import.meta.url));

const packageRoot = resolve(scriptDirectory, "..");

export const repositoryRoot = resolve(packageRoot, "../..");

export const generatedJsonPath = join(
  packageRoot,
  "tests/fixtures/native-tx-field-access-v1.generated.json",
);

export const generatedAikenPath = join(
  repositoryRoot,
  "onchain/aiken/lib/midgard/native-tx-field-access-v1-golden.test.ak",
);

const { checkOnly } = parseGoldenChannelArguments(
  "usage: node scripts/generate-native-tx-field-access-v1-goldens.mjs [--check]",
);

export const writeOrCheck = goldenChannelEmitter({ repositoryRoot, checkOnly });

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
  MIDGARD_CHUNK_BYTES_K,
  MIDGARD_CHUNK_BYTES_K + 1,
  2 * MIDGARD_CHUNK_BYTES_K,
  2 * MIDGARD_CHUNK_BYTES_K + 1,
  MIDGARD_MAX_TRANSACTION_AGGREGATE_FIELD_BYTES,
];

/** §8.6. A fixed transaction id for the tier-3 certificate vector. */
const CERTIFICATE_TX_ID = filler(32, 42);

/**
 * §8.4's tier-3 vector: a real preimage of one full chunk plus a ragged tail.
 * Its payload is `TIER3_PREIMAGE_BLOCK` repeated to `TIER3_TOTAL_LENGTH`, and
 * the block — not the 16 KiB it expands to — is what the artifacts carry, so
 * both languages can expand it and recompute `blake2b_256(chunk_j)` for
 * themselves.
 */
const TIER3_TOTAL_LENGTH = MIDGARD_CHUNK_BYTES_K + 517;

export const TIER3_PREIMAGE_BLOCK = filler(256, 77);

export const buildGolden = () => {
  const preimages = PREIMAGE_VECTORS.map((vector) => {
    const preimage = encodeMidgardFieldPreimage(vector.items);
    const commitment = midgardFieldCommitment(preimage);
    const view = buildMidgardWholeFieldView({
      fieldIndex: vector.fieldIndex,
      preimage,
      expectedCommitment: commitment,
    });
    return {
      label: vector.label,
      fieldIndex: vector.fieldIndex,
      stride: midgardFieldStride(vector.fieldIndex),
      itemCount: vector.items.length,
      itemsHex: vector.items.map(hex),
      preimageHex: hex(preimage),
      commitmentHex: hex(commitment),
      itemExtents: vector.items.map((_, index) => {
        const extent = midgardFieldItemExtent(view, index);
        return { offset: extent.offset, length: extent.length };
      }),
    };
  });

  // A real tier-3 preimage: one full chunk plus a ragged tail. What the
  // artifacts carry is the 256-byte period the payload repeats, not the 16 KiB
  // itself — so each side rebuilds the payload, splits it by its own §8.4 rule
  // and recomputes every chunk digest, instead of one side pinning digests the
  // other only counts.
  const tier3Preimage = repeatToLength(
    TIER3_PREIMAGE_BLOCK,
    TIER3_TOTAL_LENGTH,
  );
  // The block is a period of `filler`, so the compaction is lossless: these are
  // the same bytes `filler(TIER3_TOTAL_LENGTH, 77)` has always produced.
  if (!tier3Preimage.equals(filler(TIER3_TOTAL_LENGTH, 77))) {
    throw new Error(
      "tier-3 payload block is not a period of the filler it stands in for",
    );
  }
  const tier3Chunks = splitMidgardFieldPreimageIntoChunks(tier3Preimage);
  const tier3Certificate = deriveMidgardFieldPreimageCertificate({
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
      fieldCount: MIDGARD_FIELD_COUNT,
      maxFieldItemCount: MIDGARD_MAX_FIELD_ITEM_COUNT,
      maxTransactionAggregateFieldBytes:
        MIDGARD_MAX_TRANSACTION_AGGREGATE_FIELD_BYTES,
      maxSpendInputsPreimageBytes: MIDGARD_MAX_SPEND_INPUTS_PREIMAGE_BYTES,
      maximumCardanoSpendRedeemerCount:
        MIDGARD_MAXIMUM_CARDANO_SPEND_REDEEMER_COUNT,
      chunkBytesK: MIDGARD_CHUNK_BYTES_K,
      maxTier1RedeemerPreimageBytes: MIDGARD_MAX_TIER1_REDEEMER_PREIMAGE_BYTES,
      maxTier3ChunkCount: MIDGARD_MAX_TIER3_CHUNK_COUNT,
      // §5.3 item widths. The *strides* are what the encoders spend, and they
      // are pinned by the stride table below; these are the payload widths the
      // strides are built from, twinned constant-for-constant with the Aiken
      // module and otherwise referenced nowhere — exactly the shape a value can
      // drift in unnoticed, which is why they belong in the channel too.
      spendInputItemBytes: MIDGARD_SPEND_INPUT_ITEM_BYTES,
      hash28ItemBytes: MIDGARD_HASH28_ITEM_BYTES,
      addressWitnessItemBytes: MIDGARD_ADDRESS_WITNESS_ITEM_BYTES,
      carriageConstructors: [...MIDGARD_FIELD_CARRIAGE_CONSTRUCTORS],
      viewConstructors: [...MIDGARD_FIELD_VIEW_CONSTRUCTORS],
    },
    emptyFieldCommitmentHex: hex(MIDGARD_EMPTY_FIELD_COMMITMENT),
    strides: Array.from({ length: MIDGARD_FIELD_COUNT }, (_, fieldIndex) =>
      midgardFieldStride(fieldIndex),
    ),
    arrayHeaders: ARRAY_HEADER_COUNTS.map((count) => {
      const header = encodeMidgardFieldArrayHeader(count);
      const decoded = decodeMidgardFieldArrayHeader(header);
      return {
        count,
        headerHex: hex(header),
        headerLength: decoded.nextOffset,
      };
    }),
    itemWrappers: ITEM_WRAPPER_PAYLOAD_LENGTHS.map((payloadLength) => ({
      payloadLength,
      wrapperHex: hex(
        encodeMidgardDefiniteBytes(filler(payloadLength, payloadLength)),
      ).slice(0, payloadLength <= 23 ? 2 : payloadLength <= 255 ? 4 : 6),
    })),
    preimages,
    chunkCounts: CHUNK_COUNT_TOTAL_LENGTHS.map((totalLength) => ({
      totalLength,
      chunkCount: midgardExpectedChunkCount(totalLength),
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
      // The mint-welded datum commitment (#606) — the datum-shape identity
      // that replaced the retired per-(tx_id, field_index) asset-name class.
      // Recomputed on the Aiken side from the rebuilt payload.
      fieldHashHex: hex(tier3Certificate.fieldHash),
    },
    // #606 (owner ruling 2026-08-16): one constant asset name for every
    // certificate of the policy; the retired blake2b_256(field_index ‖ tx_id)
    // vector class is replaced by this constant plus the datum-shape vectors.
    certificateAssetName: {
      assetNameHex: hex(MIDGARD_FIELD_PREIMAGE_CERTIFICATE_ASSET_NAME),
      byteLength: MIDGARD_FIELD_PREIMAGE_CERTIFICATE_ASSET_NAME.length,
      ascii: MIDGARD_FIELD_PREIMAGE_CERTIFICATE_ASSET_NAME.toString("ascii"),
    },
  };
};

// ---------------------------------------------------------------------------
// Aiken rendering
// ---------------------------------------------------------------------------

export const section = (title) => [
  "// ---------------------------------------------------------------------------",
  `// ${title}`,
  "// ---------------------------------------------------------------------------",
  "",
];

export const headerLengthForCount = (count) =>
  count <= 23 ? 1 : count <= 255 ? 2 : 3;
