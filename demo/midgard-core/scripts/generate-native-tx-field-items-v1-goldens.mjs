#!/usr/bin/env node

/**
 * Produces the **per-field** cross-language golden vectors for the nine item
 * encodings of `docs/spec/midgardV1-tx.md` §5.3 (with §5.5 for outputs and §5.6
 * for mint), the §5.1 envelope each field wears, the §4 flat commitment each
 * field commits under, and the §8 carriage each field's preimage selects.
 *
 * This is the fan-out half of the channel #568 opened. That generator owns what
 * all nine fields *share* — envelope widths, strides, chunk split, certificate
 * asset names — and deliberately carries no per-field item bytes. This one owns
 * the other half: for every field index 0..8, what `enc_i` actually is.
 *
 * Every value is computed by the TypeScript twins
 * (`demo/midgardV1-core/src/codec/native-tx-field-items.ts`, plus the reused
 * canonical encoders for fields 2 and 6) and written to two places:
 *
 *   * `demo/midgardV1-core/tests/fixtures/native-tx-field-items-v1.generated.json`
 *     — recomputed by `tests/native-tx-field-items-goldens.test.ts`, so a
 *     drifting twin fails on the TypeScript side; and
 *   * `onchain/aiken/lib/midgardV1/native-tx-field-items-v1-golden.test.ak`
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

import { dirname, join, resolve } from "node:path";
import { fileURLToPath } from "node:url";

import {
  aikenBytes,
  formatAikenSource,
  goldenChannelEmitter,
  hex,
  parseGoldenChannelArguments,
} from "./golden-channel.mjs";
import {
  buildMidgardChunkedFieldView,
  buildMidgardWholeFieldView,
  decodeMidgardFieldArrayHeader,
  deriveMidgardFieldPreimageCertificate,
  MIDGARD_CHUNK_BYTES_K,
  MIDGARD_FIELD_COUNT,
  MIDGARD_MAX_TRANSACTION_AGGREGATE_FIELD_BYTES,
  MIDGARD_MAXIMUM_CARDANO_SPEND_REDEEMER_COUNT,
  midgardFieldItemAt,
  midgardFieldItemExtent,
  midgardFieldStride,
  selectMidgardFieldCarriageTier,
} from "../dist/codec/native-tx-field-access.js";
import {
  encodeMidgardFieldItems,
  encodeMidgardFieldPreimageForField,
  encodeMidgardFixedOutputIndex,
  encodeMidgardRedeemerWitnessItem,
  encodeMidgardSpendInputItem,
  MIDGARD_FIELD_NAMES,
  MIDGARD_REDEEMER_PURPOSE_TAGS,
  midgardFieldCommitmentForField,
} from "../dist/codec/native-tx-field-items.js";
import {
  encodeMidgardNativeTxProofFieldLengths,
  midgardNativeTxProofFieldPreimageLengths,
} from "../dist/codec/native.js";
import { encodeMidgardNativeScript } from "../dist/codec/native-script.js";
import { encodeMidgardVersionedScript } from "../dist/codec/versioned-script.js";
// The structured inputs live beside the fixture they produce, not here: the
// vitest suite drives the *same* definitions through `src/` while this script
// drives them through `dist/`, so an item encoder that drifts is caught on both
// sides rather than only by `--check`.
import {
  CARRIAGE_BOUNDARY_LENGTHS,
  DATUM_CANONICITY_BOUNDARIES,
  FIELD_PREIMAGE_LENGTH_SOURCE,
  FIELD_PREIMAGE_LENGTHS,
  FIELD_VECTORS,
  filler,
  FIXED_INDEX_BOUNDARIES,
  midgardV1,
  nativeCardano,
  plutusV3,
  redeemer,
  STRADDLE_BLOCK_ITEMS,
  STRADDLE_FIELD_INDEX,
  STRADDLE_ITEM_COUNT,
  STRADDLE_ITEM_INDEX,
  STRADDLE_OWNER,
  STRADDLE_REPEATS,
  STRADDLE_TX_ID,
  straddleInputs,
} from "../tests/fixtures/native-tx-field-items-v1.vectors.mjs";

const scriptDirectory = dirname(fileURLToPath(import.meta.url));
const packageRoot = resolve(scriptDirectory, "..");
const repositoryRoot = resolve(packageRoot, "../..");
const generatedJsonPath = join(
  packageRoot,
  "tests/fixtures/native-tx-field-items-v1.generated.json",
);
const generatedAikenPath = join(
  repositoryRoot,
  "onchain/aiken/lib/midgardV1/native-tx-field-items-v1-golden.test.ak",
);

const { checkOnly } = parseGoldenChannelArguments(
  "usage: node scripts/generate-native-tx-field-items-v1-goldens.mjs [--check]",
);
const writeOrCheck = goldenChannelEmitter({ repositoryRoot, checkOnly });

/**
 * §5.3 field 6's three admissible languages, with the Aiken constructor each
 * maps to. Declared once so the JSON entry and the Aiken assertion are built
 * from the same script value rather than from two independent literals.
 */
const LANGUAGE_TAG_VECTORS = [
  {
    language: "NativeCardano",
    aikenConstructor: "NativeCardanoScript",
    tag: 0,
    script: nativeCardano(90),
  },
  {
    language: "PlutusV3",
    aikenConstructor: "PlutusV3Script",
    tag: 3,
    script: plutusV3(91, 8),
  },
  {
    language: "MidgardV1",
    aikenConstructor: "MidgardV1Script",
    tag: 128,
    script: midgardV1(92, 8),
  },
];

// ---------------------------------------------------------------------------
// §8.4 straddle vector
// ---------------------------------------------------------------------------

/**
 * A real tier-3 field-1 carriage whose item 378 crosses the chunk boundary.
 *
 * The preimage is a 400-byte block of ten distinct stride-40 elements repeated
 * forty times, so both languages rebuild all 16,003 bytes from 400 and hash the
 * chunks for themselves rather than each trusting a digest the other computed.
 * Item `i` therefore carries pattern `i mod 10`, which is what makes an
 * off-by-one read visible: item 377 and item 379 are different bytes from 378.
 *
 * K is 15,148 (§8.3 erratum E1's repaired value) and item 378's payload spans
 * [15,125, 15,163), so reading it stitches 23 bytes out of chunk 0 and 15 out of
 * chunk 1 — the straddle the §8.8 door has to survive, at the stride fields 0/1
 * actually use. The index is a function of K and moved with it; the assertion
 * below is what refuses to emit a vector whose named item does not straddle.
 *
 * **Field 1 rather than field 0**, and the difference is §5.4, not taste: both
 * carry inputs under the same encoder and stride, so the bytes are the same
 * either way, but field 0's cardinality is capped by the Cardano shape bound at
 * 296 spend inputs — 11,843 preimage bytes at that cap, which still selects
 * tier 1. A maximal field 0 cannot reach tier 3, so a field-0 straddle would
 * pin an unreachable configuration. Field 1 has no such bound; §5.4's byte
 * bound alone admits 819 items at stride 40.
 */

/**
 * §5.4/§8.4 reachability, asserted rather than asserted-in-prose: the straddle
 * has to be a configuration the format actually admits *and* one that actually
 * lands above K. Both halves are derived from the published bounds, so moving
 * the vector to a field or a cardinality that cannot reach tier 3 fails the
 * generator instead of quietly pinning a fiction.
 */
const assertStraddleIsReachableV1 = (headerLength, preimageLength) => {
  const stride = midgardFieldStride(STRADDLE_FIELD_INDEX);
  if (STRADDLE_FIELD_INDEX === 0) {
    const maximalSpendInputs =
      headerLength + stride * MIDGARD_MAXIMUM_CARDANO_SPEND_REDEEMER_COUNT;
    throw new Error(
      "§5.4: field 0 is capped at " +
        `${MIDGARD_MAXIMUM_CARDANO_SPEND_REDEEMER_COUNT} spend inputs, so its largest ` +
        `admissible preimage is ${maximalSpendInputs} bytes and can never reach ` +
        `tier 3 (K=${MIDGARD_CHUNK_BYTES_K}); the straddle must live at a field that can`,
    );
  }
  const maximumItems = Math.floor(
    (MIDGARD_MAX_TRANSACTION_AGGREGATE_FIELD_BYTES - headerLength) / stride,
  );
  if (STRADDLE_ITEM_COUNT > maximumItems) {
    throw new Error(
      `§5.4: ${STRADDLE_ITEM_COUNT} items exceed field ${STRADDLE_FIELD_INDEX}'s ` +
        `byte bound of ${maximumItems} at stride ${stride}`,
    );
  }
  if (preimageLength <= MIDGARD_CHUNK_BYTES_K) {
    throw new Error(
      `§8.4: a ${preimageLength}-byte preimage does not exceed ` +
        `K=${MIDGARD_CHUNK_BYTES_K}, so it is not a tier-3 carriage at all`,
    );
  }
};

/**
 * The tier-boundary sweep has to sit *on* `K`, and `K` is a value this module
 * imports while the vector file cannot (see that file's header). Asserted here so
 * a re-pin of `K` — §8.3 erratum E1 moved it once already — cannot leave the
 * sweep pinned at the superseded boundary and go on reporting a partition it is
 * no longer sampling.
 */
const assertCarriageBoundariesStraddleKV1 = () => {
  for (const required of [MIDGARD_CHUNK_BYTES_K, MIDGARD_CHUNK_BYTES_K + 1]) {
    if (!CARRIAGE_BOUNDARY_LENGTHS.includes(required)) {
      throw new Error(
        `§8.3: CARRIAGE_BOUNDARY_LENGTHS must sample ${required.toString()} ` +
          `(K=${MIDGARD_CHUNK_BYTES_K.toString()} and K+1); got ` +
          `[${CARRIAGE_BOUNDARY_LENGTHS.join(", ")}]`,
      );
    }
  }
};

const straddleItem = (patternIndex) =>
  encodeMidgardSpendInputItem(straddleInputs()[patternIndex]);

const buildStraddle = () => {
  const selector = {
    fieldIndex: STRADDLE_FIELD_INDEX,
    items: straddleInputs(),
  };
  const preimage = encodeMidgardFieldPreimageForField(selector);
  const commitment = midgardFieldCommitmentForField(selector);
  const header = decodeMidgardFieldArrayHeader(preimage);
  assertStraddleIsReachableV1(header.nextOffset, preimage.length);
  // The repeating block is exactly `STRADDLE_BLOCK_ITEMS` wrapped elements, so
  // the artifacts carry 400 bytes rather than the 16,003 they expand to.
  const block = preimage.subarray(
    header.nextOffset,
    header.nextOffset + STRADDLE_BLOCK_ITEMS * 40,
  );
  const certificate = deriveMidgardFieldPreimageCertificate({
    owner: STRADDLE_OWNER,
    txId: STRADDLE_TX_ID,
    fieldIndex: STRADDLE_FIELD_INDEX,
    preimage,
  });
  const chunks = [];
  for (let start = 0; start < preimage.length; start += MIDGARD_CHUNK_BYTES_K) {
    chunks.push(
      preimage.subarray(
        start,
        Math.min(start + MIDGARD_CHUNK_BYTES_K, preimage.length),
      ),
    );
  }
  const view = buildMidgardChunkedFieldView({
    fieldIndex: STRADDLE_FIELD_INDEX,
    txId: certificate.txId,
    certificate,
    chunks,
    expectedCommitment: commitment,
  });
  const reads = [
    STRADDLE_ITEM_INDEX - 1,
    STRADDLE_ITEM_INDEX,
    STRADDLE_ITEM_INDEX + 1,
  ].map((index) => {
    const extent = midgardFieldItemExtent(view, index);
    return {
      itemIndex: index,
      offset: extent.offset,
      length: extent.length,
      // A read is straddling when its byte range crosses a multiple of K.
      straddles:
        Math.floor(extent.offset / MIDGARD_CHUNK_BYTES_K) !==
        Math.floor((extent.offset + extent.length - 1) / MIDGARD_CHUNK_BYTES_K),
      itemHex: hex(midgardFieldItemAt(view, index)),
    };
  });
  // The whole point of the vector: the middle read, and only the middle read,
  // crosses the boundary. A neighbour that also straddled, or a middle one that
  // did not, would leave the §8.8 stitch untested.
  const straddling = reads.filter((read) => read.straddles);
  if (
    straddling.length !== 1 ||
    straddling[0].itemIndex !== STRADDLE_ITEM_INDEX
  ) {
    throw new Error(
      `§8.4: item ${STRADDLE_ITEM_INDEX} must be the sole straddling read (got ` +
        `${straddling.map((read) => read.itemIndex).join(",") || "none"})`,
    );
  }
  return {
    fieldIndex: STRADDLE_FIELD_INDEX,
    stride: midgardFieldStride(STRADDLE_FIELD_INDEX),
    blockHex: hex(block),
    blockElementCount: STRADDLE_BLOCK_ITEMS,
    repeats: STRADDLE_REPEATS,
    itemCount: STRADDLE_ITEM_COUNT,
    headerHex: hex(preimage.subarray(0, header.nextOffset)),
    totalLength: preimage.length,
    commitmentHex: hex(commitment),
    carriageTier: selectMidgardFieldCarriageTier(preimage.length),
    chunkLengths: chunks.map((chunk) => chunk.length),
    chunkDigestsHex: certificate.chunkDigests.map(hex),
    reads,
    itemsHex: Array.from({ length: STRADDLE_BLOCK_ITEMS }, (_, index) =>
      hex(straddleItem(index)),
    ),
  };
};

// ---------------------------------------------------------------------------
// Golden assembly
// ---------------------------------------------------------------------------

/**
 * §8, simplest-fitting-first. Each field's populated vectors select a tier by
 * preimage length; the boundary lengths themselves are pinned here so the
 * partition (§8.4 — a preimage has exactly one admissible carriage) is a
 * cross-language fact rather than a convention.
 */

const buildGolden = () => {
  const fields = FIELD_VECTORS.map((field) => ({
    fieldIndex: field.fieldIndex,
    fieldName: MIDGARD_FIELD_NAMES[field.fieldIndex],
    stride: midgardFieldStride(field.fieldIndex),
    aikenProducer: field.aikenProducer,
    aikenDecoder: field.aikenDecoder,
    vectors: field.vectors.map((vector) => {
      const selector = { fieldIndex: field.fieldIndex, items: vector.items };
      const itemBytes = encodeMidgardFieldItems(selector);
      const preimage = encodeMidgardFieldPreimageForField(selector);
      const commitment = midgardFieldCommitmentForField(selector);
      const header = decodeMidgardFieldArrayHeader(preimage);
      const view = buildMidgardWholeFieldView({
        fieldIndex: field.fieldIndex,
        preimage,
        expectedCommitment: commitment,
      });
      return {
        label: vector.label,
        itemCount: itemBytes.length,
        headerLength: header.nextOffset,
        itemsHex: itemBytes.map(hex),
        preimageHex: hex(preimage),
        preimageLength: preimage.length,
        commitmentHex: hex(commitment),
        carriageTier: selectMidgardFieldCarriageTier(preimage.length),
        itemExtents: itemBytes.map((_, index) => {
          const extent = midgardFieldItemExtent(view, index);
          return { offset: extent.offset, length: extent.length };
        }),
      };
    }),
  }));

  // §5.3's two value sets, pinned as the canonical bytes each tag occupies.
  const languageTags = LANGUAGE_TAG_VECTORS.map(
    ({ language, tag, script }) => ({
      language,
      tag,
      itemHex: hex(encodeMidgardVersionedScript(script)),
    }),
  );
  const purposeTags = Object.entries(MIDGARD_REDEEMER_PURPOSE_TAGS).map(
    ([purpose, tag]) => ({
      purpose,
      tag,
      itemHex: hex(
        encodeMidgardRedeemerWitnessItem(redeemer(purpose, 1, "d87980", 2, 3)),
      ),
    }),
  );

  // §2.4. The wire order places `script_witnesses` at position 6 and
  // `address_witnesses` at 7 — transposed relative to the record declaration.
  // Both twins already agree on this and MUST NOT change it, so the vector uses
  // nine distinct lengths: a transposition would be invisible under equal ones.
  // Derived through the function that performs the transposition, not written
  // down in wire order: a pre-ordered array would prove array order only.
  const fieldPreimageLengths = midgardNativeTxProofFieldPreimageLengths(
    FIELD_PREIMAGE_LENGTH_SOURCE,
  );
  if (
    fieldPreimageLengths.length !== FIELD_PREIMAGE_LENGTHS.length ||
    fieldPreimageLengths.some(
      (length, index) => length !== FIELD_PREIMAGE_LENGTHS[index],
    )
  ) {
    throw new Error(
      "§2.4 wire order changed: the derived field-preimage lengths no longer " +
        `match the declared wire order (got ${fieldPreimageLengths.join(",")})`,
    );
  }

  assertCarriageBoundariesStraddleKV1();

  return {
    schema: "midgardV1-native-tx-field-items-v1-golden",
    version: 1,
    specDocument: "docs/spec/midgardV1-tx.md",
    generator:
      "demo/midgardV1-core/scripts/generate-native-tx-field-items-v1-goldens.mjs",
    fieldCount: MIDGARD_FIELD_COUNT,
    fields,
    languageTags,
    purposeTags,
    fixedOutputIndexes: FIXED_INDEX_BOUNDARIES.map((outputIndex) => ({
      outputIndex,
      encodedHex: hex(encodeMidgardFixedOutputIndex(outputIndex)),
    })),
    // §5.3: an out-ref's field-0/1 item *is* its ledger MPF trie key and its
    // ledger database `outref` column. These vectors are what the on-chain
    // `ledger_outref_key` is pinned against, and what the TypeScript trie-key
    // producers (`outRefToCbor`, `midgardOutRefToCbor`) are pinned against — one
    // vector, both languages, so the key cannot diverge in one of them. The
    // index set spans both sides of the minimal-CBOR boundary at 23/24, which is
    // the only place a minimal-index encoder and this one disagree in width.
    ledgerOutRefKeys: FIXED_INDEX_BOUNDARIES.map((outputIndex) => {
      const txId = filler(32, 0x11 + outputIndex);
      return {
        txIdHex: hex(txId),
        outputIndex,
        keyHex: hex(encodeMidgardSpendInputItem({ txId, outputIndex })),
      };
    }),
    datumCanonicityBoundaries: DATUM_CANONICITY_BOUNDARIES.map(
      ([label, cborHex]) => ({ label, cborHex }),
    ),
    fieldPreimageLengths: {
      lengths: fieldPreimageLengths,
      // Produced by the TypeScript twin, whose own array already places the
      // script-witness length before the address-witness one.
      encodedHex: hex(
        encodeMidgardNativeTxProofFieldLengths(fieldPreimageLengths),
      ),
    },
    carriageTiers: CARRIAGE_BOUNDARY_LENGTHS.map((preimageLength) => ({
      preimageLength,
      tier: selectMidgardFieldCarriageTier(preimageLength),
    })),
    straddle: buildStraddle(),
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

/** Aiken literals for the structured items of the directly-assertable fields. */
const aikenItemLiteral = (fieldIndex, item) => {
  switch (fieldIndex) {
    case 0:
    case 1:
      return `MidgardTxInput { tx_id: ${aikenBytes(hex(item.txId))}, output_index: ${item.outputIndex} }`;
    case 3:
    case 4:
      return aikenBytes(hex(item));
    case 6: {
      const language =
        item.language === "NativeCardano"
          ? "NativeCardanoScript"
          : item.language === "PlutusV3"
            ? "PlutusV3Script"
            : "MidgardV1Script";
      // The Aiken record carries the already-serialised script payload, which
      // for a native script is the encoded native-script CBOR — the same bytes
      // the TypeScript encoder derives from its structured `nativeScript`, so
      // it is produced by the native-script encoder rather than recovered by
      // slicing a header whose width is not fixed.
      const scriptBytes =
        item.language === "NativeCardano"
          ? hex(encodeMidgardNativeScript(item.nativeScript))
          : hex(item.scriptBytes);
      return `MidgardVersionedScript { language: ${language}, script_bytes: ${aikenBytes(scriptBytes)} }`;
    }
    case 7:
      return `MidgardAddressWitness { verification_key: ${aikenBytes(hex(item.verificationKey))}, signature: ${aikenBytes(hex(item.signature))} }`;
    case 8:
      return `MidgardRedeemerWitness { purpose: ${item.purpose}Redeemer, index: ${item.index}, redeemer_cbor: ${aikenBytes(hex(item.redeemerCbor))}, execution_units: MidgardExecutionUnits { memory: ${item.executionUnits.memory}, steps: ${item.executionUnits.steps} } }`;
    default:
      return undefined;
  }
};

/**
 * §5.6 mint `Data` literals, for the ordering negatives only.
 *
 * The two policy ids are the pair the `multi_policy` positive vector uses,
 * sorted ascending here under the same comparator §5.6 names, so a "descending"
 * literal below is descending by construction rather than by hope — and stays
 * descending if `filler` ever changes.
 */
const MINT_NEGATIVE_POLICY_IDS = [filler(28, 5), filler(28, 6)]
  .slice()
  .sort((left, right) => Buffer.compare(left, right))
  .map(hex);

const aikenMintPolicyId = (rank) =>
  `builtin.b_data(${aikenBytes(MINT_NEGATIVE_POLICY_IDS[rank])})`;

const aikenMintAssets = (assets) =>
  `builtin.map_data([${assets
    .map(
      ([assetNameHex, quantity]) =>
        `Pair(builtin.b_data(${aikenBytes(assetNameHex)}), builtin.i_data(${quantity}))`,
    )
    .join(", ")}])`;

const aikenMintData = (policies) =>
  `builtin.map_data([${policies
    .map(
      ([rank, assets]) =>
        `Pair(${aikenMintPolicyId(rank)}, ${aikenMintAssets(assets)})`,
    )
    .join(", ")}])`;

/** The Aiken item encoder that takes the literal above. */
const AIKEN_ITEM_ENCODERS = {
  0: "encode_midgard_tx_input",
  1: "encode_midgard_tx_input",
  3: undefined,
  4: undefined,
  6: "encode_midgard_versioned_script",
  7: "encode_midgard_address_witness",
  8: "encode_midgard_redeemer_witness",
};

const renderAiken = (golden) => {
  const lines = [
    `// Generated by ${golden.generator}.`,
    "// Do not edit; regenerate from the TypeScript twins.",
    "//",
    "// Per-field cross-language golden vectors for the nine item encodings of",
    `// \`${golden.specDocument}\` §5.3 (with §5.5 for outputs and §5.6 for mint), the`,
    "// §5.1 envelope each field wears, the §4 flat commitment each commits under, and",
    "// the §8 carriage each preimage selects.",
    "//",
    "// Every constant below was produced by the TypeScript twins",
    "// (`demo/midgardV1-core/src/codec/native-tx-field-items.ts`) and is recomputed",
    "// here by the Aiken producers, so the two encoders cannot silently diverge. The",
    "// shared surface all nine fields agree on is pinned by the sibling module",
    "// `native-tx-field-access-v1-golden.test.ak` (#568); this one is the per-field",
    "// fan-out (#569).",
    "",
    "use aiken/builtin",
    "use aiken/collection/list",
    "use aiken/crypto.{blake2b_256}",
    "use aiken/primitive/bytearray",
    "use cardano/transaction.{OutputReference}",
    "use midgardV1/fraud_proofs/native_tx/compact.{",
    "  encode_native_tx_field_preimage_lengths_v1,",
    "}",
    "use midgardV1/fraud_proofs/native_tx/components.{",
    "  decode_midgard_tx_input_cbor, encode_fixed_output_index,",
    "  encode_midgard_address_witness, encode_midgard_redeemer_witness,",
    "  encode_midgard_tx_input, encode_midgard_versioned_script,",
    "}",
    "use midgardV1/fraud_proofs/native_tx/preimages.{",
    "  decode_midgard_tx_address_witnesses_preimage_cbor,",
    "  decode_midgard_tx_hash28_list_preimage_cbor,",
    "  decode_midgard_tx_inputs_preimage_cbor,",
    "  decode_midgard_tx_mint_preimage_cbor,",
    "  decode_midgard_tx_outputs_preimage_cbor,",
    "  decode_midgard_tx_redeemer_witnesses_preimage_cbor,",
    "  decode_midgard_tx_script_witnesses_preimage_cbor,",
    "  encode_address_witness_preimage, encode_hash28_list_preimage,",
    "  encode_input_preimage, encode_mint_policy_item, encode_mint_preimage,",
    "  encode_output_preimage, encode_redeemer_witness_preimage,",
    "  encode_script_witness_preimage,",
    "}",
    "use midgardV1/fraud_proofs/native_tx/types.{",
    "  CertRedeemer, MidgardAddressWitness, MidgardExecutionUnits,",
    "  MidgardRedeemerWitness, MidgardTxInput, MidgardV1Script,",
    "  MidgardVersionedScript, MintRedeemer, NativeCardanoScript,",
    "  NativeTxFieldPreimageLengthsV1, PlutusV3Script, ProposeRedeemer,",
    "  ReceiveRedeemer, RewardRedeemer, SpendRedeemer, VoteRedeemer,",
    "}",
    "use midgardV1/fraud_proofs/transition_trace/proof.{ledger_outref_key}",
    "use midgardV1/native_tx_field_access_v1.{",
    "  Chunked, Whole, chunk_bytes_k, decode_field_array_header,",
    "  encode_field_preimage, field_commitment, field_commitment_from_items,",
    "  field_item_at, field_item_extent, field_stride,",
    "}",
    "",
  ];

  // -- constants ------------------------------------------------------------
  lines.push(...section("Generated per-field constants"));
  for (const field of golden.fields) {
    for (const vector of field.vectors) {
      const prefix = `golden_f${field.fieldIndex}_${vector.label}`;
      lines.push(
        `const ${prefix}_items: List<ByteArray> = [`,
        ...vector.itemsHex.map((item) => `  ${aikenBytes(item)},`),
        "]",
        "",
        `const ${prefix}_preimage = ${aikenBytes(vector.preimageHex)}`,
        "",
        `const ${prefix}_commitment = ${aikenBytes(vector.commitmentHex)}`,
        "",
      );
      if (vector.itemCount > 0) {
        lines.push(
          `const ${prefix}_view = Whole {`,
          `  bytes: ${prefix}_preimage,`,
          `  count: ${vector.itemCount},`,
          `  stride: ${field.stride},`,
          "}",
          "",
        );
      }
    }
  }

  // -- per-vector envelope, commitment and extents ---------------------------
  lines.push(
    ...section("§5.1 envelope, §4 commitment and §7.2 item extents, per field"),
  );
  for (const field of golden.fields) {
    for (const vector of field.vectors) {
      const prefix = `golden_f${field.fieldIndex}_${vector.label}`;
      lines.push(
        `/// Field ${field.fieldIndex} (${field.fieldName}), \`${vector.label}\`: ${vector.itemCount} item(s),`,
        `/// ${vector.preimageLength} preimage bytes, §8 tier \`${vector.carriageTier}\`.`,
        `test ${prefix}_matches_typescript() {`,
        "  and {",
        `    encode_field_preimage(${prefix}_items) == ${prefix}_preimage,`,
        `    field_commitment(${prefix}_preimage) == ${prefix}_commitment,`,
        `    field_commitment_from_items(${prefix}_items) == ${prefix}_commitment,`,
        `    decode_field_array_header(${prefix}_preimage) == Pair(${vector.headerLength}, ${vector.itemCount}),`,
        `    field_stride(${field.fieldIndex}) == ${field.stride},`,
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
      );
    }
  }

  // -- producer round-trips --------------------------------------------------
  lines.push(
    ...section("§5.3 producer round-trips — decode, re-encode, compare"),
  );
  lines.push(
    "/// The half of item-encoding agreement that a byte literal cannot carry: the",
    "/// Aiken decoder accepts the TypeScript preimage, and the field's own Aiken",
    "/// producer re-encodes it to the identical bytes. A decoder that accepted the",
    "/// bytes while its encoder spelled them differently fails here, and fields 2 and",
    "/// 5 — whose items are whole records rather than cheap literals — are covered by",
    "/// exactly this and nothing else.",
    "",
  );
  for (const field of golden.fields) {
    const prefix = `golden_f${field.fieldIndex}`;
    lines.push(
      `test ${prefix}_producer_round_trip_matches_typescript() {`,
      "  and {",
      ...field.vectors.map(
        (vector) =>
          `    ${field.aikenProducer}(${field.aikenDecoder}(${prefix}_${vector.label}_preimage)) == ${prefix}_${vector.label}_preimage,`,
      ),
      "  }",
      "}",
      "",
    );
  }

  // -- direct item encoders --------------------------------------------------
  lines.push(
    ...section("§5.3 item encoders called directly on structured values"),
  );
  for (const field of FIELD_VECTORS) {
    const encoder = AIKEN_ITEM_ENCODERS[field.fieldIndex];
    if (encoder === undefined) {
      continue;
    }
    const goldenField = golden.fields.find(
      (entry) => entry.fieldIndex === field.fieldIndex,
    );
    const assertions = [];
    for (const vector of field.vectors) {
      const goldenVector = goldenField.vectors.find(
        (entry) => entry.label === vector.label,
      );
      vector.items.forEach((item, index) => {
        const literal = aikenItemLiteral(field.fieldIndex, item);
        assertions.push(
          `    ${encoder}(${literal}) == ${aikenBytes(goldenVector.itemsHex[index])},`,
        );
      });
    }
    if (assertions.length === 0) {
      continue;
    }
    lines.push(
      `test golden_f${field.fieldIndex}_item_encoder_matches_typescript() {`,
      "  and {",
      ...assertions,
      "  }",
      "}",
      "",
    );
  }

  // -- §5.3 width assertions, in the negative --------------------------------
  //
  // §9.1 names the 28-byte width assertion explicitly, and a positive vector
  // cannot see it: every item in one is already the right width, so a *deleted*
  // assertion is invisible. Each test below is `fail` with a body that is
  // trivially true, which means the only way it can fail — and so the only way
  // it can pass — is for the encoder to abort. `== True` would be satisfied by
  // an encoder that returned the wrong bytes; aborting is the whole claim.
  lines.push(...section("§5.3 width assertions — the negatives that bite"));
  const widthNegative = (name, doc, expression) => [
    ...doc,
    `test ${name}() fail {`,
    `  bytearray.length(${expression}) >= 0`,
    "}",
    "",
  ];
  lines.push(
    ...widthNegative(
      "golden_f3_f4_item_width_rejects_27_bytes",
      [
        "/// Fields 3/4 have no interior CBOR — the item *is* the raw 28-byte hash, so",
        "/// the width assertion inside `encode_hash28_list_preimage` is the only thing",
        "/// fixing the stride at 30.",
      ],
      `encode_hash28_list_preimage([${aikenBytes(hex(filler(27, 1)))}])`,
    ),
    ...widthNegative(
      "golden_f3_f4_item_width_rejects_29_bytes",
      ["/// The same assertion from the other side."],
      `encode_hash28_list_preimage([${aikenBytes(hex(filler(29, 1)))}])`,
    ),
    ...widthNegative(
      "golden_f0_f1_item_rejects_a_short_tx_id",
      [
        "/// Fields 0/1: a 31-byte tx id would make the item 37 bytes and silently",
        "/// break stride-40 arithmetic for every item after it.",
      ],
      `encode_midgard_tx_input(MidgardTxInput { tx_id: ${aikenBytes(hex(filler(31, 1)))}, output_index: 0 })`,
    ),
    ...widthNegative(
      "golden_f0_f1_decoder_rejects_a_non_minimal_tx_id_header",
      [
        "/// Fields 0/1, the decode side of the same width. `59 0020` is a second,",
        '/// wider spelling of "32 bytes" that `decode_definite_bytes_at` will read,',
        "/// so a 39-byte item would otherwise decode to the *same* out-ref as the",
        "/// canonical 38-byte one — two trie keys naming one out-ref. The exact-38",
        "/// guard in `decode_midgard_tx_input_cbor` is what rejects it, and its",
        "/// TypeScript twin `decodeMidgardSpendInputItem` rejects it on width too.",
      ],
      `encode_midgard_tx_input(decode_midgard_tx_input_cbor(${aikenBytes(
        `82590020${hex(filler(32, 1))}190002`,
      )}))`,
    ),
    ...widthNegative(
      "golden_f7_item_rejects_a_short_verification_key",
      [
        "/// Field 7 is structurally fixed at 101 bytes: 32-byte key, 64-byte signature.",
      ],
      `encode_midgard_address_witness(MidgardAddressWitness { verification_key: ${aikenBytes(hex(filler(31, 1)))}, signature: ${aikenBytes(hex(filler(64, 2)))} })`,
    ),
    ...widthNegative(
      "golden_f7_item_rejects_a_short_signature",
      ["/// The other half of field 7's width."],
      `encode_midgard_address_witness(MidgardAddressWitness { verification_key: ${aikenBytes(hex(filler(32, 1)))}, signature: ${aikenBytes(hex(filler(63, 2)))} })`,
    ),
  );

  // -- §5.6 ordering assertions, in the negative -----------------------------
  //
  // §5.6 orders keys at two levels — asset names within a policy, policy ids
  // across the field — and duplicates reject at both. No positive vector can
  // see either rule: every one of them is already ordered, so a *deleted*
  // check is invisible there. These four are the Aiken twins of the TypeScript
  // ordering negatives in `tests/native-tx-field-items-goldens.test.ts`;
  // without them "§5.6 ordering is enforced" would be a claim about one twin
  // and an assumption about the other.
  lines.push(...section("§5.6 ordering assertions — the negatives that bite"));
  lines.push(
    ...widthNegative(
      "golden_f5_asset_names_reject_descending_order",
      [
        "/// §5.6 asset-name order is length-first, then byte-lexicographic, so `42`",
        "/// (1 B) precedes `4141` (2 B) and this run descends. The producer must",
        "/// refuse it: the decoder does, so bytes that got past here would never",
        "/// decode on either side.",
      ],
      `encode_mint_policy_item(${aikenMintPolicyId(0)}, ${aikenMintAssets([
        ["4141", 1],
        ["42", 2],
      ])})`,
    ),
    ...widthNegative(
      "golden_f5_asset_names_reject_a_repeat",
      ["/// The duplicate half of the same rule, one level down."],
      `encode_mint_policy_item(${aikenMintPolicyId(0)}, ${aikenMintAssets([
        ["41", 1],
        ["41", 2],
      ])})`,
    ),
    ...widthNegative(
      "golden_f5_policy_ids_reject_descending_order",
      [
        "/// The field-level half: policy items carry the same comparator, and no",
        "/// single item can see the order it sits in — only the run can.",
      ],
      `encode_mint_preimage(${aikenMintData([
        [1, [["41", 1]]],
        [0, [["41", 2]]],
      ])})`,
    ),
    ...widthNegative(
      "golden_f5_policy_ids_reject_a_repeat",
      ["/// And the duplicate half of the field-level rule."],
      `encode_mint_preimage(${aikenMintData([
        [0, [["41", 1]]],
        [0, [["41", 2]]],
      ])})`,
    ),
  );

  // -- §5.3 value sets -------------------------------------------------------
  lines.push(...section("§5.3 the two value sets and the fixed output index"));
  lines.push(
    "/// §5.3 fields 0/1: the output index is always the fixed 3-byte `19 XXXX` form,",
    "/// even for 0 and 23 which minimal CBOR spells in one byte. This is the sole",
    "/// deliberately non-minimal encoding in the format.",
    "test golden_fixed_output_index_matches_typescript() {",
    "  and {",
    ...golden.fixedOutputIndexes.map(
      (entry) =>
        `    encode_fixed_output_index(${entry.outputIndex}) == ${aikenBytes(entry.encodedHex)},`,
    ),
    "  }",
    "}",
    "",
    "/// §5.3 fields 0/1: an out-ref's item encoding *is* its ledger MPF trie key and",
    "/// its ledger database `outref` column. `ledger_outref_key` is pinned here",
    "/// against bytes produced by the TypeScript trie-key producers, so the two",
    "/// languages cannot key the ledger differently. The index set spans both sides",
    "/// of the minimal-CBOR boundary at 23/24 — the only place where a minimal-index",
    "/// encoder and this one differ in width, and therefore the only place a",
    "/// regression would hide.",
    "test golden_ledger_outref_key_matches_typescript() {",
    "  and {",
    ...golden.ledgerOutRefKeys.map(
      (entry) =>
        `    ledger_outref_key(OutputReference { transaction_id: ${aikenBytes(
          entry.txIdHex,
        )}, output_index: ${entry.outputIndex} }) == ${aikenBytes(entry.keyHex)},`,
    ),
    "  }",
    "}",
    "",
    "/// §5.3 field 6: exactly three admissible language tags. `MidgardV1` is 128 and",
    "/// is the only one that is not a single byte — its canonical form is `18 80`.",
    "/// The tag bytes are sliced out of the *encoder's own output*, not out of a",
    "/// literal: slicing a literal and comparing it to a literal would assert",
    "/// nothing about either language's encoder.",
    "test golden_script_language_tags_match_typescript() {",
    "  and {",
    ...LANGUAGE_TAG_VECTORS.map(
      ({ language, aikenConstructor, tag, script }) => {
        const tagHex = tag === 128 ? "1880" : tag.toString(16).padStart(2, "0");
        const scriptBytes =
          language === "NativeCardano"
            ? hex(encodeMidgardNativeScript(script.nativeScript))
            : hex(script.scriptBytes);
        return `    bytearray.take(bytearray.drop(encode_midgard_versioned_script(MidgardVersionedScript { language: ${aikenConstructor}, script_bytes: ${aikenBytes(scriptBytes)} }), 1), ${tagHex.length / 2}) == ${aikenBytes(tagHex)},`;
      },
    ),
    "  }",
    "}",
    "",
    "/// §5.3 field 8: exactly seven admissible purpose tags, every one ≤ 23 and so",
    "/// exactly one byte equal to its value. Values 0–5 reuse Cardano's own",
    "/// `RedeemerTag` numbering; 6 (`Receive`) is Midgard-only.",
    "test golden_redeemer_purpose_tags_match_typescript() {",
    "  and {",
    ...golden.purposeTags.map(
      (entry) =>
        `    encode_midgard_redeemer_witness(MidgardRedeemerWitness { purpose: ${entry.purpose}Redeemer, index: 1, redeemer_cbor: #"d87980", execution_units: MidgardExecutionUnits { memory: 2, steps: 3 } }) == ${aikenBytes(entry.itemHex)},`,
    ),
    "  }",
    "}",
    "",
  );

  // -- §2.4 wire-order transposition ----------------------------------------
  lines.push(
    ...section("§2.4 field-preimage lengths — the wire-order transposition"),
  );
  lines.push(
    "/// §2.4 places `script_witnesses` at wire position 6 and `address_witnesses` at",
    "/// 7 — transposed relative to the record declaration, which lists address before",
    "/// script. Both twins already agree on this and MUST NOT change it. The nine",
    "/// lengths below are pairwise distinct precisely so a transposition cannot hide:",
    `/// the address length is ${golden.fieldPreimageLengths.lengths[7]} and the script length is ${golden.fieldPreimageLengths.lengths[6]}, and the`,
    "/// encoded bytes put the script one first.",
    "test golden_field_preimage_lengths_match_typescript() {",
    "  encode_native_tx_field_preimage_lengths_v1(",
    "    NativeTxFieldPreimageLengthsV1 {",
    `      spend_inputs: ${golden.fieldPreimageLengths.lengths[0]},`,
    `      reference_inputs: ${golden.fieldPreimageLengths.lengths[1]},`,
    `      outputs: ${golden.fieldPreimageLengths.lengths[2]},`,
    `      required_observers: ${golden.fieldPreimageLengths.lengths[3]},`,
    `      required_signers: ${golden.fieldPreimageLengths.lengths[4]},`,
    `      mint: ${golden.fieldPreimageLengths.lengths[5]},`,
    `      address_witnesses: ${golden.fieldPreimageLengths.lengths[7]},`,
    `      script_witnesses: ${golden.fieldPreimageLengths.lengths[6]},`,
    `      redeemers: ${golden.fieldPreimageLengths.lengths[8]},`,
    "    },",
    `  ) == ${aikenBytes(golden.fieldPreimageLengths.encodedHex)}`,
    "}",
    "",
  );

  // -- §8.4 straddle ---------------------------------------------------------
  const straddle = golden.straddle;
  lines.push(
    ...section(
      `§8.4 tier-3 straddle at field ${straddle.fieldIndex}'s stride-${straddle.stride}`,
    ),
  );
  lines.push(
    `const golden_straddle_block = ${aikenBytes(straddle.blockHex)}`,
    "",
    `const golden_straddle_header = ${aikenBytes(straddle.headerHex)}`,
    "",
    `const golden_straddle_commitment = ${aikenBytes(straddle.commitmentHex)}`,
    "",
    "fn golden_repeat_block(block: ByteArray, times: Int, acc: ByteArray) -> ByteArray {",
    "  if times <= 0 {",
    "    acc",
    "  } else {",
    "    golden_repeat_block(block, times - 1, bytearray.concat(acc, block))",
    "  }",
    "}",
    "",
    "/// The straddle preimage, rebuilt here rather than read out of the vector: a",
    `/// ${straddle.blockElementCount}-element block of ${straddle.blockElementCount * straddle.stride} bytes repeated ${straddle.repeats} times behind the §5.1`,
    `/// header, giving ${straddle.itemCount} items and ${straddle.totalLength} bytes. Both languages expand it and`,
    "/// hash the chunks for themselves, so the digests below are recomputed on each",
    "/// side rather than pinned on one and counted on the other.",
    "///",
    `/// The carriage is field ${straddle.fieldIndex} (reference inputs), not field 0. The two share an`,
    `/// item encoder and stride ${straddle.stride}, so these are field 0's bytes as well — but §5.4`,
    `/// caps field 0 at ${MIDGARD_MAXIMUM_CARDANO_SPEND_REDEEMER_COUNT} spend inputs, a maximal preimage of ${straddle.headerHex.length / 2 + straddle.stride * MIDGARD_MAXIMUM_CARDANO_SPEND_REDEEMER_COUNT} bytes,`,
    "/// which still selects tier 1. Only field 1 can legally carry this many.",
    "fn golden_straddle_preimage() -> ByteArray {",
    "  bytearray.concat(",
    "    golden_straddle_header,",
    `    golden_repeat_block(golden_straddle_block, ${straddle.repeats}, #""),`,
    "  )",
    "}",
    "",
    "fn golden_straddle_chunks(preimage: ByteArray) -> List<ByteArray> {",
    `  let count = ${straddle.chunkLengths.length}`,
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
    `/// Item ${straddle.reads.find((read) => read.straddles)?.itemIndex ?? STRADDLE_ITEM_INDEX} crosses the K boundary: its payload spans`,
    `/// [${straddle.reads.find((read) => read.straddles)?.offset}, ${(straddle.reads.find((read) => read.straddles)?.offset ?? 0) + (straddle.reads.find((read) => read.straddles)?.length ?? 0)}) while chunk 0 ends at ${MIDGARD_CHUNK_BYTES_K}, so reading it stitches`,
    "/// bytes out of both chunks. Its neighbours are read too — they carry different",
    "/// bytes, so an off-by-one that silently returned an adjacent item would fail.",
    "test golden_straddle_read_matches_typescript() {",
    "  let preimage = golden_straddle_preimage()",
    "  let chunks = golden_straddle_chunks(preimage)",
    "  let view =",
    "    Chunked {",
    "      chunks,",
    "      chunk_digests: list.map(chunks, blake2b_256),",
    `      count: ${straddle.itemCount},`,
    `      stride: ${straddle.stride},`,
    "    }",
    "  and {",
    `    bytearray.length(preimage) == ${straddle.totalLength},`,
    `    field_commitment(preimage) == golden_straddle_commitment,`,
    `    list.map(chunks, bytearray.length) == [${straddle.chunkLengths.join(", ")}],`,
    `    list.map(chunks, blake2b_256) == [`,
    ...straddle.chunkDigestsHex.map((digest) => `      ${aikenBytes(digest)},`),
    "    ],",
    ...straddle.reads.flatMap((read) => [
      `    field_item_extent(view, ${read.itemIndex}) == Pair(${read.offset}, ${read.length}),`,
      `    field_item_at(view, ${read.itemIndex}) == ${aikenBytes(read.itemHex)},`,
    ]),
    "  }",
    "}",
    "",
  );

  return lines.join("\n");
};

// ---------------------------------------------------------------------------
// Emission
// ---------------------------------------------------------------------------

const golden = buildGolden();
writeOrCheck(generatedJsonPath, `${JSON.stringify(golden, null, 2)}\n`);
writeOrCheck(
  generatedAikenPath,
  formatAikenSource({
    source: renderAiken(golden),
    fileName: "native-tx-field-items-v1-golden.test.ak",
    repositoryRoot,
    tmpPrefix: "midgardV1-569-aiken-format-",
  }),
);
