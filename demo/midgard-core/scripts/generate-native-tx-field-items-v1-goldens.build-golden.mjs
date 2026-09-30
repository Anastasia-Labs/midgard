import {
  encodeMidgardNativeTxProofFieldLengths,
  midgardNativeTxProofFieldPreimageLengths,
} from "../dist/codec/native.js";
import { encodeMidgardNativeScript } from "../dist/codec/native-script.js";
import {
  buildMidgardWholeFieldView,
  decodeMidgardFieldArrayHeader,
  MIDGARD_FIELD_COUNT,
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
import { encodeMidgardVersionedScript } from "../dist/codec/versioned-script.js";
import {
  CARRIAGE_BOUNDARY_LENGTHS,
  DATUM_CANONICITY_BOUNDARIES,
  FIELD_PREIMAGE_LENGTH_SOURCE,
  FIELD_PREIMAGE_LENGTHS,
  FIELD_VECTORS,
  filler,
  FIXED_INDEX_BOUNDARIES,
  redeemer,
} from "../tests/fixtures/native-tx-field-items-v1.vectors.mjs";
import {
  assertCarriageBoundariesStraddleKV1,
  buildStraddle,
  LANGUAGE_TAG_VECTORS,
} from "./generate-native-tx-field-items-v1-goldens.build-straddle.mjs";
import { aikenBytes, hex } from "./golden-channel.mjs";

// ---------------------------------------------------------------------------
// Golden assembly
// ---------------------------------------------------------------------------

/**
 * §8, simplest-fitting-first. Each field's populated vectors select a tier by
 * preimage length; the boundary lengths themselves are pinned here so the
 * partition (§8.4 — a preimage has exactly one admissible carriage) is a
 * cross-language fact rather than a convention.
 */

export const buildGolden = () => {
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
    schema: "midgard-native-tx-field-items-v1-golden",
    version: 1,
    specDocument: "docs/spec/midgard-tx.md",
    generator:
      "demo/midgard-core/scripts/generate-native-tx-field-items-v1-goldens.mjs",
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

export const section = (title) => [
  "// ---------------------------------------------------------------------------",
  `// ${title}`,
  "// ---------------------------------------------------------------------------",
  "",
];

/** Aiken literals for the structured items of the directly-assertable fields. */
export const aikenItemLiteral = (fieldIndex, item) => {
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

export const aikenMintPolicyId = (rank) =>
  `builtin.b_data(${aikenBytes(MINT_NEGATIVE_POLICY_IDS[rank])})`;

export const aikenMintAssets = (assets) =>
  `builtin.map_data([${assets
    .map(
      ([assetNameHex, quantity]) =>
        `Pair(builtin.b_data(${aikenBytes(assetNameHex)}), builtin.i_data(${quantity}))`,
    )
    .join(", ")}])`;

export const aikenMintData = (policies) =>
  `builtin.map_data([${policies
    .map(
      ([rank, assets]) =>
        `Pair(${aikenMintPolicyId(rank)}, ${aikenMintAssets(assets)})`,
    )
    .join(", ")}])`;

/** The Aiken item encoder that takes the literal above. */
export const AIKEN_ITEM_ENCODERS = {
  0: "encode_midgard_tx_input",
  1: "encode_midgard_tx_input",
  3: undefined,
  4: undefined,
  6: "encode_midgard_versioned_script",
  7: "encode_midgard_address_witness",
  8: "encode_midgard_redeemer_witness",
};
