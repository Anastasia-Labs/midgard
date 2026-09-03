import { CML } from "@lucid-evolution/lucid";

import {
  aikenSerialisedPlutusDataCborPreservingMapOrder,
  assertMidgardPlutusDataWellFormed,
} from "../plutus-data-cbor.js";
import { MidgardTxCodecError, MidgardTxCodecErrorCodes } from "./errors.js";
import {
  decodeMidgardFieldPreimage,
  encodeMidgardFieldPreimage,
} from "./native-tx-field-access.js";
import {
  decodeMidgardRedeemerWitnessItem,
  midgardRedeemerPurposeFromTag,
} from "./native-tx-field-item-decoders.js";
import {
  encodeMidgardRedeemerWitnessItem,
  MIDGARD_REDEEMER_PURPOSE_TAGS,
  type MidgardRedeemerPurpose,
} from "./native-tx-field-items.js";

export type NormalizedRedeemer = {
  readonly tag: CML.RedeemerTag;
  readonly index: bigint;
  readonly dataCbor: Buffer;
  readonly memory: bigint;
  readonly steps: bigint;
};

const supportedCardanoRedeemerTag = (
  tag: number,
): tag is
  | CML.RedeemerTag.Spend
  | CML.RedeemerTag.Mint
  | CML.RedeemerTag.Reward =>
  tag === Number(CML.RedeemerTag.Spend) ||
  tag === Number(CML.RedeemerTag.Mint) ||
  tag === Number(CML.RedeemerTag.Reward);

const redeemerPointer = (
  redeemer: Pick<NormalizedRedeemer, "tag" | "index">,
): string => `${redeemer.tag.toString()}:${redeemer.index.toString()}`;

const compareRedeemers = (
  left: NormalizedRedeemer,
  right: NormalizedRedeemer,
): number =>
  left.tag - right.tag ||
  (left.index < right.index ? -1 : left.index > right.index ? 1 : 0);

const normalizeRedeemers = (
  redeemers: readonly NormalizedRedeemer[],
  fieldName: string,
): readonly NormalizedRedeemer[] => {
  const sorted = [...redeemers].sort(compareRedeemers);
  for (let index = 1; index < sorted.length; index += 1) {
    if (
      redeemerPointer(sorted[index - 1]!) === redeemerPointer(sorted[index]!)
    ) {
      throw new MidgardTxCodecError(
        MidgardTxCodecErrorCodes.SchemaMismatch,
        `${fieldName} contains a duplicate redeemer pointer`,
        redeemerPointer(sorted[index]!),
      );
    }
  }
  return sorted;
};

const ensureSupportedCardanoRedeemerTag = (
  tag: number,
  fieldName: string,
): CML.RedeemerTag => {
  if (!supportedCardanoRedeemerTag(tag)) {
    throw new MidgardTxCodecError(
      MidgardTxCodecErrorCodes.ConversionUnsupportedFeature,
      "Cardano and Midgard redeemer purposes cannot be converted without loss",
      `${fieldName}.tag=${tag.toString()}`,
    );
  }
  return tag;
};

/**
 * Validates one canonical Plutus Data value without materializing a CML
 * object. Well-formedness uses the recursion-free
 * `assertMidgardPlutusDataWellFormed` pass instead of the former
 * `CML.PlutusData.from_cbor_bytes` probe, whose wasm build traps near 1,522
 * nested nodes, so validation depth is bounded only by the bytes that carry
 * the value.
 */
const validateCanonicalPlutusDataCbor = (
  dataCbor: Uint8Array,
  fieldName: string,
): Buffer => {
  const source = Buffer.from(dataCbor);
  try {
    assertMidgardPlutusDataWellFormed(source);
  } catch (error) {
    throw new MidgardTxCodecError(
      MidgardTxCodecErrorCodes.SchemaMismatch,
      `${fieldName} must contain one Plutus Data item`,
      String(error),
    );
  }
  let canonical: Buffer;
  try {
    canonical = Buffer.from(
      aikenSerialisedPlutusDataCborPreservingMapOrder(source.toString("hex")),
      "hex",
    );
  } catch (error) {
    throw new MidgardTxCodecError(
      MidgardTxCodecErrorCodes.SchemaMismatch,
      `${fieldName} must contain supported Plutus Data`,
      String(error),
    );
  }
  if (!canonical.equals(source)) {
    throw new MidgardTxCodecError(
      MidgardTxCodecErrorCodes.SchemaMismatch,
      `${fieldName} must contain canonical Plutus Data CBOR`,
    );
  }
  return canonical;
};

/**
 * Validates like `validateCanonicalPlutusDataCbor` and additionally parses
 * the value into a `CML.PlutusData` for callers that must hand Cardano a CML
 * object. The CML parse is unavoidable here and stays subject to CML's own
 * wasm recursion ceiling (~1,522 nested nodes), so this must only be used
 * where a CML value is genuinely required — today that is the reverse
 * Midgard-to-Cardano bridge, never the Cardano-to-Midgard admission path.
 */
const canonicalPlutusData = (
  dataCbor: Uint8Array,
  fieldName: string,
): { readonly data: CML.PlutusData; readonly cbor: Buffer } => {
  const cbor = validateCanonicalPlutusDataCbor(dataCbor, fieldName);
  let data: CML.PlutusData;
  try {
    data = CML.PlutusData.from_cbor_bytes(cbor);
  } catch (error) {
    throw new MidgardTxCodecError(
      MidgardTxCodecErrorCodes.SchemaMismatch,
      `${fieldName} must contain one Plutus Data item`,
      String(error),
    );
  }
  return { data, cbor };
};

const normalizeCardanoPlutusData = (
  data: CML.PlutusData,
  fieldName: string,
): Buffer => {
  try {
    return Buffer.from(
      aikenSerialisedPlutusDataCborPreservingMapOrder(data.to_cbor_hex()),
      "hex",
    );
  } catch (error) {
    throw new MidgardTxCodecError(
      MidgardTxCodecErrorCodes.SchemaMismatch,
      `${fieldName} must contain supported Plutus Data`,
      String(error),
    );
  }
};

/**
 * §5.3's `purpose_tag` for a Cardano `RedeemerTag`. Values 0–5 reuse Cardano's
 * own numbering, so this is a lookup rather than a translation — but it is
 * spelled through the §5.3 table rather than by passing the CML number along,
 * so a future divergence between the two numberings surfaces here instead of
 * silently changing committed bytes.
 *
 * The table read is `midgardRedeemerPurposeFromTag`, the §5.3 decoder's own —
 * not a reverse scan of `MIDGARD_REDEEMER_PURPOSE_TAGS` spelled again here.
 * Only the diagnostic differs: a tag out of §5.3's set reached from Cardano is an
 * unsupported *conversion*, which is the error class this module's callers
 * discriminate on, so the decoder's grammar error is re-raised as one.
 */
const midgardRedeemerPurposeForCardanoTag = (
  tag: CML.RedeemerTag,
  fieldName: string,
): MidgardRedeemerPurpose => {
  try {
    return midgardRedeemerPurposeFromTag(Number(tag));
  } catch {
    throw new MidgardTxCodecError(
      MidgardTxCodecErrorCodes.ConversionUnsupportedFeature,
      "Cardano redeemer tag has no §5.3 purpose",
      `${fieldName}.tag=${tag.toString()}`,
    );
  }
};

/**
 * The §5.1 preimage of field 8 (`redeemer_tx_wits`).
 *
 * Each item is §5.3's four-element `enc_8` inside the per-item byte-string
 * envelope. The retired counted scheme concatenated raw item CBOR here; §5.1
 * prohibits that form for all nine fields.
 */
const encodeMidgardRedeemerPreimageCbor = (
  redeemers: readonly NormalizedRedeemer[],
  fieldName: string,
): Buffer =>
  encodeMidgardFieldPreimage(
    redeemers.map((redeemer) =>
      encodeMidgardRedeemerWitnessItem({
        purpose: midgardRedeemerPurposeForCardanoTag(redeemer.tag, fieldName),
        index: redeemer.index,
        redeemerCbor: redeemer.dataCbor,
        executionUnits: { memory: redeemer.memory, steps: redeemer.steps },
      }),
    ),
  );

export const cardanoRedeemersToMidgardPreimageCbor = (
  redeemers: CML.Redeemers | undefined,
  fieldName = "transaction_witness_set.redeemers",
): Buffer => {
  if (redeemers === undefined) {
    return encodeMidgardFieldPreimage([]);
  }

  const flat = redeemers.to_flat_format();
  const normalized: NormalizedRedeemer[] = [];
  for (let index = 0; index < flat.len(); index += 1) {
    const redeemer = flat.get(index);
    const tag = ensureSupportedCardanoRedeemerTag(
      redeemer.tag(),
      `${fieldName}[${index}]`,
    );
    const dataCbor = normalizeCardanoPlutusData(
      redeemer.data(),
      `${fieldName}[${index}].data`,
    );
    const executionUnits = redeemer.ex_units();
    normalized.push({
      tag,
      index: redeemer.index(),
      dataCbor,
      memory: executionUnits.mem(),
      steps: executionUnits.steps(),
    });
  }

  return encodeMidgardRedeemerPreimageCbor(
    normalizeRedeemers(normalized, fieldName),
    fieldName,
  );
};

/**
 * §5.1 then §5.3: the field-8 items, read back into the codec's normalized
 * shape. Ordering and duplicate-pointer rejection stay in
 * {@link normalizeRedeemers}, which both directions share.
 */
export const decodeMidgardRedeemerPreimageCbor = (
  preimageCbor: Uint8Array,
  fieldName = "native.redeemers",
): readonly NormalizedRedeemer[] =>
  normalizeRedeemers(
    decodeMidgardFieldPreimage(preimageCbor)
      .map(decodeMidgardRedeemerWitnessItem)
      .map((witness, index): NormalizedRedeemer => {
        const itemField = `${fieldName}[${index}]`;
        return {
          tag: ensureSupportedCardanoRedeemerTag(
            MIDGARD_REDEEMER_PURPOSE_TAGS[witness.purpose],
            itemField,
          ),
          index: witness.index,
          dataCbor: validateCanonicalPlutusDataCbor(
            witness.redeemerCbor,
            `${itemField}.data_cbor`,
          ),
          memory: witness.executionUnits.memory,
          steps: witness.executionUnits.steps,
        };
      }),
    fieldName,
  );

export const midgardRedeemersToCardano = (
  preimageCbor: Uint8Array,
  fieldName = "native.redeemers",
): CML.Redeemers | undefined => {
  const normalized = decodeMidgardRedeemerPreimageCbor(preimageCbor, fieldName);
  if (normalized.length === 0) {
    return undefined;
  }

  const redeemerMap = CML.MapRedeemerKeyToRedeemerVal.new();
  for (const redeemer of normalized) {
    const { data } = canonicalPlutusData(
      redeemer.dataCbor,
      `${fieldName}.${redeemerPointer(redeemer)}.data_cbor`,
    );
    redeemerMap.insert(
      CML.RedeemerKey.new(redeemer.tag, redeemer.index),
      CML.RedeemerVal.new(
        data,
        CML.ExUnits.new(redeemer.memory, redeemer.steps),
      ),
    );
  }
  return CML.Redeemers.new_map_redeemer_key_to_redeemer_val(redeemerMap);
};
