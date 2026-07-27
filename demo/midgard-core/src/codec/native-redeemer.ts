import { CML } from "@lucid-evolution/lucid";

import {
  asArray,
  asBytes,
  decodeSingleCbor,
  encodeCbor,
} from "./cbor.js";
import { MidgardTxCodecError, MidgardTxCodecErrorCodes } from "./errors.js";
import { asFixedArray, asUnsigned } from "./native-validation.js";

type NormalizedRedeemer = {
  readonly tag: CML.RedeemerTag;
  readonly index: bigint;
  readonly dataCbor: Buffer;
  readonly memory: bigint;
  readonly steps: bigint;
};

const supportedCardanoRedeemerTag = (
  tag: number,
): tag is CML.RedeemerTag.Spend | CML.RedeemerTag.Mint | CML.RedeemerTag.Reward =>
  tag === CML.RedeemerTag.Spend ||
  tag === CML.RedeemerTag.Mint ||
  tag === CML.RedeemerTag.Reward;

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
    if (redeemerPointer(sorted[index - 1]!) === redeemerPointer(sorted[index]!)) {
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

const canonicalPlutusData = (
  dataCbor: Uint8Array,
  fieldName: string,
): { readonly data: CML.PlutusData; readonly cbor: Buffer } => {
  let data: CML.PlutusData;
  try {
    data = CML.PlutusData.from_cbor_bytes(dataCbor);
  } catch (error) {
    throw new MidgardTxCodecError(
      MidgardTxCodecErrorCodes.SchemaMismatch,
      `${fieldName} must contain one Plutus Data item`,
      String(error),
    );
  }
  const canonical = Buffer.from(data.to_canonical_cbor_bytes());
  if (!canonical.equals(dataCbor)) {
    throw new MidgardTxCodecError(
      MidgardTxCodecErrorCodes.SchemaMismatch,
      `${fieldName} must contain canonical Plutus Data CBOR`,
    );
  }
  return { data, cbor: canonical };
};

export const cardanoRedeemersToMidgardPreimageCbor = (
  redeemers: CML.Redeemers | undefined,
  fieldName = "transaction_witness_set.redeemers",
): Buffer => {
  if (redeemers === undefined) {
    return encodeCbor([]);
  }

  const flat = redeemers.to_flat_format();
  const normalized: NormalizedRedeemer[] = [];
  for (let index = 0; index < flat.len(); index += 1) {
    const redeemer = flat.get(index);
    const tag = ensureSupportedCardanoRedeemerTag(
      redeemer.tag(),
      `${fieldName}[${index}]`,
    );
    const dataCbor = Buffer.from(
      redeemer.data().to_canonical_cbor_bytes(),
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

  return encodeCbor(
    normalizeRedeemers(normalized, fieldName).map((redeemer) => [
      redeemer.tag,
      redeemer.index,
      redeemer.dataCbor,
      [redeemer.memory, redeemer.steps],
    ]),
  );
};

export const midgardRedeemersToCardano = (
  preimageCbor: Uint8Array,
  fieldName = "native.redeemers",
): CML.Redeemers | undefined => {
  const decoded = decodeSingleCbor(preimageCbor);
  const entries = asArray(decoded, fieldName);
  if (entries.length === 0) {
    return undefined;
  }

  const normalized = normalizeRedeemers(
    entries.map((entry, index): NormalizedRedeemer => {
      const itemField = `${fieldName}[${index}]`;
      const item = asFixedArray(entry, 4, itemField);
      const tagValue = asUnsigned(item[0], `${itemField}.tag`);
      if (tagValue > BigInt(Number.MAX_SAFE_INTEGER)) {
        throw new MidgardTxCodecError(
          MidgardTxCodecErrorCodes.InvalidFieldType,
          `${itemField}.tag exceeds the supported integer range`,
          tagValue.toString(),
        );
      }
      const tag = ensureSupportedCardanoRedeemerTag(
        Number(tagValue),
        itemField,
      );
      const indexValue = asUnsigned(item[1], `${itemField}.index`);
      const dataCbor = asBytes(item[2], `${itemField}.data_cbor`);
      const { cbor } = canonicalPlutusData(
        dataCbor,
        `${itemField}.data_cbor`,
      );
      const executionUnits = asFixedArray(
        item[3],
        2,
        `${itemField}.execution_units`,
      );
      return {
        tag,
        index: indexValue,
        dataCbor: cbor,
        memory: asUnsigned(
          executionUnits[0],
          `${itemField}.execution_units.memory`,
        ),
        steps: asUnsigned(
          executionUnits[1],
          `${itemField}.execution_units.steps`,
        ),
      };
    }),
    fieldName,
  );

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
  return CML.Redeemers.new_map_redeemer_key_to_redeemer_val(
    redeemerMap,
  );
};
