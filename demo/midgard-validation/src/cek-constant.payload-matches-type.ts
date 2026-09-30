import {
  type Data,
  DataB,
  DataConstr,
  dataFromCbor,
  DataI,
  DataList,
} from "@harmoniclabs/plutus-data";
import { type ConstType, ConstTyTag } from "@harmoniclabs/uplc";

import {
  asByteArray,
  encodeCardanoBytes,
  encodeCardanoInteger,
  encodeCardanoList,
  encodeSmallCborArgument,
  MIDGARD_CEK_MAX_DIRECT_CONSTANT_PAYLOAD_BYTES,
  type MidgardCekConstantType,
  type MidgardCekConstantWitness,
  parseMidgardCekConstantType,
  sameBytes,
} from "./cek-constant.semantic-data.js";
import { isPlutusDataMap } from "./plutus-data-narrowing.js";

export const decodeMidgardCekConstantTypeCbor = (
  typeCbor: Uint8Array,
): MidgardCekConstantType => {
  if (typeCbor.length > 64) {
    throw new Error("V1 constant type exceeds its direct bound");
  }
  const typeData = dataFromCbor(typeCbor);
  if (
    !sameBytes(encodeMidgardCekPlutusData(typeData), typeCbor) ||
    !(typeData instanceof DataList)
  ) {
    throw new Error("V1 constant type is not canonical");
  }
  return parseMidgardCekConstantType(
    typeData.list.map((tag): ConstTyTag => {
      if (
        !(tag instanceof DataI) ||
        tag.int < 0n ||
        tag.int > 11n ||
        tag.int === 7n
      ) {
        throw new Error("V1 constant type has an unknown tag");
      }
      return Number(tag.int) as ConstTyTag;
    }) as ConstType,
  );
};

/**
 * Exact `cbor.serialise(Data)`/cardano-node representation. The upstream
 * harmonic serializer currently loses every byte after the first 64 in
 * dynamic byte strings and rejects negative bignums below the uint64 major-1
 * domain. Consensus code therefore encodes both scalar classes directly.
 */
export const encodeMidgardCekPlutusData = (data: Data): Buffer => {
  if (data instanceof DataI) {
    return encodeCardanoInteger(data.int);
  }
  if (data instanceof DataB) {
    return encodeCardanoBytes(asByteArray(data.bytes));
  }
  if (data instanceof DataList) {
    return encodeCardanoList(
      data.list.map((item) => encodeMidgardCekPlutusData(item)),
    );
  }
  if (isPlutusDataMap(data)) {
    return Buffer.concat([
      encodeSmallCborArgument(5, BigInt(data.map.length)),
      ...data.map.flatMap((entry) => [
        encodeMidgardCekPlutusData(entry.fst),
        encodeMidgardCekPlutusData(entry.snd),
      ]),
    ]);
  }
  if (data instanceof DataConstr) {
    const fields = encodeCardanoList(
      data.fields.map((field) => encodeMidgardCekPlutusData(field)),
    );
    if (data.constr <= 6n) {
      return Buffer.concat([
        encodeSmallCborArgument(6, 121n + data.constr),
        fields,
      ]);
    }
    if (data.constr <= 127n) {
      return Buffer.concat([
        encodeSmallCborArgument(6, 1280n + data.constr - 7n),
        fields,
      ]);
    }
    return Buffer.concat([
      encodeSmallCborArgument(6, 102n),
      Buffer.from([0x82]),
      encodeCardanoInteger(data.constr),
      fields,
    ]);
  }
  throw new Error("V1 constant contains unknown Plutus Data");
};

const payloadMatchesType = (
  type: MidgardCekConstantType,
  payload: Data,
): boolean => {
  switch (type.kind) {
    case "integer":
      return payload instanceof DataI;
    case "bytes":
      return payload instanceof DataB;
    case "string":
      if (!(payload instanceof DataB)) return false;
      try {
        const bytes = asByteArray(payload.bytes);
        return sameBytes(
          Buffer.from(
            new TextDecoder("utf-8", { fatal: true }).decode(bytes),
            "utf8",
          ),
          bytes,
        );
      } catch {
        return false;
      }
    case "unit":
      return (
        payload instanceof DataConstr &&
        payload.constr === 0n &&
        payload.fields.length === 0
      );
    case "boolean":
      return (
        payload instanceof DataConstr &&
        (payload.constr === 0n || payload.constr === 1n) &&
        payload.fields.length === 0
      );
    case "list":
      return (
        payload instanceof DataList &&
        payload.list.every((item) => payloadMatchesType(type.element, item))
      );
    case "pair":
      return (
        payload instanceof DataConstr &&
        payload.constr === 0n &&
        payload.fields.length === 2 &&
        payloadMatchesType(type.first, payload.fields[0]) &&
        payloadMatchesType(type.second, payload.fields[1])
      );
    case "data":
      return true;
    case "blsG1":
      return (
        payload instanceof DataB && asByteArray(payload.bytes).length === 48
      );
    case "blsG2":
      return (
        payload instanceof DataB && asByteArray(payload.bytes).length === 96
      );
    case "blsMillerLoopResult":
      return false;
  }
};

/**
 * Decodes the exact constant witness accepted by the L1 verifier. Both Data
 * values must round-trip to the supplied canonical CBOR, and the semantic
 * payload must match the recursively decoded constant type.
 */
export const decodeMidgardCekConstantWitness = (
  witness: MidgardCekConstantWitness,
): {
  readonly type: MidgardCekConstantType;
  readonly payload: Data;
} => {
  if (witness.typeCbor.length > 64) {
    throw new Error("V1 constant type exceeds its direct bound");
  }
  if (
    witness.payloadCbor.length > MIDGARD_CEK_MAX_DIRECT_CONSTANT_PAYLOAD_BYTES
  ) {
    throw new Error("V1 constant payload exceeds its direct bound");
  }
  const typeData = dataFromCbor(witness.typeCbor);
  const payload = dataFromCbor(witness.payloadCbor);
  if (
    !sameBytes(encodeMidgardCekPlutusData(typeData), witness.typeCbor) ||
    !sameBytes(encodeMidgardCekPlutusData(payload), witness.payloadCbor)
  ) {
    throw new Error("V1 constant witness is not canonical Data CBOR");
  }
  if (!(typeData instanceof DataList)) {
    throw new Error("V1 constant type is not a tag list");
  }
  const tags = typeData.list.map((tag): ConstTyTag => {
    if (
      !(tag instanceof DataI) ||
      tag.int < 0n ||
      tag.int > 11n ||
      tag.int === 7n
    ) {
      throw new Error("V1 constant type has an unknown tag");
    }
    return Number(tag.int) as ConstTyTag;
  }) as ConstType;
  const type = parseMidgardCekConstantType(tags);
  if (!payloadMatchesType(type, payload)) {
    throw new Error("V1 constant payload does not match its type");
  }
  return Object.freeze({ type, payload });
};

export const midgardConstantTypeToTags = (
  type: MidgardCekConstantType,
): ConstType => {
  switch (type.kind) {
    case "integer":
      return [ConstTyTag.int];
    case "bytes":
      return [ConstTyTag.byteStr];
    case "string":
      return [ConstTyTag.str];
    case "unit":
      return [ConstTyTag.unit];
    case "boolean":
      return [ConstTyTag.bool];
    case "list":
      return [ConstTyTag.list, ...midgardConstantTypeToTags(type.element)];
    case "pair":
      return [
        ConstTyTag.pair,
        ...midgardConstantTypeToTags(type.first),
        ...midgardConstantTypeToTags(type.second),
      ];
    case "data":
      return [ConstTyTag.data];
    case "blsG1":
      return [ConstTyTag.bls12_381_G1_element];
    case "blsG2":
      return [ConstTyTag.bls12_381_G2_element];
    case "blsMillerLoopResult":
      return [ConstTyTag.bls12_381_MlResult];
  }
};

export const encodeMidgardCekConstantTypeCbor = (
  type: MidgardCekConstantType,
): Buffer =>
  encodeMidgardCekPlutusData(
    new DataList(
      midgardConstantTypeToTags(type).map((tag) => new DataI(BigInt(tag))),
    ),
  );
