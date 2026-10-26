import {
  MIDGARD_CEK_MAX_DIRECT_CONSTANT_PAYLOAD_BYTES,
  type MidgardCekConstantType,
  MidgardCekConstantTypeTags as Tags,
  type MidgardCekConstantWitness,
  sameBytes,
} from "./cek-constant.semantic-data.js";
import {
  isMidgardCekCanonicalValueData,
  midgardCekValueEntryCount,
} from "./cek-constant.value.js";
import { plutusDataFromCborIterative } from "./plutus-data-iterative.decode.js";
import { encodeMidgardCekPlutusData } from "./plutus-data-iterative.encode.js";
import { type ConstType, ConstTyTag } from "./uplc-constant.js";
import { type Data, DataB, DataConstr, DataI, DataList } from "./uplc-data.js";

type ParsedTags = {
  readonly type: MidgardCekConstantType;
  readonly next: number;
};

/** Parses Midgard's committed type tags, which use Cardano's numbering. */
const parseTagsAt = (tags: readonly Data[], offset: number): ParsedTags => {
  const item = tags[offset];
  if (!(item instanceof DataI) || item.int < 0n || item.int > 13n) {
    throw new Error("V1 constant type has an unknown tag");
  }
  const tag = Number(item.int);
  const leaf = (type: MidgardCekConstantType): ParsedTags => ({
    type,
    next: offset + 1,
  });
  switch (tag) {
    case Tags.Integer:
      return leaf({ kind: "integer" });
    case Tags.ByteString:
      return leaf({ kind: "bytes" });
    case Tags.String:
      return leaf({ kind: "string" });
    case Tags.Unit:
      return leaf({ kind: "unit" });
    case Tags.Boolean:
      return leaf({ kind: "boolean" });
    case Tags.List:
    case Tags.Array: {
      const element = parseTagsAt(tags, offset + 1);
      return {
        type: {
          kind: tag === Tags.List ? "list" : "array",
          element: element.type,
        },
        next: element.next,
      };
    }
    case Tags.Pair: {
      const first = parseTagsAt(tags, offset + 1);
      const second = parseTagsAt(tags, first.next);
      return {
        type: { kind: "pair", first: first.type, second: second.type },
        next: second.next,
      };
    }
    case Tags.Data:
      return leaf({ kind: "data" });
    case Tags.BlsG1:
      return leaf({ kind: "blsG1" });
    case Tags.BlsG2:
      return leaf({ kind: "blsG2" });
    case Tags.BlsMillerLoopResult:
      return leaf({ kind: "blsMillerLoopResult" });
    case Tags.Value:
      return leaf({ kind: "value" });
    default:
      throw new Error("V1 constant type has an unknown tag");
  }
};

const parseTypeTags = (typeData: Data): MidgardCekConstantType => {
  if (!(typeData instanceof DataList)) {
    throw new Error("V1 constant type is not a tag list");
  }
  const parsed = parseTagsAt(typeData.list, 0);
  if (parsed.next !== typeData.list.length) {
    throw new Error("V1 constant type has trailing tags");
  }
  return parsed.type;
};

export const decodeMidgardCekConstantTypeCbor = (
  typeCbor: Uint8Array,
): MidgardCekConstantType => {
  if (typeCbor.length > 64) {
    throw new Error("V1 constant type exceeds its direct bound");
  }
  const typeData = plutusDataFromCborIterative(typeCbor);
  if (
    !sameBytes(encodeMidgardCekPlutusData(typeData), typeCbor) ||
    !(typeData instanceof DataList)
  ) {
    throw new Error("V1 constant type is not canonical");
  }
  return parseTypeTags(typeData);
};

/**
 * The number of type tags Cardano's flat spends on a constant type, where a
 * list, an array and a pair each carry their type-application tags.
 */
export const midgardCekConstantTypeFlatSize = (
  type: MidgardCekConstantType,
): number => {
  if (type.kind === "list" || type.kind === "array") {
    return 2 + midgardCekConstantTypeFlatSize(type.element);
  }
  if (type.kind === "pair") {
    return (
      3 +
      midgardCekConstantTypeFlatSize(type.first) +
      midgardCekConstantTypeFlatSize(type.second)
    );
  }
  return 1;
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
        const bytes = payload.bytes;
        return sameBytes(
          Buffer.from(
            new TextDecoder("utf-8", { fatal: true, ignoreBOM: true }).decode(
              bytes,
            ),
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
      return payload instanceof DataB && payload.bytes.length === 48;
    case "blsG2":
      return payload instanceof DataB && payload.bytes.length === 96;
    case "blsMillerLoopResult":
      return false;
    case "array":
      return (
        payload instanceof DataList &&
        payload.list.every((item) => payloadMatchesType(type.element, item))
      );
    case "value":
      return isMidgardCekCanonicalValueData(payload);
  }
};

/** Memory of the PV 11 constant kinds: an array's length, a Value's entries. */
export const midgardCekPv11ConstantMemorySize = (
  type: MidgardCekConstantType & { readonly kind: "array" | "value" },
  payload: Data,
): bigint => {
  if (type.kind === "value") return midgardCekValueEntryCount(payload);
  if (!(payload instanceof DataList)) {
    throw new Error("V1 array payload is not DataList");
  }
  return BigInt(payload.list.length);
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
  const typeData = plutusDataFromCborIterative(witness.typeCbor);
  const payload = plutusDataFromCborIterative(witness.payloadCbor);
  if (
    !sameBytes(encodeMidgardCekPlutusData(typeData), witness.typeCbor) ||
    !sameBytes(encodeMidgardCekPlutusData(payload), witness.payloadCbor)
  ) {
    throw new Error("V1 constant witness is not canonical Data CBOR");
  }
  const type = parseTypeTags(typeData);
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
    case "array":
    case "value":
      throw new Error("Harmonic numbers Value and array types differently");
  }
};

/** Midgard's committed type tags, in Cardano's numbering. */
const midgardCekConstantTypeTags = (type: MidgardCekConstantType): number[] => {
  if (type.kind === "list") {
    return [Tags.List, ...midgardCekConstantTypeTags(type.element)];
  }
  if (type.kind === "array") {
    return [Tags.Array, ...midgardCekConstantTypeTags(type.element)];
  }
  if (type.kind === "pair") {
    return [
      Tags.Pair,
      ...midgardCekConstantTypeTags(type.first),
      ...midgardCekConstantTypeTags(type.second),
    ];
  }
  if (type.kind === "value") return [Tags.Value];
  return midgardConstantTypeToTags(type);
};

export const encodeMidgardCekConstantTypeCbor = (
  type: MidgardCekConstantType,
): Buffer =>
  encodeMidgardCekPlutusData(
    new DataList(
      midgardCekConstantTypeTags(type).map((tag) => new DataI(BigInt(tag))),
    ),
  );
