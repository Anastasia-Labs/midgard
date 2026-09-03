import {
  hashMidgardCekBlobChunk,
  hashMidgardCekValueNode,
  MIDGARD_CEK_MAX_SOURCE_CONSTANT_PAYLOAD_BYTES,
} from "@al-ft/midgard-core";
import { encodeCborBytes } from "@al-ft/midgard-core/codec/cbor";
import {
  type Data,
  DataB,
  DataConstr,
  dataFromCbor,
  DataI,
  DataList,
  DataMap,
  isData,
} from "@harmoniclabs/plutus-data";
import {
  type ConstType,
  ConstTyTag,
  type ConstValue,
  UPLCConst,
} from "@harmoniclabs/uplc";

import { commitMidgardCekDataTree } from "./cek-data-tree.js";

export type MidgardCekConstantType =
  | { readonly kind: "integer" }
  | { readonly kind: "bytes" }
  | { readonly kind: "string" }
  | { readonly kind: "unit" }
  | { readonly kind: "boolean" }
  | {
      readonly kind: "list";
      readonly element: MidgardCekConstantType;
    }
  | {
      readonly kind: "pair";
      readonly first: MidgardCekConstantType;
      readonly second: MidgardCekConstantType;
    }
  | { readonly kind: "data" }
  | { readonly kind: "blsG1" }
  | { readonly kind: "blsG2" }
  | { readonly kind: "blsMillerLoopResult" };

type ParsedType = {
  readonly type: MidgardCekConstantType;
  readonly nextOffset: number;
};

const parseTypeAt = (
  tags: readonly ConstTyTag[],
  offset: number,
): ParsedType => {
  const tag = tags[offset];
  switch (tag) {
    case ConstTyTag.int:
      return { type: { kind: "integer" }, nextOffset: offset + 1 };
    case ConstTyTag.byteStr:
      return { type: { kind: "bytes" }, nextOffset: offset + 1 };
    case ConstTyTag.str:
      return { type: { kind: "string" }, nextOffset: offset + 1 };
    case ConstTyTag.unit:
      return { type: { kind: "unit" }, nextOffset: offset + 1 };
    case ConstTyTag.bool:
      return { type: { kind: "boolean" }, nextOffset: offset + 1 };
    case ConstTyTag.list: {
      const element = parseTypeAt(tags, offset + 1);
      return {
        type: { kind: "list", element: element.type },
        nextOffset: element.nextOffset,
      };
    }
    case ConstTyTag.pair: {
      const first = parseTypeAt(tags, offset + 1);
      const second = parseTypeAt(tags, first.nextOffset);
      return {
        type: {
          kind: "pair",
          first: first.type,
          second: second.type,
        },
        nextOffset: second.nextOffset,
      };
    }
    case ConstTyTag.data:
      return { type: { kind: "data" }, nextOffset: offset + 1 };
    case ConstTyTag.bls12_381_G1_element:
      return { type: { kind: "blsG1" }, nextOffset: offset + 1 };
    case ConstTyTag.bls12_381_G2_element:
      return { type: { kind: "blsG2" }, nextOffset: offset + 1 };
    case ConstTyTag.bls12_381_MlResult:
      return {
        type: { kind: "blsMillerLoopResult" },
        nextOffset: offset + 1,
      };
    default:
      throw new Error("V1 constant has an unknown type tag");
  }
};

export const parseMidgardCekConstantType = (
  tags: ConstType,
): MidgardCekConstantType => {
  const parsed = parseTypeAt(tags, 0);
  if (parsed.nextOffset !== tags.length) {
    throw new Error("V1 constant type has trailing tags");
  }
  return parsed.type;
};

const asByteArray = (value: unknown): Uint8Array => {
  if (
    typeof value !== "object" ||
    value === null ||
    !("toBuffer" in value) ||
    typeof value.toBuffer !== "function"
  ) {
    throw new Error("V1 bytes constant has an invalid value");
  }
  const bytes = value.toBuffer();
  if (!(bytes instanceof Uint8Array)) {
    throw new Error("V1 bytes constant did not produce bytes");
  }
  return bytes;
};

const semanticData = (
  type: MidgardCekConstantType,
  value: ConstValue,
): Data => {
  switch (type.kind) {
    case "integer":
      if (typeof value !== "bigint" && typeof value !== "number") {
        throw new Error("V1 integer constant has an invalid value");
      }
      return new DataI(BigInt(value));
    case "bytes":
      return new DataB(asByteArray(value));
    case "string":
      if (typeof value !== "string") {
        throw new Error("V1 string constant has an invalid value");
      }
      return new DataB(Buffer.from(value, "utf8"));
    case "unit":
      if (value !== undefined) {
        throw new Error("V1 unit constant has an invalid value");
      }
      return new DataConstr(0, []);
    case "boolean":
      if (typeof value !== "boolean") {
        throw new Error("V1 boolean constant has an invalid value");
      }
      return new DataConstr(value ? 1 : 0, []);
    case "list":
      if (!Array.isArray(value)) {
        throw new Error("V1 list constant has an invalid value");
      }
      return new DataList(
        value.map((item) => semanticData(type.element, item)),
      );
    case "pair": {
      if (
        typeof value !== "object" ||
        value === null ||
        !("fst" in value) ||
        !("snd" in value)
      ) {
        throw new Error("V1 pair constant has an invalid value");
      }
      return new DataConstr(0, [
        semanticData(type.first, value.fst as ConstValue),
        semanticData(type.second, value.snd as ConstValue),
      ]);
    }
    case "data":
      if (!isData(value)) {
        throw new Error("V1 data constant has an invalid value");
      }
      return value;
    case "blsG1":
    case "blsG2":
    case "blsMillerLoopResult":
      // Flat does not permit BLS values as source constants. Runtime BLS
      // results use dedicated proof nodes produced by their builtin rules.
      throw new Error(
        "V1 source programs cannot contain encoded BLS constants",
      );
  }
};

export type MidgardCekCanonicalConstant = {
  readonly type: MidgardCekConstantType;
  readonly typeCbor: Buffer;
  readonly payloadCbor: Buffer;
};

export type MidgardCekConstantWitness = {
  readonly typeCbor: Uint8Array;
  readonly payloadCbor: Uint8Array;
};

export type MidgardCekSemanticConstantWitness = {
  readonly typeCbor: Uint8Array;
  readonly payload: {
    readonly root: Uint8Array;
    readonly cborLength: bigint;
    readonly memory: bigint;
  };
  readonly memory: bigint;
};

// A direct constant is one independently revealed L1 proof preimage. The
// profile reserves 7 KiB for the one-step evidence and transaction framing,
// so the payload must remain strictly below the 16 KiB proof floor.
export const MIDGARD_CEK_MAX_DIRECT_CONSTANT_PAYLOAD_BYTES =
  MIDGARD_CEK_MAX_SOURCE_CONSTANT_PAYLOAD_BYTES;

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

const sameBytes = (left: Uint8Array, right: Uint8Array): boolean =>
  Buffer.from(left).equals(Buffer.from(right));

const encodeSmallCborArgument = (major: number, value: bigint): Buffer => {
  if (value < 0n) {
    throw new Error("CBOR argument must be non-negative");
  }
  const prefix = major << 5;
  if (value < 24n) return Buffer.from([prefix | Number(value)]);
  if (value <= 0xffn) {
    return Buffer.from([prefix | 24, Number(value)]);
  }
  if (value <= 0xffffn) {
    const encoded = Buffer.alloc(3);
    encoded[0] = prefix | 25;
    encoded.writeUInt16BE(Number(value), 1);
    return encoded;
  }
  if (value <= 0xffff_ffffn) {
    const encoded = Buffer.alloc(5);
    encoded[0] = prefix | 26;
    encoded.writeUInt32BE(Number(value), 1);
    return encoded;
  }
  if (value <= 0xffff_ffff_ffff_ffffn) {
    const encoded = Buffer.alloc(9);
    encoded[0] = prefix | 27;
    encoded.writeBigUInt64BE(value, 1);
    return encoded;
  }
  throw new Error("CBOR argument exceeds uint64");
};

const encodeCardanoBytes = (bytes: Uint8Array): Buffer => {
  const exact = Buffer.from(bytes);
  if (exact.length <= 64) {
    return encodeCborBytes(exact);
  }
  const chunks: Buffer[] = [Buffer.from([0x5f])];
  for (let offset = 0; offset < exact.length; offset += 64) {
    chunks.push(encodeCborBytes(exact.subarray(offset, offset + 64)));
  }
  chunks.push(Buffer.from([0xff]));
  return Buffer.concat(chunks);
};

const encodeCardanoList = (items: readonly Buffer[]): Buffer =>
  items.length === 0
    ? Buffer.from([0x80])
    : Buffer.concat([Buffer.from([0x9f]), ...items, Buffer.from([0xff])]);

const UINT64_MAX = 0xffff_ffff_ffff_ffffn;

const shortestBigEndianMagnitude = (value: bigint): Buffer => {
  if (value <= UINT64_MAX) {
    throw new Error("CBOR bignum magnitude must exceed uint64");
  }
  const hex = value.toString(16);
  return Buffer.from(hex.length % 2 === 0 ? hex : `0${hex}`, "hex");
};

const encodeCardanoInteger = (value: bigint): Buffer => {
  if (value >= 0n) {
    return value <= UINT64_MAX
      ? encodeSmallCborArgument(0, value)
      : Buffer.concat([
          Buffer.from([0xc2]),
          encodeCborBytes(shortestBigEndianMagnitude(value)),
        ]);
  }
  const magnitude = -value - 1n;
  return magnitude <= UINT64_MAX
    ? encodeSmallCborArgument(1, magnitude)
    : Buffer.concat([
        Buffer.from([0xc3]),
        encodeCborBytes(shortestBigEndianMagnitude(magnitude)),
      ]);
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
  if (data instanceof DataMap) {
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

const midgardConstantTypeToTags = (type: MidgardCekConstantType): ConstType => {
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

const semanticUplcConstant = (
  type: MidgardCekConstantType,
  payload: Data,
): UPLCConst => {
  switch (type.kind) {
    case "integer": {
      if (!(payload instanceof DataI)) {
        throw new Error("V1 integer payload is not DataI");
      }
      return UPLCConst.int(payload.int);
    }
    case "bytes": {
      if (!(payload instanceof DataB)) {
        throw new Error("V1 byte-string payload is not DataB");
      }
      return UPLCConst.byteString(payload.bytes);
    }
    case "string": {
      if (!(payload instanceof DataB)) {
        throw new Error("V1 string payload is not DataB");
      }
      return UPLCConst.str(
        new TextDecoder("utf-8", { fatal: true }).decode(
          asByteArray(payload.bytes),
        ),
      );
    }
    case "unit":
      return UPLCConst.unit;
    case "boolean": {
      if (!(payload instanceof DataConstr)) {
        throw new Error("V1 boolean payload is not DataConstr");
      }
      return UPLCConst.bool(payload.constr === 1n);
    }
    case "list": {
      if (!(payload instanceof DataList)) {
        throw new Error("V1 list payload is not DataList");
      }
      const elementType = midgardConstantTypeToTags(type.element);
      return UPLCConst.listOf(elementType)(
        payload.list.map(
          (item) => semanticUplcConstant(type.element, item).value,
        ) as never,
      );
    }
    case "pair": {
      if (
        !(payload instanceof DataConstr) ||
        payload.constr !== 0n ||
        payload.fields.length !== 2
      ) {
        throw new Error("V1 pair payload is malformed");
      }
      return UPLCConst.pairOf(
        midgardConstantTypeToTags(type.first),
        midgardConstantTypeToTags(type.second),
      )(
        semanticUplcConstant(type.first, payload.fields[0]).value,
        semanticUplcConstant(type.second, payload.fields[1]).value,
      );
    }
    case "data":
      return UPLCConst.data(payload);
    case "blsG1":
    case "blsG2":
    case "blsMillerLoopResult":
      throw new Error(
        "V1 BLS constants require their dedicated runtime proof nodes",
      );
  }
};

/**
 * Reconstructs the exact Harmonic UPLC constant consumed by the pinned
 * reference evaluator from the canonical semantic witness checked on L1.
 */
export const midgardCekConstantWitnessToUplc = (
  witness: MidgardCekConstantWitness,
): UPLCConst => {
  const decoded = decodeMidgardCekConstantWitness(witness);
  return semanticUplcConstant(decoded.type, decoded.payload);
};

/**
 * Converts a reference-evaluator constant back into the canonical L1
 * witness. The ordinary witness decoder remains authoritative for the direct
 * one-step payload bound.
 */
export const midgardCekConstantWitnessFromUplc = (constant: {
  readonly type: ConstType;
  readonly value: ConstValue;
}): MidgardCekConstantWitness => {
  const canonical = encodeMidgardCekCanonicalConstant(
    new UPLCConst(constant.type, constant.value as never),
  );
  const witness = Object.freeze({
    typeCbor: canonical.typeCbor,
    payloadCbor: canonical.payloadCbor,
  });
  decodeMidgardCekConstantWitness(witness);
  return witness;
};

export const hashMidgardCekConstantWitness = (
  witness: MidgardCekConstantWitness,
): Uint8Array => {
  const decoded = decodeMidgardCekConstantWitness(witness);
  const semantic = commitMidgardCekDataTree(decoded.payload);
  return hashMidgardCekValueNode({
    kind: "constant",
    typeRoot: hashMidgardCekBlobChunk(witness.typeCbor),
    payloadRoot: semantic.root,
    payloadLength: BigInt(encodeMidgardCekPlutusData(decoded.payload).length),
    semanticRoot: semantic.root,
    memory: midgardCekConstantMemorySize(decoded.type, decoded.payload),
  });
};

export const hashMidgardCekSemanticConstantWitness = (
  witness: MidgardCekSemanticConstantWitness,
): Uint8Array => {
  if (witness.typeCbor.length > 64) {
    throw new Error("V1 semantic constant type exceeds its bound");
  }
  decodeMidgardCekConstantTypeCbor(witness.typeCbor);
  if (
    witness.payload.root.length !== 32 ||
    witness.payload.cborLength < 0n ||
    witness.payload.memory < 0n ||
    witness.memory < 0n
  ) {
    throw new Error("V1 semantic constant summary is invalid");
  }
  return hashMidgardCekValueNode({
    kind: "constant",
    typeRoot: hashMidgardCekBlobChunk(witness.typeCbor),
    payloadRoot: witness.payload.root,
    payloadLength: witness.payload.cborLength,
    semanticRoot: witness.payload.root,
    memory: witness.memory,
  });
};

const byteLengthOrOne = (bytes: Uint8Array): bigint =>
  BigInt(Math.max(1, bytes.length));

/**
 * Plutus' ExMemory size for an integer. This is the signed CBOR-style byte
 * magnitude used by cardano-node's CEK cost model, not the encoded payload
 * length.
 */
export const midgardCekIntegerMemorySize = (value: bigint): bigint => {
  const doubledMagnitude = value < 0n ? (-value - 1n) << 1n : value << 1n;
  if (doubledMagnitude === 0n) {
    return 1n;
  }
  return BigInt(Math.floor((doubledMagnitude.toString(2).length - 1) / 8) + 1);
};

export const midgardCekByteStringMemorySize = (value: Uint8Array): bigint =>
  byteLengthOrOne(value);

/**
 * Plutus Data charges four memory words for every node, then the signed
 * integer or byte-string size for leaf payloads.
 */
export const midgardCekDataMemorySize = (value: Data): bigint => {
  if (value instanceof DataConstr) {
    return (
      4n +
      value.fields.reduce(
        (total, field) => total + midgardCekDataMemorySize(field),
        0n,
      )
    );
  }
  if (value instanceof DataMap) {
    return (
      4n +
      value.map.reduce(
        (total, entry) =>
          total +
          midgardCekDataMemorySize(entry.fst) +
          midgardCekDataMemorySize(entry.snd),
        0n,
      )
    );
  }
  if (value instanceof DataList) {
    return (
      4n +
      value.list.reduce(
        (total, item) => total + midgardCekDataMemorySize(item),
        0n,
      )
    );
  }
  if (value instanceof DataI) {
    return 4n + midgardCekIntegerMemorySize(value.int);
  }
  if (value instanceof DataB) {
    return 4n + byteLengthOrOne(asByteArray(value.bytes));
  }
  throw new Error("V1 data constant has an unknown node");
};

/**
 * ExMemory size of the semantic payload committed by a constant witness.
 * Lists and pairs sum element sizes without an extra container charge.
 */
export const midgardCekConstantMemorySize = (
  type: MidgardCekConstantType,
  payload: Data,
): bigint => {
  switch (type.kind) {
    case "integer":
      if (!(payload instanceof DataI)) {
        throw new Error("V1 integer payload is not DataI");
      }
      return midgardCekIntegerMemorySize(payload.int);
    case "bytes":
    case "string":
      if (!(payload instanceof DataB)) {
        throw new Error("V1 byte payload is not DataB");
      }
      return byteLengthOrOne(asByteArray(payload.bytes));
    case "unit":
    case "boolean":
      if (!(payload instanceof DataConstr)) {
        throw new Error("V1 scalar payload is not DataConstr");
      }
      return 1n;
    case "list":
      if (!(payload instanceof DataList)) {
        throw new Error("V1 list payload is not DataList");
      }
      return payload.list.reduce(
        (total, item) =>
          total + midgardCekConstantMemorySize(type.element, item),
        0n,
      );
    case "pair":
      if (
        !(payload instanceof DataConstr) ||
        payload.constr !== 0n ||
        payload.fields.length !== 2
      ) {
        throw new Error("V1 pair payload is malformed");
      }
      return (
        midgardCekConstantMemorySize(type.first, payload.fields[0]) +
        midgardCekConstantMemorySize(type.second, payload.fields[1])
      );
    case "data":
      return midgardCekDataMemorySize(payload);
    case "blsG1":
      return 48n;
    case "blsG2":
      return 96n;
    case "blsMillerLoopResult":
      return 192n;
  }
};

/**
 * Canonical semantic representation consumed by both the off-chain CEK and
 * the L1 builtin verifier. Raw Flat is intentionally not the runtime payload:
 * script identity commits the canonical program envelope instead.
 */
export const encodeMidgardCekCanonicalConstant = (
  constant: UPLCConst,
): MidgardCekCanonicalConstant => {
  const type = parseMidgardCekConstantType(constant.type);
  return Object.freeze({
    type,
    typeCbor: encodeMidgardCekPlutusData(
      new DataList(constant.type.map((tag) => new DataI(BigInt(tag)))),
    ),
    payloadCbor: encodeMidgardCekPlutusData(semanticData(type, constant.value)),
  });
};

export const midgardCekUplcConstantMemorySize = (
  constant: UPLCConst,
): bigint => {
  const canonical = encodeMidgardCekCanonicalConstant(constant);
  return midgardCekConstantMemorySize(
    canonical.type,
    dataFromCbor(canonical.payloadCbor),
  );
};
