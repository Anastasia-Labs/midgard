import { MIDGARD_CEK_MAX_SOURCE_CONSTANT_PAYLOAD_BYTES } from "@al-ft/midgard-core";
import { encodeCborBytes } from "@al-ft/midgard-core/codec/cbor";
import {
  type Data,
  DataB,
  DataConstr,
  DataI,
  DataList,
  isData,
} from "@harmoniclabs/plutus-data";
import {
  type ConstType,
  ConstTyTag,
  type ConstValue,
} from "@harmoniclabs/uplc";

import { isByteStringLike } from "./plutus-data-narrowing.js";

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

export const asByteArray = (value: unknown): Uint8Array => {
  if (!isByteStringLike(value)) {
    throw new Error("V1 bytes constant has an invalid value");
  }
  const bytes = value.toBuffer();
  if (!(bytes instanceof Uint8Array)) {
    throw new Error("V1 bytes constant did not produce bytes");
  }
  return bytes;
};

export const semanticData = (
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

export const sameBytes = (left: Uint8Array, right: Uint8Array): boolean =>
  Buffer.from(left).equals(Buffer.from(right));

export const encodeSmallCborArgument = (
  major: number,
  value: bigint,
): Buffer => {
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

export const encodeCardanoBytes = (bytes: Uint8Array): Buffer => {
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

export const encodeCardanoList = (items: readonly Buffer[]): Buffer =>
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

export const encodeCardanoInteger = (value: bigint): Buffer => {
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
