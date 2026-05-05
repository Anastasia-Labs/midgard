import { decodeFirst, encode, rfc8949EncodeOptions } from "cborg";
import { MidgardTxCodecError, MidgardTxCodecErrorCodes } from "./errors.js";

export type CborReadOptions = {
  readonly allowTags?: boolean;
};

export type CborItemSpan = {
  readonly start: number;
  readonly end: number;
  readonly major: number;
};

const DECODER_OPTIONS = {
  strict: true,
  allowIndefinite: false,
  allowUndefined: false,
  useMaps: true,
  rejectDuplicateMapKeys: true,
};

const err = (message: string, detail?: string): MidgardTxCodecError =>
  new MidgardTxCodecError(MidgardTxCodecErrorCodes.CborDecode, message, detail);

export const compareBytes = (left: Uint8Array, right: Uint8Array): number => {
  const limit = Math.min(left.length, right.length);
  for (let i = 0; i < limit; i += 1) {
    const diff = left[i] - right[i];
    if (diff !== 0) {
      return diff;
    }
  }
  return left.length - right.length;
};

export const compareCborKeyBytes = (
  left: Uint8Array,
  right: Uint8Array,
): number => left.length - right.length || compareBytes(left, right);

const ensureSafeLength = (value: bigint, offset: number): number => {
  if (value > BigInt(Number.MAX_SAFE_INTEGER)) {
    throw err("CBOR length exceeds JavaScript safe integer range", `offset=${offset}`);
  }
  return Number(value);
};

const readArgument = (
  bytes: Uint8Array,
  offset: number,
): {
  readonly major: number;
  readonly additional: number;
  readonly value: bigint;
  readonly nextOffset: number;
} => {
  if (offset >= bytes.length) {
    throw err("Unexpected end of CBOR", `offset=${offset}`);
  }
  const initial = bytes[offset];
  const major = initial >> 5;
  const additional = initial & 0x1f;

  if (additional < 24) {
    return {
      major,
      additional,
      value: BigInt(additional),
      nextOffset: offset + 1,
    };
  }
  if (additional === 31) {
    throw err("Indefinite-length CBOR is not valid", `offset=${offset}`);
  }
  if (additional > 27) {
    throw err("Reserved CBOR additional-info value", `offset=${offset}`);
  }

  const byteLength =
    additional === 24 ? 1 : additional === 25 ? 2 : additional === 26 ? 4 : 8;
  if (offset + 1 + byteLength > bytes.length) {
    throw err("Unexpected end of CBOR while reading argument", `offset=${offset}`);
  }

  let value = 0n;
  for (let i = 0; i < byteLength; i += 1) {
    value = (value << 8n) | BigInt(bytes[offset + 1 + i]);
  }

  if (
    (additional === 24 && value < 24n) ||
    (additional === 25 && value <= 0xffn) ||
    (additional === 26 && value <= 0xffffn) ||
    (additional === 27 && value <= 0xffffffffn)
  ) {
    throw err("Non-minimal CBOR integer or length encoding", `offset=${offset}`);
  }

  return {
    major,
    additional,
    value,
    nextOffset: offset + 1 + byteLength,
  };
};

export const readCborUnsigned = (
  bytes: Uint8Array,
  offset: number,
  fieldName = "uint",
): { readonly value: bigint; readonly nextOffset: number } => {
  const header = readArgument(bytes, offset);
  if (header.major !== 0) {
    throw err(`${fieldName} must be an unsigned integer`, `offset=${offset}`);
  }
  return { value: header.value, nextOffset: header.nextOffset };
};

export const readCborTag = (
  bytes: Uint8Array,
  offset: number,
  fieldName = "tag",
): { readonly value: bigint; readonly nextOffset: number } => {
  const header = readArgument(bytes, offset);
  if (header.major !== 6) {
    throw err(`${fieldName} must be a CBOR tag`, `offset=${offset}`);
  }
  return { value: header.value, nextOffset: header.nextOffset };
};

export const readCborInteger = (
  bytes: Uint8Array,
  offset: number,
  fieldName = "integer",
): { readonly value: bigint; readonly nextOffset: number } => {
  const header = readArgument(bytes, offset);
  if (header.major === 0) {
    return { value: header.value, nextOffset: header.nextOffset };
  }
  if (header.major === 1) {
    return { value: -1n - header.value, nextOffset: header.nextOffset };
  }
  throw err(`${fieldName} must be an integer`, `offset=${offset}`);
};

export const readCborBytes = (
  bytes: Uint8Array,
  offset: number,
  fieldName = "bytes",
): { readonly value: Buffer; readonly nextOffset: number } => {
  const header = readArgument(bytes, offset);
  if (header.major !== 2) {
    throw err(`${fieldName} must be a byte string`, `offset=${offset}`);
  }
  const length = ensureSafeLength(header.value, offset);
  const end = header.nextOffset + length;
  if (end > bytes.length) {
    throw err("CBOR byte string exceeds input length", `offset=${offset}`);
  }
  return {
    value: Buffer.from(bytes.subarray(header.nextOffset, end)),
    nextOffset: end,
  };
};

export const readCborArrayHeader = (
  bytes: Uint8Array,
  offset: number,
  fieldName = "array",
): { readonly length: number; readonly nextOffset: number } => {
  const header = readArgument(bytes, offset);
  if (header.major !== 4) {
    throw err(`${fieldName} must be an array`, `offset=${offset}`);
  }
  return {
    length: ensureSafeLength(header.value, offset),
    nextOffset: header.nextOffset,
  };
};

export const readCborMapHeader = (
  bytes: Uint8Array,
  offset: number,
  fieldName = "map",
): { readonly length: number; readonly nextOffset: number } => {
  const header = readArgument(bytes, offset);
  if (header.major !== 5) {
    throw err(`${fieldName} must be a map`, `offset=${offset}`);
  }
  return {
    length: ensureSafeLength(header.value, offset),
    nextOffset: header.nextOffset,
  };
};

export const skipCborItem = (
  bytes: Uint8Array,
  offset: number,
  options: CborReadOptions = {},
): CborItemSpan => {
  const start = offset;
  const header = readArgument(bytes, offset);

  switch (header.major) {
    case 0:
    case 1:
      return { start, end: header.nextOffset, major: header.major };
    case 2:
    case 3: {
      const length = ensureSafeLength(header.value, offset);
      const end = header.nextOffset + length;
      if (end > bytes.length) {
        throw err("CBOR string exceeds input length", `offset=${offset}`);
      }
      return { start, end, major: header.major };
    }
    case 4: {
      let cursor = header.nextOffset;
      const length = ensureSafeLength(header.value, offset);
      for (let i = 0; i < length; i += 1) {
        cursor = skipCborItem(bytes, cursor, options).end;
      }
      return { start, end: cursor, major: header.major };
    }
    case 5: {
      let cursor = header.nextOffset;
      const length = ensureSafeLength(header.value, offset);
      const seen = new Set<string>();
      let previousKey: Buffer | undefined;
      for (let i = 0; i < length; i += 1) {
        const key = skipCborItem(bytes, cursor, options);
        const keyBytes = Buffer.from(bytes.subarray(key.start, key.end));
        const keyHex = keyBytes.toString("hex");
        if (seen.has(keyHex)) {
          throw err("Duplicate CBOR map key", `offset=${key.start}`);
        }
        seen.add(keyHex);
        if (
          previousKey !== undefined &&
          compareCborKeyBytes(previousKey, keyBytes) > 0
        ) {
          throw err("Non-canonical CBOR map key ordering", `offset=${key.start}`);
        }
        previousKey = keyBytes;
        cursor = skipCborItem(bytes, key.end, options).end;
      }
      return { start, end: cursor, major: header.major };
    }
    case 6:
      if (options.allowTags !== true) {
        throw err("CBOR tags are not valid in this Midgard codec", `offset=${offset}`);
      }
      return {
        start,
        end: skipCborItem(bytes, header.nextOffset, options).end,
        major: header.major,
      };
    case 7: {
      if (header.additional === 20 || header.additional === 21 || header.additional === 22) {
        return { start, end: header.nextOffset, major: header.major };
      }
      if (header.additional === 23) {
        throw err("CBOR undefined is not valid", `offset=${offset}`);
      }
      throw err("CBOR simple values and floats are not valid", `offset=${offset}`);
    }
    default:
      throw err("Unsupported CBOR major type", `offset=${offset}`);
  }
};

export const assertCanonicalCbor = (
  bytes: Uint8Array,
  fieldName = "cbor",
  options: CborReadOptions = {},
): void => {
  const span = skipCborItem(bytes, 0, options);
  if (span.end !== bytes.length) {
    throw new MidgardTxCodecError(
      MidgardTxCodecErrorCodes.CborDecode,
      `${fieldName} has trailing bytes`,
      `offset=${span.end}`,
    );
  }
};

export const decodeSingleCbor = (
  bytes: Uint8Array,
  options: CborReadOptions = {},
): unknown => {
  try {
    assertCanonicalCbor(bytes, "cbor", options);
    const [value, remainder] = decodeFirst(bytes, DECODER_OPTIONS);
    if (remainder.length !== 0) {
      throw err("Trailing bytes after CBOR value");
    }
    return value;
  } catch (e) {
    if (e instanceof MidgardTxCodecError) {
      throw e;
    }
    throw new MidgardTxCodecError(
      MidgardTxCodecErrorCodes.CborDecode,
      "Failed to decode CBOR",
      String(e),
    );
  }
};

export const encodeCbor = (value: unknown): Buffer => {
  try {
    return Buffer.from(encode(value, rfc8949EncodeOptions));
  } catch (e) {
    throw new MidgardTxCodecError(
      MidgardTxCodecErrorCodes.CborEncode,
      "Failed to encode CBOR",
      String(e),
    );
  }
};

const encodeArgument = (major: number, value: bigint): Buffer => {
  if (value < 0n) {
    throw new MidgardTxCodecError(
      MidgardTxCodecErrorCodes.CborEncode,
      "CBOR argument must be non-negative",
      value.toString(),
    );
  }
  const prefix = major << 5;
  if (value < 24n) {
    return Buffer.from([prefix | Number(value)]);
  }
  if (value <= 0xffn) {
    return Buffer.from([prefix | 24, Number(value)]);
  }
  if (value <= 0xffffn) {
    const out = Buffer.alloc(3);
    out[0] = prefix | 25;
    out.writeUInt16BE(Number(value), 1);
    return out;
  }
  if (value <= 0xffffffffn) {
    const out = Buffer.alloc(5);
    out[0] = prefix | 26;
    out.writeUInt32BE(Number(value), 1);
    return out;
  }
  const out = Buffer.alloc(9);
  out[0] = prefix | 27;
  out.writeBigUInt64BE(value, 1);
  return out;
};

export const encodeCborUnsigned = (value: bigint): Buffer =>
  encodeArgument(0, value);

export const encodeCborInteger = (value: bigint): Buffer =>
  value >= 0n ? encodeArgument(0, value) : encodeArgument(1, -1n - value);

export const encodeCborBytes = (value: Uint8Array): Buffer =>
  Buffer.concat([encodeArgument(2, BigInt(value.length)), Buffer.from(value)]);

export const encodeCborArrayRaw = (
  items: readonly Uint8Array[],
): Buffer =>
  Buffer.concat([
    encodeArgument(4, BigInt(items.length)),
    ...items.map((item) => Buffer.from(item)),
  ]);

export const encodeCborMapRaw = (
  entries: readonly (readonly [Uint8Array, Uint8Array])[],
): Buffer =>
  Buffer.concat([
    encodeArgument(5, BigInt(entries.length)),
    ...entries.flatMap(([key, value]) => [Buffer.from(key), Buffer.from(value)]),
  ]);

export const encodeCborTagRaw = (tag: bigint, value: Uint8Array): Buffer =>
  Buffer.concat([encodeArgument(6, tag), Buffer.from(value)]);

export const asMap = (
  value: unknown,
  fieldName: string,
): Map<unknown, unknown> => {
  if (!(value instanceof Map)) {
    throw new MidgardTxCodecError(
      MidgardTxCodecErrorCodes.InvalidFieldType,
      `${fieldName} must be a CBOR map`,
    );
  }
  return value;
};

export const asArray = (value: unknown, fieldName: string): unknown[] => {
  if (!Array.isArray(value)) {
    throw new MidgardTxCodecError(
      MidgardTxCodecErrorCodes.InvalidFieldType,
      `${fieldName} must be a CBOR array`,
    );
  }
  return value;
};

export const asBoolean = (value: unknown, fieldName: string): boolean => {
  if (typeof value !== "boolean") {
    throw new MidgardTxCodecError(
      MidgardTxCodecErrorCodes.InvalidFieldType,
      `${fieldName} must be a boolean`,
    );
  }
  return value;
};

export const asBigInt = (value: unknown, fieldName: string): bigint => {
  if (typeof value === "bigint") {
    return value;
  }
  if (typeof value === "number" && Number.isInteger(value) && value >= 0) {
    return BigInt(value);
  }
  throw new MidgardTxCodecError(
    MidgardTxCodecErrorCodes.InvalidFieldType,
    `${fieldName} must be an unsigned integer`,
  );
};

export const asBytes = (value: unknown, fieldName: string): Buffer => {
  if (value instanceof Uint8Array) {
    return Buffer.from(value);
  }
  throw new MidgardTxCodecError(
    MidgardTxCodecErrorCodes.InvalidFieldType,
    `${fieldName} must be bytes`,
  );
};
