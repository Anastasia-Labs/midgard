import { decodeFirst, encode, rfc8949EncodeOptions } from "cborg";

import {
  type CborItemSpan,
  type CborReadOptions,
  compareCborKeyBytes,
  DECODER_OPTIONS,
  ensureSafeLength,
  err,
  FATAL_UTF8_DECODER,
  readArgument,
} from "./cbor.read-argument.js";
import { MidgardTxCodecError, MidgardTxCodecErrorCodes } from "./errors.js";

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
      if (header.major === 3) {
        if (
          length >= 3 &&
          bytes[header.nextOffset] === 0xef &&
          bytes[header.nextOffset + 1] === 0xbb &&
          bytes[header.nextOffset + 2] === 0xbf
        ) {
          throw err(
            "CBOR text string must not begin with a UTF-8 BOM",
            `offset=${offset}`,
          );
        }
        try {
          FATAL_UTF8_DECODER.decode(bytes.subarray(header.nextOffset, end));
        } catch {
          throw err("CBOR text string is not valid UTF-8", `offset=${offset}`);
        }
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
          throw err(
            "Non-canonical CBOR map key ordering",
            `offset=${key.start}`,
          );
        }
        previousKey = keyBytes;
        cursor = skipCborItem(bytes, key.end, options).end;
      }
      return { start, end: cursor, major: header.major };
    }
    case 6:
      if (options.allowTags !== true) {
        throw err(
          "CBOR tags are not valid in this Midgard codec",
          `offset=${offset}`,
        );
      }
      return {
        start,
        end: skipCborItem(bytes, header.nextOffset, options).end,
        major: header.major,
      };
    case 7: {
      if (
        header.additional === 20 ||
        header.additional === 21 ||
        header.additional === 22
      ) {
        return { start, end: header.nextOffset, major: header.major };
      }
      if (header.additional === 23) {
        throw err("CBOR undefined is not valid", `offset=${offset}`);
      }
      throw err(
        "CBOR simple values and floats are not valid",
        `offset=${offset}`,
      );
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

/**
 * `cborg` declares `decodeFirst` as returning `[any, Uint8Array]`. The decoded
 * value is untrusted input that every caller validates, so it is laundered to
 * `unknown` at this one boundary rather than letting `any` spread through the
 * decoders.
 */
const decodeFirstUnknown = (
  bytes: Uint8Array,
): readonly [unknown, Uint8Array] =>
  decodeFirst(bytes, DECODER_OPTIONS) as readonly [unknown, Uint8Array];

export const decodeSingleCbor = (
  bytes: Uint8Array,
  options: CborReadOptions = {},
): unknown => {
  try {
    assertCanonicalCbor(bytes, "cbor", options);
    const [value] = decodeFirstUnknown(bytes);
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

export const assertCanonicalCborRoundTrip = <T>(
  original: Uint8Array,
  decoded: T,
  encodeDecoded: (decoded: T) => Buffer,
  message: string,
): Buffer => {
  const encoded = encodeDecoded(decoded);
  // Callers keep field-specific trailing-byte checks before this byte comparison.
  if (!encoded.equals(Buffer.from(original))) {
    throw new MidgardTxCodecError(MidgardTxCodecErrorCodes.CborDecode, message);
  }
  return encoded;
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

export const encodeCborArrayRaw = (items: readonly Uint8Array[]): Buffer =>
  Buffer.concat([
    encodeArgument(4, BigInt(items.length)),
    ...items.map((item) => Buffer.from(item)),
  ]);

export const encodeCborMapRaw = (
  entries: readonly (readonly [Uint8Array, Uint8Array])[],
): Buffer =>
  Buffer.concat([
    encodeArgument(5, BigInt(entries.length)),
    ...entries.flatMap(([key, value]) => [
      Buffer.from(key),
      Buffer.from(value),
    ]),
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
