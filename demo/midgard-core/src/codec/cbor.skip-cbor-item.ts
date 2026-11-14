import {
  buildCanonicalCborValue,
  skipCborItem,
} from "./cbor.iterative-decode.js";
import { encodeCborIteratively } from "./cbor.iterative-encode.js";
import { MidgardTxCodecError, MidgardTxCodecErrorCodes } from "./errors.js";

export { skipCborItem };

export const assertCanonicalCbor = (
  bytes: Uint8Array,
  fieldName = "cbor",
): void => {
  const span = skipCborItem(bytes, 0);
  if (span.end !== bytes.length) {
    throw new MidgardTxCodecError(
      MidgardTxCodecErrorCodes.CborDecode,
      `${fieldName} has trailing bytes`,
      `offset=${span.end}`,
    );
  }
};

/**
 * Decodes one canonical CBOR item. The value is untrusted input that every
 * caller validates, so it is returned as `unknown`.
 */
export const decodeSingleCbor = (bytes: Uint8Array): unknown => {
  try {
    assertCanonicalCbor(bytes, "cbor");
    if (!(bytes instanceof Uint8Array)) {
      throw new Error("CBOR decode error: data to decode must be a Uint8Array");
    }
    return buildCanonicalCborValue(bytes);
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
    return encodeCborIteratively(value);
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
