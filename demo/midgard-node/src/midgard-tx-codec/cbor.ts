import { decodeFirst, encode, rfc8949EncodeOptions } from "cborg";
import { MidgardTxCodecError, MidgardTxCodecErrorCodes } from "./errors.js";

/**
 * Strict CBOR configuration used by the Midgard tx codec.
 *
 * The codec intentionally rejects indefinite values, undefined, duplicate map
 * keys, and trailing bytes so the serialized transaction form remains
 * deterministic and audit-friendly.
 */
const DECODER_OPTIONS = {
  strict: true,
  allowIndefinite: false,
  allowUndefined: false,
  useMaps: true,
  rejectDuplicateMapKeys: true,
};

/**
 * Decodes exactly one CBOR value and rejects any leftover bytes.
 */
export const decodeSingleCbor = (bytes: Uint8Array): unknown => {
  try {
    const [value, remainder] = decodeFirst(bytes, DECODER_OPTIONS);
    if (remainder.length !== 0) {
      throw new MidgardTxCodecError(
        MidgardTxCodecErrorCodes.CborDecode,
        "Trailing bytes after CBOR value",
      );
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

/**
 * Encodes a JavaScript value using RFC 8949 canonical options.
 */
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

/**
 * Narrows a decoded CBOR value to a map.
 */
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

/**
 * Narrows a decoded CBOR value to an array.
 */
export const asArray = (value: unknown, fieldName: string): unknown[] => {
  if (!Array.isArray(value)) {
    throw new MidgardTxCodecError(
      MidgardTxCodecErrorCodes.InvalidFieldType,
      `${fieldName} must be a CBOR array`,
    );
  }
  return value;
};

/**
 * Narrows a decoded CBOR value to a boolean.
 */
export const asBoolean = (value: unknown, fieldName: string): boolean => {
  if (typeof value !== "boolean") {
    throw new MidgardTxCodecError(
      MidgardTxCodecErrorCodes.InvalidFieldType,
      `${fieldName} must be a boolean`,
    );
  }
  return value;
};

/**
 * Narrows a decoded CBOR value to an unsigned integer represented as `bigint`.
 *
 * Small non-negative JavaScript numbers are accepted and normalized into
 * `bigint` so callers can use a single integer representation.
 */
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

/**
 * Narrows a decoded CBOR value to a byte string and returns it as a `Buffer`.
 */
export const asBytes = (value: unknown, fieldName: string): Buffer => {
  if (value instanceof Uint8Array) {
    return Buffer.from(value);
  }
  throw new MidgardTxCodecError(
    MidgardTxCodecErrorCodes.InvalidFieldType,
    `${fieldName} must be bytes`,
  );
};
