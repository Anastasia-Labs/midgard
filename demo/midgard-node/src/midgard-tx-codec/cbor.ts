import { decodeFirst, encode, rfc8949EncodeOptions } from "cborg";
import { MidgardTxCodecError, MidgardTxCodecErrorCodes } from "./errors.js";

const DECODER_OPTIONS = {
  strict: true,
  allowIndefinite: false,
  allowUndefined: false,
  useMaps: true,
  rejectDuplicateMapKeys: true,
};

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
