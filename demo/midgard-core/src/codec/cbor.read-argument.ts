import { MidgardTxCodecError, MidgardTxCodecErrorCodes } from "./errors.js";

export type CborReadOptions = {
  readonly allowTags?: boolean;
};

export type CborItemSpan = {
  readonly start: number;
  readonly end: number;
  readonly major: number;
};

export const DECODER_OPTIONS = {
  strict: true,
  allowIndefinite: false,
  allowUndefined: false,
  useMaps: true,
  rejectDuplicateMapKeys: true,
};

export const FATAL_UTF8_DECODER = new TextDecoder("utf-8", { fatal: true });

export const err = (message: string, detail?: string): MidgardTxCodecError =>
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

export const ensureSafeLength = (value: bigint, offset: number): number => {
  if (value > BigInt(Number.MAX_SAFE_INTEGER)) {
    throw err(
      "CBOR length exceeds JavaScript safe integer range",
      `offset=${offset}`,
    );
  }
  return Number(value);
};

export const readArgument = (
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
    throw err(
      "Unexpected end of CBOR while reading argument",
      `offset=${offset}`,
    );
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
    throw err(
      "Non-minimal CBOR integer or length encoding",
      `offset=${offset}`,
    );
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

export const readCborBytesHeader = (
  bytes: Uint8Array,
  offset: number,
  fieldName = "bytes",
): { readonly length: number; readonly nextOffset: number } => {
  const header = readArgument(bytes, offset);
  if (header.major !== 2) {
    throw err(`${fieldName} must be a byte string`, `offset=${offset}`);
  }
  return {
    length: ensureSafeLength(header.value, offset),
    nextOffset: header.nextOffset,
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
