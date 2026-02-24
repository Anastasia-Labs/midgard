import { blake2b } from "@noble/hashes/blake2.js";
import { MidgardTxCodecError, MidgardTxCodecErrorCodes } from "./errors.js";

export const HASH32_LENGTH = 32;

export const ensureHash32 = (value: Uint8Array, fieldName: string): Buffer => {
  const hash = Buffer.from(value);
  if (hash.length !== HASH32_LENGTH) {
    throw new MidgardTxCodecError(
      MidgardTxCodecErrorCodes.InvalidFieldType,
      `${fieldName} must be 32 bytes`,
      `length=${hash.length}`,
    );
  }
  return hash;
};

export const computeHash32 = (value: Uint8Array): Buffer =>
  Buffer.from(blake2b(value, { dkLen: HASH32_LENGTH }));

export const ensureHashMatch = (
  expectedHash: Uint8Array,
  payload: Uint8Array,
  fieldName: string,
): void => {
  const expected = ensureHash32(expectedHash, fieldName);
  const actual = computeHash32(payload);

  if (!expected.equals(actual)) {
    throw new MidgardTxCodecError(
      MidgardTxCodecErrorCodes.HashMismatch,
      `${fieldName} does not match payload hash`,
      `expected=${expected.toString("hex")} actual=${actual.toString("hex")}`,
    );
  }
};
