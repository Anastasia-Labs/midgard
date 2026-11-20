import { normalizeHex } from "@al-ft/midgard-core/hex";

import { BuilderInvariantError } from "../core/errors.js";

export const normalizeNonNegativeBigInt = (
  value: bigint | number,
  fieldName: string,
): bigint => {
  if (typeof value === "number" && !Number.isSafeInteger(value)) {
    throw new BuilderInvariantError(`${fieldName} must be a safe integer`);
  }
  const normalized = BigInt(value);
  if (normalized < 0n) {
    throw new BuilderInvariantError(`${fieldName} must be non-negative`);
  }
  return normalized;
};

export const normalizeHashHex = (
  value: string,
  fieldName: string,
  bytes: 28 | 32,
): string => {
  try {
    return normalizeHex(value, { fieldName, byteLength: bytes });
  } catch {
    throw new BuilderInvariantError(
      `${fieldName} must be a ${bytes.toString()}-byte hex string`,
      value,
    );
  }
};
