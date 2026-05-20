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
  const normalized = value.trim().toLowerCase();
  if (normalized.length !== bytes * 2 || !/^[0-9a-f]+$/.test(normalized)) {
    throw new BuilderInvariantError(
      `${fieldName} must be a ${bytes.toString()}-byte hex string`,
      value,
    );
  }
  return normalized;
};
