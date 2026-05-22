import { readFile } from "node:fs/promises";

import { normalizeHex } from "@al-ft/midgard-core/hex";

const SIGNED_INTEGER_PATTERN = /^-?\d+$/;
const UNSIGNED_INTEGER_PATTERN = /^\d+$/;
const MAX_SAFE_INTEGER_BIGINT = BigInt(Number.MAX_SAFE_INTEGER);

type IntegerBounds = {
  readonly min?: bigint;
  readonly max?: bigint;
};

type ParseStrictHexOptions = {
  readonly byteCount?: number;
  readonly trim?: boolean;
  readonly typeError: string;
  readonly invalidError: string;
};

type ParseStrictIntegerOptions = {
  readonly signed: boolean;
  readonly bigintBounds?: IntegerBounds;
  readonly stringBounds?: IntegerBounds;
  readonly numberBounds?: IntegerBounds;
  readonly numberError: string;
  readonly boundsError?: string;
  readonly invalidError: string;
};

export const parseJsonText = (
  text: string,
  invalidError?: (cause: unknown) => string,
): unknown => {
  try {
    return JSON.parse(text) as unknown;
  } catch (cause) {
    if (invalidError === undefined) {
      throw cause;
    }
    throw new Error(invalidError(cause));
  }
};

export const stringifyJson = (value: unknown): string =>
  `${JSON.stringify(
    value,
    (_key, item) => (typeof item === "bigint" ? item.toString() : item),
    2,
  )}\n`;

export const readJsonFile = async (path: string): Promise<unknown> => {
  const raw = await readFile(path, "utf8");
  return parseJsonText(raw);
};

export const parseStrictHex = (
  value: unknown,
  options: ParseStrictHexOptions,
): string => {
  if (typeof value !== "string") {
    throw new Error(options.typeError);
  }
  try {
    return normalizeHex(value, {
      allowEmpty: true,
      byteLength: options.byteCount,
      trim: options.trim === true,
    });
  } catch {
    throw new Error(options.invalidError);
  }
};

const isOutOfBounds = (value: bigint, bounds: IntegerBounds): boolean =>
  (bounds.min !== undefined && value < bounds.min) ||
  (bounds.max !== undefined && value > bounds.max);

export const parseStrictInteger = (
  value: unknown,
  options: ParseStrictIntegerOptions,
): bigint => {
  if (typeof value === "bigint") {
    if (
      options.bigintBounds !== undefined &&
      isOutOfBounds(value, options.bigintBounds)
    ) {
      throw new Error(options.boundsError ?? options.invalidError);
    }
    return value;
  }

  if (typeof value === "number") {
    if (!Number.isSafeInteger(value)) {
      throw new Error(options.numberError);
    }
    const parsed = BigInt(value);
    if (
      options.numberBounds !== undefined &&
      isOutOfBounds(parsed, options.numberBounds)
    ) {
      throw new Error(options.numberError);
    }
    return parsed;
  }

  const integerPattern = options.signed
    ? SIGNED_INTEGER_PATTERN
    : UNSIGNED_INTEGER_PATTERN;
  if (typeof value === "string" && integerPattern.test(value)) {
    const parsed = BigInt(value);
    if (
      options.stringBounds !== undefined &&
      isOutOfBounds(parsed, options.stringBounds)
    ) {
      throw new Error(options.boundsError ?? options.invalidError);
    }
    return parsed;
  }

  throw new Error(options.invalidError);
};

const isRecord = (value: unknown): value is Record<string, unknown> =>
  typeof value === "object" && value !== null && !Array.isArray(value);

export const requireRecord = (
  value: unknown,
  label: string,
): Record<string, unknown> => {
  if (!isRecord(value)) {
    throw new Error(`${label} must be a JSON object.`);
  }
  return value;
};

export const parseHex = (
  value: unknown,
  label: string,
  byteCount?: number,
): string => {
  const sizeMessage =
    byteCount === undefined
      ? "an even-length"
      : `a ${byteCount.toString()}-byte`;
  return parseStrictHex(value, {
    byteCount,
    trim: true,
    typeError: `${label} must be a hex string.`,
    invalidError: `${label} must be ${sizeMessage} hex string.`,
  });
};

export const parseInteger = (value: unknown, label: string): bigint =>
  parseStrictInteger(value, {
    signed: false,
    numberBounds: { min: 0n },
    numberError: `${label} must be a safe non-negative integer.`,
    invalidError: `${label} must be a non-negative integer.`,
  });

export const parseSignedInteger = (value: unknown, label: string): bigint =>
  parseStrictInteger(value, {
    signed: true,
    numberError: `${label} must be a safe integer.`,
    invalidError: `${label} must be an integer.`,
  });

export const parseSafeNonNegativeInteger = (
  value: unknown,
  label: string,
): bigint => {
  const bounds = { min: 0n, max: MAX_SAFE_INTEGER_BIGINT };
  const error = `${label} must be a safe non-negative integer`;
  return parseStrictInteger(value, {
    signed: false,
    bigintBounds: bounds,
    stringBounds: bounds,
    numberBounds: bounds,
    numberError: error,
    boundsError: error,
    invalidError: error,
  });
};
