import { prototypeOf } from ".././narrowing.js";
import { type DeploymentManifestCanonicalRational } from "./types.js";

export const requireRecord = (
  value: unknown,
  field: string,
): Record<string, unknown> => {
  if (typeof value !== "object" || value === null || Array.isArray(value)) {
    throw new Error(`${field} must be an object`);
  }
  const prototype = prototypeOf(value);
  if (prototype !== Object.prototype && prototype !== null) {
    throw new Error(`${field} must be a plain object`);
  }
  if (Reflect.ownKeys(value).length !== Object.keys(value).length) {
    throw new Error(`${field} must contain only string keys`);
  }
  return value as Record<string, unknown>;
};

export const requireDeploymentManifestId = (
  value: unknown,
  field: string,
): string => {
  if (typeof value !== "string" || !/^[0-9a-f]{64}$/u.test(value)) {
    throw new Error(`${field} must be lowercase SHA-256 hex`);
  }
  return value;
};

export const requireExactKeys = (
  value: Record<string, unknown>,
  required: readonly string[],
  optional: readonly string[] = [],
  field: string,
): void => {
  const allowed = new Set([...required, ...optional]);
  for (const key of Object.keys(value)) {
    if (!allowed.has(key)) {
      throw new Error(`Deployment manifest ${field}.${key} is unexpected`);
    }
  }
  for (const key of required) {
    if (!Object.prototype.hasOwnProperty.call(value, key)) {
      throw new Error(`Deployment manifest ${field}.${key} is required`);
    }
  }
};

export const requireString = (value: unknown, field: string): string => {
  if (typeof value !== "string" || value.length === 0) {
    throw new Error(`Deployment manifest ${field} must be a non-empty string`);
  }
  return value;
};

export const requireHex = (
  value: unknown,
  bytes: number | undefined,
  field: string,
): string => {
  const text = requireString(value, field);
  const pattern =
    bytes === undefined
      ? /^(?:[0-9a-f]{2})+$/u
      : new RegExp(`^[0-9a-f]{${(bytes * 2).toString()}}$`, "u");
  if (!pattern.test(text)) {
    throw new Error(
      `Deployment manifest ${field} must be lowercase canonical hex`,
    );
  }
  return text;
};

export const requireInteger = (
  value: unknown,
  field: string,
  minimum = 0,
): number => {
  if (
    typeof value !== "number" ||
    !Number.isSafeInteger(value) ||
    value < minimum
  ) {
    throw new Error(
      `Deployment manifest ${field} must be an integer >= ${minimum.toString()}`,
    );
  }
  return value;
};

export const requireCanonicalNatural = (
  value: unknown,
  field: string,
): string => {
  if (typeof value !== "string" || !/^(?:0|[1-9][0-9]*)$/u.test(value)) {
    throw new Error(
      `Deployment manifest ${field} must be a canonical natural decimal string`,
    );
  }
  return value;
};

export const greatestCommonDivisor = (left: bigint, right: bigint): bigint => {
  let a = left;
  let b = right;
  while (b !== 0n) {
    const remainder = a % b;
    a = b;
    b = remainder;
  }
  return a;
};

export const requireCanonicalRational = (
  value: unknown,
  field: string,
): DeploymentManifestCanonicalRational => {
  const candidate = requireRecord(value, `Deployment manifest ${field}`);
  requireExactKeys(candidate, ["numerator", "denominator"], [], field);
  const numerator = requireCanonicalNatural(
    candidate.numerator,
    `${field}.numerator`,
  );
  const denominator = requireCanonicalNatural(
    candidate.denominator,
    `${field}.denominator`,
  );
  if (denominator === "0") {
    throw new Error(
      `Deployment manifest ${field}.denominator must be positive`,
    );
  }
  if (greatestCommonDivisor(BigInt(numerator), BigInt(denominator)) !== 1n) {
    throw new Error(`Deployment manifest ${field} must be reduced`);
  }
  return Object.freeze({ numerator, denominator });
};

export const requireFinalOutRef = (
  value: unknown,
  field: string,
): { readonly txHash: string; readonly outputIndex: number } => {
  const outRef = requireRecord(value, `Deployment manifest ${field}`);
  requireExactKeys(outRef, ["txHash", "outputIndex"], [], field);
  return {
    txHash: requireHex(outRef.txHash, 32, `${field}.txHash`),
    outputIndex: requireInteger(outRef.outputIndex, `${field}.outputIndex`),
  };
};

export const requireIsoTimestamp = (value: unknown, field: string): string => {
  const text = requireString(value, field);
  const milliseconds = Date.parse(text);
  if (
    !Number.isFinite(milliseconds) ||
    new Date(milliseconds).toISOString() !== text
  ) {
    throw new Error(
      `Deployment manifest ${field} must be a canonical ISO timestamp`,
    );
  }
  return text;
};
