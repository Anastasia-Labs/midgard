export const asObject = (
  value: unknown,
  fieldName: string,
): Record<string, unknown> => {
  if (typeof value !== "object" || value === null || Array.isArray(value)) {
    throw new Error(`${fieldName} must be an object.`);
  }
  return value as Record<string, unknown>;
};

export const assertExactKeys = (
  value: Record<string, unknown>,
  fieldName: string,
  required: readonly string[],
  optional: readonly string[] = [],
): void => {
  const keys = Object.keys(value);
  const allowed = new Set([...required, ...optional]);
  const missing = required.filter((key) => !Object.hasOwn(value, key));
  const extra = keys.filter((key) => !allowed.has(key));
  if (missing.length > 0 || extra.length > 0) {
    throw new Error(
      `${fieldName} keys must be exact; missing=[${missing.join(",")}], extra=[${extra.join(",")}].`,
    );
  }
};

export const parseExactVersionedArtifact = (
  value: unknown,
  label: string,
  schemaVersion: string,
  keys: readonly string[],
  optional: readonly string[] = [],
): Record<string, unknown> => {
  const raw = asObject(value, label);
  assertExactKeys(raw, label, ["schemaVersion", ...keys], optional);
  if (raw.schemaVersion !== schemaVersion) {
    throw new Error(`${label} schemaVersion must be exactly ${schemaVersion}.`);
  }
  return raw;
};

export const artifactExactString = (value: unknown, label: string): string => {
  if (
    typeof value !== "string" ||
    value.length === 0 ||
    value !== value.trim()
  ) {
    throw new Error(`${label} must be an exact non-empty string.`);
  }
  return value;
};

export const artifactInteger = (
  value: unknown,
  label: string,
  minimum = 0,
): number => {
  if (
    typeof value !== "number" ||
    !Number.isSafeInteger(value) ||
    value < minimum
  ) {
    throw new Error(
      `${label} must be a safe integer >= ${minimum.toString()}.`,
    );
  }
  return value;
};

export const artifactDecimal = (value: unknown, label: string): string => {
  const decimal = artifactExactString(value, label);
  if (!/^(0|[1-9]\d*)$/.test(decimal)) {
    throw new Error(`${label} must be a canonical non-negative decimal.`);
  }
  return decimal;
};

export const artifactIsoTimestamp = (value: unknown, label: string): string => {
  const timestamp = artifactExactString(value, label);
  const parsed = Date.parse(timestamp);
  if (Number.isNaN(parsed) || new Date(parsed).toISOString() !== timestamp) {
    throw new Error(`${label} must be a canonical ISO-8601 timestamp.`);
  }
  return timestamp;
};

export const artifactHash32 = (value: unknown, label: string): string => {
  const digest = artifactExactString(value, label);
  if (!/^[0-9a-f]{64}$/.test(digest)) {
    throw new Error(`${label} must be an exact lowercase 32-byte digest.`);
  }
  return digest;
};

export const requiredString = (value: unknown, fieldName: string): string => {
  if (
    typeof value !== "string" ||
    value.length === 0 ||
    value !== value.trim()
  ) {
    throw new Error(`${fieldName} must be an exact non-empty string.`);
  }
  return value;
};

export const requiredPositiveInteger = (
  value: unknown,
  fieldName: string,
): number => {
  if (typeof value !== "number" || !Number.isSafeInteger(value) || value <= 0) {
    throw new Error(`${fieldName} must be a safe positive integer.`);
  }
  return value;
};

export const requiredPositiveIntegerOrZero = (
  value: unknown,
  fieldName: string,
): number => {
  if (typeof value !== "number" || !Number.isSafeInteger(value) || value < 0) {
    throw new Error(`${fieldName} must be a safe non-negative integer.`);
  }
  return value;
};
