/**
 * Artifact-schema core: the single home for label-threaded JSON validation
 * primitives (`(value, label) => parsed`) used by evidence artifacts, run
 * state, gate summaries, and worker payloads.
 *
 * Error-message text is part of this module's interface: gates and tests
 * assert on exact strings, so combinators that differ only in wording are
 * deliberately distinct exports rather than unified. Two dialects live here:
 *
 * - The canonical dialect (`exactRecord`, `nonEmptyString`, …): open to
 *   optional keys, trims strings, messages like "<label> must be a
 *   non-empty string".
 * - The strict dialect (`exactKeysRecord`, `boundedNonEmptyString`, …),
 *   originally from the MPF commit-candidate artifacts: exact key-set
 *   equality, bounded NUL-free strings, messages like "<label> must be a
 *   bounded nonempty string". Stricter semantics, not just different words.
 *
 * Module-local dialects that intentionally stay outside this file:
 * `deployment-manifest.ts` ("Deployment manifest <field> …", accepts
 * whitespace-only strings), `l1-tx-order-carriage.ts` (domain parsers,
 * "is not a …"), and `database/utils/exact-record.ts` (the DB-adapter
 * record contract).
 */

import { isAbsolute, resolve } from "node:path";

export type ExactArtifactRecord = Readonly<Record<string, unknown>>;

/** A label-threaded parser: validates `value` and returns the typed result. */
export type ArtifactParser<Value> = (value: unknown, label: string) => Value;

const describeKeys = (keys: readonly string[]): string =>
  keys.map((key) => JSON.stringify(key)).join(", ");

const isPlainRecord = (value: unknown): value is Record<string, unknown> => {
  if (typeof value !== "object" || value === null || Array.isArray(value)) {
    return false;
  }
  const prototype = Object.getPrototypeOf(value) as unknown;
  return prototype === Object.prototype || prototype === null;
};

export const exactRecord = (
  value: unknown,
  label: string,
  requiredKeys: readonly string[],
  optionalKeys: readonly string[] = [],
): Record<string, unknown> => {
  if (!isPlainRecord(value)) {
    throw new Error(`${label} must be a plain object`);
  }
  const record = value;
  const allowed = new Set([...requiredKeys, ...optionalKeys]);
  const unknownKeys = Object.keys(record).filter((key) => !allowed.has(key));
  if (unknownKeys.length > 0) {
    throw new Error(
      `${label} contains unknown field${unknownKeys.length === 1 ? "" : "s"}: ${describeKeys(unknownKeys)}`,
    );
  }
  const missingKeys = requiredKeys.filter(
    (key) => !Object.prototype.hasOwnProperty.call(record, key),
  );
  if (missingKeys.length > 0) {
    throw new Error(
      `${label} is missing required field${missingKeys.length === 1 ? "" : "s"}: ${describeKeys(missingKeys)}`,
    );
  }
  return record;
};

export const openRecord = (
  value: unknown,
  label: string,
): ExactArtifactRecord => {
  if (!isPlainRecord(value)) {
    throw new Error(`${label} must be a plain object`);
  }
  assertJsonValue(value, label);
  return value;
};

export const nonEmptyString = (value: unknown, label: string): string => {
  if (typeof value !== "string" || value.trim().length === 0) {
    throw new Error(`${label} must be a non-empty string`);
  }
  return value;
};

export const stringValue = (value: unknown, label: string): string => {
  if (typeof value !== "string") {
    throw new Error(`${label} must be a string`);
  }
  return value;
};

export const booleanValue = (value: unknown, label: string): boolean => {
  if (typeof value !== "boolean") {
    throw new Error(`${label} must be a boolean`);
  }
  return value;
};

export const finiteNumber = (value: unknown, label: string): number => {
  if (typeof value !== "number" || !Number.isFinite(value)) {
    throw new Error(`${label} must be a finite number`);
  }
  return value;
};

export const nonNegativeNumber = (value: unknown, label: string): number => {
  const parsed = finiteNumber(value, label);
  if (parsed < 0) {
    throw new Error(`${label} must be non-negative`);
  }
  return parsed;
};

export const integer = (value: unknown, label: string): number => {
  const parsed = finiteNumber(value, label);
  if (!Number.isSafeInteger(parsed)) {
    throw new Error(`${label} must be a safe integer`);
  }
  return parsed;
};

export const nonNegativeInteger = (value: unknown, label: string): number => {
  const parsed = integer(value, label);
  if (parsed < 0) {
    throw new Error(`${label} must be a non-negative safe integer`);
  }
  return parsed;
};

export const positiveInteger = (value: unknown, label: string): number => {
  const parsed = integer(value, label);
  if (parsed <= 0) {
    throw new Error(`${label} must be a positive safe integer`);
  }
  return parsed;
};

export const isoTimestamp = (value: unknown, label: string): string => {
  const parsed = nonEmptyString(value, label);
  if (
    Number.isNaN(Date.parse(parsed)) ||
    new Date(parsed).toISOString() !== parsed
  ) {
    throw new Error(`${label} must be a canonical ISO timestamp`);
  }
  return parsed;
};

export const oneOf = <const Values extends readonly (string | number)[]>(
  value: unknown,
  label: string,
  values: Values,
): Values[number] => {
  if (!values.some((candidate) => candidate === value)) {
    throw new Error(
      `${label} must be one of ${values.map((candidate) => JSON.stringify(candidate)).join(", ")}`,
    );
  }
  return value as Values[number];
};

export const exactLiteral = <const Value extends string | number | boolean>(
  value: unknown,
  label: string,
  expected: Value,
): Value => {
  if (value !== expected) {
    throw new Error(`${label} must be ${JSON.stringify(expected)}`);
  }
  return expected;
};

export const arrayOf = <Item>(
  value: unknown,
  label: string,
  parseItem: (item: unknown, label: string) => Item,
): readonly Item[] => {
  if (!Array.isArray(value)) {
    throw new Error(`${label} must be an array`);
  }
  return value.map((item, index) =>
    parseItem(item, `${label}[${index.toString()}]`),
  );
};

export const stringArray = (value: unknown, label: string): readonly string[] =>
  arrayOf(value, label, stringValue);

export const nullable = <Value>(
  value: unknown,
  label: string,
  parse: (input: unknown, label: string) => Value,
): Value | null => (value === null ? null : parse(value, label));

export const optional = <Value>(
  record: Record<string, unknown>,
  key: string,
  label: string,
  parse: (input: unknown, label: string) => Value,
): Value | undefined =>
  record[key] === undefined ? undefined : parse(record[key], `${label}.${key}`);

export const assertJsonValue = (value: unknown, label: string): void => {
  if (
    value === null ||
    typeof value === "string" ||
    typeof value === "boolean"
  ) {
    return;
  }
  if (typeof value === "number") {
    finiteNumber(value, label);
    return;
  }
  if (Array.isArray(value)) {
    value.forEach((entry, index) =>
      assertJsonValue(entry, `${label}[${index.toString()}]`),
    );
    return;
  }
  if (typeof value === "object") {
    if (!isPlainRecord(value)) {
      throw new Error(`${label} must contain only plain JSON objects`);
    }
    for (const [key, entry] of Object.entries(value)) {
      assertJsonValue(entry, `${label}.${key}`);
    }
    return;
  }
  throw new Error(`${label} must contain only JSON values`);
};

export const jsonValue = (value: unknown, label: string): unknown => {
  assertJsonValue(value, label);
  return value;
};

export const nullableString = (value: unknown, label: string): string | null =>
  nullable(value, label, stringValue);

export const nullableNonEmptyString = (
  value: unknown,
  label: string,
): string | null => nullable(value, label, nonEmptyString);

export const nullableNonNegativeNumber = (
  value: unknown,
  label: string,
): number | null => nullable(value, label, nonNegativeNumber);

export const NODE_SIGNAL_NAMES = [
  "SIGABRT",
  "SIGALRM",
  "SIGBUS",
  "SIGCHLD",
  "SIGCONT",
  "SIGFPE",
  "SIGHUP",
  "SIGILL",
  "SIGINT",
  "SIGIO",
  "SIGIOT",
  "SIGKILL",
  "SIGPIPE",
  "SIGPOLL",
  "SIGPROF",
  "SIGPWR",
  "SIGQUIT",
  "SIGSEGV",
  "SIGSTKFLT",
  "SIGSTOP",
  "SIGSYS",
  "SIGTERM",
  "SIGTRAP",
  "SIGTSTP",
  "SIGTTIN",
  "SIGTTOU",
  "SIGURG",
  "SIGUSR1",
  "SIGUSR2",
  "SIGVTALRM",
  "SIGWINCH",
  "SIGXCPU",
  "SIGXFSZ",
] as const satisfies readonly NodeJS.Signals[];

export const nodeSignal = (value: unknown, label: string): NodeJS.Signals =>
  oneOf(value, label, NODE_SIGNAL_NAMES);

// ---------------------------------------------------------------------------
// Strict dialect (originally the MPF commit-candidate artifact primitives).
// Exact key-set equality, bounded strings, canonical paths/digests. Messages
// and acceptance are pinned by artifact tests; do not fold into the canonical
// combinators above.
// ---------------------------------------------------------------------------

export const exactKeysRecord = (
  value: unknown,
  label: string,
  keys: readonly string[],
): Record<string, unknown> => {
  if (
    value === null ||
    typeof value !== "object" ||
    Array.isArray(value) ||
    JSON.stringify(Object.keys(value).sort()) !==
      JSON.stringify([...keys].sort())
  ) {
    throw new Error(`${label} must contain exactly: ${keys.join(", ")}`);
  }
  return value as Record<string, unknown>;
};

export const boundedNonEmptyString = (
  value: unknown,
  label: string,
  maxLength = 4096,
): string => {
  if (
    typeof value !== "string" ||
    value.trim().length === 0 ||
    value.length > maxLength ||
    value.includes("\0")
  ) {
    throw new Error(`${label} must be a bounded nonempty string`);
  }
  return value;
};

export const sha256Digest = (value: unknown, label: string): string => {
  if (typeof value !== "string" || !/^[0-9a-f]{64}$/u.test(value)) {
    throw new Error(`${label} must be a lowercase SHA-256 digest`);
  }
  return value;
};

export const canonicalAbsolutePath = (
  value: unknown,
  label: string,
): string => {
  const path = boundedNonEmptyString(value, label);
  if (!isAbsolute(path) || resolve(path) !== path) {
    throw new Error(`${label} must be a canonical absolute path`);
  }
  return path;
};

export const positiveSafeInteger = (value: unknown, label: string): number => {
  if (!Number.isSafeInteger(value) || (value as number) <= 0) {
    throw new Error(`${label} must be a positive safe integer`);
  }
  return value as number;
};

export const nonNegativeSafeInteger = (
  value: unknown,
  label: string,
): number => {
  if (!Number.isSafeInteger(value) || (value as number) < 0) {
    throw new Error(`${label} must be a nonnegative safe integer`);
  }
  return value as number;
};

export const canonicalUtcTimestamp = (
  value: unknown,
  label: string,
): string => {
  const parsed = typeof value === "string" ? new Date(value) : null;
  if (
    typeof value !== "string" ||
    !/^\d{4}-\d{2}-\d{2}T\d{2}:\d{2}:\d{2}\.\d{3}Z$/u.test(value) ||
    parsed === null ||
    !Number.isFinite(parsed.getTime()) ||
    parsed.toISOString() !== value
  ) {
    throw new Error(`${label} must be a canonical UTC timestamp`);
  }
  return value;
};

export const positiveFiniteNumber = (value: unknown, label: string): number => {
  if (typeof value !== "number" || !Number.isFinite(value) || value <= 0) {
    throw new Error(`${label} must be a positive finite number`);
  }
  return value;
};

export const nonNegativeFiniteNumber = (
  value: unknown,
  label: string,
): number => {
  if (typeof value !== "number" || !Number.isFinite(value) || value < 0) {
    throw new Error(`${label} must be a nonnegative finite number`);
  }
  return value;
};

// ---------------------------------------------------------------------------
// Declarative layer: an exact record described as a table of field parsers.
// Field labels thread as `<label>.<key>`, identical to the imperative
// convention, so adopting this layer never changes an error message. Only
// canonical-dialect artifacts (and new artifacts) should use it; families
// with prose per-field labels keep their imperative decode bodies.
// ---------------------------------------------------------------------------

type ParsedShape<Fields extends Record<string, ArtifactParser<unknown>>> = {
  -readonly [Key in keyof Fields]: Fields[Key] extends ArtifactParser<
    infer Value
  >
    ? Value
    : never;
};

export const objectOf =
  <
    Required extends Record<string, ArtifactParser<unknown>>,
    Optional extends Record<string, ArtifactParser<unknown>> = Record<
      never,
      never
    >,
  >(
    requiredFields: Required,
    optionalFields?: Optional,
  ): ArtifactParser<ParsedShape<Required> & Partial<ParsedShape<Optional>>> =>
  (value, label) => {
    const record = exactRecord(
      value,
      label,
      Object.keys(requiredFields),
      Object.keys(optionalFields ?? {}),
    );
    const result: Record<string, unknown> = {};
    for (const [key, parseField] of Object.entries(requiredFields)) {
      result[key] = parseField(record[key], `${label}.${key}`);
    }
    for (const [key, parseField] of Object.entries(optionalFields ?? {})) {
      if (record[key] !== undefined) {
        result[key] = parseField(record[key], `${label}.${key}`);
      }
    }
    return result as ParsedShape<Required> & Partial<ParsedShape<Optional>>;
  };

export const strictObjectOf =
  <Fields extends Record<string, ArtifactParser<unknown>>>(
    fields: Fields,
  ): ArtifactParser<ParsedShape<Fields>> =>
  (value, label) => {
    const record = exactKeysRecord(value, label, Object.keys(fields));
    const result: Record<string, unknown> = {};
    for (const [key, parseField] of Object.entries(fields)) {
      result[key] = parseField(record[key], `${label}.${key}`);
    }
    return result as ParsedShape<Fields>;
  };
