export type ExactArtifactRecord = Readonly<Record<string, unknown>>;

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
