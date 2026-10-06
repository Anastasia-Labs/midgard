/**
 * Per-test-file memoization of pure, expensive fixture builders.
 *
 * A memo entry is keyed by a SHA-256 digest of a canonical, type-tagged
 * encoding of the builder's complete input: every byte, every key (including
 * keys whose value is `undefined`) and the distinction between a `Buffer`, a
 * plain `Uint8Array`, a number and a bigint. A plain object's keys are encoded
 * in sorted order, so two objects with the same entries in a different order
 * are equal as data: the builders memoized here (`native-trace-memo.ts`) read
 * their input by field name and check the consensus profile as canonical JSON,
 * so key order never reaches what they build. A `Map` keeps its insertion
 * order, which iterating it observes. Anything the encoding cannot
 * describe exactly (a class instance, a function, a symbol, a typed array
 * other than a `Uint8Array`, a sparse array, a cycle) is refused rather than
 * approximated, so two inputs share an entry only when they are equal as
 * data.
 *
 * The memo hands out deep copies only. The cached value is built from a
 * private copy of the input and never leaves the memo, so no caller can reach
 * another caller's value, mutate it, or mutate the input the cached value was
 * built from.
 */
import { createHash, type Hash } from "node:crypto";

const describe = (value: object): string =>
  Object.getPrototypeOf(value)?.constructor?.name ?? "null-prototype object";

const isPlainObject = (value: object): boolean => {
  const prototype = Object.getPrototypeOf(value);
  return prototype === Object.prototype || prototype === null;
};

const refuse = (what: string, path: string): never => {
  throw new Error(`Fixture memo cannot represent ${what} at ${path}`);
};

const encode = (
  hash: Hash,
  value: unknown,
  path: string,
  ancestors: Set<object>,
): void => {
  switch (typeof value) {
    case "undefined":
      hash.update("u");
      return;
    case "boolean":
      hash.update(value ? "T" : "F");
      return;
    case "number":
      hash.update(`d${Object.is(value, -0) ? "-0" : value.toString()};`);
      return;
    case "bigint":
      hash.update(`i${value.toString()};`);
      return;
    case "string":
      hash.update(`s${value.length.toString()}:`);
      hash.update(value, "utf8");
      return;
    case "symbol":
    case "function":
      return refuse(`a ${typeof value}`, path);
    case "object":
      break;
  }
  if (value === null) {
    hash.update("z");
    return;
  }
  if (ancestors.has(value)) return refuse("a cycle", path);
  if (Buffer.isBuffer(value)) {
    hash.update(`B${value.length.toString()}:`);
    hash.update(value);
    return;
  }
  if (Object.getPrototypeOf(value) === Uint8Array.prototype) {
    const bytes = value as Uint8Array;
    hash.update(`b${bytes.length.toString()}:`);
    hash.update(bytes);
    return;
  }
  ancestors.add(value);
  if (Array.isArray(value)) {
    if (Object.getPrototypeOf(value) !== Array.prototype)
      refuse(describe(value), path);
    hash.update(`a${value.length.toString()}:`);
    for (let index = 0; index < value.length; index += 1) {
      if (!(index in value)) refuse("a sparse array", path);
      encode(hash, value[index], `${path}[${index.toString()}]`, ancestors);
    }
  } else if (
    value instanceof Map &&
    Object.getPrototypeOf(value) === Map.prototype
  ) {
    hash.update(`m${value.size.toString()}:`);
    let index = 0;
    for (const [key, item] of value) {
      encode(hash, key, `${path}.<key ${index.toString()}>`, ancestors);
      encode(hash, item, `${path}.<value ${index.toString()}>`, ancestors);
      index += 1;
    }
  } else if (isPlainObject(value)) {
    if (Object.getOwnPropertySymbols(value).length > 0)
      refuse("symbol keys", path);
    const keys = Object.keys(value).sort();
    hash.update(
      `${Object.getPrototypeOf(value) === null ? "n" : "o"}${keys.length.toString()}:`,
    );
    for (const key of keys) {
      encode(hash, key, path, ancestors);
      encode(
        hash,
        (value as Record<string, unknown>)[key],
        `${path}.${key}`,
        ancestors,
      );
    }
  } else {
    refuse(describe(value), path);
  }
  ancestors.delete(value);
};

/** SHA-256 of the canonical type-tagged encoding of `value`. */
export const canonicalFixtureDigest = (value: unknown): string => {
  const hash = createHash("sha256");
  encode(hash, value, "input", new Set());
  return hash.digest("hex");
};

const copy = (
  value: unknown,
  path: string,
  copies: Map<object, unknown>,
): unknown => {
  if (typeof value === "function" || typeof value === "symbol")
    return refuse(`a ${typeof value}`, path);
  if (typeof value !== "object" || value === null) return value;
  const existing = copies.get(value);
  if (existing !== undefined) return existing;
  if (Buffer.isBuffer(value)) {
    const result = Buffer.from(value);
    copies.set(value, result);
    return result;
  }
  if (Object.getPrototypeOf(value) === Uint8Array.prototype) {
    const result = new Uint8Array(value as Uint8Array);
    copies.set(value, result);
    return result;
  }
  let result: unknown;
  if (Array.isArray(value)) {
    if (Object.getPrototypeOf(value) !== Array.prototype)
      refuse(describe(value), path);
    const items: unknown[] = new Array(value.length);
    copies.set(value, items);
    for (let index = 0; index < value.length; index += 1) {
      if (!(index in value)) refuse("a sparse array", path);
      items[index] = copy(value[index], `${path}[${index.toString()}]`, copies);
    }
    result = items;
  } else if (
    value instanceof Map &&
    Object.getPrototypeOf(value) === Map.prototype
  ) {
    const map = new Map<unknown, unknown>();
    copies.set(value, map);
    let index = 0;
    for (const [key, item] of value) {
      map.set(
        copy(key, `${path}.<key ${index.toString()}>`, copies),
        copy(item, `${path}.<value ${index.toString()}>`, copies),
      );
      index += 1;
    }
    result = map;
  } else if (isPlainObject(value)) {
    if (Object.getOwnPropertySymbols(value).length > 0)
      refuse("symbol keys", path);
    const object: Record<string, unknown> = Object.create(
      Object.getPrototypeOf(value),
    );
    copies.set(value, object);
    for (const key of Object.keys(value)) {
      object[key] = copy(
        (value as Record<string, unknown>)[key],
        `${path}.${key}`,
        copies,
      );
    }
    result = object;
  } else {
    return refuse(describe(value), path);
  }
  if (Object.isFrozen(value)) Object.freeze(result);
  return result;
};

/**
 * An independent deep copy: same structure, same prototypes (`Buffer`,
 * `Uint8Array`, `Array`, `Map`, plain objects), same sharing between parts and
 * same frozenness, with no object in common with `value`.
 */
export const deepCopyFixture = <T>(value: T): T =>
  copy(value, "value", new Map()) as T;

/**
 * `build(input)` once per distinct input per test file; every call, the first
 * included, receives its own deep copy of the result. `build` receives a
 * private deep copy of the input, so the caller's later edits to its input
 * cannot reach the cached result. A failed build is not cached.
 */
export const createFixtureMemo = <Input, Output>(
  build: (input: Input) => Promise<Output>,
) => {
  const entries = new Map<string, Promise<Output>>();
  return async (input: Input): Promise<Output> => {
    const key = canonicalFixtureDigest(input);
    let entry = entries.get(key);
    if (entry === undefined) {
      entry = build(deepCopyFixture(input));
      entries.set(key, entry);
      entry.catch(() => {
        if (entries.get(key) === entry) entries.delete(key);
      });
    }
    return deepCopyFixture(await entry);
  };
};
