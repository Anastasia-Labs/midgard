import { createHash } from "node:crypto";
import { isProxy } from "node:util/types";

export const WATCHER_DURABLE_STORE_SCHEMA_VERSION =
  "midgard-watcher-durable-store-v1" as const;

export const WATCHER_DURABLE_CACHE_SCHEMA_VERSION =
  "midgard-watcher-durable-cache-v1" as const;

export const WATCHER_DURABLE_MIGRATION_VERSION = 1 as const;

const MIGRATION_NAME =
  "fresh_install_canonical_v1_spent_protocol_utxo_journal" as const;

export const HEX_32 = /^[0-9a-f]{64}$/u;

export const OUT_REF = /^[0-9a-f]{64}#(?:0|[1-9][0-9]*)$/u;

export const CANONICAL_NATURAL = /^(?:0|[1-9][0-9]*)$/u;

export const CANONICAL_POSITIVE = /^[1-9][0-9]*$/u;

export const STABLE_NAME = /^[a-z][a-z0-9]*(?:[-_.][a-z0-9]+)*$/u;

export const PROVIDER_ID = /^[a-z][a-z0-9-]{0,62}$/u;

const LOWER_HEX_BYTES = /^(?:[0-9a-f]{2})+$/u;

export const sha256Utf8 = (value: string): string =>
  createHash("sha256").update(value, "utf8").digest("hex");

export const sha256Bytes = (value: Uint8Array): string =>
  createHash("sha256").update(value).digest("hex");

export const WATCHER_DURABLE_MIGRATION_MANIFEST_SHA256 = sha256Utf8(
  `${WATCHER_DURABLE_MIGRATION_VERSION}:${MIGRATION_NAME}:${WATCHER_DURABLE_STORE_SCHEMA_VERSION}`,
);

export type WatcherDurableStoreErrorCode =
  | "broken_reference"
  | "cache_mismatch"
  | "deployment_marker_mismatch"
  | "duplicate_key"
  | "integrity_mismatch"
  | "invalid_encoding"
  | "invalid_field"
  | "migration_conflict"
  | "missing_field"
  | "noncanonical_encoding"
  | "persistence_failure"
  | "unknown_field"
  | "unsupported_schema"
  | "unsorted_records";

export class WatcherDurableStoreError extends Error {
  readonly code: WatcherDurableStoreErrorCode;
  readonly path: string;

  constructor(code: WatcherDurableStoreErrorCode, path: string) {
    super(`Watcher durable store rejected: ${code} at ${path}`);
    this.name = "WatcherDurableStoreError";
    this.code = code;
    this.path = path;
  }
}

export const fail = (
  code: WatcherDurableStoreErrorCode,
  path: string,
): never => {
  throw new WatcherDurableStoreError(code, path);
};

export type JsonRecord = Record<string, unknown>;

export type CanonicalJson =
  | null
  | boolean
  | number
  | string
  | readonly CanonicalJson[]
  | { readonly [key: string]: CanonicalJson };

const plainRecord = (value: unknown, path: string): JsonRecord => {
  if (typeof value !== "object" || value === null || Array.isArray(value)) {
    fail("invalid_field", path);
  }
  const candidate = value as object;
  const prototype = Object.getPrototypeOf(candidate);
  if (prototype !== Object.prototype && prototype !== null) {
    fail("invalid_field", path);
  }
  if (Reflect.ownKeys(candidate).length !== Object.keys(candidate).length) {
    fail("invalid_field", path);
  }
  return value as JsonRecord;
};

export const exactRecord = (
  value: unknown,
  path: string,
  requiredKeys: readonly string[],
): JsonRecord => {
  const record = plainRecord(value, path);
  const required = new Set(requiredKeys);
  for (const key of Object.keys(record)) {
    if (!required.has(key)) {
      fail("unknown_field", `${path}.${key}`);
    }
  }
  for (const key of requiredKeys) {
    if (!Object.prototype.hasOwnProperty.call(record, key)) {
      fail("missing_field", `${path}.${key}`);
    }
  }
  return record;
};

export const exactString = (
  value: unknown,
  path: string,
  pattern: RegExp,
): string => {
  if (typeof value !== "string") {
    fail("invalid_field", path);
  }
  const stringValue = value as string;
  if (!pattern.test(stringValue)) {
    fail("invalid_field", path);
  }
  return stringValue;
};

export const exactLiteral = <T extends string>(
  value: unknown,
  path: string,
  allowed: readonly T[],
): T => {
  if (typeof value !== "string" || !allowed.includes(value as T)) {
    fail("invalid_field", path);
  }
  return value as T;
};

// Reuse only encodings whose entire reachable JSON tree has been validated
// and is frozen. A frozen container with a mutable child is never cacheable.
// Weak keys let discarded snapshot revisions and their encodings be collected.
export const immutableCanonicalJson = new WeakMap<object, string>();

export const canonicalJson = (
  value: unknown,
  path = "$",
  ancestors = new WeakSet<object>(),
): string => {
  if (
    value === null ||
    typeof value === "boolean" ||
    typeof value === "string"
  ) {
    return JSON.stringify(value);
  }
  if (typeof value === "number") {
    if (!Number.isSafeInteger(value)) {
      fail("invalid_field", path);
    }
    return value.toString();
  }
  if (typeof value !== "object" || value === null) {
    fail("invalid_field", path);
  }
  const objectValue = value as object;
  if (isProxy(objectValue)) {
    fail("invalid_field", path);
  }
  if (ancestors.has(objectValue)) {
    fail("invalid_field", path);
  }
  const cached = immutableCanonicalJson.get(objectValue);
  if (cached !== undefined) return cached;
  ancestors.add(objectValue);
  let immutable = Object.isFrozen(objectValue);
  const encodeChild = (child: unknown, childPath: string): string => {
    const encoded = canonicalJson(child, childPath, ancestors);
    if (
      typeof child === "object" &&
      child !== null &&
      !immutableCanonicalJson.has(child)
    ) {
      immutable = false;
    }
    return encoded;
  };
  let result: string;
  if (Array.isArray(value)) {
    if (
      Object.getPrototypeOf(value) !== Array.prototype ||
      Reflect.ownKeys(value).length !== value.length + 1 ||
      Reflect.ownKeys(value).some(
        (key) =>
          key !== "length" &&
          (typeof key !== "string" ||
            !/^(?:0|[1-9][0-9]*)$/u.test(key) ||
            Number(key) >= value.length),
      )
    ) {
      fail("invalid_field", path);
    }
    for (let index = 0; index < value.length; index += 1) {
      const descriptor = Object.getOwnPropertyDescriptor(
        value,
        index.toString(),
      );
      if (
        descriptor === undefined ||
        !descriptor.enumerable ||
        descriptor.get !== undefined ||
        descriptor.set !== undefined
      ) {
        fail("invalid_field", `${path}.${index}`);
      }
    }
    result = `[${value
      .map((member, index) => encodeChild(member, `${path}.${index}`))
      .join(",")}]`;
  } else {
    const record = plainRecord(value, path);
    for (const key of Object.keys(record)) {
      const descriptor = Object.getOwnPropertyDescriptor(record, key);
      if (
        descriptor === undefined ||
        !descriptor.enumerable ||
        descriptor.get !== undefined ||
        descriptor.set !== undefined
      ) {
        fail("invalid_field", `${path}.${key}`);
      }
    }
    result = `{${Object.keys(record)
      .sort()
      .map(
        (key) =>
          `${JSON.stringify(key)}:${encodeChild(record[key], `${path}.${key}`)}`,
      )
      .join(",")}}`;
  }
  ancestors.delete(objectValue);
  if (immutable) immutableCanonicalJson.set(objectValue, result);
  return result;
};

export const watcherCanonicalJson = (value: unknown): string =>
  canonicalJson(value);

export const immutableCanonicalDigests = new WeakMap<object, string>();

export const watcherSha256CanonicalJson = (value: unknown): string => {
  const encoded = watcherCanonicalJson(value);
  if (
    typeof value !== "object" ||
    value === null ||
    !immutableCanonicalJson.has(value)
  )
    return sha256Utf8(encoded);
  let digest = immutableCanonicalDigests.get(value);
  if (digest === undefined) {
    digest = sha256Utf8(encoded);
    immutableCanonicalDigests.set(value, digest);
  }
  return digest;
};

export const watcherSameCanonicalJson = (
  left: unknown,
  right: unknown,
): boolean => {
  try {
    return watcherCanonicalJson(left) === watcherCanonicalJson(right);
  } catch {
    return false;
  }
};

export type WatcherDurablePayload = Readonly<{
  cborHex: string;
  sha256: string;
}>;

export const parsePayload = (
  value: unknown,
  path: string,
): WatcherDurablePayload => {
  const payload = exactRecord(value, path, ["cborHex", "sha256"]);
  const cborHex = exactString(
    payload.cborHex,
    `${path}.cborHex`,
    LOWER_HEX_BYTES,
  );
  const digest = exactString(payload.sha256, `${path}.sha256`, HEX_32);
  if (sha256Bytes(Buffer.from(cborHex, "hex")) !== digest) {
    fail("integrity_mismatch", `${path}.sha256`);
  }
  return { cborHex, sha256: digest };
};

export const makeWatcherDurablePayload = (
  cborHex: string,
): WatcherDurablePayload => {
  if (!LOWER_HEX_BYTES.test(cborHex)) {
    fail("invalid_field", "$.cborHex");
  }
  return {
    cborHex,
    sha256: sha256Bytes(Buffer.from(cborHex, "hex")),
  };
};
