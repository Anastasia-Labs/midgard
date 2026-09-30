import { createHash } from "node:crypto";

import { watcherSha256CanonicalJson } from "../storage/durable-store.js";
import {
  CANONICAL_NATURAL,
  normalizationSessionStates,
  normalizedBlockProvenance,
  transportAttestationStates,
  UINT64_MAX,
  WATCHER_L1_ADAPTER_BOUNDS,
  type WatcherL1NormalizationSession,
  type WatcherL1NormalizationSessionStats,
  type WatcherL1TransportAttestationContext,
  type WatcherL1TransportAttestationDetails,
  type WatcherNormalizedL1Block,
} from "./l1-adapter.watcher-local-node-query-transport.js";

export const watcherL1NormalizationSessionStats = (
  session: WatcherL1NormalizationSession,
): WatcherL1NormalizationSessionStats => {
  const state = normalizationSessionStates.get(session);
  if (state === undefined) {
    throw new Error("unknown watcher L1 normalization session");
  }
  return Object.freeze({
    retainedEntries: state.transactions.size,
    retainedBytes: state.retainedBytes,
    maximumEntries: WATCHER_L1_ADAPTER_BOUNDS.normalizationSessionEntries,
    maximumBytes: WATCHER_L1_ADAPTER_BOUNDS.normalizationSessionBytes,
  });
};

export const isWatcherL1AdapterNormalizedBlock = (
  value: unknown,
): value is WatcherNormalizedL1Block => {
  if (typeof value !== "object" || value === null) {
    return false;
  }
  const context = normalizedBlockProvenance.get(value);
  return (
    context !== undefined &&
    watcherL1TransportAttestationDetails(context) !== null
  );
};

export const watcherL1TransportAttestationDetails = (
  context: unknown,
): WatcherL1TransportAttestationDetails | null => {
  if (typeof context !== "object" || context === null) {
    return null;
  }
  const state = transportAttestationStates.get(
    context as WatcherL1TransportAttestationContext,
  );
  return state !== undefined &&
    state.active &&
    state.upstreamIsLive() &&
    state.transports.every(
      (transport) =>
        !transport.destroyed &&
        transport.readable &&
        transport.writable &&
        transport.readyState === "open",
    )
    ? state.details
    : null;
};

export const isWatcherL1BlockAttestedBy = (
  block: unknown,
  context: unknown,
): boolean =>
  typeof block === "object" &&
  block !== null &&
  typeof context === "object" &&
  context !== null &&
  normalizedBlockProvenance.get(block) === context &&
  watcherL1TransportAttestationDetails(context) !== null;

export const closeWatcherL1TransportAttestationContext = (
  context: WatcherL1TransportAttestationContext,
): void => {
  const state =
    transportAttestationStates.get(context) ??
    fail("invalid_field", "$.transportAttestationContext");
  state.active = false;
  clearTimeout(state.renewalTimer);
  for (const transport of state.ownedTransports) {
    transport.destroy();
  }
};

export type WatcherL1AdapterErrorCode =
  | "content_digest_mismatch"
  | "duplicate_identity"
  | "identity_mismatch"
  | "invalid_field"
  | "missing_field"
  | "network_mismatch"
  | "out_of_bounds"
  | "provider_mismatch"
  | "unknown_field"
  | "unsafe_value"
  | "unsupported_schema";

export type WatcherL1AdapterDiagnostic = Readonly<{
  code: WatcherL1AdapterErrorCode;
  path: string;
  message: string;
}>;

export class WatcherL1AdapterError extends Error {
  readonly code: WatcherL1AdapterErrorCode;
  readonly path: string;

  constructor(code: WatcherL1AdapterErrorCode, path: string) {
    super(`Watcher L1 observation rejected: ${code} at ${path}`);
    this.name = "WatcherL1AdapterError";
    this.code = code;
    this.path = path;
  }
}

export const fail = (code: WatcherL1AdapterErrorCode, path: string): never => {
  throw new WatcherL1AdapterError(code, path);
};

export const watcherL1AdapterDiagnostic = (
  error: unknown,
): WatcherL1AdapterDiagnostic => {
  if (error instanceof WatcherL1AdapterError) {
    return {
      code: error.code,
      path: error.path,
      message: error.message,
    };
  }
  return {
    code: "invalid_field",
    path: "$",
    message: "Watcher L1 observation rejected: invalid_field at $",
  };
};

export type JsonRecord = Record<string, unknown>;

export type CanonicalJson =
  | null
  | boolean
  | string
  | readonly CanonicalJson[]
  | { readonly [key: string]: CanonicalJson };

export type ParseBudget = {
  collectionMembers: number;
  publicBytes: number;
};

export const plainRecord = (value: unknown, path: string): JsonRecord => {
  if (typeof value !== "object" || value === null || Array.isArray(value)) {
    fail("invalid_field", path);
  }
  const candidate = value as object;
  const prototype = Object.getPrototypeOf(candidate);
  if (prototype !== Object.prototype && prototype !== null) {
    fail("unsafe_value", path);
  }
  if (Reflect.ownKeys(candidate).length !== Object.keys(candidate).length) {
    fail("unsafe_value", path);
  }
  for (const key of Object.keys(candidate)) {
    const descriptor = Object.getOwnPropertyDescriptor(candidate, key);
    if (
      descriptor === undefined ||
      descriptor.get !== undefined ||
      descriptor.set !== undefined
    ) {
      fail("unsafe_value", path);
    }
  }
  return value as JsonRecord;
};

export const exactRecord = (
  value: unknown,
  path: string,
  keys: readonly string[],
): JsonRecord => {
  const record = plainRecord(value, path);
  const expected = new Set(keys);
  for (const key of Object.keys(record)) {
    if (!expected.has(key)) {
      fail("unknown_field", `${path}.${key}`);
    }
  }
  for (const key of keys) {
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
  if (typeof value !== "string" || !pattern.test(value)) {
    fail("invalid_field", path);
  }
  return value as string;
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

export const exactNatural = (value: unknown, path: string): string => {
  const natural = exactString(value, path, CANONICAL_NATURAL);
  if (natural.length > 20 || BigInt(natural) > UINT64_MAX) {
    fail("out_of_bounds", path);
  }
  return natural;
};

export const exactArray = (
  value: unknown,
  path: string,
): readonly unknown[] => {
  if (!Array.isArray(value)) {
    fail("invalid_field", path);
  }
  const values = value as readonly unknown[];
  if (
    Object.getPrototypeOf(values) !== Array.prototype ||
    Reflect.ownKeys(values).some(
      (key) =>
        typeof key !== "string" ||
        (key !== "length" &&
          (!CANONICAL_NATURAL.test(key) ||
            BigInt(key) >= BigInt(values.length))),
    ) ||
    Object.keys(values).length !== values.length
  ) {
    fail("unsafe_value", path);
  }
  for (let index = 0; index < values.length; index += 1) {
    const descriptor = Object.getOwnPropertyDescriptor(
      values,
      index.toString(),
    );
    if (
      descriptor === undefined ||
      descriptor.get !== undefined ||
      descriptor.set !== undefined
    ) {
      fail("unsafe_value", path);
    }
  }
  if (values.length > WATCHER_L1_ADAPTER_BOUNDS.arrayMembers) {
    fail("out_of_bounds", path);
  }
  return values;
};

const reserveCollectionMembers = (
  budget: ParseBudget,
  count: number,
  path: string,
): void => {
  budget.collectionMembers += count;
  if (
    budget.collectionMembers > WATCHER_L1_ADAPTER_BOUNDS.totalCollectionMembers
  ) {
    fail("out_of_bounds", path);
  }
};

export const preflightTransactionCollections = (
  value: unknown,
  budget: ParseBudget,
): readonly unknown[] => {
  const transactions = exactArray(value, "$.transactions");
  reserveCollectionMembers(budget, transactions.length, "$.transactions");
  for (let index = 0; index < transactions.length; index += 1) {
    const path = `$.transactions[${index.toString()}]`;
    const unparsed = plainRecord(transactions[index], path);
    const transaction = exactRecord(transactions[index], path, [
      "txHash",
      ...(Object.prototype.hasOwnProperty.call(unparsed, "transactionIndex")
        ? ["transactionIndex"]
        : []),
      "fullTransaction",
      "body",
      "witnessSet",
      "utxos",
      "scripts",
      "datums",
      "redeemers",
    ]);
    for (const field of ["utxos", "scripts", "datums", "redeemers"] as const) {
      const members = exactArray(transaction[field], `${path}.${field}`);
      reserveCollectionMembers(budget, members.length, "$.transactions");
    }
  }
  return transactions;
};

export const sha256Bytes = (value: Uint8Array): string =>
  createHash("sha256").update(value).digest("hex");

export const digestCanonicalJson = (value: CanonicalJson): string =>
  watcherSha256CanonicalJson(value);
