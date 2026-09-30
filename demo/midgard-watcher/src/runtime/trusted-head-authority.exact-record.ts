import { createHash, createHmac } from "node:crypto";
import { readFileSync } from "node:fs";
import { type FileHandle, open } from "node:fs/promises";
import { isAbsolute, normalize } from "node:path";

import { type WatcherRollbackDurableTrustedHead } from "../l1/rollback-engine.js";
import { watcherCanonicalJson } from "../storage/durable-store.js";

export const WATCHER_TRUSTED_HEAD_AUTHORITY_SCHEMA_VERSION =
  "midgard-watcher-trusted-head-authority-v1" as const;

export const WATCHER_TRUSTED_HEAD_AUTHORITY_RECORD_SCHEMA_VERSION =
  "midgard-watcher-trusted-head-authority-record-v1" as const;

export const RECORD_FILE = /^([0-9]{20})\.json$/u;

const UINT64_MAX = 18_446_744_073_709_551_615n;

const MAX_RECORD_BYTES = 16_384;

export const RECORD_SCAN_BATCH_SIZE = 8;

export const MAX_CACHED_RECORDS = 4_096;

export const MAX_REQUEST_BYTES = 32_768;

export const LOOPBACK_HOSTS = new Set([
  "127.0.0.1",
  "localhost",
  "::1",
  "[::1]",
]);

export class TrustedHeadCallerError extends Error {}

export type TrustedHeadAuthorityRecord = Readonly<{
  schemaVersion: typeof WATCHER_TRUSTED_HEAD_AUTHORITY_RECORD_SCHEMA_VERSION;
  revision: string;
  priorRecordSha256: string | null;
  head: WatcherRollbackDurableTrustedHead;
  recordAuthenticationKeyId: string;
  recordMac: string;
}>;

type TrustedHeadAuthorityRecordContent = Omit<
  TrustedHeadAuthorityRecord,
  "recordMac"
>;

export const exactRecord = (
  value: unknown,
  keys: readonly string[],
): Readonly<Record<string, unknown>> | null => {
  if (
    typeof value !== "object" ||
    value === null ||
    Array.isArray(value) ||
    Object.getPrototypeOf(value) !== Object.prototype ||
    Reflect.ownKeys(value).length !== Object.keys(value).length
  ) {
    return null;
  }
  const record = value as Readonly<Record<string, unknown>>;
  const actual = Object.keys(record).sort();
  const expected = [...keys].sort();
  return actual.length === expected.length &&
    actual.every((key, index) => key === expected[index])
    ? record
    : null;
};

export const canonicalDirectory = (value: unknown): string => {
  if (
    typeof value !== "string" ||
    value !== value.trim() ||
    !isAbsolute(value) ||
    normalize(value) !== value ||
    value === "/" ||
    value === "/tmp" ||
    value.startsWith("/tmp/")
  ) {
    throw new Error(
      "trusted-head authority requires a canonical durable directory",
    );
  }
  return value;
};

export const revision = (head: WatcherRollbackDurableTrustedHead): bigint => {
  const value = BigInt(head.revision);
  if (value > UINT64_MAX) {
    throw new Error("trusted-head authority revision exceeds uint64");
  }
  return value;
};

export const recordName = (value: bigint): string =>
  `${value.toString().padStart(20, "0")}.json`;

export const sameHead = (
  left: WatcherRollbackDurableTrustedHead | null,
  right: WatcherRollbackDurableTrustedHead | null,
): boolean =>
  left === null || right === null
    ? left === right
    : watcherCanonicalJson(left) === watcherCanonicalJson(right);

export const sameCanonical = (left: unknown, right: unknown): boolean => {
  try {
    return watcherCanonicalJson(left) === watcherCanonicalJson(right);
  } catch {
    return false;
  }
};

export const sha256 = (bytes: Uint8Array | string): string =>
  createHash("sha256").update(bytes).digest("hex");

export const recordKeyId = (key: Uint8Array): string => sha256(key);

export const recordMac = (
  key: Uint8Array,
  content: TrustedHeadAuthorityRecordContent,
): string =>
  createHmac("sha256", key)
    .update(
      `${WATCHER_TRUSTED_HEAD_AUTHORITY_RECORD_SCHEMA_VERSION}:${watcherCanonicalJson(content)}`,
      "utf8",
    )
    .digest("hex");

export const makeAuthorityRecord = (input: {
  readonly head: WatcherRollbackDurableTrustedHead;
  readonly priorRecordSha256: string | null;
  readonly recordAuthenticationKey: Uint8Array;
}): TrustedHeadAuthorityRecord => {
  const content = Object.freeze({
    schemaVersion: WATCHER_TRUSTED_HEAD_AUTHORITY_RECORD_SCHEMA_VERSION,
    revision: input.head.revision,
    priorRecordSha256: input.priorRecordSha256,
    head: input.head,
    recordAuthenticationKeyId: recordKeyId(input.recordAuthenticationKey),
  });
  return Object.freeze({
    ...content,
    recordMac: recordMac(input.recordAuthenticationKey, content),
  });
};

export const readBounded = (path: string): Uint8Array => {
  const bytes = readFileSync(path);
  if (bytes.byteLength === 0 || bytes.byteLength > MAX_RECORD_BYTES) {
    throw new Error("trusted-head authority record size is invalid");
  }
  return Uint8Array.from(bytes);
};

export const parseJson = (bytes: Uint8Array): unknown => {
  try {
    return JSON.parse(new TextDecoder("utf-8", { fatal: true }).decode(bytes));
  } catch {
    throw new Error("trusted-head authority record is malformed");
  }
};

export const syncDirectory = async (directory: string): Promise<void> => {
  let handle: FileHandle | undefined;
  try {
    handle = await open(directory, "r");
    await handle.sync();
  } finally {
    await handle?.close();
  }
};

export type WatcherTrustedHeadAuthorityStore = Readonly<{
  readRecordAuthenticationKeyId(): Promise<string>;
  readCurrent(): Promise<WatcherRollbackDurableTrustedHead | null>;
  compareAndSwap(input: {
    readonly expectedTrustedHead: unknown | null;
    readonly nextTrustedHead: unknown;
  }): Promise<boolean>;
}>;
