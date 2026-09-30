import { NATIVE_CHAIN_SYNC_SCHEMA_VERSION } from "@al-ft/midgard-core/native-reward-account";

import { type WatcherConfig } from "../runtime/config.js";

export const WATCHER_NATIVE_CHAIN_SYNC_SCHEMA_VERSION =
  NATIVE_CHAIN_SYNC_SCHEMA_VERSION;

export const HEX_32 = /^[0-9a-f]{64}$/u;

export const NATURAL = /^(?:0|[1-9][0-9]*)$/u;

export const MAX_BLOCK_CBOR_HEX = 8 * 1024 * 1024;

export const MAX_STDERR_BYTES = 1024 * 1024;

export const MAX_STDERR_DIAGNOSTIC_BYTES = 8 * 1024;

export const MAX_INTERSECTIONS = 128;

export const MAX_IDENTITY_FILE_BYTES = 4 * 1024 * 1024;

export const MAX_QUERY_STDOUT_BYTES = MAX_BLOCK_CBOR_HEX + 16_384;

export const MAX_UINT64 = (1n << 64n) - 1n;

export const NETWORK_MAGIC = Object.freeze({
  Mainnet: 764_824_073,
  Preprod: 1,
  Preview: 2,
} as const);

export class NativeChainSyncStartupFailure extends Error {
  readonly code: string;

  constructor(code: string) {
    super(`native chain-sync startup failed: ${code}`);
    this.name = "NativeChainSyncStartupFailure";
    this.code = code;
  }
}

export type WatcherNativeChainSyncPoint =
  | Readonly<{ kind: "origin" }>
  | Readonly<{ kind: "point"; blockHash: string; slot: string }>;

type NativeTip =
  | Readonly<{ kind: "origin" }>
  | Readonly<{
      kind: "point";
      blockHash: string;
      blockNo: string;
      slot: string;
    }>;

export type WatcherNativeChainSyncRollForward = Readonly<{
  schemaVersion: typeof WATCHER_NATIVE_CHAIN_SYNC_SCHEMA_VERSION;
  kind: "roll_forward";
  blockHash: string;
  blockType: string;
  prevHash: string;
  slot: string;
  blockNo: string;
  rawBlockCbor: string;
  tip: NativeTip;
}>;

export type WatcherNativeChainSyncRollBackward = Readonly<{
  schemaVersion: typeof WATCHER_NATIVE_CHAIN_SYNC_SCHEMA_VERSION;
  kind: "roll_backward";
  point: WatcherNativeChainSyncPoint;
  tip: NativeTip;
}>;

export type WatcherNativeChainSyncEvent =
  | WatcherNativeChainSyncRollForward
  | WatcherNativeChainSyncRollBackward;

export type WatcherNativeChainSyncAuthority = Readonly<{
  schemaVersion: "midgard-watcher-native-chain-sync-authority-v1";
  authorityDigest: string;
}>;

export type NativeBlockPoint = Readonly<{
  blockHash: string;
  blockNo: string;
  slot: string;
}>;

export type NativeOperation =
  | Readonly<{ kind: "stream" }>
  | Readonly<{
      kind: "exact_point";
      predecessorBlockNo: string;
      target: NativeBlockPoint;
      timeoutMs: number;
    }>;

export type WatcherNativeChainSyncAuthorityDetails = Readonly<{
  network: WatcherConfig["targetNetwork"];
  authorityNodeId: string;
  genesisIdentitySha256: string;
  socketPath: string;
  startupDigest: string;
  operation: NativeOperation;
  selectedIntersection: WatcherNativeChainSyncPoint;
  currentTip: NativeTip;
}>;

export type WatcherNativeChainSyncRuntime = Readonly<{
  authority: WatcherNativeChainSyncAuthority;
  done: Promise<void>;
  close(): Promise<void>;
}>;

export const authorityDetails = new WeakMap<
  WatcherNativeChainSyncAuthority,
  WatcherNativeChainSyncAuthorityDetails
>();

export const authorityLiveness = new WeakMap<
  WatcherNativeChainSyncAuthority,
  { active: boolean }
>();

export const watcherNativeChainSyncAuthorityDetails = (
  authority: WatcherNativeChainSyncAuthority,
): WatcherNativeChainSyncAuthorityDetails | null =>
  authorityLiveness.get(authority)?.active === true
    ? (authorityDetails.get(authority) ?? null)
    : null;

export const eventReceiptBrand = Symbol("native-chain-sync-event-receipt");

/** Process-local acquisition provenance, not block or historical admission. */
export type WatcherNativeChainSyncEventReceipt = Readonly<{
  [eventReceiptBrand]: true;
}>;

type NativeEventReceiptRead = Readonly<{
  authority: WatcherNativeChainSyncAuthority;
  startupDigest: string;
  /** The identical parsed event delivered to onEvent, including its observed tip. */
  event: WatcherNativeChainSyncEvent;
  /** SHA256 of watcherCanonicalJson(event), excluding any line terminator. */
  eventDigest: string;
}>;

export const receiptsByEvent = new WeakMap<
  WatcherNativeChainSyncEvent,
  WatcherNativeChainSyncEventReceipt
>();

export const eventReceipts = new WeakMap<
  WatcherNativeChainSyncEventReceipt,
  Readonly<{ value: NativeEventReceiptRead; isLive(): boolean }>
>();

/** Only supervisor-delivered object identity can acquire a live receipt. */
export const watcherNativeChainSyncEventReceipt = (
  event: WatcherNativeChainSyncEvent,
): WatcherNativeChainSyncEventReceipt | null => {
  const receipt = receiptsByEvent.get(event);
  return receipt !== undefined && eventReceipts.get(receipt)?.isLive() === true
    ? receipt
    : null;
};

/** Rollback, observed helper failure/exit, and close revoke prior provenance. */
export const readWatcherNativeChainSyncEventReceipt = (
  receipt: WatcherNativeChainSyncEventReceipt,
): NativeEventReceiptRead => {
  const state = eventReceipts.get(receipt);
  if (state === undefined || !state.isLive()) {
    throw new Error("native chain-sync event receipt is absent or stale");
  }
  return state.value;
};

export const exactRecord = (
  value: unknown,
  keys: readonly string[],
  label: string,
): Readonly<Record<string, unknown>> => {
  if (
    typeof value !== "object" ||
    value === null ||
    Array.isArray(value) ||
    Object.getPrototypeOf(value) !== Object.prototype ||
    Reflect.ownKeys(value).length !== Object.keys(value).length
  ) {
    throw new Error(`${label} is not an exact plain object`);
  }
  const record = value as Readonly<Record<string, unknown>>;
  const actual = Object.keys(record).sort();
  const expected = [...keys].sort();
  if (
    actual.length !== expected.length ||
    actual.some((key, index) => key !== expected[index])
  ) {
    throw new Error(
      `${label} has unknown or missing fields: missing=${JSON.stringify(expected.filter((key) => !actual.includes(key)))} unknown=${JSON.stringify(actual.filter((key) => !expected.includes(key)))}`,
    );
  }
  return record;
};

export const string = (
  value: unknown,
  pattern: RegExp,
  label: string,
): string => {
  if (typeof value !== "string" || !pattern.test(value)) {
    throw new Error(`${label} is invalid`);
  }
  return value;
};

export const parseTip = (value: unknown): NativeTip => {
  if (
    typeof value === "object" &&
    value !== null &&
    (value as { kind?: unknown }).kind === "origin"
  ) {
    exactRecord(value, ["kind"], "native tip");
    return Object.freeze({ kind: "origin" });
  }
  const tip = exactRecord(
    value,
    ["blockHash", "blockNo", "kind", "slot"],
    "native tip",
  );
  if (tip.kind !== "point") throw new Error("native tip kind is invalid");
  return Object.freeze({
    kind: "point" as const,
    blockHash: string(tip.blockHash, HEX_32, "native tip hash"),
    blockNo: string(tip.blockNo, NATURAL, "native tip block number"),
    slot: string(tip.slot, NATURAL, "native tip slot"),
  });
};

export const parsePoint = (
  value: unknown,
  label: string,
): WatcherNativeChainSyncPoint => {
  if (
    typeof value === "object" &&
    value !== null &&
    (value as { kind?: unknown }).kind === "origin"
  ) {
    exactRecord(value, ["kind"], label);
    return Object.freeze({ kind: "origin" });
  }
  const point = exactRecord(value, ["blockHash", "kind", "slot"], label);
  if (point.kind !== "point") throw new Error(`${label} kind is invalid`);
  return Object.freeze({
    kind: "point" as const,
    blockHash: string(point.blockHash, HEX_32, `${label} hash`),
    slot: string(point.slot, NATURAL, `${label} slot`),
  });
};
