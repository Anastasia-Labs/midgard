import type { ReadNativeLedgerFile } from "@al-ft/midgard-l1-follower/native-reward-account";
import type { WatcherConfig } from "midgard-watcher";

/**
 * The devnet stack's native chain-sync records: the events its supervisor
 * reads from the local node (`native-chain-sync.ts`), their exact-record
 * parsers and the receipts that bind them to one live supervisor. The watcher
 * itself follows L1 through its chain follower and reads none of these.
 */
export const WATCHER_NATIVE_CHAIN_SYNC_SCHEMA_VERSION =
  "midgard-watcher-native-chain-sync-v1" as const;

export const HEX_32 = /^[0-9a-f]{64}$/u;

export const NATURAL = /^(?:0|[1-9][0-9]*)$/u;

export const MAX_BLOCK_CBOR_HEX = 8 * 1024 * 1024;

export const MAX_INTERSECTIONS = 128;

export const MAX_UINT64 = (1n << 64n) - 1n;

export class NativeChainSyncStartupFailure extends Error {
  readonly code: string;

  constructor(code: string) {
    super(`native chain-sync startup failed: ${code}`);
    this.name = "NativeChainSyncStartupFailure";
    this.code = code;
  }
}

/**
 * Native chain-sync startup failures (the devnet stack retries them with this
 * test explicitly) that say only that the node did not answer: the node
 * transport was not ready (its sidecar was starting or restarting, or the
 * node's socket did not accept or dropped the connection, during the
 * handshake or after it), an auxiliary node connection could not be opened,
 * or the read did not start in time. A node that is restarting or still
 * opening its database gives exactly these. A rejected intersection, an
 * invalid startup, a refused handshake (`node_handshake_failed`: another
 * network magic, no common version) and every identity mismatch are not
 * among them.
 */
const NODE_UNAVAILABLE_CODES: ReadonlySet<string> = new Set([
  "sidecar_starting",
  "sidecar_restarting",
  "sidecar_unavailable",
  "sidecar_exited",
  "node_unreachable",
  "node_connection_lost",
  "node_unresponsive",
  "node_unavailable",
  "startup_timed_out",
]);

export const isWatcherNativeNodeUnavailable = (
  error: unknown,
): error is NativeChainSyncStartupFailure =>
  error instanceof NativeChainSyncStartupFailure &&
  NODE_UNAVAILABLE_CODES.has(error.code);

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

/** Rollback, an observed stream failure, and close revoke prior provenance. */
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

export const parseWatcherNativeChainSyncEvent = (
  value: unknown,
): WatcherNativeChainSyncEvent => {
  const base = exactRecord(
    value,
    typeof value === "object" &&
      value !== null &&
      (value as { kind?: unknown }).kind === "roll_forward"
      ? [
          "blockHash",
          "blockNo",
          "blockType",
          "kind",
          "prevHash",
          "rawBlockCbor",
          "schemaVersion",
          "slot",
          "tip",
        ]
      : ["kind", "point", "schemaVersion", "tip"],
    "native chain-sync event",
  );
  if (base.schemaVersion !== WATCHER_NATIVE_CHAIN_SYNC_SCHEMA_VERSION) {
    throw new Error("native chain-sync event schema changed");
  }
  const tip = parseTip(base.tip);
  if (base.kind === "roll_forward") {
    const rawBlockCbor = string(
      base.rawBlockCbor,
      /^(?:[0-9a-f]{2})+$/u,
      "native raw block CBOR",
    );
    if (rawBlockCbor.length > MAX_BLOCK_CBOR_HEX) {
      throw new Error("native raw block CBOR exceeds the supervisor bound");
    }
    return Object.freeze({
      schemaVersion: WATCHER_NATIVE_CHAIN_SYNC_SCHEMA_VERSION,
      kind: "roll_forward",
      blockHash: string(base.blockHash, HEX_32, "native block hash"),
      blockType: string(base.blockType, NATURAL, "native block type"),
      prevHash:
        base.blockNo === "0" && base.prevHash === ""
          ? ""
          : string(base.prevHash, HEX_32, "native previous block hash"),
      slot: string(base.slot, NATURAL, "native chain-sync slot"),
      blockNo: string(base.blockNo, NATURAL, "native block number"),
      rawBlockCbor,
      tip,
    });
  }
  if (base.kind !== "roll_backward") {
    throw new Error("native chain-sync event kind is unsupported");
  }
  return Object.freeze({
    schemaVersion: WATCHER_NATIVE_CHAIN_SYNC_SCHEMA_VERSION,
    kind: "roll_backward",
    point: parsePoint(base.point, "native rollback point"),
    tip,
  });
};

export type NativeStreamInput = {
  readonly binaryPath: string;
  readonly watcherConfig: WatcherConfig;
  readonly signal?: AbortSignal;
  readonly intersection: WatcherNativeChainSyncPoint;
  readonly startupTimeoutMs: number;
  readonly onEvent: (event: WatcherNativeChainSyncEvent) => Promise<void>;
  /** Private runtime lifetime hook, after native admission is revoked. */
  readonly onAuthorityRevoked?: () => void;
  readonly unsafeReadIdentityFileForTest?: ReadNativeLedgerFile;
};
