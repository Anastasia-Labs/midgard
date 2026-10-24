// The watcher's view of the node transport: its points, tips and events in
// the watcher's native chain-sync shapes, and its refusals as startup failures.
import {
  bytesToHex,
  type ChainPoint,
  chainPoint,
  type ChainSyncEvent,
  type ChainTip,
  IntersectNotFoundError,
  ORIGIN,
  SidecarExitedError,
  TransportFailedError,
  TransportRequestError,
  TransportTimeoutError,
  TransportUnavailableError,
} from "@al-ft/l1-node-transport";
import {
  WATCHER_NATIVE_CHAIN_SYNC_SCHEMA_VERSION,
  type WatcherNativeChainSyncEvent,
  type WatcherNativeChainSyncPoint,
  watcherNativeChainSyncRecord,
  type WatcherNativeChainSyncRollForward,
} from "midgard-watcher";

const { MAX_BLOCK_CBOR_HEX, NativeChainSyncStartupFailure } =
  watcherNativeChainSyncRecord;

type NativeTip = WatcherNativeChainSyncRollForward["tip"];

export const toChainPoint = (point: WatcherNativeChainSyncPoint): ChainPoint =>
  point.kind === "origin"
    ? ORIGIN
    : chainPoint(BigInt(point.slot), point.blockHash);

export const fromChainPoint = (
  point: ChainPoint,
): WatcherNativeChainSyncPoint =>
  point.kind === "origin"
    ? Object.freeze({ kind: "origin" })
    : Object.freeze({
        kind: "point",
        blockHash: point.hash,
        slot: point.slot.toString(),
      });

export const fromChainTip = (tip: ChainTip): NativeTip =>
  tip.point.kind === "origin"
    ? Object.freeze({ kind: "origin" })
    : Object.freeze({
        kind: "point",
        blockHash: tip.point.hash,
        blockNo: tip.blockNo.toString(),
        slot: tip.point.slot.toString(),
      });

/** A transport event in the watcher's event shape. */
export const watcherEvent = (
  event: ChainSyncEvent,
): WatcherNativeChainSyncEvent => {
  const tip = fromChainTip(event.tip);
  if (event.kind === "roll_backward") {
    return Object.freeze({
      schemaVersion: WATCHER_NATIVE_CHAIN_SYNC_SCHEMA_VERSION,
      kind: "roll_backward",
      point: fromChainPoint(event.point),
      tip,
    });
  }
  if (
    event.block.byteLength === 0 ||
    event.block.byteLength * 2 > MAX_BLOCK_CBOR_HEX
  ) {
    throw new Error("native raw block CBOR exceeds the supervisor bound");
  }
  // The node gives no parent hash only for the chain's first block.
  if (event.prevHash === null && event.blockNo !== 0n) {
    throw new Error("native block without a parent hash is not block zero");
  }
  return Object.freeze({
    schemaVersion: WATCHER_NATIVE_CHAIN_SYNC_SCHEMA_VERSION,
    kind: "roll_forward",
    blockHash: event.point.hash,
    blockType: event.blockType.toString(),
    prevHash: event.prevHash ?? "",
    slot: event.point.slot.toString(),
    blockNo: event.blockNo.toString(),
    rawBlockCbor: bytesToHex(event.block),
    tip,
  });
};

/**
 * The startup failure a transport refusal stands for, named by its code;
 * `isWatcherNativeNodeUnavailable` says which codes only mean that the node
 * or its sidecar did not answer. A failed transport (`TransportFailedError`)
 * is named by its reason, such as `node_handshake_failed`, which is not one.
 */
export const startupFailure = (error: unknown): unknown => {
  if (error instanceof IntersectNotFoundError)
    return new NativeChainSyncStartupFailure("intersection_failed");
  if (error instanceof TransportFailedError)
    return new NativeChainSyncStartupFailure(error.reason);
  if (error instanceof TransportUnavailableError)
    return new NativeChainSyncStartupFailure(error.reason);
  if (error instanceof TransportTimeoutError)
    return new NativeChainSyncStartupFailure("startup_timed_out");
  if (error instanceof SidecarExitedError)
    return new NativeChainSyncStartupFailure(
      error.exit.fatal?.code ?? "sidecar_exited",
    );
  if (error instanceof TransportRequestError)
    return new NativeChainSyncStartupFailure(error.code);
  return error;
};
