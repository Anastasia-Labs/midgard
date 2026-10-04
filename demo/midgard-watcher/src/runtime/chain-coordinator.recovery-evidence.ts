import type { WatcherLocalKupmiosNativeObservation } from "../l1/local-kupmios-native-observation.js";
import {
  readWatcherNativeChainSyncEventReceipt,
  type WatcherNativeChainSyncEvent,
  watcherNativeChainSyncEventReceipt,
} from "../l1/native-chain-sync.js";
import type { WatcherDurableRuntime } from "../storage/durable-runtime.js";
import { WatcherDurableAuthorityConflict } from "../storage/durable-runtime.load-published-authority.js";
import {
  recoverWatcherCoordinatorAfterRestart,
  type WatcherProcessedHead,
} from "./chain-coordinator.canonical-path-from-history.js";
import { WatcherCoordinatorIntegrityHeld } from "./chain-coordinator.integrity-hold.js";

export const headOf = (
  record: Readonly<{ blockHash: string; blockNo: string; slot: string }>,
): WatcherProcessedHead =>
  Object.freeze({
    blockHash: record.blockHash,
    blockNo: record.blockNo,
    slot: record.slot,
  });

/** Integrity holds stay live; authenticated rollback evidence may reopen them. */
export const retryQuarantinedRecovery = (
  durable: WatcherDurableRuntime,
  event: WatcherNativeChainSyncEvent,
  assertCurrent: () => void,
): Promise<boolean> =>
  event.kind === "roll_backward"
    ? recoverWatcherCoordinatorAfterRestart({
        durable,
        restartIntersection: event.point,
        assertCurrent,
      })
    : Promise.resolve(true);

export const persistQuietRecoveryEvidence = async (
  durable: WatcherDurableRuntime,
  observed: WatcherLocalKupmiosNativeObservation,
): Promise<void> => {
  const persisted = await durable.persistObservation(observed);
  if (persisted.persistence === "conflict")
    throw new WatcherDurableAuthorityConflict(
      "watcher quiet observation persistence conflicted",
    );
};

/** A MAC-owned prior branch cannot substitute for the current native lease. */
export const guardRecoveryEvent = (
  event: WatcherNativeChainSyncEvent,
  isCurrent: () => boolean,
  unsafeAllowUnprovenancedEvents = false,
): (() => void) => {
  const receipt = watcherNativeChainSyncEventReceipt(event);
  return () => {
    if (!isCurrent() || (receipt === null && !unsafeAllowUnprovenancedEvents))
      throw new WatcherCoordinatorIntegrityHeld(
        "native_generation_changed",
        "native recovery event generation is no longer current",
      );
    if (receipt !== null) {
      try {
        readWatcherNativeChainSyncEventReceipt(receipt);
      } catch (error) {
        throw new WatcherCoordinatorIntegrityHeld(
          "native_generation_changed",
          error instanceof Error
            ? error.message
            : "native recovery source expired",
        );
      }
    }
  };
};
