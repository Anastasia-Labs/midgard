import type { WatcherFaultDecisionBridge } from "../fault-proofs/production-fault-decision-bridge-v1.js";
import type {
  WatcherAuthenticatedStateQueueObservation,
  WatcherStateQueueObservationSource,
  WatcherStateQueueRecovery,
} from "../indexers/production-state-queue-observation-v1.js";
import type { WatcherLocalKupmiosNativeObservation } from "../l1/local-kupmios-native-observation-v1.js";
import type { WatcherNativeBlockAdmission } from "../l1/native-block-admission-v1.js";
import type { WatcherNativeChainSyncPoint } from "../l1/native-chain-sync-v1.js";
import type { WatcherSqliteStateQueueObservationStore } from "../storage/sqlite-durable-backend-v1.js";
import type { WatcherChainCoordinatorHooks } from "./production-chain-coordinator-v1.js";

export const WATCHER_STATE_QUEUE_RUNTIME_SCHEMA_VERSION =
  "midgard-watcher-production-state-queue-runtime-v1" as const;

export type WatcherStateQueueRuntime = Readonly<{
  schemaVersion: typeof WATCHER_STATE_QUEUE_RUNTIME_SCHEMA_VERSION;
  replayIntersection: Readonly<{
    blockHash: string;
    blockNo: string;
    slot: string;
    chainPointId: string;
  }>;
  catchupBoundary: Readonly<{
    blockHash: string;
    blockNo: string;
    slot: string;
    chainPointId: string;
    finalityDepth: string;
    ogmiosTipBlockNo: string;
  }>;
  caughtUp: Promise<void>;
  current(): WatcherAuthenticatedStateQueueObservation;
  bindFaultDecisionBridge(
    bridge: WatcherFaultDecisionBridge,
  ): WatcherChainCoordinatorHooks;
}>;

const sameRecoveryPoint = (
  left: WatcherStateQueueRecovery["replayIntersection"],
  right: WatcherStateQueueRecovery["catchupBoundary"],
): boolean =>
  left.blockHash === right.blockHash &&
  left.blockNo === right.blockNo &&
  left.slot === right.slot &&
  left.chainPointId === right.chainPointId;

const createRuntime = async (input: {
  readonly store: WatcherSqliteStateQueueObservationStore;
  readonly source: WatcherStateQueueObservationSource;
}): Promise<WatcherStateQueueRuntime> => {
  const persisted = await input.store.readAll();
  const restoreAndRevokeDiscarded = async (
    candidates: readonly unknown[],
  ): Promise<WatcherStateQueueRecovery> => {
    const restored = await input.source.restore({
      persistedObservations: candidates,
    });
    if (restored.discardedObservationCount === 0) return restored;
    if (
      !Number.isSafeInteger(restored.discardedObservationCount) ||
      restored.discardedObservationCount < 1 ||
      restored.discardedObservationCount >= candidates.length
    ) {
      throw new Error(
        "state-queue source returned an invalid discarded suffix count",
      );
    }
    await input.store.rollbackTo(
      Object.freeze({
        kind: "point",
        blockHash: restored.replayIntersection.blockHash,
        slot: restored.replayIntersection.slot,
      }),
    );
    const retained = await input.store.readAll();
    if (
      retained.length !==
      candidates.length - restored.discardedObservationCount
    ) {
      throw new Error(
        "state-queue store did not revoke the exact rejected durable suffix",
      );
    }
    const confirmed = await input.source.restore({
      persistedObservations: retained,
    });
    if (
      confirmed.discardedObservationCount !== 0 ||
      confirmed.previous.observationDigest !==
        restored.previous.observationDigest ||
      confirmed.replayIntersection.blockHash !==
        restored.replayIntersection.blockHash ||
      confirmed.replayIntersection.blockNo !==
        restored.replayIntersection.blockNo ||
      confirmed.replayIntersection.slot !== restored.replayIntersection.slot ||
      confirmed.replayIntersection.chainPointId !==
        restored.replayIntersection.chainPointId
    ) {
      throw new Error(
        "state-queue retained prefix changed after durable suffix revocation",
      );
    }
    return confirmed;
  };
  const recovery =
    persisted.length === 0
      ? await input.source.bootstrap()
      : await restoreAndRevokeDiscarded(persisted);
  if (persisted.length === 0 && recovery.discardedObservationCount !== 0) {
    throw new Error(
      "state-queue bootstrap reported a discarded durable suffix",
    );
  }
  let previous = recovery.previous;
  let catchupBoundary = recovery.catchupBoundary;
  if (persisted.length === 0) {
    const appended = await input.store.append(previous);
    if (appended !== "appended") {
      throw new Error(
        "state-queue bootstrap did not append its authenticated cursor",
      );
    }
  }
  let caughtUp = sameRecoveryPoint(
    recovery.replayIntersection,
    catchupBoundary,
  );
  let resolveCaughtUp!: () => void;
  let rejectCaughtUp!: (reason: Error) => void;
  const caughtUpPromise = new Promise<void>((resolve, reject) => {
    resolveCaughtUp = resolve;
    rejectCaughtUp = reject;
  });
  // A process mounts this promise in readiness. Avoid an unhandled rejection
  // if a startup failure occurs before the readiness surface is installed.
  void caughtUpPromise.catch(() => undefined);
  if (caughtUp) resolveCaughtUp();
  let bound = false;

  const admitCatchupProgress = (block: WatcherNativeBlockAdmission): void => {
    if (caughtUp) return;
    const currentBlockNo = BigInt(block.blockNo);
    const boundaryBlockNo = BigInt(catchupBoundary.blockNo);
    if (currentBlockNo > boundaryBlockNo) {
      const error = new Error(
        "native replay skipped the authenticated state-queue catch-up boundary",
      );
      rejectCaughtUp(error);
      throw error;
    }
    if (currentBlockNo !== boundaryBlockNo) return;
    if (
      block.blockHash !== catchupBoundary.blockHash ||
      block.slot !== catchupBoundary.slot
    ) {
      const error = new Error(
        "native replay reached a foreign state-queue catch-up boundary",
      );
      rejectCaughtUp(error);
      throw error;
    }
    caughtUp = true;
    resolveCaughtUp();
  };

  return Object.freeze({
    schemaVersion: WATCHER_STATE_QUEUE_RUNTIME_SCHEMA_VERSION,
    replayIntersection: recovery.replayIntersection,
    get catchupBoundary() {
      return catchupBoundary;
    },
    caughtUp: caughtUpPromise,
    current: () => previous,
    bindFaultDecisionBridge: (bridge) => {
      if (bound) {
        throw new Error("state-queue runtime already has a decision bridge");
      }
      bound = true;
      return Object.freeze({
        onRollback: async (point: WatcherNativeChainSyncPoint) => {
          // This must remain before the first await: already-running proof
          // workflows lose their exact generation authority immediately.
          bridge.invalidateForRollback();
          await input.store.rollbackTo(point);
          const retained = await input.store.readAll();
          if (retained.length === 0) {
            throw new Error(
              "native rollback removed the authenticated state-queue bootstrap cursor",
            );
          }
          const restored = await restoreAndRevokeDiscarded(retained);
          previous = restored.previous;
          if (!caughtUp) {
            catchupBoundary = restored.catchupBoundary;
            if (
              sameRecoveryPoint(
                restored.replayIntersection,
                restored.catchupBoundary,
              )
            ) {
              caughtUp = true;
              resolveCaughtUp();
            }
          }
          await bridge.prepareForRecovery(previous);
        },
        onFinalized: async ({
          nativeBlock,
          localObservation,
        }: Readonly<{
          nativeBlock: WatcherNativeBlockAdmission;
          localObservation: WatcherLocalKupmiosNativeObservation;
        }>) => {
          const next = await input.source.observe({
            nativeBlock,
            localObservation,
            previous,
          });
          if (
            next !== null &&
            next.observationDigest !== previous.observationDigest
          ) {
            const appended = await input.store.append(next);
            if (appended !== "appended") {
              throw new Error("state-queue successor was not durably appended");
            }
            previous = next;
          }
          admitCatchupProgress(nativeBlock);
          await bridge.reconcileAndDispatch(previous);
        },
      });
    },
  });
};

export const createWatcherStateQueueRuntime = createRuntime;
