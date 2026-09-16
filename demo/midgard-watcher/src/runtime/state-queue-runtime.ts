import type { WatcherFaultDecisionBridge } from "../fault-proofs/fault-decision-bridge.js";
import type {
  WatcherAuthenticatedStateQueueObservation,
  WatcherStateQueueObservationSource,
  WatcherStateQueueRecovery,
} from "../indexers/authenticated-state-queue-observation.js";
import type { WatcherLocalKupmiosNativeObservation } from "../l1/local-kupmios-native-observation.js";
import type { WatcherNativeBlockAdmission } from "../l1/native-block-admission.js";
import type { WatcherNativeChainSyncPoint } from "../l1/native-chain-sync.js";
import type { WatcherSqliteStateQueueObservationStore } from "../storage/sqlite-durable-backend.js";
import type { WatcherBlockRelevance } from "./block-relevance.js";
import type { WatcherChainCoordinatorHooks } from "./chain-coordinator.js";

export const WATCHER_STATE_QUEUE_RUNTIME_SCHEMA_VERSION =
  "midgard-watcher-production-state-queue-runtime-v1" as const;

export type WatcherAvailabilityLifecycle = Readonly<{
  reconcile(
    observation: WatcherAuthenticatedStateQueueObservation,
    actuate: boolean,
  ): Promise<void>;
  invalidateForRollback(point?: WatcherNativeChainSyncPoint): void;
}>;

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
    availability?: WatcherAvailabilityLifecycle,
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
  // Rebuilt from the finalized cursor and canonical suffix after every rollback.
  let included = previous;
  let classificationDirty = false;
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
    current: () => included,
    bindFaultDecisionBridge: (bridge, availability) => {
      if (bound) {
        throw new Error("state-queue runtime already has a decision bridge");
      }
      bound = true;
      return Object.freeze({
        onRollback: async (point: WatcherNativeChainSyncPoint) => {
          // This must remain before the first await: already-running proof
          // workflows lose their exact generation authority immediately.
          bridge.invalidateForRollback();
          availability?.invalidateForRollback(point);
          classificationDirty = false;
          await input.store.rollbackTo(point);
          const retained = await input.store.readAll();
          if (retained.length === 0) {
            throw new Error(
              "native rollback removed the authenticated state-queue bootstrap cursor",
            );
          }
          const restored = await restoreAndRevokeDiscarded(retained);
          previous = restored.previous;
          included = previous;
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
          await availability?.reconcile(previous, false);
          await bridge.prepareForRecovery(previous);
        },
        onIncluded: async ({ nativeBlock, localObservation, relevance }) => {
          if (!caughtUp || input.source.observeIncluded === undefined) return;
          if (
            BigInt(nativeBlock.blockNo) <= BigInt(included.nativePoint.blockNo)
          )
            return;
          if (relevance === "touched") {
            if (localObservation === null)
              throw new Error(
                "included queue block omitted authenticated observation",
              );
            included = await input.source.observeIncluded({
              nativeBlock,
              localObservation,
              previous: included,
            });
            classificationDirty = true;
            await bridge.reconcileAndDispatch(included);
            classificationDirty = false;
          } else if (classificationDirty) {
            await bridge.reconcileAndDispatch(included);
            classificationDirty = false;
          } else {
            await bridge.retryDeferredClassification(included);
          }
          // Incomplete submitted journals include terminals waiting for anchoring.
          // Recovery schedules one reconciliation and releases the execution slot.
          await bridge.recoverExisting({ nativeProgress: nativeBlock });
        },
        onFinalized: async ({
          nativeBlock,
          localObservation,
          relevance,
        }: Readonly<{
          nativeBlock: WatcherNativeBlockAdmission;
          localObservation: WatcherLocalKupmiosNativeObservation | null;
          relevance: WatcherBlockRelevance;
        }>) => {
          if (relevance === "quiet") {
            // Keep queue evidence cached while waking yielded proofs on fresh
            // canonical progress. Inclusion-capable sources wake once later in
            // coordinator order, after finalized history has advanced.
            admitCatchupProgress(nativeBlock);
            if (caughtUp && input.source.observeIncluded === undefined)
              await bridge.recoverExisting({ nativeProgress: nativeBlock });
            return;
          }
          if (localObservation === null) {
            throw new Error(
              "touched block finalized without a local observation",
            );
          }
          // The coordinator records block progress only after this hook
          // returns, so a crash while dispatching replays the block whose
          // observation is already the durable cursor. That replay reuses
          // the cursor instead of observing the block a second time.
          const alreadyObserved =
            previous.nativePoint.blockHash === nativeBlock.blockHash &&
            previous.nativePoint.blockNo === nativeBlock.blockNo &&
            previous.nativePoint.slot === nativeBlock.slot;
          const next = alreadyObserved
            ? previous
            : await input.source.observe({
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
          const finalized =
            availability === undefined
              ? previous
              : (input.source.latestFinalizedObservation?.() ?? previous);
          await availability?.reconcile(finalized, true);
          if (
            BigInt(included.nativePoint.blockNo) <=
            BigInt(previous.nativePoint.blockNo)
          )
            included = finalized;
          classificationDirty = true;
          // Canonical progress can finalize several older blocks before the
          // coordinator delivers the next inclusion. Keep every history and
          // availability update above, then classify their combined context
          // once on that fresh inclusion (including a quiet block).
          if (input.source.observeIncluded === undefined) {
            await bridge.reconcileAndDispatch(included);
            classificationDirty = false;
          }
          await bridge.recoverExisting(
            input.source.observeIncluded === undefined
              ? { nativeProgress: nativeBlock }
              : undefined,
          );
        },
      });
    },
  });
};

export const createWatcherStateQueueRuntime = createRuntime;
