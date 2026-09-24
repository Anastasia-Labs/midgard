import { computeFraudProofRawL1PointId } from "@al-ft/midgard-fault-proofs";

import {
  type WatcherFaultDecisionBridge,
  WatcherFaultDecisionRetired,
} from "../fault-proofs/fault-decision-bridge.js";
import type { WatcherNativeBlockAdmission } from "../l1/native-block-admission.js";
import type { WatcherNativeChainSyncPoint } from "../l1/native-chain-sync.js";
import {
  type WatcherChainCoordinatorHooks,
  WatcherConsumerDeliveryHeld,
} from "./chain-coordinator.js";
import type { WatcherAvailabilityLifecycle } from "./state-queue-runtime.js";
import {
  WatcherUserEventOperationRetired,
  type WatcherUserEventRuntime,
} from "./user-event-runtime.js";

/** Owns the gap between immediate revocation and authenticated history recovery.
 * It never treats a failed acquisition as authority; consumers stay held with
 * the failure retained until a later acquisition actually succeeds. */
export const createWatcherHistoryRecovery = (input: {
  readonly history: Pick<
    WatcherUserEventRuntime,
    "read" | "handleRollback" | "classify" | "advanceThrough" | "coverQuiet"
  >;
  readonly queue: WatcherChainCoordinatorHooks;
  readonly bridge: Pick<
    WatcherFaultDecisionBridge,
    "invalidateForRollback" | "beforeHistoryAdvance"
  >;
  readonly availability: Pick<
    WatcherAvailabilityLifecycle,
    "invalidateForRollback"
  >;
  readonly quarantined: () => boolean;
  readonly resume: () => Promise<void>;
  readonly retryDelayMs: number;
  readonly onPending: (pending: boolean) => void;
}) => {
  if (!Number.isSafeInteger(input.retryDelayMs) || input.retryDelayMs <= 0)
    throw new Error("History recovery requires a positive retry delay");
  let closed = false;
  let generation = 0;
  let point: WatcherNativeChainSyncPoint | null = null;
  let running: Promise<void> | null = null;
  let retry: ReturnType<typeof setTimeout> | null = null;
  const asError = (error: unknown): Error =>
    error instanceof Error
      ? error
      : new Error(
          typeof error === "string" ? error : "History recovery failed",
          { cause: error },
        );
  let lastError: Error | null = null;
  let fatal: Error | null = null;
  const changes = new Set<() => void>();
  const changed = () => {
    for (const notify of changes) notify();
  };
  let resolveDone!: () => void;
  let rejectDone!: (error: unknown) => void;
  const done = new Promise<void>((resolve, reject) => {
    resolveDone = resolve;
    rejectDone = reject;
  });
  void done.catch(() => undefined);
  const cancelRetry = () => {
    if (retry !== null) clearTimeout(retry);
    retry = null;
  };
  const fail = (error: unknown) => {
    fatal = asError(error);
    cancelRetry();
    rejectDone(error);
    changed();
  };
  const wake = (expected: number) => {
    if (closed || expected !== generation) return;
    void input.resume().catch((error: unknown) => {
      if (!closed) fail(error);
    });
  };
  const acquire = () => {
    if (closed || fatal !== null || running !== null || point === null) return;
    const expected = generation;
    const requested = point;
    // Invocation itself synchronously fences the old publisher and permits.
    let attempt: Promise<void>;
    try {
      attempt = input.history.handleRollback(requested);
    } catch (error) {
      fail(error);
      return;
    }
    running = attempt;
    void attempt.then(
      () => {
        if (closed || expected !== generation) return;
        running = null;
        point = null;
        lastError = null;
        cancelRetry();
        input.onPending(false);
        wake(expected);
        changed();
      },
      (error: unknown) => {
        if (closed || expected !== generation) return;
        running = null;
        lastError = asError(error);
        if (input.history.read().status !== "suspended") {
          fail(error);
          return;
        }
        input.onPending(true);
        retry = setTimeout(() => {
          retry = null;
          acquire();
        }, input.retryDelayMs);
        retry.unref();
      },
    );
  };
  const ready = () => {
    if (closed) throw new Error("History recovery is closed");
    if (fatal !== null) throw fatal;
    if (input.quarantined())
      throw new Error(
        "History consumers cannot run during finality quarantine",
      );
    const status = input.history.read().status;
    if (status === "failed" || status === "closed")
      throw new Error(`History owner is ${status}`);
    if (status !== "ready" || running !== null) {
      acquire();
      throw new WatcherConsumerDeliveryHeld();
    }
    // The owner's native monitor may have completed the same recovery first.
    if (point !== null) {
      point = null;
      lastError = null;
      cancelRetry();
      input.onPending(false);
    }
  };
  const waitForRecovery = async () => {
    // A separate owner monitor can finish recovery without invoking these hooks.
    // Poll only this private barrier at the configured acquisition interval.
    while (true) {
      try {
        ready();
        return;
      } catch (error) {
        if (!(error instanceof WatcherConsumerDeliveryHeld)) throw error;
      }
      await new Promise<void>((resolve) => {
        const notify = () => {
          clearTimeout(timer);
          changes.delete(notify);
          resolve();
        };
        const timer = setTimeout(notify, input.retryDelayMs);
        changes.add(notify);
      });
    }
  };
  let monitorWake: Promise<void> | null = null;
  const requestResume = () => {
    if (closed || monitorWake !== null) return;
    const expected = generation;
    monitorWake = waitForRecovery().then(
      () => {
        monitorWake = null;
        wake(expected);
      },
      (error: unknown) => {
        monitorWake = null;
        if (!closed) fail(error);
      },
    );
  };
  const readyForDelivery = () => {
    try {
      ready();
    } catch (error) {
      if (error instanceof WatcherConsumerDeliveryHeld) requestResume();
      throw error;
    }
  };
  const deliverQueue = async (work: () => Promise<void>) => {
    const expected = generation;
    try {
      await work();
    } catch (error) {
      if (error instanceof WatcherFaultDecisionRetired) {
        lastError = asError(error);
        requestResume();
        throw new WatcherConsumerDeliveryHeld();
      }
      throw error;
    }
    if (closed || generation !== expected)
      throw new WatcherConsumerDeliveryHeld();
    readyForDelivery();
  };
  const hooks: WatcherChainCoordinatorHooks = {
    onRollbackArrived: (rollbackPoint) => {
      input.bridge.invalidateForRollback();
      input.availability.invalidateForRollback(rollbackPoint);
      generation += 1;
      cancelRetry();
      point = rollbackPoint;
      running = null;
      lastError = null;
      input.onPending(true);
      acquire();
    },
    // Structural rewind stays serialized; volatile authority was already revoked.
    onRollback: input.queue.onRollback,
    onIncluded: async (included) => {
      readyForDelivery();
      await deliverQueue(async () => {
        await input.queue.onIncluded?.(included);
      });
    },
    onFinalized: async (finalized) => {
      readyForDelivery();
      const ownerGeneration = input.history.read().generation;
      try {
        if (finalized.relevance === "quiet")
          await input.history.coverQuiet(finalized.nativeBlock);
        else {
          const head = input.history.read().currentPoint;
          if (BigInt(finalized.nativeBlock.blockNo) > BigInt(head.blockNo))
            input.bridge.beforeHistoryAdvance();
          await input.history.advanceThrough({
            blockHash: finalized.nativeBlock.blockHash,
            blockNo: finalized.nativeBlock.blockNo,
            slot: finalized.nativeBlock.slot,
            pointId: computeFraudProofRawL1PointId(finalized.nativeBlock),
          });
        }
      } catch (error) {
        const current = input.history.read();
        if (
          error instanceof WatcherUserEventOperationRetired &&
          current.generation !== ownerGeneration &&
          (current.status === "suspended" || current.status === "ready")
        ) {
          lastError = asError(error);
          requestResume();
          throw new WatcherConsumerDeliveryHeld();
        }
        throw error;
      }
      readyForDelivery();
      await deliverQueue(() => input.queue.onFinalized(finalized));
    },
  };
  return Object.freeze({
    hooks,
    done,
    waitForRecovery,
    classify: (
      block: WatcherNativeBlockAdmission,
      extraTrackedOutRefs: Iterable<string>,
    ) =>
      input.history.read().status === "suspended"
        ? ("touched" as const)
        : input.history.classify(block, extraTrackedOutRefs),
    status: () =>
      Object.freeze({
        pending:
          point !== null ||
          running !== null ||
          input.history.read().status !== "ready",
        lastError: lastError?.message ?? null,
      }),
    close: () => {
      closed = true;
      generation += 1;
      cancelRetry();
      resolveDone();
      changed();
    },
  });
};
