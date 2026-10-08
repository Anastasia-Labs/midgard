import {
  computeFraudProofRawL1PointId,
  type FraudProofRawL1Point,
} from "@al-ft/midgard-fault-proofs";
import type {
  FactStore,
  FollowStatus,
  Point,
} from "@al-ft/midgard-l1-follower";

import type { WatcherAvailabilityRuntime } from "../availability/runtime.js";
import {
  type WatcherFaultDecisionBridge,
  WatcherFaultDecisionRetired,
} from "../fault-proofs/fault-decision-bridge.js";
import type { WatcherAuthenticatedStateQueueObservation } from "../indexers/authenticated-state-queue-observation.js";
import type { WatcherNativeChainSyncPoint } from "../l1/native-chain-sync.js";
import {
  readWatcherObservation,
  type WatcherObservationAuthority,
  type WatcherObservationRead,
} from "../l1-follower/observation.js";
import {
  WatcherUserEventOperationRetired,
  type WatcherUserEventRuntime,
} from "./user-event-runtime.js";

/**
 * The watcher's decision driver (ticket W1, with W3's rollback handling):
 * every fault decision, availability action and history advance is a pure
 * function of the follower's facts at the tip and at the release depth.
 *
 * One pass reads both observations in their own read transactions and hands
 * them to the availability runtime, the user-event history and the decision
 * bridge, in that order. Passes are single-flight and coalesced: every
 * follower change wakes the driver, and a change during a pass runs one more
 * pass after it.
 *
 * A follower rewind (any depth below k) revokes the bridge's and the
 * availability runtime's authority synchronously, inside the store's
 * generation notification, so no action decided on the removed blocks
 * proceeds; the next pass recomputes everything from the facts. Nothing is
 * quarantined and nothing needs a restart. A rewind the follower refuses
 * (beyond k) stops the follower with an intervention, which the follower's
 * readiness reports; this driver keeps serving the last facts.
 *
 * Liveness: a pass never throws out of the driver. A failure is a named
 * readiness reason with its detail and a retry after `retryDelayMs`.
 */

/** A pass failed; the detail names the failure. Retried after the delay. */
export const WATCHER_DECISION_PASS_FAILED = "watcher_decision_pass_failed";
/** The local user-event history failed or closed; event-backed families cannot decide. */
export const WATCHER_USER_EVENT_HISTORY_UNAVAILABLE =
  "user_event_history_unavailable";
/** The release-depth observation is not available yet (for example a short chain). */
export const WATCHER_RELEASE_OBSERVATION_PENDING =
  "release_observation_pending";
/**
 * The follower in this process has not finished its store start yet. The
 * start may reset the store (a tracked-set addition), so the facts before it
 * may be incomplete for this process's tracked set: no pass reads them.
 */
export const WATCHER_FOLLOWER_NOT_STARTED = "l1_follower_not_started";

/** The driver's `started` gate: the follower published a status with a cursor. */
export const watcherFollowerStarted = (
  status: Pick<FollowStatus, "cursor"> | null | undefined,
): boolean => (status?.cursor ?? null) !== null;

export type WatcherDecisionReadiness = Readonly<{
  reason: string;
  detail: string;
}>;

type DriverHistory = Pick<
  WatcherUserEventRuntime,
  "read" | "advanceThrough" | "handleRollback"
>;

export type WatcherDecisionDriverInput = Readonly<{
  store: Pick<FactStore, "transaction" | "onGeneration" | "cursor">;
  /** Hears every follower status change (new block, rewind, wait). */
  onFollowerChange(listener: () => void): () => void;
  authority: WatcherObservationAuthority;
  sourceId: string;
  /** The release depth (the manifest's confirmation depth). */
  releaseDepth: number;
  bridge: Pick<
    WatcherFaultDecisionBridge,
    | "prepareForRecovery"
    | "recoverExisting"
    | "reconcileAndDispatch"
    | "invalidateForRollback"
    | "beforeHistoryAdvance"
  >;
  availability: Pick<
    WatcherAvailabilityRuntime,
    "reconcile" | "invalidateForRollback"
  >;
  /** The local user-event history; it advances through release-final points. */
  history?: DriverHistory;
  /** Replay-transcript retirement at release-final observations. */
  retirement?: Readonly<{
    /** No proof work is pending or running: retirement may sweep. */
    ready(): boolean;
    retire(
      observation: WatcherAuthenticatedStateQueueObservation,
    ): Promise<unknown>;
    reset(): Promise<void>;
  }>;
  /** The tip the driver last decided at, for L1 freshness reporting. */
  onDecided?: (tip: WatcherAuthenticatedStateQueueObservation) => void;
  retryDelayMs: number;
  log?: (line: string) => void;
}>;

export type WatcherDecisionDriver = Readonly<{
  /** The newest tip observation a pass completed with. */
  current(): WatcherAuthenticatedStateQueueObservation;
  /**
   * The tip observation the running (or last) pass read, before it decides;
   * null before the first read and after a rollback until the next one. The
   * availability actor's L1 payload reads are scoped to it, so they work in
   * the pass that first decides.
   */
  inclusion(): WatcherAuthenticatedStateQueueObservation | null;
  /** Why decisions are held; empty when the last pass completed. */
  readiness(): readonly WatcherDecisionReadiness[];
  /** Settles with the recovered workflow count after the first completed pass. */
  recovered: Promise<number>;
  /** Settles once a pass completes with the follower at the node tip. */
  caughtUp: Promise<void>;
  /** Runs (or joins) a pass. */
  wake(): void;
  /** Resolves when no pass is running or owed. */
  idle(): Promise<void>;
  status(): Readonly<{
    passes: number;
    rewinds: number;
    lastError: string | null;
  }>;
  close(): Promise<void>;
}>;

const message = (error: unknown): string =>
  error instanceof Error ? error.message : String(error);

const rawPoint = (
  observation: WatcherAuthenticatedStateQueueObservation,
): FraudProofRawL1Point => {
  const { blockHash, slot, blockNo } = observation.nativePoint;
  return {
    blockHash,
    slot,
    blockNo,
    pointId: computeFraudProofRawL1PointId({ blockHash, slot, blockNo }),
  };
};

const nativePoint = (point: Point): WatcherNativeChainSyncPoint => ({
  kind: "point",
  blockHash: point.hash.toString("hex"),
  slot: point.slot.toString(),
});

/** A failure that only says a rewind or a newer pass retired the work. */
const retired = (error: unknown): boolean =>
  error instanceof WatcherFaultDecisionRetired ||
  error instanceof WatcherUserEventOperationRetired;

export const createWatcherDecisionDriver = (
  input: WatcherDecisionDriverInput,
  options: Readonly<{
    /** Whether the follower's last status had its cursor at the node tip. */
    atTip(): boolean;
    /**
     * Whether the follower in this process finished its store start (it
     * published a status with a cursor). Absent: the caller started it.
     */
    started?(): boolean;
  }>,
): WatcherDecisionDriver => {
  if (!Number.isSafeInteger(input.retryDelayMs) || input.retryDelayMs <= 0)
    throw new Error("the decision driver needs a positive retry delay");
  const log = input.log ?? (() => undefined);
  let closed = false;
  let running: Promise<void> | null = null;
  let owed = false;
  let retry: ReturnType<typeof setTimeout> | null = null;
  let passes = 0;
  let rewinds = 0;
  let lastError: string | null = null;
  let current: WatcherAuthenticatedStateQueueObservation | null = null;
  let inclusion: WatcherAuthenticatedStateQueueObservation | null = null;
  let held: WatcherDecisionReadiness[] = [
    { reason: "watcher_decision_pending", detail: "no pass has completed" },
  ];
  // Every rewind owes the bridge a recovery preparation before its next
  // dispatch, and owes the history a rollback to the lowest rewind target.
  let recoveryPending = true;
  let historyRewindTo: Point | null = null;
  let recoveredCount: number | null = null;

  let resolveRecovered!: (count: number) => void;
  const recovered = new Promise<number>((resolve) => {
    resolveRecovered = resolve;
  });
  let resolveCaughtUp!: () => void;
  let caughtUpSettled = false;
  const caughtUp = new Promise<void>((resolve) => {
    resolveCaughtUp = resolve;
  });
  const idleWaiters = new Set<() => void>();

  const unsubscribeGeneration = input.store.onGeneration(({ rewound }) => {
    // Synchronous: no runnable authority survives into the next await.
    input.bridge.invalidateForRollback();
    input.availability.invalidateForRollback();
    inclusion = null;
    rewinds += 1;
    recoveryPending = true;
    if (historyRewindTo === null || rewound.to.slot < historyRewindTo.slot)
      historyRewindTo = rewound.to;
    if (input.retirement !== undefined)
      void input.retirement.reset().catch((error: unknown) => {
        log(`replay-transcript retirement reset failed: ${message(error)}`);
      });
    wake();
  });
  const unsubscribeChange = input.onFollowerChange(() => wake());

  const observe = async (depth: number): Promise<WatcherObservationRead> =>
    await readWatcherObservation(input.store as FactStore, {
      authority: input.authority,
      sourceId: input.sourceId,
      depth,
      releaseDepth: input.releaseDepth,
    });

  const rewindHistory = async (history: DriverHistory): Promise<void> => {
    const target = historyRewindTo;
    if (target === null) return;
    const head = history.read().currentPoint;
    if (BigInt(head.slot) > BigInt(target.slot))
      await history.handleRollback(nativePoint(target));
    // A newer rewind during the call keeps its own (lower or equal) target.
    if (historyRewindTo === target) historyRewindTo = null;
  };

  const pass = async (): Promise<void> => {
    if (options.started?.() === false) {
      held = [
        {
          reason: WATCHER_FOLLOWER_NOT_STARTED,
          detail: "the follower has not finished starting its store",
        },
      ];
      return;
    }
    const reasons: WatcherDecisionReadiness[] = [];
    const history = input.history;
    if (history !== undefined) {
      const status = history.read().status;
      if (status === "failed" || status === "closed")
        reasons.push({
          reason: WATCHER_USER_EVENT_HISTORY_UNAVAILABLE,
          detail: `the local user-event history is ${status}`,
        });
      else await rewindHistory(history);
    }
    const tip = await observe(1);
    if (tip.kind === "unready") {
      held = [{ reason: tip.reason, detail: tip.detail }, ...reasons];
      return;
    }
    inclusion = tip.observation;
    const release = await observe(input.releaseDepth);
    if (release.kind === "ok") {
      await input.availability.reconcile(release.observation, true);
      if (
        history !== undefined &&
        history.read().status === "ready" &&
        BigInt(release.observation.nativePoint.blockNo) >
          BigInt(history.read().currentPoint.blockNo)
      ) {
        input.bridge.beforeHistoryAdvance();
        await history.advanceThrough(rawPoint(release.observation));
      }
    } else
      reasons.push({
        reason: WATCHER_RELEASE_OBSERVATION_PENDING,
        detail: `${release.reason}: ${release.detail}`,
      });
    if (recoveryPending) {
      recoveryPending = false;
      try {
        await input.bridge.prepareForRecovery(tip.observation);
      } catch (error) {
        recoveryPending = true;
        throw error;
      }
    }
    await input.bridge.reconcileAndDispatch(tip.observation);
    const recoveredNow = await input.bridge.recoverExisting();
    current = tip.observation;
    if (recoveredCount === null) {
      recoveredCount = recoveredNow;
      resolveRecovered(recoveredNow);
    }
    if (input.retirement !== undefined && release.kind === "ok") {
      if (input.retirement.ready())
        await input.retirement.retire(release.observation);
      else await input.retirement.reset();
    }
    input.onDecided?.(tip.observation);
    held = reasons;
    lastError = null;
    if (!caughtUpSettled && options.atTip()) {
      caughtUpSettled = true;
      resolveCaughtUp();
    }
  };

  const settleIdle = () => {
    if (running !== null || owed) return;
    for (const resolve of idleWaiters) resolve();
    idleWaiters.clear();
  };

  const run = (): void => {
    if (closed || running !== null) return;
    owed = false;
    passes += 1;
    running = pass().then(
      () => undefined,
      (error: unknown) => {
        if (retired(error)) {
          // A rewind retired the work; its notification already owes a pass.
          owed = true;
          return;
        }
        lastError = message(error);
        held = [{ reason: WATCHER_DECISION_PASS_FAILED, detail: lastError }];
        log(`decision pass failed: ${lastError}`);
        if (retry === null && !closed) {
          retry = setTimeout(() => {
            retry = null;
            wake();
          }, input.retryDelayMs);
          retry.unref();
        }
      },
    );
    void running.finally(() => {
      running = null;
      if (owed && !closed) run();
      else settleIdle();
    });
  };

  function wake(): void {
    if (closed) return;
    if (running !== null) {
      owed = true;
      return;
    }
    run();
  }

  return Object.freeze({
    current: () => {
      if (current === null)
        throw new Error("the decision driver has no observation yet");
      return current;
    },
    inclusion: () => inclusion,
    readiness: () => Object.freeze([...held]),
    recovered,
    caughtUp,
    wake,
    idle: () =>
      running === null && !owed
        ? Promise.resolve()
        : new Promise<void>((resolve) => {
            idleWaiters.add(resolve);
          }),
    status: () => Object.freeze({ passes, rewinds, lastError }),
    close: async () => {
      closed = true;
      unsubscribeGeneration();
      unsubscribeChange();
      if (retry !== null) clearTimeout(retry);
      retry = null;
      await running?.catch(() => undefined);
      for (const resolve of idleWaiters) resolve();
      idleWaiters.clear();
    },
  });
};
