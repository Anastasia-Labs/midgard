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
  readHandledFollowerGeneration,
  writeHandledFollowerGeneration,
} from "../l1-follower/follower-generation.js";
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
 * A rewind or store reset the notification never reached (one at a start
 * before the driver subscribed, or one its process stopped before handling)
 * is pulled by generation at the top of every pass, before anything reads
 * the facts: from the generation the driver last handled
 * (`follower-generation.ts`) through the store's rollback log. Both paths
 * meet in one handler, idempotent per generation: the invalidations, the
 * rewind count and `onRewind` run once for each new generation, and the
 * history target is the lowest one either path saw (rolling the history
 * back to a point at or above its head is a no-op). The pull reads from the
 * higher of the durable marker and the generation this process last rolled
 * the history back through, so a pull of a generation already applied in
 * this process (the marker waits on the retirement reset) rolls nothing
 * back again.
 *
 * The durable marker moves only once the history is back at every target
 * and the replay-transcript retirement reset the last rewind started held.
 * The pass waits for that reset at most `retryDelayMs`; while it is still
 * pending or failed, the driver is unready by name and the marker waits
 * (a failed reset is retried by the next pass, one `retryDelayMs` later).
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
/**
 * The replay-transcript retirement reset a rewind started failed; the
 * detail names the failure. The handled-generation marker waits for it, and
 * the next pass retries it.
 */
export const WATCHER_RETIREMENT_RESET_FAILED =
  "watcher_retirement_reset_failed";
/**
 * The replay-transcript retirement reset a rewind started has not settled
 * within the pass's wait. The handled-generation marker waits for it.
 */
export const WATCHER_RETIREMENT_RESET_PENDING =
  "watcher_retirement_reset_pending";

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
  store: Pick<
    FactStore,
    "transaction" | "onGeneration" | "cursor" | "rewindsSince"
  >;
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
  /** Once per follower generation a rewind or reset raised, pushed or pulled. */
  onRewind?: (generation: number) => void;
  /**
   * The delay before a failed pass (or a failed retirement reset) is
   * retried, and the most a pass waits for a pending retirement reset.
   */
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
  // The highest follower generation this process handled; null before the
  // first push or pull.
  let handledGeneration: number | null = null;
  // The highest generation this process rolled the history back through
  // (every target up to it applied); null before the first. The pull reads
  // from it when it is above the durable marker.
  let historyAppliedGeneration: number | null = null;
  // The last retirement reset a rewind started; the durable handled
  // generation moves only once it held.
  type RetirementReset = {
    outcome: "pending" | "held" | "failed";
    detail: string;
    settled: Promise<void>;
  };
  let retirementReset: RetirementReset | null = null;

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

  /** Runs a pass after `retryDelayMs`, unless one is already scheduled. */
  const scheduleRetry = (): void => {
    if (retry !== null || closed) return;
    retry = setTimeout(() => {
      retry = null;
      wake();
    }, input.retryDelayMs);
    retry.unref();
  };

  const resetRetirement = (): void => {
    const retirement = input.retirement;
    if (retirement === undefined) return;
    const reset: RetirementReset = {
      outcome: "pending",
      detail: "",
      settled: Promise.resolve(),
    };
    const failed = (error: unknown): void => {
      reset.outcome = "failed";
      reset.detail = message(error);
      log(`replay-transcript retirement reset failed: ${reset.detail}`);
      // The next pass retries it and names the failure.
      scheduleRetry();
    };
    try {
      reset.settled = retirement.reset().then(() => {
        reset.outcome = "held";
        // The next pass moves the marker and clears the reason.
        wake();
      }, failed);
    } catch (error) {
      failed(error);
    }
    retirementReset = reset;
  };

  /** One rewind or reset to `to` at `generation`, pushed or pulled. */
  const rewound = (generation: number, to: Point): void => {
    if (historyRewindTo === null || to.slot < historyRewindTo.slot)
      historyRewindTo = to;
    if (handledGeneration !== null && generation <= handledGeneration) return;
    handledGeneration = generation;
    // Synchronous: no runnable authority survives into the next await.
    input.bridge.invalidateForRollback();
    input.availability.invalidateForRollback();
    inclusion = null;
    rewinds += 1;
    recoveryPending = true;
    resetRetirement();
    input.onRewind?.(generation);
  };

  const unsubscribeGeneration = input.store.onGeneration(
    ({ generation, rewound: event }) => {
      rewound(generation, event.to);
      wake();
    },
  );
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

  /**
   * The pull: every rewind after the generation last handled durably. A
   * rewind found here that the push already delivered changes nothing but
   * the history target, which is the same or lower.
   */
  const catchUp = async (): Promise<
    Readonly<{
      handled: number | null;
      generation: number | null;
    }>
  > => {
    const handled = await readHandledFollowerGeneration(input.store);
    const applied = historyAppliedGeneration;
    const since = await input.store.rewindsSince(
      handled === null || (applied !== null && applied > handled)
        ? applied
        : handled,
    );
    if (since === null) return { handled, generation: null };
    if (since.target !== null) rewound(since.generation, since.target);
    else if (handledGeneration === null || since.generation > handledGeneration)
      handledGeneration = since.generation;
    return { handled, generation: since.generation };
  };

  /**
   * Moves the durable handled generation, once the history is back at every
   * target and the last retirement reset held. Waits for a pending reset at
   * most `retryDelayMs`. Returns why the marker waits, or null.
   */
  const recordHandled = async (
    pulled: Readonly<{ handled: number | null; generation: number | null }>,
  ): Promise<WatcherDecisionReadiness | null> => {
    if (pulled.generation === null || pulled.generation === pulled.handled)
      return null;
    const reset = retirementReset;
    if (reset !== null) {
      if (reset.outcome === "pending") {
        let timer: ReturnType<typeof setTimeout> | undefined;
        await Promise.race([
          reset.settled,
          new Promise<void>((resolve) => {
            timer = setTimeout(resolve, input.retryDelayMs);
            timer.unref();
          }),
        ]);
        clearTimeout(timer);
      }
      if (reset.outcome === "pending")
        // Its settling wakes the driver.
        return {
          reason: WATCHER_RETIREMENT_RESET_PENDING,
          detail: `the replay-transcript retirement reset has not settled within ${input.retryDelayMs.toString()} ms`,
        };
      if (reset.outcome === "failed") {
        // Retried here; a later pass records the generation once it held.
        if (retirementReset === reset) resetRetirement();
        return {
          reason: WATCHER_RETIREMENT_RESET_FAILED,
          detail: reset.detail,
        };
      }
      if (retirementReset === reset) retirementReset = null;
    }
    await writeHandledFollowerGeneration(input.store, pulled.generation);
    return null;
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
    const pulled = await catchUp();
    const reasons: WatcherDecisionReadiness[] = [];
    const history = input.history;
    let historyApplied = true;
    if (history !== undefined) {
      const status = history.read().status;
      if (status === "failed" || status === "closed") {
        historyApplied = false;
        reasons.push({
          reason: WATCHER_USER_EVENT_HISTORY_UNAVAILABLE,
          detail: `the local user-event history is ${status}`,
        });
      } else await rewindHistory(history);
    }
    // The history went back to every target this pass saw (or there is none).
    if (history === undefined || (historyApplied && historyRewindTo === null)) {
      historyAppliedGeneration = handledGeneration;
      const waiting = await recordHandled(pulled);
      if (waiting !== null) reasons.push(waiting);
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
        scheduleRetry();
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
