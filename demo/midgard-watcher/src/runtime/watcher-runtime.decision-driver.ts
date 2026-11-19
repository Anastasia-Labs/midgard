import type { FactStore } from "@al-ft/midgard-l1-follower";

import type { WatcherAvailabilityRuntime } from "../availability/runtime.js";
import {
  type WatcherFaultDecisionBridge,
  WatcherFaultDecisionRetired,
} from "../fault-proofs/fault-decision-bridge.selected-target.js";
import type { WatcherAuthenticatedStateQueueObservation } from "../indexers/authenticated-state-queue-observation.js";
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
  advanceWatcherReplayCheckpoint,
  type WatcherReplayCheckpointDriver,
} from "./watcher-runtime.advance-replayed-ledger.js";
import { decisionFailure } from "./watcher-runtime.decision-failure.js";
import {
  WATCHER_DECISION_PASS_FAILED,
  WATCHER_FOLLOWER_NOT_STARTED,
  WATCHER_RELEASE_OBSERVATION_PENDING,
  type WatcherDecisionReadiness,
} from "./watcher-runtime.decision-readiness.js";

/**
 * The watcher's decision driver (ticket W1, with W3's rollback handling):
 * every fault decision and availability action is a pure function of the
 * follower's facts at the tip and at the release depth.
 *
 * One pass reads both observations in their own read transactions and hands
 * them to the availability runtime and the decision bridge, in that order.
 * Passes are single-flight and coalesced: every follower change wakes the
 * driver, and a change during a pass runs one more pass after it.
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
 * rewind count and `onRewind` run once for each new generation, so a pull of
 * a generation this process already handled changes nothing. The pass then
 * moves the durable marker to the pulled generation.
 *
 * Liveness: a pass never throws out of the driver. A failure is a named
 * readiness reason with its detail. Only a transient failure
 * (`isDecisionFailureTransient`: the L1 follower or its node transport did
 * not answer, or the store refused a connection or was busy) is retried
 * after `retryDelayMs`; any other failure is not retried on a timer, and the
 * pass runs again only when the follower moves or rewinds. Those waits are
 * on the L1 source and the store a running watcher rides out, so they have
 * no deadline.
 */
export {
  WATCHER_DECISION_PASS_FAILED,
  WATCHER_FOLLOWER_NOT_STARTED,
  WATCHER_RELEASE_OBSERVATION_PENDING,
  type WatcherDecisionReadiness,
  watcherFollowerStarted,
} from "./watcher-runtime.decision-readiness.js";

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
    | "readiness"
    | "status"
  >;
  availability: Pick<
    WatcherAvailabilityRuntime,
    "reconcile" | "invalidateForRollback"
  >;
  /** A durable ledger head beyond the follower's rollback limit. */
  replayedLedger?: WatcherReplayCheckpointDriver;
  /** The tip the driver last decided at, for L1 freshness reporting. */
  onDecided?: (tip: WatcherAuthenticatedStateQueueObservation) => void;
  /** Once per follower generation a rewind or reset raised, pushed or pulled. */
  onRewind?: (generation: number) => void;
  /** The delay before a failed pass is retried. */
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
  /** Why decisions are held, then the bridge's held headers; empty when
   * the last pass completed and no header's classification is held. */
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

/** A failure that only says a rewind or a newer pass retired the work. */
const retired = (error: unknown): boolean =>
  error instanceof WatcherFaultDecisionRetired;

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
  // dispatch.
  let recoveryPending = true;
  let recoveredCount: number | null = null;
  // The highest follower generation this process handled; null before the
  // first push or pull.
  let handledGeneration: number | null = null;

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

  /** One rewind or reset at `generation`, pushed or pulled. */
  const rewound = (generation: number): void => {
    if (handledGeneration !== null && generation <= handledGeneration) return;
    handledGeneration = generation;
    // Synchronous: no runnable authority survives into the next await.
    input.bridge.invalidateForRollback();
    input.availability.invalidateForRollback();
    inclusion = null;
    rewinds += 1;
    recoveryPending = true;
    input.onRewind?.(generation);
  };

  const unsubscribeGeneration = input.store.onGeneration(({ generation }) => {
    rewound(generation);
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

  /**
   * The pull: every rewind after the generation last handled durably. A
   * rewind found here that the push already delivered changes nothing.
   */
  const catchUp = async (): Promise<
    Readonly<{
      handled: number | null;
      generation: number | null;
    }>
  > => {
    const handled = await readHandledFollowerGeneration(input.store);
    const since = await input.store.rewindsSince(handled);
    if (since === null) return { handled, generation: null };
    if (since.target !== null) rewound(since.generation);
    else if (handledGeneration === null || since.generation > handledGeneration)
      handledGeneration = since.generation;
    return { handled, generation: since.generation };
  };

  /** Moves the durable handled generation to the one a pull found. */
  const recordHandled = async (
    pulled: Readonly<{ handled: number | null; generation: number | null }>,
  ): Promise<void> => {
    if (pulled.generation === null || pulled.generation === pulled.handled)
      return;
    await writeHandledFollowerGeneration(input.store, pulled.generation);
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
    await recordHandled(pulled);
    const tip = await observe(1);
    if (tip.kind === "unready") {
      held = [{ reason: tip.reason, detail: tip.detail }, ...reasons];
      return;
    }
    inclusion = tip.observation;
    const release = await observe(input.releaseDepth);
    if (release.kind === "ok")
      await input.availability.reconcile(release.observation, true);
    else
      reasons.push({
        reason: WATCHER_RELEASE_OBSERVATION_PENDING,
        detail: `${release.reason}: ${release.detail}`,
      });
    reasons.push(
      ...(await advanceWatcherReplayCheckpoint(
        input,
        release,
        observe,
        scheduleRetry,
      )),
    );
    if (recoveryPending) {
      recoveryPending = false;
      try {
        await input.bridge.prepareForRecovery(tip.observation);
      } catch (error) {
        // A later header's classification failure leaves the decided prefix
        // of this tip prepared: recovery is done, and the reconciliation
        // below dispatches that prefix and reports the failure. Any other
        // failure prepared nothing, and recovery stays owed.
        if (
          input.bridge.status().observationDigest !==
          tip.observation.observationDigest
        ) {
          recoveryPending = true;
          throw error;
        }
      }
    }
    await input.bridge.reconcileAndDispatch(tip.observation);
    const recoveredNow = await input.bridge.recoverExisting();
    current = tip.observation;
    if (recoveredCount === null) {
      recoveredCount = recoveredNow;
      resolveRecovered(recoveredNow);
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
        const { transient, detail } = decisionFailure(error);
        lastError = detail;
        held = [{ reason: WATCHER_DECISION_PASS_FAILED, detail: lastError }];
        log(`decision pass failed: ${lastError}`);
        if (transient) scheduleRetry();
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
    readiness: () => Object.freeze([...held, ...input.bridge.readiness()]),
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
