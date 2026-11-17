import { performance } from "node:perf_hooks";

import {
  Data,
  Duration,
  Effect,
  Exit,
  Metric,
  Option,
  Ref,
  Schedule,
} from "effect";

import { FollowerWriteFixture } from "../services/follower-write-gate.js";
import {
  Database,
  Globals,
  L1ControlPlaneTimeoutError,
  Lucid,
  MidgardContracts,
  NodeConfig,
  withL1ControlPlane,
} from "../services/index.js";
import { type IntentJournal, openPlan } from "../services/intent-journal.js";
import {
  awaitPostMergeSnapshot,
  findLandedBlock,
  LANDED_QUEUE_VISIBILITY_BUDGET_MS,
  landedStateQueueSnapshot,
  refreshStateQueueGlobalsFromSnapshot,
  requireLandedStateQueue,
  type StateQueueSnapshot,
} from "../services/landed-state-queue.js";
import {
  mergeBlockCounter,
  mergeDurationTimer,
  mergeLandingPendingSkipCounter,
} from "../transactions/state-queue/merge-to-confirmed-state.merge-options.js";
import {
  recordMergeTickIdleness,
  skipIdleMergeTick,
} from "./merge.idle-backoff.js";
import { mergeActionWithL1ControlPlaneHeld } from "./merge.merge-action-with-l1-control-plane-held.js";
import {
  type MergeActionResult,
  SCHEDULED_MERGE_CONTROL_PLANE_WAIT_MS,
  type SubmittedMerge,
  withScheduledMergeControlPlaneWait,
} from "./merge.registered-merge-due-work-skip.js";
import { emitQueueStateMetrics } from "./queue-metrics.js";

/**
 * The merge refused because no intent journal can record it: the process
 * runs no L1 follower (`IntentJournalWithoutFollower`), or the follower has
 * no view yet. None of its work ran: no L1 read, no transaction.
 */
export class MergeIntentJournalUnavailable extends Data.TaggedError(
  "MergeIntentJournalUnavailable",
)<{
  readonly message: string;
  readonly reason: string;
  readonly detail: string;
}> {}

/**
 * Refuses a merge no intent journal can record, so a merge never goes out
 * unjournaled. Only the explicit model-fixture capability runs one without
 * a journal view.
 */
const requireMergeIntentJournal = Effect.gen(function* () {
  const plan = yield* openPlan;
  if (plan.kind !== "none") return;
  if (Option.isSome(yield* Effect.serviceOption(FollowerWriteFixture))) return;
  return yield* Effect.fail(
    new MergeIntentJournalUnavailable({
      message: "The merge needs an intent journal with a follower view",
      reason: plan.reason,
      detail: plan.detail,
    }),
  );
});

/**
 * The single entry point for a merge attempt; the scheduled merge fiber runs
 * it. It waits up to `SCHEDULED_MERGE_CONTROL_PLANE_WAIT_MS` for the L1
 * control plane and skips if the plane stays busy.
 *
 * It runs under the process-wide L1 control plane, so merges in one process
 * are serialized, and the state-queue mutation lease taken inside
 * additionally serializes merges against every other process sharing the
 * database. The hold covers the merge transaction up to its L1 confirmation;
 * the wait for the follower's landed queue to show the merge follows it.
 * The merge writes no local ledger: landed-block processing folds the block
 * into `confirmed_ledger` once the landed queue shows the merge.
 *
 * The process keeps a record of the latest merge it submitted that reached
 * the landing wait (`MERGE_AWAITING_LANDING`). A scheduled attempt skips
 * while the landed queue still shows that merge's header, and clears the
 * record once it no longer does, so a landing wait that timed out does not
 * start a second merge of the same header. The record is stamped, on the
 * monotonic clock, only when its landing wait fails, and an unstamped record
 * never expires, so a wait still running is never cut short. A stamped
 * record expires one landing-wait budget after its wait failed: a merge
 * whose header the queue still shows by then was dropped or rolled back,
 * and the next scheduled attempt clears the record and runs. Neither the
 * lease nor the L1 control plane is held through that wait. The skip is not
 * what keeps the chain safe: a second merge of the same header spends the
 * queue inputs the first spent, so it fails at L1 either way.
 */
export const mergeAction = () =>
  Effect.gen(function* () {
    const globals = yield* Globals;
    yield* Ref.set(globals.HEARTBEAT_MERGE, Date.now());
    yield* requireMergeIntentJournal;
    const landingPending = yield* mergeLandingPendingSkip(globals);
    if (landingPending !== undefined) return landingPending;
    const submitted = yield* Ref.make(Option.none<SubmittedMerge>());
    const onSubmitted = (merge: SubmittedMerge) =>
      Ref.set(submitted, Option.some(merge));
    const held = yield* scheduledMergeAttempt(globals, onSubmitted).pipe(
      Effect.catchIf(
        (error): error is L1ControlPlaneTimeoutError =>
          error instanceof L1ControlPlaneTimeoutError,
        (timeout) => submittedOverHoldTimeout(submitted, timeout),
      ),
    );
    if (held.status !== "submitted") return held;
    const { txHash } = held;
    yield* Ref.set(globals.MERGE_AWAITING_LANDING, {
      txHash,
      headerHash: held.headerHash,
      waitFailedAtMonotonicMs: null,
    });
    return yield* awaitMergeLanded(held).pipe(
      Effect.onExit((exit) =>
        Exit.isSuccess(exit)
          ? clearMergeAwaitingLanding(globals, txHash)
          : stampMergeLandingWaitFailed(globals, txHash),
      ),
    );
  });

/** Starts the awaited merge's expiry: its landing wait failed or stopped. */
const stampMergeLandingWaitFailed = (globals: Globals, txHash: string) =>
  Ref.update(globals.MERGE_AWAITING_LANDING, (current) =>
    current?.txHash === txHash && current.waitFailedAtMonotonicMs === null
      ? { ...current, waitFailedAtMonotonicMs: performance.now() }
      : current,
  );

/** Clears the awaited merge, unless a newer one replaced it. */
const clearMergeAwaitingLanding = (globals: Globals, txHash: string) =>
  Ref.update(globals.MERGE_AWAITING_LANDING, (current) =>
    current?.txHash === txHash ? null : current,
  );

/**
 * The skip of a scheduled merge while the landed queue still shows the
 * header of this process's latest merge awaiting landing; undefined, after
 * clearing that record, once the queue no longer shows it (stamped or not)
 * or one landing-wait budget has passed since its landing wait failed.
 */
const mergeLandingPendingSkip = (globals: Globals) =>
  Effect.gen(function* () {
    const awaiting = yield* Ref.get(globals.MERGE_AWAITING_LANDING);
    if (awaiting === null) return undefined;
    const contracts = yield* MidgardContracts;
    const queue = yield* requireLandedStateQueue(
      contracts.stateQueue,
      "merge_landing_pending",
    );
    if (findLandedBlock(queue, awaiting.headerHash) === undefined) {
      yield* clearMergeAwaitingLanding(globals, awaiting.txHash);
      return undefined;
    }
    const failedAt = awaiting.waitFailedAtMonotonicMs;
    const sinceFailedMs = failedAt === null ? 0 : performance.now() - failedAt;
    if (
      failedAt !== null &&
      sinceFailedMs >= LANDED_QUEUE_VISIBILITY_BUDGET_MS
    ) {
      yield* Effect.logWarning(
        `🔸 Running scheduled merge: the previous merge did not land within its budget, so it was dropped or rolled back (tx=${awaiting.txHash},headerHash=${awaiting.headerHash},sinceWaitFailedMs=${Math.round(sinceFailedMs).toString()}).`,
      );
      yield* clearMergeAwaitingLanding(globals, awaiting.txHash);
      return undefined;
    }
    const reason = `awaiting_landing_tx=${awaiting.txHash}`;
    yield* Metric.increment(mergeLandingPendingSkipCounter);
    yield* Effect.logInfo(
      `🔸 Skipping scheduled merge while this node's previous merge awaits landing (${reason},headerHash=${awaiting.headerHash}).`,
    );
    return {
      status: "skipped_merge_landing_pending",
      reason,
      headerHash: awaiting.headerHash,
    } satisfies MergeActionResult;
  });

/**
 * The L1 control plane's hold timeout interrupts a merge attempt that runs
 * too long. A merge transaction already submitted then still lands, so the
 * attempt awaits its landing like any other; only an attempt interrupted
 * before its submission reports the timeout.
 */
const submittedOverHoldTimeout = (
  submitted: Ref.Ref<Option.Option<SubmittedMerge>>,
  timeout: L1ControlPlaneTimeoutError,
) =>
  Effect.gen(function* () {
    const merge = yield* Ref.get(submitted);
    if (Option.isNone(merge)) return yield* Effect.fail(timeout);
    yield* Effect.logWarning(
      `🔸 Merge submitted before the L1 control-plane hold timed out; awaiting its landing (header=${merge.value.headerHash},tx=${merge.value.txHash},timeout=${timeout.message}).`,
    );
    return merge.value;
  });

/**
 * Ends a submitted merge's attempt once the follower's landed queue shows
 * the merge, outside the L1 control-plane hold, and records the merge's
 * metrics. The cached queue state is then refreshed under a brief L1
 * control-plane hold, as block commitment refreshes it, from a snapshot read
 * inside that hold, so it never overwrites a commitment's fresher values.
 */
const awaitMergeLanded = ({
  headerHash,
  txHash,
  startedAtMs,
  trigger,
}: SubmittedMerge) =>
  Effect.gen(function* () {
    const contracts = yield* MidgardContracts;
    const globals = yield* Globals;
    const landed = yield* awaitPostMergeSnapshot(
      contracts.stateQueue,
      headerHash,
    );
    yield* Metric.increment(mergeBlockCounter).pipe(
      Effect.withSpan("increment-merge-block-counter"),
    );
    yield* mergeDurationTimer(
      Effect.succeed(Duration.millis(Date.now() - startedAtMs)),
    );
    const snapshot = yield* refreshQueueGlobalsAfterMerge(globals, landed);
    return {
      status: "merged",
      postMergeSnapshot: snapshot,
      headerHash,
      txHash,
      trigger,
    } satisfies MergeActionResult;
  });

/**
 * Re-reads the landed queue under a brief L1 control-plane hold and caches
 * it. The snapshot is read inside the hold, so no commitment's newer cached
 * values are overwritten; if the hold or the read fails, the cache is left
 * as it is (the next commitment refreshes it) and `landed` is returned.
 */
const refreshQueueGlobalsAfterMerge = (
  globals: Globals,
  landed: StateQueueSnapshot,
) =>
  withL1ControlPlane(
    globals,
    { scope: "state_queue_merge" },
    Effect.gen(function* () {
      const contracts = yield* MidgardContracts;
      const snapshot = yield* landedStateQueueSnapshot(
        contracts.stateQueue,
        "post_merge",
      );
      yield* refreshStateQueueGlobalsFromSnapshot(globals, snapshot);
      return snapshot;
    }),
  ).pipe(
    Effect.tap((snapshot) =>
      Effect.zipRight(
        emitQueueStateMetrics,
        Effect.logInfo(
          `🔸 Refreshed live state-queue tail after merge: tail=${snapshot.tailCommitBase.outRef},snapshot=${snapshot.snapshotId}`,
        ),
      ),
    ),
    Effect.catchAll((error) =>
      Effect.as(
        Effect.logWarning(
          `🔸 Merge landed; left the cached state-queue tail for the next commitment to refresh (${String(error)}).`,
        ),
        landed,
      ),
    ),
  );

const scheduledMergeAttempt = (
  globals: Globals,
  onSubmitted: (merge: SubmittedMerge) => Effect.Effect<void>,
) =>
  Effect.gen(function* () {
    const attempt = yield* withScheduledMergeControlPlaneWait({
      globals,
      effect: mergeActionWithL1ControlPlaneHeld(onSubmitted),
    });
    if (Option.isSome(attempt)) {
      return attempt.value;
    }
    const reason = `l1_control_plane_wait_exceeded_ms=${SCHEDULED_MERGE_CONTROL_PLANE_WAIT_MS.toString()}`;
    yield* Effect.logInfo(
      `🔸 Skipping scheduled merge because the L1 control-plane remained busy (${reason}).`,
    );
    return {
      status: "skipped_l1_control_plane_busy",
      reason,
    } satisfies MergeActionResult;
  });

/**
 * Fiber wrapper that repeats merge attempts on the provided schedule.
 */
export const mergeFiber = (
  schedule: Schedule.Schedule<number>,
): Effect.Effect<
  void,
  never,
  Lucid | MidgardContracts | Database | Globals | NodeConfig | IntentJournal
> =>
  Effect.gen(function* () {
    yield* Effect.logInfo("🟠 Merge fiber started.");
    const globals = yield* Globals;
    const nodeConfig = yield* NodeConfig;
    const action = Effect.gen(function* () {
      if (yield* skipIdleMergeTick(globals)) return;
      const result = yield* mergeAction().pipe(
        Effect.tapErrorCause(() =>
          recordMergeTickIdleness(globals, undefined, 0),
        ),
      );
      yield* recordMergeTickIdleness(
        globals,
        result,
        nodeConfig.WAIT_BETWEEN_MERGE_TXS,
      );
    }).pipe(
      Effect.withSpan("merge-confirmed-state-fiber"),
      Effect.catchAllCause(Effect.logWarning),
    );
    yield* Effect.repeat(action, schedule);
  });
