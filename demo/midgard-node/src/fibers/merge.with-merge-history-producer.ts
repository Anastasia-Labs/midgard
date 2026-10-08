import { Data, Effect, Either, Exit, Option, Ref, Schedule } from "effect";

import { DatabaseError } from "../database/utils/common.js";
import {
  runHistoryProducer,
  UnownedHistoryFixture,
} from "../services/event-history-producer.js";
import {
  Database,
  Globals,
  L1ControlPlaneTimeoutError,
  Lucid,
  MidgardContracts,
  NodeConfig,
  withL1ControlPlane,
} from "../services/index.js";
import type { IntentJournal } from "../services/intent-journal.js";
import {
  awaitPostMergeSnapshot,
  refreshStateQueueGlobalsFromSnapshot,
} from "../services/landed-state-queue.js";
import {
  recordMergeTickIdleness,
  skipIdleMergeTick,
} from "./merge.idle-backoff.js";
import { mergeActionWithL1ControlPlaneHeld } from "./merge.merge-action-with-l1-control-plane-held.js";
import {
  type ConfirmedMerge,
  MERGE_L1_CONTROL_PLANE_MAX_HOLD_MS,
  type MergeActionResult,
  SCHEDULED_MERGE_CONTROL_PLANE_WAIT_MS,
  withScheduledMergeControlPlaneWait,
} from "./merge.registered-merge-due-work-skip.js";

/**
 * The merge could not take the history producer permit, so none of its work
 * ran: no L1 read, no transaction, no local write. A standalone process (no
 * history owner) and a node whose history owner is not Ready both land here.
 */
export class MergeProducerPermitUnavailable extends Data.TaggedError(
  "MergeProducerPermitUnavailable",
)<{
  readonly message: string;
  readonly cause: unknown;
}> {}

/**
 * Runs `work` under the history producer permit. A merge finalizes locally by
 * writing history rows (confirmed ledger, deposit/withdrawal/forced statuses,
 * receipt settlements) and every such write requires a registered producer.
 *
 * With a history owner in `Globals` the work always registers with it, even
 * when the caller already holds a permit, since producer registration nests.
 * Without an owner, only the explicit model-fixture capability runs the work
 * unregistered (its writes still pass `withHistoryWrite`'s fixture gate).
 *
 * A registration that refuses before the work starts fails with
 * `MergeProducerPermitUnavailable`. Once the work ran, its own failure wins,
 * with its type, even over a supersession found by the owner's trailing
 * currency check; a successful work whose trailing check fails reports that
 * check's failure.
 */
const withMergeHistoryProducer = <A, E, R>(
  work: Effect.Effect<A, E, R>,
): Effect.Effect<
  A,
  E | DatabaseError | MergeProducerPermitUnavailable,
  R | Globals | Database
> =>
  Effect.gen(function* () {
    const globals = yield* Globals;
    const owner = yield* Ref.get(globals.EVENT_HISTORY_OWNER);
    if (owner === undefined) {
      const fixture = yield* Effect.serviceOption(UnownedHistoryFixture);
      if (Option.isSome(fixture)) return yield* work;
    }
    const ran = yield* Ref.make<Option.Option<Either.Either<A, E>>>(
      Option.none(),
    );
    const registration = yield* runHistoryProducer(
      Effect.either(work).pipe(
        Effect.tap((outcome) => Ref.set(ran, Option.some(outcome))),
      ),
    ).pipe(Effect.either);
    const outcome = yield* Ref.get(ran);
    if (Option.isNone(outcome)) {
      return yield* Effect.fail(
        new MergeProducerPermitUnavailable({
          message:
            "The merge needs the history producer permit, which this process could not take",
          cause: Either.isLeft(registration)
            ? registration.left
            : "registration returned without running the merge",
        }),
      );
    }
    if (Either.isLeft(outcome.value))
      return yield* Effect.fail(outcome.value.left);
    if (Either.isLeft(registration))
      return yield* Effect.fail(registration.left);
    return outcome.value.right;
  });

export type MergeActionOptions = {
  /**
   * Merge only if the oldest queued block is this header; otherwise skip with
   * `skipped_merge_candidate_changed` without building a transaction.
   */
  readonly expectedHeaderHash?: string;
};

/**
 * The single entry point for every merge trigger: the scheduled fiber, the
 * admin `GET /merge` route and `reconcile merge-complete --repair`.
 *
 * It holds the history producer permit for the whole attempt, so no caller can
 * reach the local finalization writes without it, and it runs under the
 * process-wide L1 control plane, so scheduled and manual merges in one process
 * are serialized. The state-queue mutation lease taken inside additionally
 * serializes merges against every other process sharing the database.
 */
export const mergeAction = (
  force: boolean = false,
  { expectedHeaderHash }: MergeActionOptions = {},
) =>
  withMergeHistoryProducer(
    Effect.gen(function* () {
      const globals = yield* Globals;
      yield* Ref.set(globals.HEARTBEAT_MERGE, Date.now());
      const confirmed = yield* Ref.make(Option.none<ConfirmedMerge>());
      const attempt = force
        ? withL1ControlPlane(
            globals,
            {
              scope: "state_queue_merge",
              maxHoldMs: MERGE_L1_CONTROL_PLANE_MAX_HOLD_MS,
            },
            mergeActionWithL1ControlPlaneHeld(
              true,
              expectedHeaderHash,
              confirmed,
            ),
          )
        : scheduledMergeAttempt(globals, expectedHeaderHash, confirmed);
      return yield* attempt.pipe(
        Effect.catchIf(
          (error): error is L1ControlPlaneTimeoutError =>
            error instanceof L1ControlPlaneTimeoutError,
          (timeout) => reportConfirmedMergeOverHoldTimeout(confirmed, timeout),
        ),
      );
    }),
  );

/**
 * The L1 control plane's hold timeout interrupts a merge attempt that runs
 * too long, but a merge already confirmed on L1 finishes its local
 * finalization regardless (it is uninterruptible), so the timeout must not
 * relabel it. A finalization that completed reports the merge, with a fresh
 * read of the state queue; one that failed reports its own failure. Only an
 * attempt interrupted before its merge was confirmed reports the timeout.
 */
const reportConfirmedMergeOverHoldTimeout = (
  confirmed: Ref.Ref<Option.Option<ConfirmedMerge>>,
  timeout: L1ControlPlaneTimeoutError,
) =>
  Effect.gen(function* () {
    const settled = yield* Ref.get(confirmed);
    if (Option.isNone(settled)) return yield* Effect.fail(timeout);
    const { exit, headerHash, txHash, trigger } = settled.value;
    if (Exit.isFailure(exit)) {
      yield* Effect.logError(
        `🔸 Merge confirmed on L1 but its local finalization failed while the L1 control-plane hold timed out; reporting the finalization failure (header=${headerHash},tx=${txHash},timeout=${timeout.message}).`,
      );
      return yield* Effect.failCause(exit.cause);
    }
    yield* Effect.logWarning(
      `🔸 Merge completed its local finalization past the L1 control-plane hold timeout; reporting the merge (header=${headerHash},tx=${txHash},timeout=${timeout.message}).`,
    );
    const contracts = yield* MidgardContracts;
    const globals = yield* Globals;
    const snapshot = yield* awaitPostMergeSnapshot(
      contracts.stateQueue,
      headerHash,
    );
    yield* refreshStateQueueGlobalsFromSnapshot(globals, snapshot);
    return {
      status: "merged",
      postMergeSnapshot: snapshot,
      headerHash,
      txHash,
      trigger,
    } satisfies MergeActionResult;
  });

const scheduledMergeAttempt = (
  globals: Globals,
  expectedHeaderHash: string | undefined,
  confirmed: Ref.Ref<Option.Option<ConfirmedMerge>>,
) =>
  Effect.gen(function* () {
    const attempt = yield* withScheduledMergeControlPlaneWait({
      globals,
      effect: mergeActionWithL1ControlPlaneHeld(
        false,
        expectedHeaderHash,
        confirmed,
      ),
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
