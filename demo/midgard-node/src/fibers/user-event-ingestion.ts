import type * as SDK from "@al-ft/midgard-sdk";
import {
  Cause,
  Duration,
  Effect,
  Exit,
  Metric,
  Option,
  Ref,
  Schedule,
} from "effect";

import { DatabaseError } from "../database/utils/common.js";
import {
  clearLivenessReason,
  setLivenessReason,
} from "../services/globals.liveness-reasons.js";
import { L1ControlPlaneTimeoutError } from "../services/globals.next-l1-provider-health-evidence.js";
import { Database, Globals, withL1ControlPlane } from "../services/index.js";

export type UserEventFetchBounds = Pick<
  SDK.UserEventFetchConfig,
  "inclusionTimeLowerBound" | "inclusionTimeUpperBound"
>;

export type UserEventReconcileResult = {
  readonly reconciledCount: number;
  readonly completedAt: Date;
};

const userEventVisibleSetSizeGauge = Metric.gauge(
  "user_event_visible_set_size",
  { description: "Size of the latest full visible user-event set reconciled" },
);

export const persistVisibleUserEventUTxOs = <
  Utxo,
  Entry,
  EntryError,
  EntryRequirements,
>({
  visibleUtxos,
  toEntry,
  insertEntries,
  emptyLogMessage,
  foundLogMessage,
  source,
}: {
  readonly visibleUtxos: readonly Utxo[];
  readonly toEntry: (
    utxo: Utxo,
  ) => Effect.Effect<Entry, EntryError, EntryRequirements>;
  readonly insertEntries: (
    entries: readonly Entry[],
  ) => Effect.Effect<void, DatabaseError, Database>;
  readonly emptyLogMessage: string;
  readonly foundLogMessage: (count: number) => string;
  /** Tags the visible-set size gauge; untagged reconciles record none. */
  readonly source?: string;
}): Effect.Effect<
  UserEventReconcileResult,
  EntryError | DatabaseError,
  EntryRequirements | Database
> =>
  Effect.gen(function* () {
    if (source !== undefined) {
      yield* Metric.tagged(
        userEventVisibleSetSizeGauge,
        "source",
        source,
      )(Effect.succeed(visibleUtxos.length));
    }
    if (visibleUtxos.length <= 0) {
      yield* Effect.logDebug(emptyLogMessage);
      return {
        reconciledCount: 0,
        completedAt: new Date(),
      } as const;
    }

    yield* Effect.logInfo(foundLogMessage(visibleUtxos.length));

    const entries = yield* Effect.forEach(visibleUtxos, toEntry);
    yield* insertEntries(entries);
    return {
      reconciledCount: entries.length,
      completedAt: new Date(),
    } as const;
  });

export const logReconciledVisibleUserEvents = ({
  reconciledCount,
  message,
}: {
  readonly reconciledCount: number;
  readonly message: (count: number) => string;
}): Effect.Effect<void> =>
  reconciledCount <= 0 ? Effect.void : Effect.logInfo(message(reconciledCount));

export const runCommitTimeUserEventIngestionBarrier = <Error, Requirements>({
  inclusionTimeUpperBound,
  inclusionTimeUpperBoundOffsetMs,
  startLogMessage,
  completedLogMessage,
  reconcile,
}: {
  readonly inclusionTimeUpperBound: Date;
  readonly inclusionTimeUpperBoundOffsetMs: number;
  readonly startLogMessage: (inclusionTimeUpperBound: Date) => string;
  readonly completedLogMessage: (input: {
    readonly reconciledCount: number;
    readonly completedAt: Date;
    readonly inclusionTimeUpperBound: Date;
  }) => string;
  readonly reconcile: (
    bounds: UserEventFetchBounds,
  ) => Effect.Effect<UserEventReconcileResult, Error, Requirements>;
}): Effect.Effect<Date, Error, Requirements> =>
  Effect.gen(function* () {
    yield* Effect.logInfo(startLogMessage(inclusionTimeUpperBound));
    const { reconciledCount, completedAt } = yield* reconcile({
      inclusionTimeUpperBound: BigInt(
        inclusionTimeUpperBound.getTime() + inclusionTimeUpperBoundOffsetMs,
      ),
    });
    yield* Effect.logInfo(
      completedLogMessage({
        reconciledCount,
        completedAt,
        inclusionTimeUpperBound,
      }),
    );
    return inclusionTimeUpperBound;
  });

/** The hold of a visible-set reconcile never drops below this. */
export const USER_EVENT_INGESTION_HOLD_FLOOR_MS = 30_000;
/** Nor rises above this, however slow the provider has been. */
export const USER_EVENT_INGESTION_HOLD_CEILING_MS = 600_000;
/** A reconcile's hold covers this many times its last successful duration. */
export const USER_EVENT_INGESTION_HOLD_DURATION_FACTOR = 3;
/** Consecutive failed reconciles that raise a liveness reason. */
export const USER_EVENT_INGESTION_STALLED_STREAK = 3;

export type UserEventIngestionHoldState = {
  readonly lastSuccessDurationMs: number;
  /** Failed reconciles since the last success, of any cause. */
  readonly consecutiveFailures: number;
  /** Those of them that ran out of hold: the only evidence the set needs
   * more time. A fast provider error says nothing about that. */
  readonly consecutiveHoldTimeouts: number;
};

/**
 * The hold for the next full visible-set reconcile: generous against the
 * observed fetch duration, doubled per consecutive hold timeout, and clamped
 * to the floor and ceiling. The full set is always reconciled; only the time
 * it may take adapts.
 */
export const userEventIngestionHoldMs = (
  state: UserEventIngestionHoldState,
  bounds: { readonly floorMs: number; readonly ceilingMs: number },
): number =>
  Math.min(
    bounds.ceilingMs,
    Math.max(
      bounds.floorMs,
      USER_EVENT_INGESTION_HOLD_DURATION_FACTOR * state.lastSuccessDurationMs,
      bounds.floorMs * 2 ** Math.min(state.consecutiveHoldTimeouts, 30),
    ),
  );

const userEventIngestionDurationTimer = Metric.timer(
  "user_event_ingestion_duration_ms",
  "Duration of one full visible user-event set reconcile",
);
const userEventIngestionFailuresGauge = Metric.gauge(
  "user_event_ingestion_consecutive_failures",
  { description: "Consecutive failed visible user-event set reconciles" },
);
const userEventIngestionHoldGauge = Metric.gauge(
  "user_event_ingestion_hold_budget_ms",
  { description: "L1 control-plane hold of the next visible-set reconcile" },
);

export const repeatVisibleUserEventIngestionFiber = <
  ActionError,
  ActionRequirements,
>({
  schedule,
  startLogMessage,
  spanName,
  action,
  holdFloorMs = USER_EVENT_INGESTION_HOLD_FLOOR_MS,
  holdCeilingMs = USER_EVENT_INGESTION_HOLD_CEILING_MS,
}: {
  readonly schedule: Schedule.Schedule<number>;
  readonly startLogMessage: string;
  readonly spanName: string;
  readonly action: Effect.Effect<void, ActionError, ActionRequirements>;
  readonly holdFloorMs?: number;
  readonly holdCeilingMs?: number;
}): Effect.Effect<void, never, ActionRequirements | Globals> =>
  Effect.gen(function* () {
    const globals = yield* Globals;
    yield* Effect.logInfo(startLogMessage);
    const state = yield* Ref.make<UserEventIngestionHoldState>({
      lastSuccessDurationMs: 0,
      consecutiveFailures: 0,
      consecutiveHoldTimeouts: 0,
    });
    const livenessSource = `user_event_ingestion:${spanName}`;
    const durationTimer = Metric.tagged(
      userEventIngestionDurationTimer,
      "scope",
      spanName,
    );
    const failuresGauge = Metric.tagged(
      userEventIngestionFailuresGauge,
      "scope",
      spanName,
    );
    const holdGauge = Metric.tagged(
      userEventIngestionHoldGauge,
      "scope",
      spanName,
    );
    const repeatableAction = Effect.gen(function* () {
      const maxHoldMs = userEventIngestionHoldMs(yield* Ref.get(state), {
        floorMs: holdFloorMs,
        ceilingMs: holdCeilingMs,
      });
      yield* holdGauge(Effect.succeed(maxHoldMs));
      const outcome = yield* withL1ControlPlane(
        globals,
        { scope: spanName, maxHoldMs },
        Effect.gen(function* () {
          const startedAtMs = Date.now();
          yield* action;
          return Date.now() - startedAtMs;
        }),
      ).pipe(Effect.withSpan(spanName), Effect.exit);
      if (Exit.isSuccess(outcome)) {
        yield* durationTimer(Effect.succeed(Duration.millis(outcome.value)));
        yield* Ref.set(state, {
          lastSuccessDurationMs: outcome.value,
          consecutiveFailures: 0,
          consecutiveHoldTimeouts: 0,
        });
        yield* failuresGauge(Effect.succeed(0));
        yield* clearLivenessReason(globals, livenessSource);
        return;
      }
      const heldTooLong = Option.exists(
        Cause.failureOption(outcome.cause),
        (error) => error instanceof L1ControlPlaneTimeoutError,
      );
      const failures = (yield* Ref.updateAndGet(state, (current) => ({
        ...current,
        consecutiveFailures: current.consecutiveFailures + 1,
        consecutiveHoldTimeouts:
          current.consecutiveHoldTimeouts + (heldTooLong ? 1 : 0),
      }))).consecutiveFailures;
      yield* failuresGauge(Effect.succeed(failures));
      yield* Effect.logWarning(outcome.cause);
      if (failures >= USER_EVENT_INGESTION_STALLED_STREAK) {
        yield* setLivenessReason(
          globals,
          livenessSource,
          `user_event_ingestion_stalled:${spanName}:${failures.toString()}`,
        );
      }
    });
    yield* Effect.repeat(repeatableAction, schedule);
  });
