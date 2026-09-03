import { Cause, Duration, Effect, Metric, Ref, Schedule } from "effect";

import * as TxAdmissionsDB from "../database/txAdmissions.js";
import { DatabaseError } from "../database/utils/common.js";
import { Database } from "../services/database.js";
import { AdmissionBacklogGaugeState, Globals } from "../services/globals.js";

export const beginAdmissionBacklogRefresh = (
  current: AdmissionBacklogGaugeState,
): AdmissionBacklogGaugeState => ({
  ADMISSION_BACKLOG_BASE:
    current.ADMISSION_BACKLOG_BASE + current.ADMISSION_BACKLOG_LOCAL_DELTA,
  ADMISSION_BACKLOG_LOCAL_DELTA: 0n,
  ADMISSION_BACKLOG_IN_FLIGHT: current.ADMISSION_BACKLOG_IN_FLIGHT,
  ADMISSION_BACKLOG_REFRESHED_AT: current.ADMISSION_BACKLOG_REFRESHED_AT,
});

export const completeAdmissionBacklogRefresh = (
  current: AdmissionBacklogGaugeState,
  liveCount: bigint,
  refreshedAtMs: number,
): AdmissionBacklogGaugeState => ({
  ADMISSION_BACKLOG_BASE: liveCount,
  ADMISSION_BACKLOG_LOCAL_DELTA: current.ADMISSION_BACKLOG_LOCAL_DELTA,
  ADMISSION_BACKLOG_IN_FLIGHT: current.ADMISSION_BACKLOG_IN_FLIGHT,
  ADMISSION_BACKLOG_REFRESHED_AT: refreshedAtMs,
});

export type AdmissionBacklogReservation = {
  readonly reserved: boolean;
  /** Value passed to TxAdmissionsDB.admit to preserve its duplicate-first gate. */
  readonly currentBacklog: bigint;
};

const stateValue = (state: AdmissionBacklogGaugeState): bigint =>
  state.ADMISSION_BACKLOG_BASE +
  state.ADMISSION_BACKLOG_LOCAL_DELTA +
  state.ADMISSION_BACKLOG_IN_FLIGHT;

const admissionBacklogGaugeValue = Metric.gauge(
  "admission_backlog_gauge_value",
  {
    description: "Hybrid durable-admission backlog gauge value",
    bigint: true,
  },
);

const admissionBacklogGaugeStalenessMs = Metric.gauge(
  "admission_backlog_gauge_staleness_ms",
  {
    description: "Milliseconds since the last live admission backlog count",
  },
);

const admissionBacklogRefreshDuration = Metric.timer(
  "admission_backlog_refresh_duration",
  "Duration of live durable-admission backlog refreshes",
);

const reportGaugeMetrics = (
  value: bigint,
  refreshedAtMs: number,
): Effect.Effect<void> =>
  Effect.gen(function* () {
    yield* admissionBacklogGaugeValue(Effect.succeed(value));
    yield* admissionBacklogGaugeStalenessMs(
      Effect.succeed(
        refreshedAtMs === 0 ? 0 : Math.max(0, Date.now() - refreshedAtMs),
      ),
    );
  });

export const readAdmissionBacklogGauge: Effect.Effect<bigint, never, Globals> =
  Effect.gen(function* () {
    const globals = yield* Globals;
    const state = yield* Ref.get(globals.ADMISSION_BACKLOG_GAUGE);
    const value = stateValue(state);
    yield* reportGaugeMetrics(value, state.ADMISSION_BACKLOG_REFRESHED_AT);
    return value;
  });

export const noteLocalAdmit: Effect.Effect<void, never, Globals> = Effect.gen(
  function* () {
    const globals = yield* Globals;
    const state = yield* Ref.updateAndGet(
      globals.ADMISSION_BACKLOG_GAUGE,
      (current) => ({
        ...current,
        ADMISSION_BACKLOG_LOCAL_DELTA:
          current.ADMISSION_BACKLOG_LOCAL_DELTA + 1n,
      }),
    );
    yield* reportGaugeMetrics(
      stateValue(state),
      state.ADMISSION_BACKLOG_REFRESHED_AT,
    );
  },
);

/**
 * Atomically checks the process-local cap and reserves one slot. Distinct
 * concurrent submit fibers therefore cannot all observe the same stale value.
 */
export const reserveAdmissionBacklogSlot = (
  maxBacklog: number,
): Effect.Effect<AdmissionBacklogReservation, never, Globals> =>
  Effect.gen(function* () {
    const globals = yield* Globals;
    const max = BigInt(Math.max(0, maxBacklog));
    return yield* Ref.modify(
      globals.ADMISSION_BACKLOG_GAUGE,
      (current): [AdmissionBacklogReservation, AdmissionBacklogGaugeState] => {
        const currentBacklog = stateValue(current);
        if (currentBacklog >= max) {
          return [{ reserved: false, currentBacklog }, current];
        }
        return [
          { reserved: true, currentBacklog },
          {
            ...current,
            ADMISSION_BACKLOG_IN_FLIGHT:
              current.ADMISSION_BACKLOG_IN_FLIGHT + 1n,
          },
        ];
      },
    );
  });

const finishAdmissionBacklogReservation = (
  admitted: boolean,
): Effect.Effect<void, never, Globals> =>
  Effect.gen(function* () {
    const globals = yield* Globals;
    yield* Ref.update(globals.ADMISSION_BACKLOG_GAUGE, (current) => {
      if (current.ADMISSION_BACKLOG_IN_FLIGHT <= 0n) {
        throw new Error("Admission backlog reservation underflow");
      }
      return {
        ...current,
        ADMISSION_BACKLOG_LOCAL_DELTA:
          current.ADMISSION_BACKLOG_LOCAL_DELTA + (admitted ? 1n : 0n),
        ADMISSION_BACKLOG_IN_FLIGHT: current.ADMISSION_BACKLOG_IN_FLIGHT - 1n,
      };
    });
  });

export const commitAdmissionBacklogSlot: Effect.Effect<void, never, Globals> =
  finishAdmissionBacklogReservation(true);

export const releaseAdmissionBacklogSlot: Effect.Effect<void, never, Globals> =
  finishAdmissionBacklogReservation(false);

export const admissionFailureDefinitelyDidNotInsert = (
  failure:
    | { readonly _tag: string; readonly mayHaveCommitted?: boolean }
    | undefined,
): boolean =>
  failure?._tag === "TxAdmissionConflictError" ||
  failure?._tag === "TxAdmissionBacklogFullError" ||
  (failure?._tag === "AdmissionWriterShutdownError" &&
    failure.mayHaveCommitted === false);

export const refreshAdmissionBacklogGauge: Effect.Effect<
  void,
  DatabaseError,
  Database | Globals
> = Effect.gen(function* () {
  const globals = yield* Globals;
  const startedAt = Date.now();

  // Preserve the currently reported value while the COUNT is in flight. Any
  // admits after this atomic rollover accrue in the new local delta. The live
  // COUNT can include some of those admits too, which only makes the gauge
  // conservatively high until the next refresh; it can never under-count them.
  yield* Ref.update(
    globals.ADMISSION_BACKLOG_GAUGE,
    beginAdmissionBacklogRefresh,
  );

  const liveCount = yield* TxAdmissionsDB.countBacklog;
  const refreshedAtMs = Date.now();
  const state = yield* Ref.updateAndGet(
    globals.ADMISSION_BACKLOG_GAUGE,
    (current) =>
      completeAdmissionBacklogRefresh(current, liveCount, refreshedAtMs),
  );
  yield* admissionBacklogRefreshDuration(
    Effect.succeed(Duration.millis(refreshedAtMs - startedAt)),
  );
  yield* reportGaugeMetrics(stateValue(state), refreshedAtMs);
});

export const admissionBacklogGaugeFiber = (
  schedule: Schedule.Schedule<number>,
  beforeRefresh: Effect.Effect<void> = Effect.void,
): Effect.Effect<void, never, Database | Globals> =>
  Effect.gen(function* () {
    yield* Effect.logInfo("📏 Admission backlog gauge fiber started.");
    yield* Effect.repeat(
      beforeRefresh.pipe(
        Effect.zipRight(refreshAdmissionBacklogGauge),
        Effect.withSpan("admission-backlog-gauge-fiber"),
        Effect.catchAllCause((cause) =>
          Cause.isInterruptedOnly(cause)
            ? Effect.interrupt
            : Effect.logWarning(cause),
        ),
      ),
      schedule,
    );
  });
