import { Cause, Duration, Effect, Metric, Ref, Schedule } from "effect";

import * as TxAdmissionsDB from "../database/txAdmissions.js";
import { DatabaseError } from "../database/utils/common.js";
import { isConnectionClassError } from "../provider-retry.js";
import { Database } from "../services/database.js";
import { AdmissionBacklogGaugeState, Globals } from "../services/globals.js";
import {
  ADMISSION_BACKLOG_UNREAD,
  retryStartupStep,
  STARTUP_DATABASE_BUDGET,
  type StartupStepBudget,
  type StartupStepFailedError,
} from "../services/startup-waiting.js";

export const beginAdmissionBacklogRefresh = (
  current: AdmissionBacklogGaugeState,
): AdmissionBacklogGaugeState => ({
  ...current,
  ADMISSION_BACKLOG_BASE:
    current.ADMISSION_BACKLOG_BASE + current.ADMISSION_BACKLOG_LOCAL_DELTA,
  ADMISSION_BACKLOG_LOCAL_DELTA: 0n,
  ADMISSION_BACKLOG_BYTES_BASE:
    current.ADMISSION_BACKLOG_BYTES_BASE +
    current.ADMISSION_BACKLOG_BYTES_LOCAL_DELTA,
  ADMISSION_BACKLOG_BYTES_LOCAL_DELTA: 0n,
});

export const completeAdmissionBacklogRefresh = (
  current: AdmissionBacklogGaugeState,
  live: TxAdmissionsDB.BacklogSize,
  refreshedAtMs: number,
): AdmissionBacklogGaugeState => ({
  ...current,
  ADMISSION_BACKLOG_BASE: live.rows,
  ADMISSION_BACKLOG_BYTES_BASE: live.bytes,
  ADMISSION_BACKLOG_REFRESHED_AT: refreshedAtMs,
});

/** What one reserved transaction holds in the gauge until it settles. */
export type AdmissionBacklogSlot = {
  readonly bytes: bigint;
};

export type AdmissionBacklogReservation =
  | ({ readonly reserved: true } & AdmissionBacklogSlot)
  | {
      readonly reserved: false;
      /** Which bound refused: the row count or the payload bytes. */
      readonly unit: "rows" | "bytes";
      readonly backlog: bigint;
      readonly maxBacklog: bigint;
    };

export type AdmissionBacklogLimits = {
  readonly maxRows: number;
  readonly maxBytes: number;
};

const stateValue = (state: AdmissionBacklogGaugeState): bigint =>
  state.ADMISSION_BACKLOG_BASE +
  state.ADMISSION_BACKLOG_LOCAL_DELTA +
  state.ADMISSION_BACKLOG_IN_FLIGHT;

const stateBytes = (state: AdmissionBacklogGaugeState): bigint =>
  state.ADMISSION_BACKLOG_BYTES_BASE +
  state.ADMISSION_BACKLOG_BYTES_LOCAL_DELTA +
  state.ADMISSION_BACKLOG_BYTES_IN_FLIGHT;

const admissionBacklogGaugeValue = Metric.gauge(
  "admission_backlog_gauge_value",
  {
    description: "Hybrid durable-admission backlog gauge value",
    bigint: true,
  },
);

const admissionBacklogGaugeBytes = Metric.gauge(
  "admission_backlog_gauge_bytes",
  {
    description: "Hybrid durable-admission backlog gauge payload bytes",
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
  state: AdmissionBacklogGaugeState,
): Effect.Effect<void> =>
  Effect.gen(function* () {
    const refreshedAtMs = state.ADMISSION_BACKLOG_REFRESHED_AT;
    yield* admissionBacklogGaugeValue(Effect.succeed(stateValue(state)));
    yield* admissionBacklogGaugeBytes(Effect.succeed(stateBytes(state)));
    yield* admissionBacklogGaugeStalenessMs(
      Effect.succeed(
        refreshedAtMs === 0 ? 0 : Math.max(0, Date.now() - refreshedAtMs),
      ),
    );
  });

/** The gauge's backlog rows and payload bytes, read from memory. */
export const readAdmissionBacklogSize: Effect.Effect<
  TxAdmissionsDB.BacklogSize,
  never,
  Globals
> = Effect.gen(function* () {
  const globals = yield* Globals;
  const state = yield* Ref.get(globals.ADMISSION_BACKLOG_GAUGE);
  yield* reportGaugeMetrics(state);
  return { rows: stateValue(state), bytes: stateBytes(state) };
});

export const readAdmissionBacklogGauge: Effect.Effect<bigint, never, Globals> =
  Effect.map(readAdmissionBacklogSize, (size) => size.rows);

/** Counts one row of `bytes` that some path admitted without a reservation. */
export const noteLocalAdmit = (
  bytes: number,
): Effect.Effect<void, never, Globals> =>
  Effect.gen(function* () {
    const globals = yield* Globals;
    const state = yield* Ref.updateAndGet(
      globals.ADMISSION_BACKLOG_GAUGE,
      (current) => ({
        ...current,
        ADMISSION_BACKLOG_LOCAL_DELTA:
          current.ADMISSION_BACKLOG_LOCAL_DELTA + 1n,
        ADMISSION_BACKLOG_BYTES_LOCAL_DELTA:
          current.ADMISSION_BACKLOG_BYTES_LOCAL_DELTA + BigInt(bytes),
      }),
    );
    yield* reportGaugeMetrics(state);
  });

/**
 * Atomically checks both process-local caps and reserves one row of `bytes`.
 * Distinct concurrent submit fibers therefore cannot all observe the same
 * stale value. A refusal costs no database work.
 */
export const reserveAdmissionBacklogSlot = ({
  maxRows,
  maxBytes,
  bytes,
}: AdmissionBacklogLimits & {
  readonly bytes: number;
}): Effect.Effect<AdmissionBacklogReservation, never, Globals> =>
  Effect.gen(function* () {
    const globals = yield* Globals;
    const rowLimit = BigInt(Math.max(0, maxRows));
    const byteLimit = BigInt(Math.max(0, maxBytes));
    const requested = BigInt(bytes);
    return yield* Ref.modify(
      globals.ADMISSION_BACKLOG_GAUGE,
      (current): [AdmissionBacklogReservation, AdmissionBacklogGaugeState] => {
        const rows = stateValue(current);
        if (rows >= rowLimit) {
          return [
            {
              reserved: false,
              unit: "rows",
              backlog: rows,
              maxBacklog: rowLimit,
            },
            current,
          ];
        }
        const backlogBytes = stateBytes(current);
        if (backlogBytes + requested > byteLimit) {
          return [
            {
              reserved: false,
              unit: "bytes",
              backlog: backlogBytes,
              maxBacklog: byteLimit,
            },
            current,
          ];
        }
        return [
          { reserved: true, bytes: requested },
          {
            ...current,
            ADMISSION_BACKLOG_IN_FLIGHT:
              current.ADMISSION_BACKLOG_IN_FLIGHT + 1n,
            ADMISSION_BACKLOG_BYTES_IN_FLIGHT:
              current.ADMISSION_BACKLOG_BYTES_IN_FLIGHT + requested,
          },
        ];
      },
    );
  });

const finishAdmissionBacklogReservation = (
  slot: AdmissionBacklogSlot,
  admitted: boolean,
): Effect.Effect<void, never, Globals> =>
  Effect.gen(function* () {
    const globals = yield* Globals;
    yield* Ref.update(globals.ADMISSION_BACKLOG_GAUGE, (current) => {
      if (
        current.ADMISSION_BACKLOG_IN_FLIGHT <= 0n ||
        current.ADMISSION_BACKLOG_BYTES_IN_FLIGHT < slot.bytes
      ) {
        throw new Error("Admission backlog reservation underflow");
      }
      return {
        ...current,
        ADMISSION_BACKLOG_LOCAL_DELTA:
          current.ADMISSION_BACKLOG_LOCAL_DELTA + (admitted ? 1n : 0n),
        ADMISSION_BACKLOG_IN_FLIGHT: current.ADMISSION_BACKLOG_IN_FLIGHT - 1n,
        ADMISSION_BACKLOG_BYTES_LOCAL_DELTA:
          current.ADMISSION_BACKLOG_BYTES_LOCAL_DELTA +
          (admitted ? slot.bytes : 0n),
        ADMISSION_BACKLOG_BYTES_IN_FLIGHT:
          current.ADMISSION_BACKLOG_BYTES_IN_FLIGHT - slot.bytes,
      };
    });
  });

/** The reserved row was written: it now counts until the next refresh. */
export const commitAdmissionBacklogSlot = (
  slot: AdmissionBacklogSlot,
): Effect.Effect<void, never, Globals> =>
  finishAdmissionBacklogReservation(slot, true);

/** The reserved row was not written (a duplicate or a refusal). */
export const releaseAdmissionBacklogSlot = (
  slot: AdmissionBacklogSlot,
): Effect.Effect<void, never, Globals> =>
  finishAdmissionBacklogReservation(slot, false);

export const admissionFailureDefinitelyDidNotInsert = (
  failure:
    | { readonly _tag: string; readonly mayHaveCommitted?: boolean }
    | undefined,
): boolean =>
  failure?._tag === "TxAdmissionConflictError" ||
  (failure?._tag === "AdmissionWriterShutdownError" &&
    failure.mayHaveCommitted === false);

export const refreshAdmissionBacklogGauge: Effect.Effect<
  void,
  DatabaseError,
  Database | Globals
> = Effect.gen(function* () {
  const globals = yield* Globals;
  const startedAt = Date.now();

  // Preserve the currently reported rows and bytes while the live count is in
  // flight. Any admits after this atomic rollover accrue in the new local
  // deltas. The live count can include some of those admits too, which only
  // makes the gauge conservatively high until the next refresh; it can never
  // under-count them.
  yield* Ref.update(
    globals.ADMISSION_BACKLOG_GAUGE,
    beginAdmissionBacklogRefresh,
  );

  const live = yield* TxAdmissionsDB.countBacklogSize;
  const refreshedAtMs = Date.now();
  const state = yield* Ref.updateAndGet(
    globals.ADMISSION_BACKLOG_GAUGE,
    (current) => completeAdmissionBacklogRefresh(current, live, refreshedAtMs),
  );
  yield* admissionBacklogRefreshDuration(
    Effect.succeed(Duration.millis(refreshedAtMs - startedAt)),
  );
  yield* reportGaugeMetrics(state);
});

/**
 * The startup's first refresh of the gauge (`refresh`, by default
 * `refreshAdmissionBacklogGauge`). It blocks the startup: the gauge is the
 * base admission's backlog caps (`reserveAdmissionBacklogSlot`) count
 * from, so serving before it was read would admit past
 * `MAX_DURABLE_ADMISSION_BACKLOG` or `MAX_DURABLE_ADMISSION_BACKLOG_BYTES`. A connection-class failure
 * (`isConnectionClassError`) is waited out under the database budget, the
 * startup waiting under `admission_backlog_unread`; any other failure, or
 * one past the budget, fails the step (`StartupStepFailedError`).
 */
export const refreshAdmissionBacklogGaugeOnStartup = <R>(
  refresh: Effect.Effect<void, DatabaseError, R>,
  budget: StartupStepBudget = { maxElapsed: STARTUP_DATABASE_BUDGET },
): Effect.Effect<void, StartupStepFailedError, R> =>
  retryStartupStep(refresh, {
    key: "admission_backlog_refresh",
    reason: ADMISSION_BACKLOG_UNREAD,
    retryable: isConnectionClassError,
    budget,
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
