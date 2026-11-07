import { Cause, Effect, Metric, Ref, Schedule } from "effect";

import * as TxAdmissionsDB from "../database/txAdmissions.js";
import { DatabaseError } from "../database/utils/common.js";
import { Database } from "../services/database.js";

/** How often the terminal admission sweep runs. */
export const ADMISSION_RETENTION_SWEEP_INTERVAL_MS = 60_000;

/** Terminal rows one delete statement examines. */
export const ADMISSION_RETENTION_BATCH_ROWS = 5_000;

/** Statements one sweep runs at most: 300,000 rows a minute keeps up with
 * the 5,000 TPS ingress design rate, and a larger pile of old rows drains
 * over several sweeps instead of one long loop. */
export const ADMISSION_RETENTION_MAX_BATCHES_PER_SWEEP = 60;

const admissionRetentionDeletedCounter = Metric.counter(
  "tx_admission_retention_deleted_total",
  {
    description:
      "Terminal admission rows deleted after the admission retention window",
    bigint: true,
    incremental: true,
  },
);

export type AdmissionRetentionSweep = Readonly<{
  examined: number;
  deleted: number;
}>;

/**
 * One retention sweep: deletes terminal admission rows older than
 * `retentionMs` in bounded batches (`TxAdmissionsDB.pruneTerminalBatch`
 * says which rows stay). `cursor` carries the walk across sweeps, so rows
 * that must stay never hide the deletable rows behind them; once a batch
 * reaches the newest row past the window the walk starts again from the
 * oldest.
 */
export const sweepTerminalAdmissions = ({
  retentionMs,
  cursor,
  batchRows = ADMISSION_RETENTION_BATCH_ROWS,
  maxBatches = ADMISSION_RETENTION_MAX_BATCHES_PER_SWEEP,
}: {
  readonly retentionMs: number;
  readonly cursor: Ref.Ref<TxAdmissionsDB.TerminalAdmissionCursor | null>;
  readonly batchRows?: number;
  readonly maxBatches?: number;
}): Effect.Effect<AdmissionRetentionSweep, DatabaseError, Database> =>
  Effect.gen(function* () {
    let examined = 0;
    let deleted = 0;
    for (let batch = 0; batch < maxBatches; batch += 1) {
      const result = yield* TxAdmissionsDB.pruneTerminalBatch({
        olderThanMs: retentionMs,
        after: yield* Ref.get(cursor),
        limit: batchRows,
      });
      examined += result.examined;
      deleted += result.deleted;
      const reachedEnd = result.examined < batchRows;
      yield* Ref.set(cursor, reachedEnd ? null : result.last);
      if (reachedEnd) break;
    }
    if (deleted > 0) {
      yield* Metric.incrementBy(
        admissionRetentionDeletedCounter,
        BigInt(deleted),
      );
    }
    return { examined, deleted };
  });

/**
 * Deletes terminal admission rows once they are older than `retentionMs`;
 * 0 keeps them forever. A failed sweep is logged and retried at the next
 * tick: it only delays the deletion.
 */
export const admissionRetentionFiber = (
  schedule: Schedule.Schedule<number>,
  retentionMs: number,
): Effect.Effect<void, never, Database> =>
  Effect.gen(function* () {
    if (retentionMs <= 0) {
      yield* Effect.logInfo(
        "🧹 Admission retention is off: terminal admission rows are kept.",
      );
      return;
    }
    yield* Effect.logInfo(
      `🧹 Admission retention fiber started (window ${retentionMs.toString()} ms).`,
    );
    const cursor =
      yield* Ref.make<TxAdmissionsDB.TerminalAdmissionCursor | null>(null);
    yield* Effect.repeat(
      sweepTerminalAdmissions({ retentionMs, cursor }).pipe(
        Effect.tap(({ deleted }) =>
          deleted > 0
            ? Effect.logDebug(
                `🧹 Deleted ${deleted.toString()} terminal admission rows.`,
              )
            : Effect.void,
        ),
        Effect.withSpan("admission-retention-fiber"),
        Effect.catchAllCause((cause) =>
          Cause.isInterruptedOnly(cause)
            ? Effect.interrupt
            : Effect.logWarning(cause),
        ),
      ),
      schedule,
    );
  });
