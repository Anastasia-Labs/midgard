/**
 * The import checker's alarms. They only alert: an imported block stays
 * followed whatever its checks find, and the node stays ready.
 *
 * - `import_check_rejected`: a run the local evaluator rejects (the
 *   transaction was committed Accepted);
 * - `uplc_import_fault`: a run that faulted (killed, crashed, or failed to
 *   evaluate) twice, the second time on a fresh instance; it stays in the
 *   set;
 * - `import_check_cap`: the block's checker time ran out; its unstarted
 *   transactions stay in the set as skipped;
 * - `unverified_deadline_missed`: an entry reached its deadline with no
 *   local verdict and left the set.
 *
 * Each alarm logs one `ImportCheckAlarm` error event and increments
 * `import_check_alarm_total`, tagged `alarm`.
 */
import { Effect, Metric } from "effect";

import { type ImportCheckState } from "../database/importChecks.js";

export const IMPORT_CHECK_ALARM_EVENT = "ImportCheckAlarm";

export type ImportCheckAlarm =
  | Readonly<{
      alarm: "import_check_rejected" | "uplc_import_fault";
      headerHash: string;
      transactionId: string;
      detail: string;
    }>
  | Readonly<{
      alarm: "import_check_cap";
      headerHash: string;
      skipped: number;
    }>
  | Readonly<{
      alarm: "unverified_deadline_missed";
      headerHash: string;
      transactionId: string;
      deadlineMs: number;
      state: ImportCheckState;
    }>;

export type ImportCheckAlarmSink = (
  alarm: ImportCheckAlarm,
) => Effect.Effect<void>;

export const importCheckAlarmCounter = Metric.counter(
  "import_check_alarm_total",
  {
    description: "Alarms the import checker raised, tagged by alarm",
    bigint: true,
    incremental: true,
  },
);

/** The node's sink: an error event and the tagged counter. */
export const logImportCheckAlarm: ImportCheckAlarmSink = (alarm) =>
  Effect.logError(IMPORT_CHECK_ALARM_EVENT).pipe(
    Effect.annotateLogs({ ...alarm }),
    Effect.zipRight(
      Metric.increment(
        Metric.tagged(importCheckAlarmCounter, "alarm", alarm.alarm),
      ),
    ),
  );
