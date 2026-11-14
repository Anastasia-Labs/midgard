/**
 * The import checker's alarms. They only alert: an imported block stays
 * followed whatever its checks find, and the node stays ready.
 *
 * - `import_check_rejected`: a run the local evaluator rejects (the
 *   transaction was committed Accepted); the entry stays as `rejected`;
 * - `uplc_import_fault`: a run that faulted (crashed, or failed to
 *   evaluate) twice, the second time on a fresh instance; it stays in the
 *   set as `faulted`, and its script enters the negative cache;
 * - `import_check_timeout`: a run killed at its kill, or whose worker failed
 *   before the run was sent to it. That is wall-clock time and machine
 *   state, not the transaction, so it is no verdict: the entry waits as
 *   `timed_out`, its script is not cached, and the backlog lane runs it
 *   again from `retryAtMs`, backing off, until its deadline. Raised for the
 *   entry's first kill only; its deadline, if no verdict comes first,
 *   raises `unverified_deadline_missed`;
 * - `import_check_cap`: the block's checker time ran out; its unstarted
 *   transactions stay in the set as skipped, for the backlog lane;
 * - `unverified_near_deadline`: the backlog's projected completion of the
 *   unrun entries due by a deadline (not yet past) runs past that deadline
 *   minus the opener's margin; raised once per deadline. With
 *   `structural`, the pool has one worker and reserves none for the
 *   backlog: every unrun entry is projected past its deadline whatever the
 *   load, so the alarm says only that checks wait for idle admission;
 * - `unverified_deadline_missed`: an entry reached its deadline with no
 *   local verdict, raised once per entry: when it leaves the set, or while
 *   it is still being checked (its verdict, when it comes, applies as
 *   usual). An entry of a removed block, or a rejected one, leaves at its
 *   deadline with no alarm.
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
      alarm: "import_check_timeout";
      headerHash: string;
      transactionId: string;
      detail: string;
      /** When the backlog lane runs it again. */
      retryAtMs: number;
    }>
  | Readonly<{
      alarm: "import_check_cap";
      headerHash: string;
      skipped: number;
    }>
  | Readonly<{
      alarm: "unverified_near_deadline";
      deadlineMs: number;
      /** The entries due by the deadline. */
      entries: number;
      projectedMs: number;
      /** Set on a one-worker pool: no backlog worker is reserved. */
      structural?: true;
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
