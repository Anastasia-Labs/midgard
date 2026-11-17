import type { ValidationTraceReplayHold } from "@al-ft/midgard-fault-proofs";

import { watcherValidationTraceReplayStuckAlertSubject } from "../runtime/operations-observability.alert-book.js";
import type { WatcherOperationsSink } from "../runtime/operations-observability.watcher-operations-metrics.js";
import { type PerHeaderAlert, perHeaderAlert } from "./per-header-alert.js";

/**
 * The operator's `validation_trace_replay_stuck` alert, one subject per
 * header, raised by the replay holds' `onReplayStuck` on every stuck retry.
 * It stands until the header is decided or leaves the queue: the header's
 * settled outcome clears it (operations-observability.alert-book.ts), and so
 * does `reconcile` once the header is released or a rollback drops it from
 * the observed queue.
 */
export const createWatcherValidationTraceReplayStuckAlert = (
  operationsSink: Pick<WatcherOperationsSink, "setAlert">,
  nowMs: () => number = Date.now,
): Readonly<{
  stuck: (hold: Pick<ValidationTraceReplayHold, "headerHash">) => void;
  reconcile: PerHeaderAlert["reconcile"];
}> => {
  const alert = perHeaderAlert({
    code: "validation_trace_replay_stuck",
    subjectOf: watcherValidationTraceReplayStuckAlertSubject,
    operationsSink,
    observedAtMs: () => nowMs().toString(),
  });
  return Object.freeze({
    stuck: (hold) => alert.set(hold.headerHash, true),
    reconcile: alert.reconcile,
  });
};
