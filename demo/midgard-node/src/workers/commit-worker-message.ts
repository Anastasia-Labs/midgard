import { Effect } from "effect";

import { applyCommitDaFrameNotice } from "../fibers/block-commitment.commit-da-frame-readiness.js";
import { applyCommitWorkerReadiness } from "../fibers/block-commitment.worker-readiness.js";
import {
  type Globals,
  publishMempoolLedgerDelta,
} from "../services/globals.js";
import type { UnwrittenHold } from "../services/intent-journal.holds.js";
import {
  applyCommitPlanningNotice,
  type CommitPlanningNotice,
} from "./commit-planning-notice.js";
import {
  type IntentRefusalHoldsNotice,
  type MempoolLedgerRevertedNotice,
  WorkerOutput,
} from "./utils/commit-block-header.js";
import { type CommitDaFrameNotice } from "./utils/commit-block-planner.commit-da-frame-notice.js";

export const publishFullMempoolLedgerReload = (
  globals: Globals,
  deltaLogMax: number,
): Effect.Effect<void> =>
  publishMempoolLedgerDelta(
    globals,
    { full: true, upserts: [], deletes: [] },
    deltaLogMax,
  ).pipe(Effect.asVoid);

/** A message the commit worker posts: its output, or a notice ahead of it. */
export type CommitWorkerMessage =
  | WorkerOutput
  | MempoolLedgerRevertedNotice
  | CommitDaFrameNotice
  | CommitPlanningNotice
  | IntentRefusalHoldsNotice;

/**
 * Applies a notice the commit worker posts while it still runs and returns
 * undefined; any other message is the worker's output and is returned. A
 * commit-stage rejection rewrites mempool_ledger rows (reverted outputs,
 * restored inputs, rejected descendants) without a delta, so its notice
 * reloads the cache from the durable table. A DA frame notice raises or
 * clears the commit DA frame liveness reason; the worker's output never does,
 * because a "nothing to commit" output also ends a tick whose step-down
 * dropped every transaction from an over-frame ledger. A notice of refusal
 * holds the worker could not write goes to `adoptRefusalHolds` (the node
 * journal's `adopt`), which names them on `/readyz` and writes them.
 */
export const takeCommitWorkerOutput = (
  globals: Globals,
  message: CommitWorkerMessage,
  deltaLogMax: number,
  adoptRefusalHolds?: (holds: readonly UnwrittenHold[]) => void,
): WorkerOutput | undefined => {
  if (message.type === "CommitPlanningNotice") {
    Effect.runSync(applyCommitPlanningNotice(globals, message));
    return undefined;
  }
  if (message.type === "IntentRefusalHoldsNotice") {
    adoptRefusalHolds?.(message.holds);
    return undefined;
  }
  if (message.type === "CommitDaFrameNotice") {
    Effect.runSync(applyCommitDaFrameNotice(globals, message));
    return undefined;
  }
  if (message.type !== "MempoolLedgerRevertedNotice") {
    Effect.runSync(applyCommitWorkerReadiness(globals, message));
    return message;
  }
  Effect.runSync(publishFullMempoolLedgerReload(globals, deltaLogMax));
  return undefined;
};
