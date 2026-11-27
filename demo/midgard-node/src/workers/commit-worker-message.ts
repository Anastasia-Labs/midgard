import { Effect, Ref } from "effect";

import { applyCommitDaFrameNotice } from "../fibers/block-commitment.commit-da-frame-readiness.js";
import {
  applyCommitWorkerReadiness,
  boundCommitWorkerLocalFaults,
} from "../fibers/block-commitment.worker-readiness.js";
import {
  noticeCommitBuildAlarm,
  settleCommitBuildAlarms,
} from "../services/globals.commit-build-alarms.js";
import type { Globals } from "../services/globals.globals.js";
import { markMempoolLedgerStale } from "../services/globals.with-l1-control-plane-held.js";
import type { UnwrittenHold } from "../services/intent-journal.holds.js";
import {
  applyCommitPlanningNotice,
  type CommitPlanningNotice,
} from "./commit-planning-notice.js";
import {
  type CommitBuildAlarmNotice,
  type IntentRefusalHoldsNotice,
  type MempoolLedgerRevertedNotice,
  revertedOutRefs,
  WorkerOutput,
} from "./utils/commit-block-header.js";
import { type CommitDaFrameNotice } from "./utils/commit-block-planner.commit-da-frame-notice.js";

/** A message the commit worker posts: its output, or a notice ahead of it. */
export type CommitWorkerMessage =
  | WorkerOutput
  | MempoolLedgerRevertedNotice
  | CommitDaFrameNotice
  | CommitPlanningNotice
  | IntentRefusalHoldsNotice
  | CommitBuildAlarmNotice;

/**
 * Applies a notice the commit worker posts while it still runs and returns
 * undefined; any other message is the worker's output and is returned. A
 * commit-stage rejection rewrites mempool_ledger rows (reverted outputs,
 * restored inputs, rejected descendants), so its notice marks the outrefs it
 * names stale in the ledger cache, or every key when its payload is
 * malformed. A DA frame notice raises or
 * clears the commit DA frame liveness reason; the worker's output never does,
 * because a "nothing to commit" output also ends a tick whose step-down
 * dropped every transaction from an over-frame ledger. A notice of refusal
 * holds the worker could not write goes to `adoptRefusalHolds` (the node
 * journal's `adopt`), which names them on `/readyz` and writes them. An
 * output also counts toward the bound on attempts failing on this process's
 * Ed25519 verifier (`boundCommitWorkerLocalFaults`).
 */
export const takeCommitWorkerOutput = (
  globals: Globals,
  message: CommitWorkerMessage,
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
  if (message.type === "CommitBuildAlarmNotice") {
    Effect.runSync(
      Ref.update(globals.COMMIT_BUILD_ALARMS, (alarms) =>
        noticeCommitBuildAlarm(alarms, message),
      ),
    );
    return undefined;
  }
  if (message.type !== "MempoolLedgerRevertedNotice") {
    Effect.runSync(applyCommitWorkerReadiness(globals, message));
    Effect.runSync(boundCommitWorkerLocalFaults(globals, message));
    Effect.runSync(
      Ref.update(globals.COMMIT_BUILD_ALARMS, (alarms) =>
        settleCommitBuildAlarms(alarms, message),
      ),
    );
    return message;
  }
  Effect.runSync(
    markMempoolLedgerStale(globals, revertedOutRefs(message) ?? "all"),
  );
  return undefined;
};
