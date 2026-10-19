import { FRAUD_PROOF_WORKFLOW_CONTINUATION_STALLED } from "@al-ft/midgard-fault-proofs";

import { WATCHER_PACKAGE_NAME } from "../runtime/scaffold.js";
import type { WatcherProofObjective } from "./fault-proof-objective-journal.js";
import { holdWatcherFaultProofForResubmission } from "./fault-proof-resubmission-hold.js";

/**
 * An objective whose runner session journaled no progress within its
 * continuation bound. It resumes on the next observation; the hold is the
 * operator's alarm until a later run journals progress.
 */
export const WATCHER_FAULT_PROOF_CONTINUATION_STALLED =
  "fault_proof_continuation_stalled";

/** Whether a result or outcome is a pending run handed back by its bound. */
export const isWatcherFaultProofContinuationStall = (
  result: unknown,
): result is Readonly<{ kind: "pending"; reason: string }> =>
  typeof result === "object" &&
  result !== null &&
  "kind" in result &&
  result.kind === "pending" &&
  "reason" in result &&
  typeof result.reason === "string" &&
  result.reason.startsWith(FRAUD_PROOF_WORKFLOW_CONTINUATION_STALLED);

/**
 * Holds a pending objective under its name: one that needs a new submission
 * after its latest safe start, or one whose session stalled, which is also
 * logged at error level.
 */
export const holdWatcherFaultProofPending = (
  holds: Map<string, Readonly<{ reason: string; detail: string }>>,
  job: WatcherProofObjective,
  result: Readonly<{ kind: unknown; reason: unknown }>,
): Readonly<{ kind: "pending"; reason: string }> | undefined => {
  const resubmission = holdWatcherFaultProofForResubmission(holds, job, result);
  if (
    resubmission !== undefined ||
    !isWatcherFaultProofContinuationStall(result)
  )
    return resubmission;
  const objective = `${job.category}/${job.headerHash}`;
  const hold = {
    reason: WATCHER_FAULT_PROOF_CONTINUATION_STALLED,
    detail: `${objective}: ${result.reason}`,
  };
  holds.set(objective, hold);
  process.stderr.write(
    `${JSON.stringify({ packageName: WATCHER_PACKAGE_NAME, level: "error", ...hold })}\n`,
  );
  return { kind: "pending", reason: result.reason };
};

/**
 * The progress callback of an objective's run: its first journaled progress
 * clears a stall hold left by an earlier run, without waiting for this run to
 * end. Any other hold is left to the run's outcome.
 */
export const releaseWatcherFaultProofStallOnProgress =
  (
    holds: Map<string, Readonly<{ reason: string; detail: string }>>,
    job: WatcherProofObjective,
  ) =>
  (): void => {
    const objective = `${job.category}/${job.headerHash}`;
    if (
      holds.get(objective)?.reason === WATCHER_FAULT_PROOF_CONTINUATION_STALLED
    )
      holds.delete(objective);
  };
