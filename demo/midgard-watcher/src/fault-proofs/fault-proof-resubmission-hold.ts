import { FRAUD_PROOF_WORKFLOW_FRESH_SUBMISSION_REQUIRED } from "@al-ft/midgard-fault-proofs";

import type { WatcherProofObjective } from "./fault-proof-objective-journal.js";

/** A reconciliation-only objective whose next action needs manual submission. */
export const WATCHER_FAULT_PROOF_RESUBMISSION_REQUIRED =
  "fault_proof_resubmission_required";

export const holdWatcherFaultProofForResubmission = (
  holds: Map<string, Readonly<{ reason: string; detail: string }>>,
  job: WatcherProofObjective,
  result: Readonly<{ kind: unknown; reason: unknown }>,
): Readonly<{ kind: "pending"; reason: string }> | undefined => {
  if (
    result.kind !== "pending" ||
    result.reason !== FRAUD_PROOF_WORKFLOW_FRESH_SUBMISSION_REQUIRED
  )
    return undefined;
  const objective = `${job.category}/${job.headerHash}`;
  holds.set(objective, {
    reason: WATCHER_FAULT_PROOF_RESUBMISSION_REQUIRED,
    detail: `${objective}: ${result.reason}`,
  });
  return { kind: "pending", reason: result.reason };
};
