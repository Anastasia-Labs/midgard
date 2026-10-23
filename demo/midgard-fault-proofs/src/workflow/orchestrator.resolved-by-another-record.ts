import type {
  FraudProofWorkflowIdentity,
  FraudProofWorkflowJournalEntry,
} from "./journal.js";
import type { FraudProofWorkflowRunResult } from "./orchestrator.fraud-proof-workflow-run-result.js";
import {
  reclaimedThreads,
  threadReclaimUnavailableReason,
} from "./own-thread-reclaim.js";
import type { VerifiedFraudProofReleaseFinalityPolicy } from "./release-finality-policy.js";
import {
  type FraudProofResolvedByAnotherRecord,
  journalAttemptsSettled,
} from "./resolved-by-another-record.js";

/**
 * Removed against a record this run did not create: done once the removal
 * is past rollback recovery and every signed attempt of this run is
 * settled, and never our conviction. A thread of ours that still stands on
 * the header holds the objective under the operator's alarm: the run that
 * reaches this point has no cancel to reclaim it with. The threads the run
 * did reclaim are reported as reclaims.
 */
export const resolvedByAnotherRecordResult = ({
  resolution,
  workflowId,
  identity,
  entries,
  releaseFinality,
  resumeOnObservation,
}: {
  readonly resolution: FraudProofResolvedByAnotherRecord;
  readonly workflowId: string;
  readonly identity: FraudProofWorkflowIdentity;
  readonly entries: readonly FraudProofWorkflowJournalEntry[];
  readonly releaseFinality: VerifiedFraudProofReleaseFinalityPolicy;
  readonly resumeOnObservation: (reason: string) => FraudProofWorkflowRunResult;
}): FraudProofWorkflowRunResult => {
  const { standingThread, ...removal } = resolution;
  if (standingThread !== undefined)
    return resumeOnObservation(
      threadReclaimUnavailableReason(
        standingThread.threadOutRef,
        "this family has no cancel to reclaim it with",
      ),
    );
  if (
    removal.confirmationDepth <
    releaseFinality.policy.automaticRecoveryMaxDepth + 2
  )
    return resumeOnObservation(
      `the header was removed against another record; the removal awaits release finality`,
    );
  if (!journalAttemptsSettled(entries))
    return resumeOnObservation(
      `the header was removed against another record; this run's signed attempts are not yet settled`,
    );
  const reclaimed = reclaimedThreads(entries);
  return {
    kind: "resolved_by_another_record",
    workflowId,
    identity,
    resolution: removal,
    entries,
    ...(reclaimed.length === 0 ? {} : { reclaimed }),
  };
};
