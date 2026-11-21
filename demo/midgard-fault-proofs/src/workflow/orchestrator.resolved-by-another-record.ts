import type {
  FraudProofWorkflowIdentity,
  FraudProofWorkflowJournalEntry,
} from "./journal.js";
import type { FraudProofWorkflowRunResult } from "./orchestrator.fraud-proof-workflow-run-result.js";
import {
  ownThreadsUnobservableReason,
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
 * the header, as the stage derivation or the adapter's own view of our
 * threads reports it, holds the objective under the operator's alarm: a
 * family with a cancel wired in front of it reclaims the thread before its
 * resolution gets here, so none is wired for this one. A run that cannot
 * list our threads on the header at all holds under its own alarm too:
 * nothing shows that none of ours stands, so it never resolves blind. The
 * threads the run did reclaim are reported as reclaims.
 */
export const resolvedByAnotherRecordResult = ({
  resolution,
  threads,
  workflowId,
  identity,
  entries,
  releaseFinality,
  resumeOnObservation,
}: {
  readonly resolution: FraudProofResolvedByAnotherRecord;
  /** Our threads as the run sees them on the header: the one still
   * standing, if any. Undefined when the run cannot list them. */
  readonly threads:
    | Readonly<{
        standing?: FraudProofResolvedByAnotherRecord["standingThread"];
      }>
    | undefined;
  readonly workflowId: string;
  readonly identity: FraudProofWorkflowIdentity;
  readonly entries: readonly FraudProofWorkflowJournalEntry[];
  readonly releaseFinality: VerifiedFraudProofReleaseFinalityPolicy;
  readonly resumeOnObservation: (reason: string) => FraudProofWorkflowRunResult;
}): FraudProofWorkflowRunResult => {
  const { standingThread, ...removal } = resolution;
  const thread = standingThread ?? threads?.standing;
  if (thread !== undefined)
    return resumeOnObservation(
      threadReclaimUnavailableReason(
        thread.threadOutRef,
        "no reclaim is wired in front of this family's workflow",
      ),
    );
  if (threads === undefined)
    return resumeOnObservation(
      ownThreadsUnobservableReason(
        "this family's workflow has no view of our threads",
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
