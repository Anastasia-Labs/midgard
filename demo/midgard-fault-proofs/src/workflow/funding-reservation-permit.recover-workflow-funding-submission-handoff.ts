import { reobserveWorkflowFundingReservationTransaction } from "./funding-reservation-permit.apply-transition.js";
import { workflowFundingHandoffJournalLost } from "./funding-reservation-permit.parse-workflow-funding-abandonment-handoff.js";
import { parseWorkflowFundingSubmissionHandoff } from "./funding-reservation-permit.parse-workflow-funding-prepared-transition.js";
import { reconcileWorkflowFundingSubmissionHandoff } from "./funding-reservation-permit.reconcile-workflow-funding-submission-handoff.js";
import type { WorkflowFundingSubmissionHandoff } from "./funding-reservation-permit.workflow-funding-reservation-port.js";
import {
  type FraudProofWorkflowIdentity,
  type FraudProofWorkflowJournalEntry,
  type FraudProofWorkflowJournalEvent,
  validateFraudProofWorkflowJournal,
} from "./journal.js";

/**
 * Restore the journal records of the reservation's pending signed attempt.
 * Losing the workflow journal costs no liveness: the store keeps the exact
 * attempt, and it is re-recorded as the first action of the recreated
 * journal. L1 reconciliation then resolves it as for any recorded intent.
 */
export const recoverWorkflowFundingSubmissionHandoff = async ({
  journal,
  workflowId,
  identity,
  entries,
  handoff,
  append,
}: {
  readonly journal: object;
  readonly workflowId: string;
  readonly identity: FraudProofWorkflowIdentity;
  readonly entries: () => readonly FraudProofWorkflowJournalEntry[];
  readonly handoff: WorkflowFundingSubmissionHandoff;
  readonly append: (event: FraudProofWorkflowJournalEvent) => Promise<unknown>;
}): Promise<void> => {
  let anchored = handoff;
  const current = entries();
  const prepared = current[1]?.event;
  if (
    workflowFundingHandoffJournalLost(handoff, current) &&
    prepared?.kind === "prepared"
  ) {
    anchored = parseWorkflowFundingSubmissionHandoff({
      ...handoff,
      preparedArtifactDigest: prepared.artifactDigest,
      expectedJournalSequence: current.length,
      submissionIntent: { ...handoff.submissionIntent, attempt: 1 },
    });
    if (
      !(await reobserveWorkflowFundingReservationTransaction({
        journal,
        transactionHash: handoff.submissionIntent.txHash,
        adoption: anchored,
      }))
    )
      throw new Error(
        "funding reservation cannot re-record its pending attempt",
      );
  }
  for (const event of reconcileWorkflowFundingSubmissionHandoff({
    handoff: anchored,
    entries: current,
  }))
    await append(event);
  validateFraudProofWorkflowJournal({
    workflowId,
    entries: entries(),
    expectedIdentity: identity,
  });
};
