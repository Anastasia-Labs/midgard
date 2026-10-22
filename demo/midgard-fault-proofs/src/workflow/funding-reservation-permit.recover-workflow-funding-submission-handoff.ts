import { readWorkflowFundingRecovery } from "./funding-reservation-permit.read-workflow-funding-recovery.js";
import {
  assertWorkflowFundingCompletionHandoffJournal,
  reconcileWorkflowFundingSubmissionHandoff,
} from "./funding-reservation-permit.reconcile-workflow-funding-submission-handoff.js";
import type { WorkflowFundingSubmissionHandoff } from "./funding-reservation-permit.workflow-funding-reservation-port.js";
import {
  type FraudProofWorkflowIdentity,
  type FraudProofWorkflowJournalEntry,
  type FraudProofWorkflowJournalEvent,
  validateFraudProofWorkflowJournal,
} from "./journal.js";

type JournalAccess = {
  readonly journal: object;
  readonly workflowId: string;
  readonly identity: FraudProofWorkflowIdentity;
  readonly entries: () => readonly FraudProofWorkflowJournalEntry[];
  readonly append: (event: FraudProofWorkflowJournalEvent) => Promise<unknown>;
};

/**
 * Re-record the reservation's pending signed attempt in its journal when the
 * store committed it before the journal recorded it. L1 reconciliation then
 * resolves it as for any recorded intent.
 */
export const recoverWorkflowFundingSubmissionHandoff = async ({
  workflowId,
  identity,
  entries,
  handoff,
  append,
}: JournalAccess & {
  readonly handoff: WorkflowFundingSubmissionHandoff;
}): Promise<void> => {
  for (const event of reconcileWorkflowFundingSubmissionHandoff({
    handoff,
    entries: entries(),
  }))
    await append(event);
  validateFraudProofWorkflowJournal({
    workflowId,
    entries: entries(),
    expectedIdentity: identity,
  });
};

/**
 * Check a workflow journal against the reservation store's handoffs, and
 * re-record a pending signed attempt the journal has not recorded yet. A
 * completion is never re-run: the runner only re-verifies its stored
 * terminal on L1 before journaling it again.
 */
export const recoverWorkflowFundingJournal = async (
  access: JournalAccess,
): ReturnType<typeof readWorkflowFundingRecovery> => {
  const { journal, entries } = access;
  const recovery = await readWorkflowFundingRecovery(journal);
  if (recovery.submissionHandoff !== null)
    await recoverWorkflowFundingSubmissionHandoff({
      ...access,
      handoff: recovery.submissionHandoff,
    });
  if (recovery.completionHandoff !== null)
    assertWorkflowFundingCompletionHandoffJournal({
      handoff: recovery.completionHandoff,
      entries: entries(),
    });
  return recovery;
};
