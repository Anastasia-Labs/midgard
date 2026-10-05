import {
  abandonWorkflowFundingReservationTransaction,
  acknowledgeWorkflowFundingAbandonment,
  assertWorkflowFundingAbandonmentHandoffJournal,
  createWorkflowFundingAbandonmentHandoff,
  type WorkflowFundingAbandonmentHandoff,
} from "./funding-reservation-permit.js";
import type {
  FraudProofWorkflowJournalEntry,
  FraudProofWorkflowJournalEvent,
  FraudProofWorkflowJournalStore,
} from "./journal.js";
import type { SignedWorkflowTransactionRetirement } from "./signed-transaction-retirement.js";

/**
 * Records `transactionHash` as superseded (or, with a retirement, retired):
 * the funding store keeps its lineage as an exclusion set, so a replacement
 * must spend one of its inputs, and the journal closes the attempt as
 * not_found. A later landing is adopted through the abandonment rows.
 */
export const supersedeWorkflowFundingAttempt = async ({
  journal,
  assertReconcile,
  entries,
  append,
  transactionHash,
  retirement,
  savedHandoff,
}: {
  readonly journal: FraudProofWorkflowJournalStore;
  /** The family's actuation check before a reconciliation write. */
  readonly assertReconcile: () => void;
  readonly entries: () =>
    | readonly FraudProofWorkflowJournalEntry[]
    | Promise<readonly FraudProofWorkflowJournalEntry[]>;
  readonly append: (event: FraudProofWorkflowJournalEvent) => Promise<void>;
  readonly transactionHash: string;
  readonly retirement: SignedWorkflowTransactionRetirement | undefined;
  readonly savedHandoff: WorkflowFundingAbandonmentHandoff | null;
}): Promise<void> => {
  const handoff =
    savedHandoff ??
    createWorkflowFundingAbandonmentHandoff({
      entries: await entries(),
      transactionHash,
      retirement,
    });
  await abandonWorkflowFundingReservationTransaction({
    journal,
    transactionHash,
    handoff,
  });
  assertReconcile();
  if (
    !assertWorkflowFundingAbandonmentHandoffJournal({
      handoff,
      entries: await entries(),
    })
  )
    await append(handoff.reconciliation);
  assertReconcile();
  await acknowledgeWorkflowFundingAbandonment({ journal, handoff });
};
