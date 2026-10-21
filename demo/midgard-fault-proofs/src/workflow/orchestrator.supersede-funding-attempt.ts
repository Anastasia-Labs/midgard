import {
  abandonWorkflowFundingReservationTransaction,
  acknowledgeWorkflowFundingAbandonment,
  assertWorkflowFundingAbandonmentHandoffJournal,
  createWorkflowFundingAbandonmentHandoff,
  readWorkflowFundingRecovery,
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

/** Supersedes the pending attempt, if any, other than `kept`: reobserving
 * `kept` displaces it, and it is superseded, never silently dropped. A
 * dropped attempt would count as live coverage, so a replacement could skip
 * its inputs and both could land, the second failing its script at
 * inclusion. */
export const supersedeDisplacedFundingAttempt = async ({
  kept,
  ...input
}: Omit<
  Parameters<typeof supersedeWorkflowFundingAttempt>[0],
  "transactionHash" | "retirement" | "savedHandoff"
> & { readonly kept: string }): Promise<void> => {
  const displaced = await readWorkflowFundingRecovery(input.journal);
  const pending = displaced.transition?.transactionHash;
  if (pending !== undefined && pending !== kept)
    await supersedeWorkflowFundingAttempt({
      ...input,
      transactionHash: pending,
      retirement: undefined,
      savedHandoff: displaced.abandonmentHandoff,
    });
};
