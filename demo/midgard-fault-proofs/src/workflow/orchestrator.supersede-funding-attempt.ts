import {
  abandonWorkflowFundingReservationTransaction,
  readWorkflowFundingRecovery,
  type WorkflowJournalWriter,
} from "./funding-reservation-permit.js";
import type { FraudProofWorkflowJournalEntry } from "./journal.js";
import type { SignedWorkflowTransactionRetirement } from "./signed-transaction-retirement.js";

/**
 * Records `transactionHash` as superseded (or, with a retirement, retired):
 * the funding store keeps its lineage as an exclusion set, so a replacement
 * must spend one of its inputs, and the journal closes the attempt as
 * not_found in the same commit. A later landing is adopted through the
 * superseded-attempt records.
 */
export const supersedeWorkflowFundingAttempt = async ({
  writer,
  assertReconcile,
  entries,
  transactionHash,
  retirement,
}: {
  readonly writer: WorkflowJournalWriter;
  /** The family's actuation check before a reconciliation write. */
  readonly assertReconcile: () => void;
  readonly entries: () =>
    | readonly FraudProofWorkflowJournalEntry[]
    | Promise<readonly FraudProofWorkflowJournalEntry[]>;
  readonly transactionHash: string;
  readonly retirement: SignedWorkflowTransactionRetirement | undefined;
}): Promise<void> => {
  const intent = [...(await entries())]
    .reverse()
    .map(({ event }) => event)
    .find(
      (event) =>
        event.kind === "submission_intent" && event.txHash === transactionHash,
    );
  if (intent?.kind !== "submission_intent")
    throw new Error("funding supersession requires its exact signed intent");
  assertReconcile();
  await abandonWorkflowFundingReservationTransaction({
    writer,
    transactionHash,
    ...(retirement === undefined ? {} : { retirement }),
    events: [
      {
        kind: "reconciled",
        actionId: intent.actionId,
        outcome: "not_found",
        txHash: transactionHash,
        ...(retirement === undefined ? {} : { retirement }),
      },
    ],
  });
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
  "transactionHash" | "retirement"
> & { readonly kept: string }): Promise<void> => {
  const displaced = await readWorkflowFundingRecovery(input.writer.journal);
  const pending = displaced.transition?.transactionHash;
  if (pending !== undefined && pending !== kept)
    await supersedeWorkflowFundingAttempt({
      ...input,
      transactionHash: pending,
      retirement: undefined,
    });
};
