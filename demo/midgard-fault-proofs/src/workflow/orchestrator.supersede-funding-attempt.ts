import {
  activeCollateralForfeitures,
  collateralForfeitureEvent,
  readShowsForfeitureReversed,
} from "./collateral-forfeiture.js";
import {
  abandonWorkflowFundingReservationTransaction,
  forfeitWorkflowFundingCollateral,
  readWorkflowFundingRecordedSignedTransaction,
  readWorkflowFundingRecovery,
  restoreWorkflowFundingCollateral,
  type WorkflowJournalWriter,
} from "./funding-reservation-permit.js";
import type { FraudProofWorkflowJournalEntry } from "./journal.js";
import type { FraudProofWorkflowReconcileResult } from "./orchestrator.fraud-proof-family-workflow-adapter.js";
import type { SignedWorkflowTransactionRetirement } from "./signed-transaction-retirement.js";

/**
 * Records `transactionHash` as superseded (or, with a retirement, retired):
 * the funding store keeps its lineage as an exclusion set, so a replacement
 * must spend one of its inputs, and the journal closes the attempt as
 * not_found in the same commit. A later landing is adopted through the
 * superseded-attempt records.
 *
 * With `landedInvalid`, the exact bytes landed failing phase 2 at the tip:
 * first the chain's taking of their collateral is accounted, in its own
 * commit, so a rollback past that block can undo exactly that.
 */
export const supersedeWorkflowFundingAttempt = async ({
  writer,
  assertReconcile,
  entries,
  transactionHash,
  retirement,
  landedInvalid,
}: {
  readonly writer: WorkflowJournalWriter;
  /** The family's actuation check before a reconciliation write. */
  readonly assertReconcile: () => void;
  readonly entries: () =>
    | readonly FraudProofWorkflowJournalEntry[]
    | Promise<readonly FraudProofWorkflowJournalEntry[]>;
  readonly transactionHash: string;
  readonly retirement: SignedWorkflowTransactionRetirement | undefined;
  readonly landedInvalid?: true;
}): Promise<void> => {
  const recorded = await entries();
  const intent = [...recorded]
    .reverse()
    .map(({ event }) => event)
    .find(
      (event) =>
        event.kind === "submission_intent" && event.txHash === transactionHash,
    );
  if (intent?.kind !== "submission_intent")
    throw new Error("funding supersession requires its exact signed intent");
  assertReconcile();
  if (landedInvalid === true)
    await forfeitWorkflowAttemptCollateral({
      writer,
      entries: recorded,
      actionId: intent.actionId,
      transactionHash,
    });
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

/** Accounts the collateral that a phase-2-invalid landing of `transactionHash`
 * took, unless a standing forfeiture already did. Without recorded signed
 * bytes there is no reservation to account in, and nothing is recorded. */
export const forfeitWorkflowAttemptCollateral = async ({
  writer,
  entries,
  actionId,
  transactionHash,
}: {
  readonly writer: WorkflowJournalWriter;
  readonly entries: readonly FraudProofWorkflowJournalEntry[];
  readonly actionId: string;
  readonly transactionHash: string;
}): Promise<void> => {
  if (activeCollateralForfeitures(entries).has(transactionHash)) return;
  const signedTransactionCborHex =
    await readWorkflowFundingRecordedSignedTransaction(
      writer.journal,
      transactionHash,
    );
  if (signedTransactionCborHex === null) return;
  const event = collateralForfeitureEvent({
    actionId,
    transactionHash,
    signedTransactionCborHex,
  });
  // The ledger runs no script without collateral, so bytes without any
  // cannot fail phase 2.
  if (event.collateralOutRefs.length === 0)
    throw new Error("a phase-2-invalid landing names no collateral");
  await forfeitWorkflowFundingCollateral({
    writer,
    transactionHash,
    events: [event],
  });
};

/** Undoes the standing forfeiture of `transactionHash` when `read` shows its
 * phase-2-invalid landing gone: a rollback past the block where it landed
 * undoes the collateral accounting done at that tip. */
export const restoreWorkflowAttemptCollateral = async ({
  writer,
  assertReconcile,
  entries,
  transactionHash,
  read,
}: {
  readonly writer: WorkflowJournalWriter;
  readonly assertReconcile: () => void;
  readonly entries: readonly FraudProofWorkflowJournalEntry[];
  readonly transactionHash: string;
  readonly read: FraudProofWorkflowReconcileResult;
}): Promise<void> => {
  const forfeited = activeCollateralForfeitures(entries).get(transactionHash);
  if (forfeited === undefined || !readShowsForfeitureReversed(read)) return;
  assertReconcile();
  await restoreWorkflowFundingCollateral({
    writer,
    transactionHash,
    events: [
      {
        kind: "collateral_forfeiture_reversed",
        actionId: forfeited.actionId,
        txHash: transactionHash,
      },
    ],
  });
};
