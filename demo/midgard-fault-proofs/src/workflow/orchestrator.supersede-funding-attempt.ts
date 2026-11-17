import { accountAttemptCollateral } from "./collateral-accounting.js";
import { collateralForfeitureEvent } from "./collateral-forfeiture.js";
import {
  abandonWorkflowFundingReservationTransaction,
  forfeitWorkflowFundingCollateral,
  readWorkflowFundingRecordedSignedTransaction,
  readWorkflowFundingRecovery,
  restoreWorkflowFundingCollateral,
  type WorkflowJournalWriter,
} from "./funding-reservation-permit.js";
import type {
  FraudProofWorkflowJournalEntry,
  FraudProofWorkflowJournalEvent,
} from "./journal.js";
import type { FraudProofWorkflowReconcileResult } from "./orchestrator.fraud-proof-family-workflow-adapter.js";
import type { SignedWorkflowTransactionRetirement } from "./signed-transaction-retirement.js";

/**
 * Records `transactionHash` as superseded (or, with a retirement, retired):
 * the funding store keeps its lineage as an exclusion set, so a replacement
 * must spend one of its inputs, and the journal closes the attempt as
 * not_found in the same commit. A later landing is adopted through the
 * superseded-attempt records.
 *
 * Should the exact bytes have landed failing phase 2 at the tip, the caller
 * first accounts the chain's taking of their collateral
 * (`accountAttemptCollateral`), in its own commit, so a rollback past that
 * block can undo exactly that.
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
 * took. Without recorded signed bytes there is no reservation to account in,
 * and nothing is recorded. */
const forfeitWorkflowAttemptCollateral = async ({
  writer,
  actionId,
  transactionHash,
}: {
  readonly writer: WorkflowJournalWriter;
  readonly actionId: string;
  readonly transactionHash: string;
}): Promise<void> => {
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

/** Undoes the standing forfeiture of `transactionHash`: a rollback past the
 * block where it landed undoes the collateral accounting done at that tip. */
const restoreWorkflowAttemptCollateral = async ({
  writer,
  actionId,
  transactionHash,
}: {
  readonly writer: WorkflowJournalWriter;
  readonly actionId: string;
  readonly transactionHash: string;
}): Promise<void> =>
  await restoreWorkflowFundingCollateral({
    writer,
    transactionHash,
    events: [
      {
        kind: "collateral_forfeiture_reversed",
        actionId,
        txHash: transactionHash,
      },
    ],
  });

/** The collateral accounting that one read of the workflow's own attempt
 * calls for (`accountAttemptCollateral`), each step under the family's
 * reconciliation check. Returns the failure, if the accounting failed. */
export const accountWorkflowAttemptCollateral = async ({
  writer,
  assertReconcile,
  entries,
  append,
  intent: { actionId, txHash: transactionHash },
  read,
}: {
  readonly writer: WorkflowJournalWriter;
  readonly assertReconcile: () => void;
  readonly entries: readonly FraudProofWorkflowJournalEntry[];
  readonly append: (event: FraudProofWorkflowJournalEvent) => Promise<unknown>;
  readonly intent: Readonly<{ actionId: string; txHash: string }>;
  readonly read: FraudProofWorkflowReconcileResult;
}): Promise<string | undefined> =>
  await accountAttemptCollateral({
    entries,
    append,
    transactionHash,
    read,
    forfeit: async () => {
      assertReconcile();
      await forfeitWorkflowAttemptCollateral({
        writer,
        actionId,
        transactionHash,
      });
    },
    restore: async () => {
      assertReconcile();
      await restoreWorkflowAttemptCollateral({
        writer,
        actionId,
        transactionHash,
      });
    },
  });
