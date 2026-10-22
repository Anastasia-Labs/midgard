import {
  assertWorkflowFundingReservationReadyToSubmit,
  confirmWorkflowFundingReservationTransaction,
  conflictWorkflowFundingReservationTransaction,
  readWorkflowFundingRecovery,
  reobserveWorkflowFundingReservationTransaction,
  workflowJournalHasFundingReservation,
  type WorkflowJournalWriter,
} from "../workflow/funding-reservation-permit.js";
import { reconcileWorkflowFundingSupersededAttempts } from "../workflow/funding-reservation-permit.reconcile-superseded-attempts.js";
import type { FraudProofWorkflowJournalEntry } from "../workflow/journal.js";
import { lastActionEvent } from "../workflow/orchestrator.normalize-workflow-terminal.js";
import { supersedeWorkflowFundingAttempt } from "../workflow/orchestrator.supersede-funding-attempt.js";
import { reconcileSignedWorkflowTransaction } from "../workflow/signed-transaction-reconciliation.js";
import {
  recoveryFrom,
  unresolvedIntent,
} from "./central-journal.recovery-from.js";
import type { MintItemStage } from "./mint-item-non-canonical.js";

export class MintItemWorkflowRecoveryPendingError extends Error {}

/** Reconcile the retained signed receipt before allowing the family to build. */
export const createMintItemCanonicalReconciliation =
  ({
    writer,
    entries,
    appendEvent,
    assertActuation,
    transactionConfirmed,
    observeSignedTransaction,
    rebroadcastSignedTransaction,
  }: {
    readonly writer: WorkflowJournalWriter;
    readonly entries: () => Promise<readonly FraudProofWorkflowJournalEntry[]>;
    readonly appendEvent: (
      event: FraudProofWorkflowJournalEntry["event"],
    ) => Promise<void>;
    readonly assertActuation: (
      checkpoint: "before_reconcile" | "before_submit",
    ) => void;
    readonly transactionConfirmed: (txHash: string) => Promise<boolean>;
    readonly observeSignedTransaction?: Parameters<
      typeof reconcileSignedWorkflowTransaction
    >[0]["observe"];
    readonly rebroadcastSignedTransaction?: Parameters<
      typeof reconcileSignedWorkflowTransaction
    >[0]["rebroadcast"];
  }) =>
  async (observedStage: MintItemStage): Promise<void> => {
    const store = writer.journal;
    // Superseded attempts never hold this journal. Stage observation drives
    // the mint item, so a late-landed superseded attempt is read from the
    // chain rather than adopted into the journal.
    await reconcileWorkflowFundingSupersededAttempts({
      writer,
      entries: await entries(),
      append: appendEvent,
      reconcile: async ({ transition }) =>
        await reconcileSignedWorkflowTransaction({
          ...transition,
          observe: observeSignedTransaction,
        }),
    });
    const current = await entries();
    const reverted = [...current].reverse().find((entry) => {
      if (entry.event.kind !== "submission_intent") return false;
      const recovery = recoveryFrom(entry);
      return (
        recovery.auxiliary !== true &&
        recovery.sourceStage === observedStage &&
        lastActionEvent(current, entry.event.actionId)?.kind === "confirmed"
      );
    });
    if (
      reverted?.event.kind === "submission_intent" &&
      !(await transactionConfirmed(reverted.event.txHash))
    ) {
      // The reobserved intent displaces a pending descendant. It is
      // superseded, never silently dropped: a dropped attempt would count as
      // live coverage and its later reconciliation would wedge on the
      // changed pending identity.
      const displaced = await readWorkflowFundingRecovery(store);
      const pending = displaced.transition?.transactionHash;
      if (pending !== undefined && pending !== reverted.event.txHash)
        await supersedeWorkflowFundingAttempt({
          writer,
          assertReconcile: () => assertActuation("before_reconcile"),
          entries,
          transactionHash: pending,
          retirement: undefined,
        });
      assertActuation("before_reconcile");
      if (
        !(await reobserveWorkflowFundingReservationTransaction({
          writer,
          transactionHash: reverted.event.txHash,
          events: [
            {
              kind: "reobserved",
              actionId: reverted.event.actionId,
              txHash: reverted.event.txHash,
            },
          ],
        }))
      )
        throw new MintItemWorkflowRecoveryPendingError(
          "An unresolved descendant retains the funding inputs",
        );
    }
    const intent = unresolvedIntent(await entries(), store);
    if (intent?.event.kind !== "submission_intent") return;
    assertActuation("before_reconcile");
    const recovery = recoveryFrom(intent);
    const intentActionId = intent.event.actionId;
    const confirmed = await transactionConfirmed(intent.event.txHash);
    if (confirmed && observedStage === recovery.targetStage) {
      await confirmWorkflowFundingReservationTransaction({
        writer,
        transactionHash: intent.event.txHash,
        events: [
          {
            kind: "reconciled",
            actionId: intent.event.actionId,
            outcome: "confirmed",
            txHash: intent.event.txHash,
          },
          {
            kind: "confirmed",
            actionId: intent.event.actionId,
            txHash: intent.event.txHash,
          },
        ],
      });
      return;
    }
    if (!confirmed) {
      const funding = await readWorkflowFundingRecovery(store);
      const recorded = funding.transition;
      const result = await reconcileSignedWorkflowTransaction({
        transactionHash: intent.event.txHash,
        signedTransactionCborHex:
          recorded?.transactionHash === intent.event.txHash
            ? recorded.signedTransactionCborHex
            : undefined,
        observe: observeSignedTransaction,
        rebroadcast: rebroadcastSignedTransaction,
        authorizeResubmission: async (signed) => {
          if (
            recorded === null ||
            signed.transactionHash !== recorded.transactionHash ||
            signed.signedTransactionCborHex !==
              recorded.signedTransactionCborHex
          )
            throw new Error(
              "Mint recovery changed the durable signed transaction",
            );
          assertActuation("before_submit");
          await assertWorkflowFundingReservationReadyToSubmit({
            journal: store,
            transactionHash: signed.transactionHash,
          });
          const broadcasts = (await entries()).filter(
            ({ event }) =>
              event.kind === "rebroadcast_intent" &&
              event.txHash === signed.transactionHash,
          ).length;
          await appendEvent({
            kind: "rebroadcast_intent",
            actionId: intentActionId,
            txHash: signed.transactionHash,
            attempt: broadcasts + 2,
          });
          assertActuation("before_submit");
        },
      });
      if (result.kind !== "not_found")
        throw new MintItemWorkflowRecoveryPendingError(
          `Exact mint transaction remains unresolved: ${result.kind}`,
        );
      // As in the workflow path: without a bound funding reservation nothing
      // makes a replacement spend this attempt's inputs, so absence without
      // retirement is held until retirement past k and is not recorded.
      if (
        result.retirement === undefined &&
        !workflowJournalHasFundingReservation(store)
      )
        throw new MintItemWorkflowRecoveryPendingError(
          "Exact mint transaction is absent without retirement, and no funding reservation keeps a replacement exclusive",
        );
      await supersedeWorkflowFundingAttempt({
        writer,
        assertReconcile: () => assertActuation("before_reconcile"),
        entries,
        transactionHash: intent.event.txHash,
        retirement: result.retirement,
      });
      return;
    }
    await conflictWorkflowFundingReservationTransaction({
      writer,
      transactionHash: intent.event.txHash,
      events: [],
    });
    throw new Error(
      "mintItemNonCanonical authenticated stage/transaction identity substitution",
    );
  };
