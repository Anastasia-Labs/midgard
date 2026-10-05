import {
  abandonWorkflowFundingReservationTransaction,
  acknowledgeWorkflowFundingAbandonment,
  assertWorkflowFundingReservationReadyToSubmit,
  confirmWorkflowFundingReservationTransaction,
  conflictWorkflowFundingReservationTransaction,
  createWorkflowFundingAbandonmentHandoff,
  readWorkflowFundingRecovery,
  reobserveWorkflowFundingReservationTransaction,
} from "../workflow/funding-reservation-permit.js";
import { reconcileLegacyWorkflowFundingAbandonment } from "../workflow/funding-reservation-permit.reopen-legacy-abandonment.js";
import type {
  FraudProofWorkflowJournalEntry,
  FraudProofWorkflowJournalStore,
} from "../workflow/journal.js";
import { lastActionEvent } from "../workflow/orchestrator.normalize-workflow-terminal.js";
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
    store,
    entries,
    appendEvent,
    assertActuation,
    transactionConfirmed,
    observeSignedTransaction,
    rebroadcastSignedTransaction,
  }: {
    readonly store: FraudProofWorkflowJournalStore;
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
    // Superseded attempts never hold this journal. Stage observation drives
    // the mint item, so a late-landed superseded attempt is read from the
    // chain rather than adopted into the journal.
    await reconcileLegacyWorkflowFundingAbandonment({
      journal: store,
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
      if (
        !(await reobserveWorkflowFundingReservationTransaction({
          journal: store,
          transactionHash: reverted.event.txHash,
        }))
      )
        throw new MintItemWorkflowRecoveryPendingError(
          "An unresolved descendant retains the funding inputs",
        );
      await appendEvent({
        kind: "reobserved",
        actionId: reverted.event.actionId,
        txHash: reverted.event.txHash,
      });
    }
    const intent = unresolvedIntent(await entries());
    if (intent?.event.kind !== "submission_intent") return;
    assertActuation("before_reconcile");
    const recovery = recoveryFrom(intent);
    const intentActionId = intent.event.actionId;
    const confirmed = await transactionConfirmed(intent.event.txHash);
    if (confirmed && observedStage === recovery.targetStage) {
      await confirmWorkflowFundingReservationTransaction({
        journal: store,
        transactionHash: intent.event.txHash,
      });
      await appendEvent({
        kind: "reconciled",
        actionId: intent.event.actionId,
        outcome: "confirmed",
        txHash: intent.event.txHash,
      });
      await appendEvent({
        kind: "confirmed",
        actionId: intent.event.actionId,
        txHash: intent.event.txHash,
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
      const handoff = createWorkflowFundingAbandonmentHandoff({
        entries: await entries(),
        transactionHash: intent.event.txHash,
        retirement: result.retirement,
      });
      await abandonWorkflowFundingReservationTransaction({
        journal: store,
        transactionHash: intent.event.txHash,
        handoff,
      });
      await appendEvent(handoff.reconciliation);
      await acknowledgeWorkflowFundingAbandonment({ journal: store, handoff });
      return;
    }
    await conflictWorkflowFundingReservationTransaction({
      journal: store,
      transactionHash: intent.event.txHash,
    });
    throw new Error(
      "mintItemNonCanonical authenticated stage/transaction identity substitution",
    );
  };
