import { retireLegacyWorkflowFundingAbandonment } from "./funding-reservation-permit.apply-transition.js";
import {
  readLegacyWorkflowFundingAbandonedTransactions,
  readWorkflowFundingRecovery,
} from "./funding-reservation-permit.read-workflow-funding-recovery.js";
import type {
  FraudProofWorkflowJournalEntry,
  FraudProofWorkflowJournalEvent,
} from "./journal.js";
import type { FraudProofWorkflowReconcileResult } from "./orchestrator.js";
import { parseSignedWorkflowTransactionRetirement } from "./signed-transaction-retirement.js";

export const reconcileLegacyWorkflowFundingAbandonment = async ({
  journal,
  entries,
  append,
  reconcile,
}: {
  readonly journal: object;
  readonly entries: readonly FraudProofWorkflowJournalEntry[];
  readonly append: (event: FraudProofWorkflowJournalEvent) => Promise<unknown>;
  readonly reconcile: (
    saved: Awaited<
      ReturnType<typeof readLegacyWorkflowFundingAbandonedTransactions>
    >[number],
  ) => Promise<FraudProofWorkflowReconcileResult>;
}): Promise<boolean> => {
  const current = await readWorkflowFundingRecovery(journal);
  return await reconcileLegacyFundingAbandonmentRecords({
    // The current certified handoff has its own journal acknowledgement path.
    savedAttempts: (
      await readLegacyWorkflowFundingAbandonedTransactions(journal)
    ).filter(
      ({ transition }) =>
        current.abandonmentHandoff === null ||
        transition.transactionHash !== current.transition?.transactionHash,
    ),
    entries,
    append,
    reconcile,
    retire: async (transactionHash, retirement) =>
      await retireLegacyWorkflowFundingAbandonment({
        journal,
        transactionHash,
        retirement,
      }),
  });
};

export const reconcileLegacyFundingAbandonmentRecords = async ({
  savedAttempts,
  entries,
  append,
  reconcile,
  retire,
}: {
  readonly savedAttempts: Awaited<
    ReturnType<typeof readLegacyWorkflowFundingAbandonedTransactions>
  >;
  readonly entries: readonly FraudProofWorkflowJournalEntry[];
  readonly append: (event: FraudProofWorkflowJournalEvent) => Promise<unknown>;
  readonly reconcile: (
    saved: Awaited<
      ReturnType<typeof readLegacyWorkflowFundingAbandonedTransactions>
    >[number],
  ) => Promise<FraudProofWorkflowReconcileResult>;
  readonly retire: (
    hash: string,
    retirement: import("./signed-transaction-retirement.js").SignedWorkflowTransactionRetirement,
  ) => Promise<void>;
}): Promise<boolean> => {
  for (const saved of savedAttempts) {
    const hash = saved.transition.transactionHash;
    if (
      !entries.some(
        ({ event }) =>
          event.kind === "submission_intent" && event.txHash === hash,
      )
    )
      throw new Error("Legacy abandonment has no exact durable signed intent");
    if (
      entries.some(
        ({ event }) =>
          (event.kind === "signed_attempt_retired" && event.txHash === hash) ||
          (event.kind === "reconciled" &&
            event.txHash === hash &&
            event.retirement !== undefined),
      )
    )
      continue;
    if (saved.handoff.reconciliation.retirement !== undefined) {
      await append({
        kind: "signed_attempt_retired",
        txHash: hash,
        retirement: saved.handoff.reconciliation.retirement,
      });
      continue;
    }
    const result = await reconcile(saved);
    if (
      (result.kind !== "not_found" &&
        result.kind !== "confirmed" &&
        result.kind !== "pending") ||
      result.retirement === undefined
    )
      return false;
    const retirement = parseSignedWorkflowTransactionRetirement(
      result.retirement,
      hash,
    );
    await retire(hash, retirement);
    await append({
      kind: "signed_attempt_retired",
      txHash: hash,
      retirement,
    });
  }
  return true;
};
