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

type SupersededAttempt = Awaited<
  ReturnType<typeof readLegacyWorkflowFundingAbandonedTransactions>
>[number];

/** Owner ruling (whichever lands wins): a superseded attempt never holds the
 * workflow. Its retirement past k is bookkeeping, and its late landing after a
 * rollback is returned so the caller adopts it as the result. */
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
    saved: SupersededAttempt,
  ) => Promise<FraudProofWorkflowReconcileResult>;
}): Promise<SupersededAttempt | null> => {
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
  readonly savedAttempts: readonly SupersededAttempt[];
  readonly entries: readonly FraudProofWorkflowJournalEntry[];
  readonly append: (event: FraudProofWorkflowJournalEvent) => Promise<unknown>;
  readonly reconcile: (
    saved: SupersededAttempt,
  ) => Promise<FraudProofWorkflowReconcileResult>;
  readonly retire: (
    hash: string,
    retirement: import("./signed-transaction-retirement.js").SignedWorkflowTransactionRetirement,
  ) => Promise<void>;
}): Promise<SupersededAttempt | null> => {
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
    if (result.kind === "confirmed") return saved;
    // Unresolved, absent or impossible at the tip: no hold, try again later.
    if (
      result.kind === "conflict" ||
      result.kind === "unknown" ||
      result.retirement === undefined
    )
      continue;
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
  return null;
};
