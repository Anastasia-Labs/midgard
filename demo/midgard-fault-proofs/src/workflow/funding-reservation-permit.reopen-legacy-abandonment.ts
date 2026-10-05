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
import {
  type SupersededAttemptReadSchedule,
  supersededAttemptReadSchedule,
} from "./superseded-attempt-read-schedule.js";

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
  nowMs = Date.now(),
  schedule = supersededAttemptReadSchedule,
}: {
  readonly journal: object;
  readonly entries: readonly FraudProofWorkflowJournalEntry[];
  readonly append: (event: FraudProofWorkflowJournalEvent) => Promise<unknown>;
  readonly reconcile: (
    saved: SupersededAttempt,
  ) => Promise<FraudProofWorkflowReconcileResult>;
  readonly nowMs?: number;
  readonly schedule?: SupersededAttemptReadSchedule;
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
    reads: { schedule, nowMs },
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
  reads,
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
  /** Bounds the L1 reads of one pass; without it every attempt is read. */
  readonly reads?: Readonly<{
    schedule: SupersededAttemptReadSchedule;
    nowMs: number;
  }>;
}): Promise<SupersededAttempt | null> => {
  const unread: SupersededAttempt[] = [];
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
    unread.push(saved);
  }
  const due =
    reads === undefined
      ? null
      : reads.schedule.due(
          unread.map(({ transition }) => transition.transactionHash),
          reads.nowMs,
        );
  for (const saved of unread) {
    const hash = saved.transition.transactionHash;
    if (due !== null && !due.includes(hash)) continue;
    const result = await reconcile(saved);
    if (
      result.kind === "confirmed" ||
      result.kind === "conflict" ||
      result.kind === "unknown" ||
      result.retirement === undefined
    ) {
      // Unresolved, absent or impossible at the tip: no hold, read again
      // later. A landing backs off too until the caller adopts it.
      reads?.schedule.unresolved(hash, reads.nowMs);
      if (result.kind === "confirmed") return saved;
      continue;
    }
    reads?.schedule.forget(hash);
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
