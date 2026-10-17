import { assertWorkflowJournalActuation } from "./actuation-permit.js";
import { reconcileLegacyWorkflowFundingAbandonment } from "./funding-reservation-permit.reopen-legacy-abandonment.js";
import type { FraudProofWorkflowJournalEvent } from "./journal.js";
import type {
  FraudProofFamilyWorkflowAdapter,
  FraudProofWorkflowAdapterContext,
} from "./orchestrator.fraud-proof-family-workflow-adapter.js";
export const reconcileLegacyFraudProofAbandonments = async (
  context: FraudProofWorkflowAdapterContext,
  journal: object,
  adapter: FraudProofFamilyWorkflowAdapter,
  append: (event: FraudProofWorkflowJournalEvent) => Promise<unknown>,
  nowMs: number,
) =>
  await reconcileLegacyWorkflowFundingAbandonment({
    journal,
    entries: context.entries,
    append,
    nowMs,
    reconcile: async ({ transition, handoff }) =>
      await adapter.reconcile({
        ...context,
        retirementOnly: true,
        action: {
          actionId: handoff.submissionIntent.actionId,
          input: handoff.submissionIntent.actionInput,
        },
        txHash: transition.transactionHash,
        signedTransactionCborHex: transition.signedTransactionCborHex,
        durableRecovery: handoff.submissionIntent.durableRecovery,
      }),
  });

export const assertWorkflowJournalReconciliation = (
  journal: import("./journal.js").FraudProofWorkflowJournalStore,
  identity: FraudProofWorkflowAdapterContext["identity"],
) => {
  if (identity.target.kind !== "state_queue_header")
    throw new Error("Reconciliation requires the state queue target");
  assertWorkflowJournalActuation({
    journal,
    deploymentFingerprint: identity.deploymentFingerprint,
    category: identity.category,
    headerHash: identity.target.headerHash,
    checkpoint: "before_reconcile",
  });
};
