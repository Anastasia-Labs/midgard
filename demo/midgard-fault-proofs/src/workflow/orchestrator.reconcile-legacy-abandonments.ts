import { assertWorkflowJournalActuation } from "./actuation-permit.js";
import { reconcileLegacyWorkflowFundingAbandonment } from "./funding-reservation-permit.reopen-legacy-abandonment.js";
import type { WorkflowJournalWriter } from "./funding-reservation-permit.workflow-journal-writer.js";
import type { FraudProofWorkflowJournalEvent } from "./journal.js";
import type {
  FraudProofWorkflowAdapterContext,
  FraudProofWorkflowTerminalVerifier,
} from "./orchestrator.fraud-proof-family-workflow-adapter.js";
import { readSignedAttempt } from "./orchestrator.read-signed-attempt.js";

/** Reads each superseded attempt by its own signed bytes alone. */
export const reconcileLegacyFraudProofAbandonments = async (
  context: FraudProofWorkflowAdapterContext,
  writer: WorkflowJournalWriter,
  verifier: Pick<
    FraudProofWorkflowTerminalVerifier,
    "observeSignedTransaction"
  >,
  append: (event: FraudProofWorkflowJournalEvent) => Promise<unknown>,
  nowMs: number,
) =>
  await reconcileLegacyWorkflowFundingAbandonment({
    writer,
    entries: context.entries,
    append,
    nowMs,
    reconcile: async ({ transition }) =>
      await readSignedAttempt(verifier, transition),
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
