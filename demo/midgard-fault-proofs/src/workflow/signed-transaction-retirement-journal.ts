import type { FraudProofWorkflowJournalEvent } from "./journal.fraud-proof-workflow-journal-event.js";
import { parseSignedWorkflowTransactionRetirement } from "./signed-transaction-retirement.js";
export const validateJournalRetirement = (
  event: Extract<FraudProofWorkflowJournalEvent, { kind: "reconciled" }>,
) => {
  if (event.outcome !== "not_found" || event.txHash === undefined)
    throw new Error(
      "Retirement receipt requires an exact not_found transaction",
    );
  parseSignedWorkflowTransactionRetirement(event.retirement, event.txHash);
};

export const validateRetiredJournalAttempt = (
  event: Extract<
    FraudProofWorkflowJournalEvent,
    { kind: "signed_attempt_retired" }
  >,
  knownSignedIntentHashes: ReadonlySet<string>,
) => {
  if (
    Object.keys(event).sort().join(",") !== "kind,retirement,txHash" ||
    !knownSignedIntentHashes.has(event.txHash)
  )
    throw new Error(
      "Retired attempt receipt requires its exact historical signed intent",
    );
  parseSignedWorkflowTransactionRetirement(event.retirement, event.txHash);
};
