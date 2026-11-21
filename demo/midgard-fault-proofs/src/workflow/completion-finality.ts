import { DEPLOYMENT_MANIFEST_L1_FINALITY } from "@al-ft/midgard-core/deployment-manifest-identity";

import type { FraudProofWorkflowJournalEvent } from "./journal.fraud-proof-workflow-journal-event.js";

export const isFinalWorkflowCompletion = (
  event: FraudProofWorkflowJournalEvent | undefined,
): event is Extract<FraudProofWorkflowJournalEvent, { kind: "completed" }> =>
  event?.kind === "completed" &&
  event.terminal.observedAt.confirmationDepth >
    DEPLOYMENT_MANIFEST_L1_FINALITY.automaticRecoveryMaxDepth + 1;
