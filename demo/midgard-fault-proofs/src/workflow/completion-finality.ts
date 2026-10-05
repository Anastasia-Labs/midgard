import { DEPLOYMENT_MANIFEST_L1_FINALITY } from "@al-ft/midgard-core/deployment-manifest-identity";

import type { FraudProofWorkflowJournalEvent } from "./journal.fraud-proof-workflow-journal-event.js";
import {
  journalJsonDigest,
  normalizeJournalJson,
} from "./journal.fraud-proof-workflow-terminal.js";

export const isFinalWorkflowCompletion = (
  event: FraudProofWorkflowJournalEvent | undefined,
): event is Extract<FraudProofWorkflowJournalEvent, { kind: "completed" }> =>
  event?.kind === "completed" &&
  event.terminal.observedAt.confirmationDepth >
    DEPLOYMENT_MANIFEST_L1_FINALITY.automaticRecoveryMaxDepth + 1;

export const terminalFactsWithoutDepth = (
  value: import("./journal.fraud-proof-workflow-terminal.js").FraudProofWorkflowTerminal,
) => ({
  ...value,
  observedAt: {
    slot: value.observedAt.slot,
    blockHash: value.observedAt.blockHash,
  },
});

export const assertFinalWorkflowTerminalReceipt = (
  saved: Parameters<typeof terminalFactsWithoutDepth>[0],
  observed: Parameters<typeof terminalFactsWithoutDepth>[0],
): void => {
  if (
    observed.observedAt.confirmationDepth <
      saved.observedAt.confirmationDepth ||
    journalJsonDigest(
      normalizeJournalJson(terminalFactsWithoutDepth(saved)),
    ) !==
      journalJsonDigest(
        normalizeJournalJson(terminalFactsWithoutDepth(observed)),
      )
  )
    throw new Error(
      "released workflow terminal facts changed on the canonical chain",
    );
};
