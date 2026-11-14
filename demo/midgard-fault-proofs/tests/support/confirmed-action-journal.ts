import type { FraudProofCatalogueCategoryName } from "@al-ft/midgard-sdk";

import {
  FRAUD_PROOF_WORKFLOW_IDENTITY_SCHEMA_VERSION,
  FRAUD_PROOF_WORKFLOW_JOURNAL_SCHEMA_VERSION,
  type FraudProofWorkflowJournalEntry,
  type JournalJsonObject,
} from "../../src/workflow/journal.js";
import type { FraudProofWorkflowAction } from "../../src/workflow/orchestrator.js";

/**
 * The journal a workflow keeps for the actions a test drives through a port
 * by hand: each confirmed action is its submission intent (with the recovery
 * the port captured) followed by its confirmation, as the orchestrator
 * records them.
 */
export const createConfirmedActionJournal = ({
  category,
  headerHash,
}: {
  readonly category: FraudProofCatalogueCategoryName;
  readonly headerHash: string;
}) => {
  const entries: FraudProofWorkflowJournalEntry[] = [];
  const identity = {
    schemaVersion: FRAUD_PROOF_WORKFLOW_IDENTITY_SCHEMA_VERSION,
    deploymentFingerprint: "00".repeat(32),
    category,
    target: { kind: "state_queue_header" as const, headerHash },
  };
  const append = (event: FraudProofWorkflowJournalEntry["event"]) => {
    entries.push({
      schemaVersion: FRAUD_PROOF_WORKFLOW_JOURNAL_SCHEMA_VERSION,
      workflowId: `${category}:${headerHash}`,
      identity,
      sequence: entries.length,
      recordedAt: new Date(entries.length * 1_000).toISOString(),
      event,
    });
  };
  return {
    entries: (): readonly FraudProofWorkflowJournalEntry[] => [...entries],
    confirm: ({
      action,
      txHash,
      durableRecovery,
    }: {
      readonly action: FraudProofWorkflowAction;
      readonly txHash: string;
      readonly durableRecovery?: JournalJsonObject;
    }) => {
      append({
        kind: "submission_intent",
        actionId: action.actionId,
        actionInput: action.input,
        ...(durableRecovery === undefined ? {} : { durableRecovery }),
        attempt: 1,
        txHash,
      });
      append({ kind: "confirmed", actionId: action.actionId, txHash });
    },
  };
};
