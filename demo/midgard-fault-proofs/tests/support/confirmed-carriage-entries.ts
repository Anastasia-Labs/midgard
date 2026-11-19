import type { UTxO } from "@lucid-evolution/lucid";

import {
  FRAUD_PROOF_WORKFLOW_JOURNAL_SCHEMA_VERSION,
  type FraudProofWorkflowIdentity,
  type FraudProofWorkflowJournalEntry,
} from "../../src/workflow/journal.js";

/**
 * A journal that confirmed every transaction with an output among `utxos`,
 * except the `foreignTxHashes` another workflow made: what a workflow that
 * published that carriage itself has journaled.
 */
export const confirmedCarriageEntries = ({
  utxos,
  foreignTxHashes = [],
  workflowId,
  identity,
}: {
  readonly utxos: readonly UTxO[];
  readonly foreignTxHashes?: readonly string[];
  readonly workflowId: string;
  readonly identity: FraudProofWorkflowIdentity;
}): FraudProofWorkflowJournalEntry[] =>
  [...new Set(utxos.map(({ txHash }) => txHash))]
    .filter((txHash) => !foreignTxHashes.includes(txHash))
    .map((txHash, sequence) => ({
      schemaVersion: FRAUD_PROOF_WORKFLOW_JOURNAL_SCHEMA_VERSION,
      workflowId,
      identity,
      recordedAt: new Date(0).toISOString(),
      sequence,
      event: { kind: "confirmed", actionId: `chain:${txHash}`, txHash },
    }));
