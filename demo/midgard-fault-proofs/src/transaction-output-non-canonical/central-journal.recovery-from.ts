import { createHash } from "node:crypto";

import {
  FRAUD_PROOF_WORKFLOW_IDENTITY_SCHEMA_VERSION,
  type FraudProofWorkflowIdentity,
  type FraudProofWorkflowJournalEntry,
} from "../workflow/journal.js";
import type {
  TransactionOutputAction,
  TransactionOutputStage,
} from "./transaction-output-non-canonical.js";

export const familyWorkflowId = (
  identity: FraudProofWorkflowIdentity,
): string => {
  if (!/^[0-9a-f]{64}$/u.test(identity.deploymentFingerprint))
    throw new Error(
      "transactionOutputNonCanonical deployment fingerprint is invalid",
    );
  if (
    identity.target.kind !== "state_queue_header" ||
    !/^[0-9a-f]{56}$/u.test(identity.target.headerHash)
  )
    throw new Error(
      "transactionOutputNonCanonical target header hash is invalid",
    );
  return createHash("sha256")
    .update(
      [
        FRAUD_PROOF_WORKFLOW_IDENTITY_SCHEMA_VERSION,
        identity.deploymentFingerprint,
        "transactionOutputNonCanonical",
        `header:${identity.target.headerHash}`,
        ...(identity.decisionDigest === undefined
          ? []
          : [`decision:${identity.decisionDigest}`]),
      ].join("\u0000"),
    )
    .digest("hex");
};

export type SubmitAction = Exclude<TransactionOutputAction, "done">;

export type DurableRecovery = Readonly<{
  familyIdentity: string;
  sourceStage: TransactionOutputStage;
  targetStage: TransactionOutputStage;
  auxiliary?: boolean;
}>;

export const TX_HASH = /^[0-9a-f]{64}$/u;

const stages: readonly TransactionOutputStage[] = [
  "none",
  "step01",
  "step02",
  "step03",
  "step04",
  "proven",
  "removed",
  "cancelled",
];

export const actionId = (action: SubmitAction): string =>
  `transactionOutputNonCanonical:${action}`;

export const now = (): string => new Date().toISOString();

export const recoveryFrom = (
  entry: FraudProofWorkflowJournalEntry,
): DurableRecovery => {
  if (entry.event.kind !== "submission_intent") {
    throw new Error(
      "transactionOutputNonCanonical journal entry is not an intent",
    );
  }
  const value = entry.event.durableRecovery;
  const familyIdentity = value?.familyIdentity;
  const sourceStage = value?.sourceStage;
  const targetStage = value?.targetStage;
  if (
    typeof familyIdentity !== "string" ||
    typeof sourceStage !== "string" ||
    typeof targetStage !== "string" ||
    !stages.includes(sourceStage as TransactionOutputStage) ||
    !stages.includes(targetStage as TransactionOutputStage)
  ) {
    throw new Error(
      "transactionOutputNonCanonical durable intent is incomplete",
    );
  }
  return {
    familyIdentity,
    sourceStage: sourceStage as TransactionOutputStage,
    targetStage: targetStage as TransactionOutputStage,
    ...(value?.auxiliary === true ? { auxiliary: true } : {}),
  };
};

const actionFinishedAfter = (
  entries: readonly FraudProofWorkflowJournalEntry[],
  sequence: number,
  wantedActionId: string,
): boolean =>
  entries.slice(sequence + 1).some(({ event }) => {
    if (!("actionId" in event) || event.actionId !== wantedActionId) {
      return false;
    }
    return (
      event.kind === "confirmed" ||
      (event.kind === "reconciled" && event.outcome === "not_found")
    );
  });

export const unresolvedIntent = (
  entries: readonly FraudProofWorkflowJournalEntry[],
): FraudProofWorkflowJournalEntry | undefined =>
  [...entries].reverse().find((entry) => {
    const event = entry.event;
    return (
      event.kind === "submission_intent" &&
      !actionFinishedAfter(entries, entry.sequence, event.actionId)
    );
  });
