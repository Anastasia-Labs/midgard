import { createHash } from "node:crypto";

import {
  FRAUD_PROOF_WORKFLOW_IDENTITY_SCHEMA_VERSION,
  type FraudProofWorkflowIdentity,
  type FraudProofWorkflowJournalEntry,
} from "../workflow/journal.js";
import type {
  MintItemAction,
  MintItemStage,
} from "./mint-item-non-canonical.js";

export const familyWorkflowId = (
  identity: FraudProofWorkflowIdentity,
): string => {
  if (!/^[0-9a-f]{64}$/u.test(identity.deploymentFingerprint))
    throw new Error("mintItemNonCanonical deployment fingerprint is invalid");
  if (
    identity.target.kind !== "state_queue_header" ||
    !/^[0-9a-f]{56}$/u.test(identity.target.headerHash)
  )
    throw new Error("mintItemNonCanonical target header hash is invalid");
  return createHash("sha256")
    .update(
      [
        FRAUD_PROOF_WORKFLOW_IDENTITY_SCHEMA_VERSION,
        identity.deploymentFingerprint,
        "mintItemNonCanonical",
        `header:${identity.target.headerHash}`,
        ...(identity.decisionDigest === undefined
          ? []
          : [`decision:${identity.decisionDigest}`]),
      ].join("\u0000"),
    )
    .digest("hex");
};

export type SubmitAction = Exclude<MintItemAction, "done">;

export type MintItemRemovalAction = Readonly<{
  nextRemovalOutRef: string;
  fraudProofOutRef: string;
}>;

export type DurableRecovery = Readonly<{
  familyIdentity: string;
  sourceStage: MintItemStage;
  targetStage: MintItemStage;
  auxiliary?: boolean;
}>;

export const TX_HASH = /^[0-9a-f]{64}$/u;

const stages: readonly MintItemStage[] = [
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
  `mintItemNonCanonical:${action}`;

export const now = (): string => new Date().toISOString();

export const recoveryFrom = (
  entry: FraudProofWorkflowJournalEntry,
): DurableRecovery => {
  if (entry.event.kind !== "submission_intent") {
    throw new Error("mintItemNonCanonical journal entry is not an intent");
  }
  const value = entry.event.durableRecovery;
  const familyIdentity = value?.familyIdentity;
  const sourceStage = value?.sourceStage;
  const targetStage = value?.targetStage;
  if (
    typeof familyIdentity !== "string" ||
    typeof sourceStage !== "string" ||
    typeof targetStage !== "string" ||
    !stages.includes(sourceStage as MintItemStage) ||
    !stages.includes(targetStage as MintItemStage)
  ) {
    throw new Error("mintItemNonCanonical durable intent is incomplete");
  }
  return {
    familyIdentity,
    sourceStage: sourceStage as MintItemStage,
    targetStage: targetStage as MintItemStage,
    ...(value?.auxiliary === true ? { auxiliary: true } : {}),
  };
};

const actionFinishedAfter = (
  entries: readonly FraudProofWorkflowJournalEntry[],
  sequence: number,
  wantedActionId: string,
): boolean =>
  (() => {
    const latest = entries
      .slice(sequence + 1)
      .reverse()
      .find(
        ({ event }) => "actionId" in event && event.actionId === wantedActionId,
      )?.event;
    return (
      latest?.kind === "confirmed" ||
      (latest?.kind === "reconciled" &&
        latest.outcome === "not_found" &&
        (latest.retirement !== undefined ||
          entries.some(
            ({ event }) =>
              event.kind === "signed_attempt_retired" &&
              event.txHash === latest.txHash,
          )))
    );
  })();

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
