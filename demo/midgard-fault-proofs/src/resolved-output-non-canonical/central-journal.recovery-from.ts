import { type FraudProofWorkflowJournalEntry } from "../workflow/journal.js";
import type { ResolvedOutputAction, ResolvedOutputStage } from "./workflow.js";

export type SubmitAction = Exclude<ResolvedOutputAction, "done">;

export type DurableRecovery = Readonly<{
  familyIdentity: string;
  sourceStage: ResolvedOutputStage;
  targetStage: ResolvedOutputStage;
  auxiliary?: boolean;
}>;

export const TX_HASH = /^[0-9a-f]{64}$/u;

const stages: readonly ResolvedOutputStage[] = [
  "none",
  "step01",
  "step02",
  "step03",
  "reconstructing",
  "step05",
  "proven",
  "removed",
  "cancelled",
];

export const actionId = (action: SubmitAction): string =>
  `resolvedOutputNonCanonical:${action}`;

export const now = (): string => new Date().toISOString();

export const recoveryFrom = (
  entry: FraudProofWorkflowJournalEntry,
): DurableRecovery => {
  if (entry.event.kind !== "submission_intent") {
    throw new Error(
      "resolvedOutputNonCanonical journal entry is not an intent",
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
    !stages.includes(sourceStage as ResolvedOutputStage) ||
    !stages.includes(targetStage as ResolvedOutputStage)
  ) {
    throw new Error("resolvedOutputNonCanonical durable intent is incomplete");
  }
  return {
    familyIdentity,
    sourceStage: sourceStage as ResolvedOutputStage,
    targetStage: targetStage as ResolvedOutputStage,
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
