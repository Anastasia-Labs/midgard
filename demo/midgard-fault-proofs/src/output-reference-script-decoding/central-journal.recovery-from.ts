import { type FraudProofWorkflowJournalEntry } from "../workflow/journal.js";
import type {
  OutputReferenceScriptDecodingAction,
  OutputReferenceScriptDecodingStage,
} from "./workflow.js";

export type SubmitAction = Exclude<OutputReferenceScriptDecodingAction, "done">;

export type DurableRecovery = Readonly<{
  familyIdentity: string;
  sourceStage: OutputReferenceScriptDecodingStage;
  targetStage: OutputReferenceScriptDecodingStage;
  auxiliary?: boolean;
}>;

export const TX_HASH = /^[0-9a-f]{64}$/u;

const stages: readonly OutputReferenceScriptDecodingStage[] = [
  "none",
  "step01",
  "step02",
  "outputScan",
  "referenceBind",
  "scan",
  "step06",
  "proven",
  "removed",
  "cancelled",
];

export const actionId = (action: SubmitAction): string =>
  `outputReferenceScriptDecoding:${action}`;

export const now = (): string => new Date().toISOString();

export const recoveryFrom = (
  entry: FraudProofWorkflowJournalEntry,
): DurableRecovery => {
  if (entry.event.kind !== "submission_intent") {
    throw new Error(
      "outputReferenceScriptDecoding journal entry is not an intent",
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
    !stages.includes(sourceStage as OutputReferenceScriptDecodingStage) ||
    !stages.includes(targetStage as OutputReferenceScriptDecodingStage)
  ) {
    throw new Error(
      "outputReferenceScriptDecoding durable intent is incomplete",
    );
  }
  return {
    familyIdentity,
    sourceStage: sourceStage as OutputReferenceScriptDecodingStage,
    targetStage: targetStage as OutputReferenceScriptDecodingStage,
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
