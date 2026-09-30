import { type CanonicalBlockClassification } from "./classification.js";
import {
  type FraudProofWorkflowIdentity,
  type FraudProofWorkflowJournalEntry,
  type FraudProofWorkflowJournalEvent,
  type FraudProofWorkflowTerminal,
} from "./journal.js";

export const attemptCount = (
  entries: readonly FraudProofWorkflowJournalEntry[],
  actionId: string,
): number =>
  entries.filter(
    (entry) =>
      entry.event.kind === "submission_intent" &&
      entry.event.actionId === actionId,
  ).length;

export const latestSubmissionIntent = (
  entries: readonly FraudProofWorkflowJournalEntry[],
  actionId: string,
):
  | Extract<
      FraudProofWorkflowJournalEvent,
      { readonly kind: "submission_intent" }
    >
  | undefined =>
  [...entries]
    .reverse()
    .map((entry) => entry.event)
    .find(
      (
        event,
      ): event is Extract<
        FraudProofWorkflowJournalEvent,
        { readonly kind: "submission_intent" }
      > => event.kind === "submission_intent" && event.actionId === actionId,
    );

export type FraudProofWorkflowRunResult =
  | {
      readonly kind: "no_fault_detected" | "unprovable_gap";
      readonly classification: CanonicalBlockClassification;
    }
  | {
      readonly kind: "terminal_included";
      readonly workflowId: string;
      readonly identity: FraudProofWorkflowIdentity;
      readonly terminal: FraudProofWorkflowTerminal;
      readonly entries: readonly FraudProofWorkflowJournalEntry[];
    }
  | {
      readonly kind: "completed";
      readonly workflowId: string;
      readonly identity: FraudProofWorkflowIdentity;
      readonly terminal: FraudProofWorkflowTerminal;
      readonly entries: readonly FraudProofWorkflowJournalEntry[];
    }
  | {
      readonly kind: "pending" | "stalled";
      /** Keep the objective active and resume on fresh chain observation. */
      readonly resumeOnObservation?: true;
      readonly workflowId: string;
      readonly identity: FraudProofWorkflowIdentity;
      readonly reason: string;
      readonly entries: readonly FraudProofWorkflowJournalEntry[];
      /**
       * Present when a stalled run failed while the adapter built the next
       * transaction. That transaction targets the live L1 tip while the stage
       * it acts on is release-final, so the stall may only mean the
       * authenticated observation still trails the tip; the caller may resume
       * once it has caught up.
       */
      readonly phase?: "preflight";
    };
