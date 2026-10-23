import { type CanonicalBlockClassification } from "./classification.js";
import {
  type FraudProofWorkflowIdentity,
  type FraudProofWorkflowJournalEntry,
  type FraudProofWorkflowJournalEvent,
  type FraudProofWorkflowTerminal,
} from "./journal.js";
import type { FraudProofThreadReclaim } from "./own-thread-reclaim.js";
import type { FraudProofResolvedByAnotherRecord } from "./resolved-by-another-record.js";

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

/**
 * Whether a pending reconciliation of `actionId` at `txHash` is what the
 * journal last recorded (diagnostic stalls aside): reconciling the same
 * pending state again is not a new entry, so a wait that lasts many polls
 * keeps the journal bounded.
 */
export const repeatsPendingReconciliation = (
  entries: readonly FraudProofWorkflowJournalEntry[],
  actionId: string,
  txHash: string | undefined,
): boolean => {
  const last = [...entries]
    .reverse()
    .find(({ event }) => event.kind !== "stalled")?.event;
  return (
    last?.kind === "reconciled" &&
    last.outcome === "pending" &&
    last.actionId === actionId &&
    last.txHash === txHash
  );
};

/**
 * The `pending` reason of a run whose next action needs a new submission while
 * it holds reconciliation-only authority (its latest safe start has passed).
 * The run takes no further step by itself; a person resubmits.
 */
export const FRAUD_PROOF_WORKFLOW_FRESH_SUBMISSION_REQUIRED =
  "Canonical workflow requires fresh submission authority";

/**
 * The prefix of the `pending` reason of a run whose session journaled no
 * progress within the continuation bound. The run hands its turn back and
 * resumes on the next observation; it never refuses for this.
 */
export const FRAUD_PROOF_WORKFLOW_CONTINUATION_STALLED =
  "Canonical workflow journaled no progress within its continuation bound";

/** A pending run's own readiness state, named by its family. */
export type FraudProofWorkflowNamedState = Readonly<{
  name: string;
  alarm: boolean;
}>;

/** An action of the run that did not build, and why: the operator's alarm. */
export type FraudProofWorkflowAlarm = Readonly<{
  actionId: string;
  reason: string;
}>;

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
      /**
       * The header was removed against a fraud-proof record this run did not
       * create, and every signed attempt of this run is settled. The
       * objective is done, but it is not our conviction: there is no
       * terminal and no reward is claimed.
       */
      readonly kind: "resolved_by_another_record";
      readonly workflowId: string;
      readonly identity: FraudProofWorkflowIdentity;
      readonly resolution: FraudProofResolvedByAnotherRecord;
      readonly entries: readonly FraudProofWorkflowJournalEntry[];
      /**
       * Our threads on the header that this run cancelled before resolving,
       * their value returned to our wallet: our own deposit back, never a
       * reward. Absent when there were none.
       */
      readonly reclaimed?: readonly FraudProofThreadReclaim[];
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
      /**
       * A state its family names for readiness, which is neither a wait on
       * the counterparty nor a stall: the run hands its turn back at once
       * and resumes on observation, and the objective is held under `name`
       * until a later run reports something else. An `alarm` state is one
       * the operator must act on.
       */
      readonly namedState?: FraudProofWorkflowNamedState;
      /**
       * The actions this run skipped because they did not build, each an
       * alarm, in the order met; absent when none was. The run went on to
       * the moves ranked after them, so the alarms stand even when one of
       * those landed.
       */
      readonly alarms?: readonly FraudProofWorkflowAlarm[];
    };

/**
 * Whether the journal already records a stall for `reason` since its last
 * state change (its last entry other than a stall): stalling again on the
 * same cause is not a new entry, so an objective re-checked many times while
 * it is held keeps the journal bounded.
 */
export const journalRepeatsStall = (
  entries: readonly FraudProofWorkflowJournalEntry[],
  reason: string,
): boolean => {
  for (let index = entries.length - 1; index >= 0; index -= 1) {
    const event = entries[index]!.event;
    if (event.kind !== "stalled") return false;
    if (event.reason === reason) return true;
  }
  return false;
};
