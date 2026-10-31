import {
  FRAUD_PROOF_WORKFLOW_JOURNAL_SCHEMA_VERSION,
  type FraudProofWorkflowIdentity,
  type FraudProofWorkflowJournalEntry,
  type FraudProofWorkflowJournalEvent,
  type FraudProofWorkflowJournalStore,
} from "@al-ft/midgard-fault-proofs";

import type { WatcherProverFundingReservationStore } from "../../src/funding/prover-funding-reservation.js";

export type FixtureAttemptEvents = Readonly<{
  preflight: FraudProofWorkflowJournalEvent;
  submissionIntent: FraudProofWorkflowJournalEvent & {
    readonly kind: "submission_intent";
  };
}>;

/** Journal entries for a fixture's workflow, and its signed attempts
 * prepared as the permit prepares them. */
export const fixtureAttemptJournal = ({
  journal,
  workflowId,
  identity,
}: {
  readonly journal: FraudProofWorkflowJournalStore;
  readonly workflowId: string;
  readonly identity: FraudProofWorkflowIdentity;
}) => {
  /** The journal entries `events` would be, after those recorded now. */
  const entriesFor = async (
    events: readonly FraudProofWorkflowJournalEvent[],
  ): Promise<readonly FraudProofWorkflowJournalEntry[]> => {
    const recorded = (await journal.load(workflowId)).length;
    return events.map((event, index) => ({
      schemaVersion: FRAUD_PROOF_WORKFLOW_JOURNAL_SCHEMA_VERSION,
      workflowId,
      identity,
      sequence: recorded + index,
      recordedAt: new Date().toISOString(),
      event,
    }));
  };
  /** Prepares a signed attempt as the permit does: the preflight and intent
   * commit with it. */
  const prepareAttempt = async (
    store: WatcherProverFundingReservationStore,
    input: Omit<
      Parameters<WatcherProverFundingReservationStore["prepareTransition"]>[0],
      "attempt" | "journal"
    >,
    events: FixtureAttemptEvents,
  ) => {
    const entries = await entriesFor([
      events.preflight,
      events.submissionIntent,
    ]);
    return await store.prepareTransition({
      ...input,
      attempt: {
        workflowId,
        intentSequence: entries[1]!.sequence,
        category: "doubleSpend",
        actionId: events.submissionIntent.actionId,
        actionInput: events.submissionIntent.actionInput,
      },
      journal: entries,
    });
  };
  return { entriesFor, prepareAttempt };
};
