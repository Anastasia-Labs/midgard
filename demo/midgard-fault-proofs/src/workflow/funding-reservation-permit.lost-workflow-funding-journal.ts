import {
  parseWorkflowFundingAbandonmentHandoff,
  parseWorkflowFundingCompletionHandoff,
} from "./funding-reservation-permit.parse-workflow-funding-abandonment-handoff.js";
import {
  parseWorkflowFundingPreparedTransition,
  parseWorkflowFundingSubmissionHandoff,
} from "./funding-reservation-permit.parse-workflow-funding-prepared-transition.js";
import {
  exact,
  type WorkflowFundingAbandonmentHandoff,
  type WorkflowFundingCompletionHandoff,
  type WorkflowFundingJournalHandoff,
  type WorkflowFundingSubmissionHandoff,
} from "./funding-reservation-permit.workflow-funding-reservation-port.js";
import {
  type FraudProofWorkflowJournalEntry,
  type FraudProofWorkflowJournalEvent,
  journalJsonDigest,
  normalizeJournalJson,
} from "./journal.js";

const digest = (event: unknown) =>
  journalJsonDigest(normalizeJournalJson(event));

const signedRecords = <T extends { submissionIntent: { txHash: string } }>(
  values: readonly unknown[],
  parse: (value: unknown) => T,
) =>
  values.map((value) => {
    const record = exact(value, ["transition", "handoff"], "signed attempt");
    const transition = parseWorkflowFundingPreparedTransition(
      record.transition,
    );
    const handoff = parse(record.handoff);
    // For other ports: the watcher's SQLite row parsers already refuse this.
    if (transition.transactionHash !== handoff.submissionIntent.txHash)
      throw new Error("funding store changed a signed attempt's identity");
    return handoff;
  });

/**
 * The records a lost workflow journal is rebuilt from, in order: each
 * superseded attempt the reservation store keeps (its exact preflight, intent
 * and not_found outcome), then the confirmed proof creation of a stored
 * completion. A re-recorded intent keeps everything but its number, which
 * counts from 1 per action as in any journal. A pending attempt follows them,
 * re-anchored through the store. `abandoned` and `submissions` are the store's
 * signed attempts as read; any not of this workflow refuses the rebuild.
 */
export const lostWorkflowFundingJournalRecords = (input: {
  readonly workflowId: string;
  readonly abandoned: readonly unknown[];
  readonly submissions: readonly unknown[];
  readonly completion: WorkflowFundingCompletionHandoff | null;
}): readonly FraudProofWorkflowJournalEvent[] => {
  const abandoned = signedRecords(
    input.abandoned,
    parseWorkflowFundingAbandonmentHandoff,
  ).sort(
    (left, right) =>
      left.expectedJournalSequence - right.expectedJournalSequence ||
      left.submissionIntent.attempt - right.submissionIntent.attempt,
  );
  const submissions = signedRecords(
    input.submissions,
    parseWorkflowFundingSubmissionHandoff,
  );
  const attempts = new Map<string, number>();
  const records: FraudProofWorkflowJournalEvent[] = [];
  const reRecord = (
    owner: WorkflowFundingJournalHandoff,
    intent: WorkflowFundingSubmissionHandoff["submissionIntent"],
  ) => {
    const recorded = submissions.filter(
      ({ submissionIntent }) => submissionIntent.txHash === intent.txHash,
    );
    const preflight = recorded[0]?.preflight;
    if (
      // The filter is by txHash; anything but exactly one recorded attempt is refused.
      recorded.length !== 1 ||
      preflight?.txHash !== intent.txHash ||
      preflight.actionId !== intent.actionId ||
      owner.workflowId !== input.workflowId ||
      recorded[0]!.workflowId !== input.workflowId
    )
      throw new Error(
        "lost workflow journal has no exact signed attempt of its own to re-record",
      );
    const attempt = (attempts.get(intent.actionId) ?? 0) + 1;
    attempts.set(intent.actionId, attempt);
    records.push(preflight, { ...intent, attempt });
  };
  for (const handoff of abandoned) {
    reRecord(handoff, handoff.submissionIntent);
    records.push(handoff.reconciliation);
  }
  if (input.completion !== null) {
    const completion = parseWorkflowFundingCompletionHandoff(input.completion);
    const txHash = completion.completion.terminal.proofToken.createdByTxHash;
    const created = submissions.find(
      ({ submissionIntent }) => submissionIntent.txHash === txHash,
    );
    if (
      created === undefined ||
      abandoned.some(
        ({ submissionIntent }) => submissionIntent.txHash === txHash,
      )
    )
      throw new Error(
        "lost workflow journal cannot re-record a proof creation it did not sign",
      );
    reRecord(completion, created.submissionIntent);
    const { actionId } = created.submissionIntent;
    records.push(
      { kind: "reconciled", actionId, outcome: "confirmed", txHash },
      { kind: "confirmed", actionId, txHash },
    );
  }
  return Object.freeze(records);
};

const RESTORED_KINDS = new Set([
  "stalled",
  "preflight_passed",
  "submission_intent",
  "reconciled",
  "confirmed",
]);

/** Whether `entries` can be a recreated journal at all: it holds only the
 * kinds of record a rebuild restores. Callers skip reading the store's
 * records for every other journal. */
export const workflowFundingJournalRecreatable = (
  entries: readonly FraudProofWorkflowJournalEntry[],
): boolean =>
  entries[0]?.event.kind === "started" &&
  entries[1]?.event.kind === "prepared" &&
  entries.slice(2).every(({ event }) => RESTORED_KINDS.has(event.kind));

/**
 * Whether the workflow journal was lost and recreated under records the
 * reservation store kept: after its started and prepared records it holds
 * only stalls and a prefix of `records`. With no records that is a journal
 * that holds no action yet.
 */
export const workflowFundingJournalRecreated = (
  entries: readonly FraudProofWorkflowJournalEntry[],
  records: readonly FraudProofWorkflowJournalEvent[],
): boolean => {
  if (!workflowFundingJournalRecreatable(entries)) return false;
  const restored = entries
    .slice(2)
    .filter(({ event }) => event.kind !== "stalled");
  return (
    restored.length <= records.length &&
    restored.every(
      ({ event }, index) => digest(event) === digest(records[index]),
    )
  );
};

const anchored = (
  handoff: WorkflowFundingJournalHandoff,
  entries: readonly FraudProofWorkflowJournalEntry[],
) =>
  entries.length >= handoff.expectedJournalSequence &&
  entries[1]?.event.kind === "prepared" &&
  entries[1].event.artifactDigest === handoff.preparedArtifactDigest;

/**
 * Whether the pending attempt's handoff belongs to a journal that was lost:
 * the journal was recreated, yet cannot anchor the handoff where it was
 * recorded. The store is then the authority for the attempt, which is
 * re-recorded after `records`. A recreated journal that anchors the handoff
 * is the ordinary crash before its intent.
 */
export const workflowFundingHandoffJournalLost = (
  handoff: WorkflowFundingSubmissionHandoff,
  entries: readonly FraudProofWorkflowJournalEntry[],
  records: readonly FraudProofWorkflowJournalEvent[] = [],
): boolean =>
  workflowFundingJournalRecreated(entries, records) &&
  (!anchored(handoff, entries) ||
    handoff.submissionIntent.attempt !==
      records.filter(
        (event) =>
          event.kind === "submission_intent" &&
          event.actionId === handoff.submissionIntent.actionId,
      ).length +
        1);

/**
 * Whether a stored abandonment belongs to a journal that was lost: the
 * journal was recreated without its intent. The store acknowledges such an
 * abandonment before its intent is re-recorded, so a journal holding the
 * intent of an unacknowledged abandonment is the one that recorded it.
 */
export const workflowFundingAbandonmentJournalLost = (
  handoff: WorkflowFundingAbandonmentHandoff,
  entries: readonly FraudProofWorkflowJournalEntry[],
  records: readonly FraudProofWorkflowJournalEvent[],
): boolean =>
  workflowFundingJournalRecreated(entries, records) &&
  !entries.some(
    ({ event }) =>
      event.kind === "submission_intent" &&
      event.txHash === handoff.submissionIntent.txHash,
  );

/** Whether a stored completion belongs to a journal that was lost: the
 * journal was recreated and cannot anchor the completion as its next record. */
export const workflowFundingCompletionJournalLost = (
  handoff: WorkflowFundingCompletionHandoff,
  entries: readonly FraudProofWorkflowJournalEntry[],
  records: readonly FraudProofWorkflowJournalEvent[],
): boolean =>
  workflowFundingJournalRecreated(entries, records) &&
  (!anchored(handoff, entries) ||
    entries
      .slice(handoff.expectedJournalSequence)
      .some(({ event }) => event.kind !== "stalled"));
