import {
  acknowledgeLostWorkflowFundingAbandonment,
  releaseWorkflowFundingReservation,
  reobserveWorkflowFundingReservationTransaction,
} from "./funding-reservation-permit.apply-transition.js";
import { stateForJournal } from "./funding-reservation-permit.create-workflow-funding-reservation-permit.js";
import {
  lostWorkflowFundingJournalRecords,
  workflowFundingAbandonmentJournalLost,
  workflowFundingCompletionJournalLost,
  workflowFundingHandoffJournalLost,
  workflowFundingJournalRecreatable,
  workflowFundingJournalRecreated,
} from "./funding-reservation-permit.lost-workflow-funding-journal.js";
import { parseWorkflowFundingSubmissionHandoff } from "./funding-reservation-permit.parse-workflow-funding-prepared-transition.js";
import { readWorkflowFundingRecovery } from "./funding-reservation-permit.read-workflow-funding-recovery.js";
import {
  assertWorkflowFundingCompletionHandoffJournal,
  reconcileWorkflowFundingSubmissionHandoff,
} from "./funding-reservation-permit.reconcile-workflow-funding-submission-handoff.js";
import type { WorkflowFundingSubmissionHandoff } from "./funding-reservation-permit.workflow-funding-reservation-port.js";
import {
  type FraudProofWorkflowIdentity,
  type FraudProofWorkflowJournalEntry,
  type FraudProofWorkflowJournalEvent,
  validateFraudProofWorkflowJournal,
} from "./journal.js";

type JournalAccess = {
  readonly journal: object;
  readonly workflowId: string;
  readonly identity: FraudProofWorkflowIdentity;
  readonly entries: () => readonly FraudProofWorkflowJournalEntry[];
  readonly append: (event: FraudProofWorkflowJournalEvent) => Promise<unknown>;
};

/**
 * Restore the journal records of the reservation's pending signed attempt.
 * Losing the workflow journal costs no liveness: the store keeps the exact
 * attempt, and it is re-recorded after the journal's other restored
 * `records`. L1 reconciliation then resolves it as for any recorded intent.
 */
export const recoverWorkflowFundingSubmissionHandoff = async ({
  journal,
  workflowId,
  identity,
  entries,
  handoff,
  records = [],
  append,
}: JournalAccess & {
  readonly handoff: WorkflowFundingSubmissionHandoff;
  readonly records?: readonly FraudProofWorkflowJournalEvent[];
}): Promise<void> => {
  let anchored = handoff;
  const current = entries();
  const prepared = current[1]?.event;
  if (
    workflowFundingHandoffJournalLost(handoff, current, records) &&
    prepared?.kind === "prepared"
  ) {
    if (
      current.slice(2).filter(({ event }) => event.kind !== "stalled")
        .length !== records.length
    )
      throw new Error(
        "funding reservation re-records its pending attempt after its earlier records",
      );
    anchored = parseWorkflowFundingSubmissionHandoff({
      ...handoff,
      preparedArtifactDigest: prepared.artifactDigest,
      expectedJournalSequence: current.length,
      submissionIntent: {
        ...handoff.submissionIntent,
        attempt:
          records.filter(
            (event) =>
              event.kind === "submission_intent" &&
              event.actionId === handoff.submissionIntent.actionId,
          ).length + 1,
      },
    });
    if (
      !(await reobserveWorkflowFundingReservationTransaction({
        journal,
        transactionHash: handoff.submissionIntent.txHash,
        adoption: anchored,
      }))
    )
      throw new Error(
        "funding reservation cannot re-record its pending attempt",
      );
  }
  for (const event of reconcileWorkflowFundingSubmissionHandoff({
    handoff: anchored,
    entries: current,
  }))
    await append(event);
  validateFraudProofWorkflowJournal({
    workflowId,
    entries: entries(),
    expectedIdentity: identity,
  });
};

/**
 * Restore every funding record of a workflow journal that was lost, then
 * check the journal against the reservation store's handoffs. The store keeps
 * each signed attempt exactly: superseded attempts and their stored outcome
 * are re-recorded first, an abandonment the old journal never acknowledged is
 * acknowledged before its records, and a completion is re-anchored after the
 * proof creation it requires. A completion is never re-run: the runner only
 * re-verifies its stored terminal on L1 before journaling it again.
 */
export const recoverWorkflowFundingJournal = async (
  access: JournalAccess,
): ReturnType<typeof readWorkflowFundingRecovery> => {
  const { journal, workflowId, entries, append } = access;
  let recovery = await readWorkflowFundingRecovery(journal);
  const port = stateForJournal(journal)?.port;
  const records = workflowFundingJournalRecreatable(entries())
    ? lostWorkflowFundingJournalRecords({
        workflowId,
        abandoned: (await port?.readLegacyAbandonedTransactions?.()) ?? [],
        submissions: (await port?.readRecordedSubmissions?.()) ?? [],
        completion: recovery.completionHandoff,
      })
    : [];
  const abandoned = recovery.abandonmentHandoff;
  // Any other journal is the one that recorded its handoffs, checked below.
  if (
    abandoned === null
      ? workflowFundingJournalRecreated(entries(), records)
      : workflowFundingAbandonmentJournalLost(abandoned, entries(), records)
  ) {
    if (abandoned !== null)
      await acknowledgeLostWorkflowFundingAbandonment({
        journal,
        handoff: abandoned,
      });
    const restored = entries()
      .slice(2)
      .filter(({ event }) => event.kind !== "stalled").length;
    for (const event of records.slice(restored)) await append(event);
    const completion = recovery.completionHandoff;
    const prepared = entries()[1]?.event;
    if (
      completion !== null &&
      prepared?.kind === "prepared" &&
      workflowFundingCompletionJournalLost(completion, entries(), records)
    )
      await releaseWorkflowFundingReservation({
        journal,
        handoff: {
          ...completion,
          preparedArtifactDigest: prepared.artifactDigest,
          expectedJournalSequence: entries().length,
        },
      });
    recovery = await readWorkflowFundingRecovery(journal);
  }
  if (recovery.submissionHandoff !== null)
    await recoverWorkflowFundingSubmissionHandoff({
      ...access,
      handoff: recovery.submissionHandoff,
      records,
    });
  if (recovery.completionHandoff !== null)
    assertWorkflowFundingCompletionHandoffJournal({
      handoff: recovery.completionHandoff,
      entries: entries(),
    });
  return recovery;
};
