import {
  parseWorkflowFundingSubmissionHandoff,
  readWorkflowFundingRecovery,
  reobserveWorkflowFundingReservationTransaction,
} from "./funding-reservation-permit.js";
import type { readLegacyWorkflowFundingAbandonedTransactions } from "./funding-reservation-permit.read-workflow-funding-recovery.js";
import { recordBothAttemptsLanded } from "./funding-reservation-permit.reopen-legacy-abandonment.js";
import type {
  FraudProofWorkflowJournalEntry,
  FraudProofWorkflowJournalEvent,
} from "./journal.js";
import { attemptCount } from "./orchestrator.fraud-proof-workflow-run-result.js";
import { lastActionEvent } from "./orchestrator.normalize-workflow-terminal.js";

const UNRESOLVED = new Set([
  "submission_intent",
  "reobserved",
  "submission_ambiguous",
  "submitted",
  "rebroadcast_intent",
]);

/**
 * Owner ruling (whichever lands wins): a superseded attempt that lands is its
 * action's result whenever that action has none yet, however many later
 * actions were recorded since. Nothing may be in flight: an unresolved
 * attempt is reconciled first (a replacement of the same action is impossible
 * now, since it spends one of the landed attempt's inputs). A recorded result
 * is left as the chain shows it; a different attempt of the same action that
 * also landed is recorded once in the journal, and the workflow goes on.
 * Returns whether the journal now holds the adopted intent.
 */
export const adoptLandedSupersededAttempt = async ({
  journal,
  entries,
  landed,
  append,
}: {
  readonly journal: object;
  readonly entries: () => readonly FraudProofWorkflowJournalEntry[];
  readonly landed: Awaited<
    ReturnType<typeof readLegacyWorkflowFundingAbandonedTransactions>
  >[number];
  readonly append: (event: FraudProofWorkflowJournalEvent) => Promise<unknown>;
}): Promise<boolean> => {
  const current = entries();
  const recovery = await readWorkflowFundingRecovery(journal);
  const latest = [...current]
    .reverse()
    .map(({ event }) => event)
    .find(({ kind }) => kind !== "stalled");
  const intent = landed.handoff.submissionIntent;
  const last = lastActionEvent(current, intent.actionId);
  if (
    (last?.kind === "confirmed" ||
      (last?.kind === "reconciled" && last.outcome === "confirmed")) &&
    last.txHash !== undefined &&
    last.txHash !== intent.txHash
  ) {
    await recordBothAttemptsLanded({
      entries: current,
      append,
      actionId: intent.actionId,
      result: last.txHash,
      other: intent.txHash,
    });
    return false;
  }
  const preflight = [...current]
    .reverse()
    .map(({ event }) => event)
    .find(
      (
        event,
      ): event is Extract<
        FraudProofWorkflowJournalEvent,
        { kind: "preflight_passed" }
      > => event.kind === "preflight_passed" && event.txHash === intent.txHash,
    );
  if (
    recovery.transition !== null ||
    recovery.abandonmentHandoff !== null ||
    recovery.completionHandoff !== null ||
    current.some(
      ({ event }) =>
        event.kind === "terminal_included" || event.kind === "completed",
    ) ||
    latest === undefined ||
    UNRESOLVED.has(latest.kind) ||
    (latest.kind === "reconciled" && latest.outcome === "pending") ||
    last?.kind !== "reconciled" ||
    last.outcome !== "not_found" ||
    preflight === undefined
  )
    return false;
  const handoff = parseWorkflowFundingSubmissionHandoff({
    workflowId: landed.handoff.workflowId,
    identity: landed.handoff.identity,
    preparedArtifactDigest: landed.handoff.preparedArtifactDigest,
    expectedJournalSequence: current.length,
    preflight,
    submissionIntent: {
      ...intent,
      attempt: attemptCount(current, intent.actionId) + 1,
    },
  });
  if (
    !(await reobserveWorkflowFundingReservationTransaction({
      journal,
      transactionHash: intent.txHash,
      adoption: handoff,
    }))
  )
    return false;
  await append(handoff.preflight);
  await append(handoff.submissionIntent);
  return true;
};
