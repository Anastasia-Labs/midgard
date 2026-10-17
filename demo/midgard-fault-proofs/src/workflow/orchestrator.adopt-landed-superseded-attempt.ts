import {
  parseWorkflowFundingSubmissionHandoff,
  readWorkflowFundingRecovery,
  reobserveWorkflowFundingReservationTransaction,
} from "./funding-reservation-permit.js";
import type { readLegacyWorkflowFundingAbandonedTransactions } from "./funding-reservation-permit.read-workflow-funding-recovery.js";
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
 * Owner ruling (whichever lands wins): a superseded attempt that lands after
 * a rollback is the action's result. It is adopted only while its action is
 * the latest one and that action has no result: an unresolved replacement is
 * reconciled first (it is impossible now, since it spends one of the landed
 * attempt's inputs), and a recorded result is left as the chain shows it.
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
  const latestIntent = [...current]
    .reverse()
    .map(({ event }) => event)
    .find(({ kind }) => kind === "submission_intent");
  const last = lastActionEvent(current, intent.actionId);
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
    latest === undefined ||
    UNRESOLVED.has(latest.kind) ||
    (latest.kind === "reconciled" && latest.outcome === "pending") ||
    latestIntent?.kind !== "submission_intent" ||
    latestIntent.actionId !== intent.actionId ||
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
