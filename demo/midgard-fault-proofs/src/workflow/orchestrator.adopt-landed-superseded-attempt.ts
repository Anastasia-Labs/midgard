import { isFinalWorkflowCompletion } from "./completion-finality.js";
import {
  parseWorkflowFundingSubmissionHandoff,
  readWorkflowFundingRecovery,
  reobserveWorkflowFundingReservationTransaction,
} from "./funding-reservation-permit.js";
import { recordBothAttemptsLanded } from "./funding-reservation-permit.reopen-legacy-abandonment.js";
import type {
  FraudProofWorkflowJournalEntry,
  FraudProofWorkflowJournalEvent,
  FraudProofWorkflowJournalStore,
} from "./journal.js";
import type {
  FraudProofFamilyWorkflowAdapter,
  FraudProofWorkflowAdapterContext,
} from "./orchestrator.fraud-proof-family-workflow-adapter.js";
import {
  attemptCount,
  type FraudProofWorkflowRunResult,
} from "./orchestrator.fraud-proof-workflow-run-result.js";
import { lastActionEvent } from "./orchestrator.normalize-workflow-terminal.js";
import { reconcileLegacyFraudProofAbandonments } from "./orchestrator.reconcile-legacy-abandonments.js";
import {
  readSubmissionOnChain,
  reobserveRequiredWorkflowParent,
} from "./orchestrator.reobserve-required-parent.js";
import { supersedeDisplacedFundingAttempt } from "./orchestrator.supersede-funding-attempt.js";
import { supersededAttemptReadSchedule } from "./superseded-attempt-read-schedule.js";

const UNRESOLVED = new Set([
  "submission_intent",
  "reobserved",
  "submission_ambiguous",
  "submitted",
  "rebroadcast_intent",
]);

type Submission = Extract<
  FraudProofWorkflowJournalEvent,
  { kind: "submission_intent" }
>;

/**
 * Owner ruling (whichever lands wins): a superseded attempt that lands is its
 * action's result whenever that action has none on chain, however many later
 * actions were recorded since, until the workflow's final completion. Nothing
 * may be in flight: an unresolved attempt is reconciled first (a replacement
 * of the same action is impossible now, since it spends one of the landed
 * attempt's inputs). A recorded result is read first. When a rollback dropped
 * it, it is reobserved as any rolled-back action is, so what was built on it
 * reopens, and the landed attempt is adopted once it is reconciled as absent.
 * When it is on chain too, the journal records the pair once and the workflow
 * goes on with it.
 *
 * Returns "continue" when the journal moved on, a run result to return, or
 * null when there is nothing to adopt in this pass.
 */
export const adoptLandedSupersededAttempt = async ({
  adapter,
  context,
  headerHash,
  journal,
  entries,
  append,
  assertReconcile,
  stalled,
  resumeOnObservation,
  nowMs,
}: {
  readonly adapter: FraudProofFamilyWorkflowAdapter;
  readonly context: FraudProofWorkflowAdapterContext;
  readonly headerHash: string;
  readonly journal: FraudProofWorkflowJournalStore;
  readonly entries: () => readonly FraudProofWorkflowJournalEntry[];
  readonly append: (event: FraudProofWorkflowJournalEvent) => Promise<void>;
  readonly assertReconcile: () => void;
  readonly stalled: (reason: string) => Promise<FraudProofWorkflowRunResult>;
  readonly resumeOnObservation: (reason: string) => FraudProofWorkflowRunResult;
  readonly nowMs: number;
}): Promise<"continue" | FraudProofWorkflowRunResult | null> => {
  const landed = await reconcileLegacyFraudProofAbandonments(
    context,
    journal,
    adapter,
    append,
    nowMs,
  );
  if (landed === null) return null;
  const current = entries();
  const recovery = await readWorkflowFundingRecovery(journal);
  // Past the recovery window nothing reopens, so nothing is adopted.
  if (
    isFinalWorkflowCompletion(recovery.completionHandoff?.completion) ||
    current.some(({ event }) => isFinalWorkflowCompletion(event))
  )
    return null;
  const latest = [...current]
    .reverse()
    .map(({ event }) => event)
    .find(({ kind }) => kind !== "stalled");
  const unresolved =
    latest === undefined ||
    UNRESOLVED.has(latest.kind) ||
    (latest.kind === "reconciled" && latest.outcome === "pending");
  const intent = landed.handoff.submissionIntent;
  const last = lastActionEvent(current, intent.actionId);
  if (
    (last?.kind === "confirmed" ||
      (last?.kind === "reconciled" && last.outcome === "confirmed")) &&
    last.txHash !== undefined &&
    last.txHash !== intent.txHash
  ) {
    const result = [...current]
      .reverse()
      .map(({ event }) => event)
      .find(
        (event): event is Submission =>
          event.kind === "submission_intent" && event.txHash === last.txHash,
      );
    if (recovery.abandonmentHandoff !== null || result === undefined)
      return null;
    const onChain = await readSubmissionOnChain({
      adapter,
      context,
      headerHash,
      journal,
      submission: result,
      stalled,
      resumeOnObservation,
    });
    if (typeof onChain !== "boolean") return onChain;
    if (onChain) {
      await recordBothAttemptsLanded({
        entries: current,
        append,
        actionId: intent.actionId,
        result: result.txHash,
        other: intent.txHash,
      });
      return null;
    }
    // The recorded result was rolled back: reopen its action.
    const reobservation = await reobserveRequiredWorkflowParent({
      adapter,
      context,
      headerHash,
      journal,
      intent: result,
      hasUnresolvedDescendant: unresolved || recovery.transition !== null,
      intentReadAbsent: true,
      supersedeDisplacedAttempt: async () =>
        await supersedeDisplacedFundingAttempt({
          journal,
          assertReconcile,
          entries,
          append,
          kept: result.txHash,
        }),
      append,
      stalled,
      resumeOnObservation,
    });
    if (reobservation.kind === "reobserved") return "continue";
    return reobservation.kind === "included" ? null : reobservation;
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
    unresolved ||
    last?.kind !== "reconciled" ||
    last.outcome !== "not_found" ||
    preflight === undefined
  )
    return null;
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
    return null;
  await append(handoff.preflight);
  await append(handoff.submissionIntent);
  supersededAttemptReadSchedule.forget(landed.transition.transactionHash);
  return "continue";
};
