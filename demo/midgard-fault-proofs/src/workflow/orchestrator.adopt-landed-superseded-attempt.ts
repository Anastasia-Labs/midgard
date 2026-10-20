import { formatUnknownError } from "@al-ft/midgard-core";

import { assertWorkflowJournalActuation } from "./actuation-permit.js";
import { isFinalWorkflowCompletion } from "./completion-finality.js";
import {
  parseWorkflowFundingSubmissionHandoff,
  readWorkflowFundingRecordedSignedTransaction,
  readWorkflowFundingRecovery,
  reobserveWorkflowFundingReservationTransaction,
} from "./funding-reservation-permit.js";
import { recordBothAttemptsLanded } from "./funding-reservation-permit.reopen-legacy-abandonment.js";
import type {
  FraudProofWorkflowJournalEntry,
  FraudProofWorkflowJournalEvent,
  FraudProofWorkflowJournalStore,
} from "./journal.js";
import {
  FraudProofL1CheckpointChangedError,
  isFraudProofL1WaitError,
} from "./l1-source.js";
import type {
  FraudProofFamilyWorkflowAdapter,
  FraudProofWorkflowAdapterContext,
  FraudProofWorkflowReconcileResult,
  FraudProofWorkflowTerminalVerifier,
} from "./orchestrator.fraud-proof-family-workflow-adapter.js";
import {
  attemptCount,
  type FraudProofWorkflowRunResult,
} from "./orchestrator.fraud-proof-workflow-run-result.js";
import { lastActionEvent } from "./orchestrator.normalize-workflow-terminal.js";
import { readSignedAttempt } from "./orchestrator.read-signed-attempt.js";
import { reconcileLegacyFraudProofAbandonments } from "./orchestrator.reconcile-legacy-abandonments.js";
import { reobserveRequiredWorkflowParent } from "./orchestrator.reobserve-required-parent.js";
import {
  supersedeDisplacedFundingAttempt,
  supersedeWorkflowFundingAttempt,
} from "./orchestrator.supersede-funding-attempt.js";
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
 * attempt's inputs). Both attempts are read by their own signed bytes
 * through the workflow's authenticated L1, never through the family, whose
 * stage has moved on with whichever landed. When the recorded result is
 * impossible at the tip, a rollback dropped it: it is reobserved as any
 * rolled-back action is, so what was built on it reopens, closed as read, and
 * the landed attempt is adopted on the next pass. When it is on chain too, the
 * journal records the pair once and the workflow goes on with it. Any other
 * read holds, and nothing is written.
 *
 * Returns "continue" when the journal moved on, a run result to return, or
 * null when there is nothing to adopt in this pass.
 */
export const adoptLandedSupersededAttempt = async ({
  adapter,
  verifier,
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
  readonly verifier: Pick<
    FraudProofWorkflowTerminalVerifier,
    "observeSignedTransaction"
  >;
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
    verifier,
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
    const { deploymentFingerprint, category } = context.identity;
    const actuation = () =>
      assertWorkflowJournalActuation({
        journal,
        deploymentFingerprint,
        category,
        headerHash,
        checkpoint: "before_reconcile",
      });
    let read: FraudProofWorkflowReconcileResult;
    try {
      actuation();
      read = await readSignedAttempt(verifier, {
        transactionHash: result.txHash,
        signedTransactionCborHex:
          await readWorkflowFundingRecordedSignedTransaction(
            journal,
            result.txHash,
          ),
      });
      actuation();
    } catch (cause) {
      if (cause instanceof FraudProofL1CheckpointChangedError)
        return resumeOnObservation(
          `reading result ${result.txHash} of ${intent.actionId} awaits a stable boundary: ${cause.message}`,
        );
      if (isFraudProofL1WaitError(cause)) throw cause;
      return await stalled(
        `reading result ${result.txHash} of ${intent.actionId} failed: ${formatUnknownError(cause)}`,
      );
    }
    // Included, at any depth: both attempts are on chain.
    if (
      read.kind === "confirmed" ||
      (read.kind === "pending" && read.retirement !== undefined)
    ) {
      await recordBothAttemptsLanded({
        entries: current,
        append,
        actionId: intent.actionId,
        result: result.txHash,
        other: intent.txHash,
      });
      return null;
    }
    // Only a read that the result is impossible at the tip reopens it.
    if (read.kind !== "not_found")
      return resumeOnObservation(
        `attempt ${intent.txHash} of ${intent.actionId} landed; its result ${result.txHash} reads ${read.kind}`,
      );
    // The result was rolled back: reopen its action, so what was built on it
    // reopens too, and close the result as read.
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
    if (reobservation.kind === "included") return null;
    if (reobservation.kind !== "reobserved") return reobservation;
    await supersedeWorkflowFundingAttempt({
      journal,
      assertReconcile,
      entries,
      append,
      transactionHash: result.txHash,
      retirement: read.retirement,
      savedHandoff: null,
    });
    // Read the landed attempt at once: the next pass adopts it.
    supersededAttemptReadSchedule.forget(landed.transition.transactionHash);
    return "continue";
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
