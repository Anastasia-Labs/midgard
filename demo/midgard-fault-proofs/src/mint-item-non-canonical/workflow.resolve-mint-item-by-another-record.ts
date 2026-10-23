import {
  releaseIdleWorkflowFundingReservation,
  releaseUnfundedWorkflowFundingInputs,
  WorkflowFundingReservationUnavailableError,
} from "../workflow/funding-reservation-permit.js";
import { isWorkflowFundingShortfall } from "../workflow/funding-reservation-permit.workflow-funding-shortfall.js";
import type { FraudProofWorkflowJournalStore } from "../workflow/journal.js";
import type { FraudProofWorkflowRunResult } from "../workflow/orchestrator.fraud-proof-workflow-run-result.js";
import { resolvedByAnotherRecordResult } from "../workflow/orchestrator.resolved-by-another-record.js";
import {
  FraudProofThreadReclaimUnfundedError,
  ownThreadsUnobservableReason,
  threadReclaimAction,
  threadReclaimIntents,
  threadReclaimNamedState,
  threadReclaimRefusedReason,
  threadReclaimUnavailableReason,
} from "../workflow/own-thread-reclaim.js";
import type { OwnThreadReclaimPort } from "../workflow/own-thread-reclaim.with-own-thread-reclaim.js";
import type { FraudProofOwnThreads } from "../workflow/raw-l1-family-derivation.own-thread.js";
import type { VerifiedFraudProofReleaseFinalityPolicy } from "../workflow/release-finality-policy.js";
import type { FraudProofResolvedByAnotherRecord } from "../workflow/resolved-by-another-record.js";
import {
  type createMintItemNonCanonicalCentralJournalAdapter,
  MintItemWorkflowRecoveryPendingError,
} from "./central-journal.js";
import type { MintItemStage } from "./mint-item-non-canonical.js";

const stageOfStep = (step: number): MintItemStage => {
  if (step === 1) return "step01";
  if (step === 2) return "step02";
  if (step === 3) return "step03";
  return "step04";
};

const describe = (cause: unknown): string =>
  cause instanceof Error ? cause.message : String(cause);

/**
 * The mint-item header was removed against a record this run did not
 * create: another prover's, or a sibling run's on our key. A thread of ours
 * that still stands on it is first reclaimed to our wallet through the
 * family's cancel, journaled and permit-bound like any move, once the
 * removal itself is past rollback recovery. The run waits while that cancel
 * is within rollback recovery, then resolves by another record with the
 * reclaim reported as a reclaim, never as a conviction.
 */
export const resolveMintItemByAnotherRecord = async ({
  journal,
  centralJournal,
  headerHash,
  releaseFinality,
  reclaim,
  resolution,
}: {
  readonly journal: FraudProofWorkflowJournalStore;
  readonly centralJournal: ReturnType<
    typeof createMintItemNonCanonicalCentralJournalAdapter
  >;
  readonly headerHash: string;
  readonly releaseFinality: VerifiedFraudProofReleaseFinalityPolicy;
  readonly reclaim: OwnThreadReclaimPort | undefined;
  readonly resolution: FraudProofResolvedByAnotherRecord;
}): Promise<FraudProofWorkflowRunResult> => {
  const { workflowId, identity } = centralJournal;
  const pending = async (
    reason: string,
  ): Promise<FraudProofWorkflowRunResult> => {
    const namedState = threadReclaimNamedState(reason);
    return {
      kind: "pending",
      resumeOnObservation: true,
      workflowId,
      identity,
      reason,
      entries: await journal.load(workflowId),
      ...(namedState === undefined ? {} : { namedState }),
    };
  };
  const { standingThread, ...removal } = resolution;
  // Without a list of our threads, only the derivation's own thread is
  // known: none reported is not none standing, so the run holds, named,
  // and looks again at the next observation.
  if (reclaim === undefined && standingThread === undefined)
    return await pending(
      ownThreadsUnobservableReason(
        "this deployment's L1 view has no list of our threads",
      ),
    );
  let threads: FraudProofOwnThreads;
  try {
    threads =
      reclaim === undefined
        ? {
            standing: standingThread as NonNullable<
              FraudProofOwnThreads["standing"]
            >,
            spends: [],
          }
        : await reclaim.observe({ headerHash });
  } catch (cause) {
    return await pending(ownThreadsUnobservableReason(describe(cause)));
  }
  const standing = threads.standing;
  try {
    // The thread's own stage: a reclaim that landed then rolled back shows
    // the thread standing again and is reopened, never silently dropped.
    await centralJournal.reconcile(
      standing === undefined ? "cancelled" : stageOfStep(standing.step),
    );
  } catch (cause) {
    if (!(cause instanceof MintItemWorkflowRecoveryPendingError)) throw cause;
    return await pending(cause.message);
  }
  const finalDepth = releaseFinality.policy.automaticRecoveryMaxDepth + 2;
  if (standing !== undefined) {
    const { threadOutRef } = standing;
    // A removal still within rollback recovery may yet be dropped, and the
    // header with it come back live: our thread stays where it is.
    if (removal.confirmationDepth < finalDepth)
      return await pending(
        `mintItemNonCanonical header removal awaits release finality before our thread ${threadOutRef} is reclaimed`,
      );
    const unavailable =
      reclaim === undefined
        ? "this deployment has no cancel to reclaim it with"
        : reclaim.unavailable?.();
    if (reclaim === undefined || unavailable !== undefined)
      return await pending(
        threadReclaimUnavailableReason(threadOutRef, unavailable!),
      );
    const prepared = (await journal.load(workflowId)).find(
      ({ event }) => event.kind === "prepared",
    )?.event;
    const familyIdentity =
      prepared?.kind === "prepared"
        ? (prepared.artifact as { readonly familyIdentity?: unknown })
            .familyIdentity
        : undefined;
    if (typeof familyIdentity !== "string")
      return await pending(
        threadReclaimUnavailableReason(
          threadOutRef,
          "this run journaled no prepared evidence to record the reclaim under",
        ),
      );
    const action = threadReclaimAction(standing);
    const recovery = {
      familyIdentity,
      sourceStage: stageOfStep(standing.step),
      targetStage: "cancelled",
    } as const;
    // Set while the funding permit binds the cancel: a refusal there is the
    // permit's, named as such, not the cancel's being unavailable.
    let binding = false;
    try {
      await centralJournal.beginAction(familyIdentity, action);
      const preflight = await reclaim.preflight({
        actionId: action.actionId,
        headerHash,
        threadOutRef,
        step: standing.step,
      });
      binding = true;
      await centralJournal.recordReclaimIntent(action, recovery, preflight);
      binding = false;
      const txHash = await reclaim.submit({ preflight });
      await centralJournal.familyJournal.append({
        sequence: 0,
        identity: familyIdentity,
        stage: "cancelled",
        txHash,
        outputReference: null,
      });
      return await pending(
        `mintItemNonCanonical reclaim ${txHash} of our thread ${threadOutRef} awaits confirmation`,
      );
    } catch (failure) {
      if (isWorkflowFundingShortfall(failure)) {
        // Nothing runs against the thread: it waits for a top-up, and the
        // idle reservation is released so a later observation reserves the
        // wallet as it is then.
        const cause = new FraudProofThreadReclaimUnfundedError({
          actionId: action.actionId,
          threadOutRef,
          cause: failure,
        });
        await releaseIdleWorkflowFundingReservation({ journal, workflowId });
        await releaseUnfundedWorkflowFundingInputs({ journal, cause });
        return await pending(cause.message);
      }
      // The wallet's reservation is busy elsewhere: wait for it.
      if (failure instanceof WorkflowFundingReservationUnavailableError) {
        await releaseUnfundedWorkflowFundingInputs({ journal, cause: failure });
        return await pending(failure.message);
      }
      if (binding)
        return await pending(
          threadReclaimRefusedReason(threadOutRef, describe(failure)),
        );
      // Anything else holds the objective under the operator's alarm and is
      // tried again, after reconciliation, at the next observation.
      return await pending(
        threadReclaimUnavailableReason(threadOutRef, describe(failure)),
      );
    }
  }
  const entries = await journal.load(workflowId);
  const ours = threadReclaimIntents(entries);
  const shallow = threads.spends.find(
    ({ txHash, confirmationDepth }) =>
      ours.has(txHash) && confirmationDepth < finalDepth,
  );
  if (shallow !== undefined)
    return await pending(
      `mintItemNonCanonical reclaim ${shallow.txHash} of our thread ${shallow.threadOutRef} awaits release finality`,
    );
  return resolvedByAnotherRecordResult({
    resolution: removal,
    threads,
    workflowId,
    identity,
    entries,
    releaseFinality,
    resumeOnObservation: (reason) => {
      const namedState = threadReclaimNamedState(reason);
      return {
        kind: "pending",
        resumeOnObservation: true,
        workflowId,
        identity,
        reason,
        entries,
        ...(namedState === undefined ? {} : { namedState }),
      };
    },
  });
};
