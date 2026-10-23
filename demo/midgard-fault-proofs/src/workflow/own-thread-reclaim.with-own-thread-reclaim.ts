import type { FraudProofCatalogueCategoryName } from "@al-ft/midgard-sdk";

import { WorkflowActionChangedError } from "./action-changed.js";
import { isWorkflowFundingShortfall } from "./funding-reservation-permit.workflow-funding-shortfall.js";
import { isFraudProofL1WaitError } from "./l1-source.js";
import type {
  FraudProofFamilyWorkflowAdapter,
  FraudProofWorkflowAdapterContext,
  FraudProofWorkflowObservation,
  FraudProofWorkflowPreflight,
} from "./orchestrator.fraud-proof-family-workflow-adapter.js";
import {
  FRAUD_PROOF_WORKFLOW_ADAPTER,
  FRAUD_PROOF_WORKFLOW_SAFETY,
} from "./orchestrator.fraud-proof-family-workflow-adapter.js";
import { WorkflowActionUnbuiltError } from "./orchestrator.unbuilt-actions.js";
import {
  FraudProofThreadReclaimUnfundedError,
  isThreadReclaimInput,
  threadReclaimAction,
  threadReclaimIntents,
  threadReclaimUnavailableReason,
} from "./own-thread-reclaim.js";
import type { FraudProofOwnThreads } from "./raw-l1-family-derivation.own-thread.js";
import type { VerifiedFraudProofReleaseFinalityPolicy } from "./release-finality-policy.js";
import {
  isFraudProofResolvedByAnotherRecordError,
  journalCreatedRecord,
} from "./resolved-by-another-record.js";
import {
  reconcileSignedWorkflowTransaction,
  type SignedTransactionRecoveryObservation,
  type SignedWorkflowTransaction,
} from "./signed-transaction-reconciliation.js";

/**
 * The family's cancel of one of our threads: the move that burns the thread
 * token and pays the thread's value back to our wallet. `preflight` builds,
 * locally evaluates and signs it without submitting; `submit` sends those
 * exact bytes. A deployment that cannot cancel says why in `unavailable`.
 */
export type OwnThreadReclaimPort = Readonly<{
  observe(input: {
    readonly headerHash: string;
  }): Promise<FraudProofOwnThreads>;
  unavailable?(): string | undefined;
  preflight(input: {
    readonly actionId: string;
    readonly headerHash: string;
    readonly threadOutRef: string;
    readonly step: number;
  }): Promise<FraudProofWorkflowPreflight>;
  submit(input: {
    readonly preflight: FraudProofWorkflowPreflight;
  }): Promise<string>;
  observeSignedTransaction?(
    input: SignedWorkflowTransaction,
  ): Promise<SignedTransactionRecoveryObservation>;
  rebroadcastSignedTransaction?(
    input: SignedWorkflowTransaction & {
      readonly authorizeResubmission: (
        input: SignedWorkflowTransaction,
      ) => Promise<void>;
    },
  ): Promise<string>;
}>;

const headerOf = (
  category: string,
  context: FraudProofWorkflowAdapterContext,
): string => {
  if (context.identity.target.kind !== "state_queue_header")
    throw new Error(`${category} thread reclaim changed workflow target`);
  return context.identity.target.headerHash;
};

/**
 * Puts the reclaim of our own threads in front of a family's resolution by
 * another record. The header is gone through a removal against a record
 * this run did not create, whether another prover's (the stage derivation's
 * typed error) or a sibling run's on our key (a completed terminal whose
 * record this journal never confirmed). While a thread of ours stands on it,
 * the run waits for the removal to pass rollback recovery, and its next
 * action is then that thread's cancel; while a confirmed cancel is still
 * within rollback recovery, the run waits. Only then does the resolution
 * reach the run. Every other action is the family's own. The adapter also
 * reports our threads, so the orchestrator can refuse any resolution while
 * one still stands.
 */
export const withOwnThreadReclaim = <
  Category extends FraudProofCatalogueCategoryName,
>({
  category,
  base,
  reclaim,
  releaseFinality,
}: {
  readonly category: Category;
  readonly base: FraudProofFamilyWorkflowAdapter;
  readonly reclaim: OwnThreadReclaimPort;
  readonly releaseFinality: Pick<
    VerifiedFraudProofReleaseFinalityPolicy,
    "policy"
  >;
}): FraudProofFamilyWorkflowAdapter => {
  if (
    base.adapterVersion !== FRAUD_PROOF_WORKFLOW_ADAPTER ||
    base.category !== category ||
    base.safety.evidenceSource !== FRAUD_PROOF_WORKFLOW_SAFETY.evidenceSource ||
    base.safety.scriptCarriage !== FRAUD_PROOF_WORKFLOW_SAFETY.scriptCarriage ||
    base.safety.localEvaluation !== FRAUD_PROOF_WORKFLOW_SAFETY.localEvaluation
  ) {
    throw new Error(`${category} thread reclaim changed adapter identity`);
  }
  const finalDepth = releaseFinality.policy.automaticRecoveryMaxDepth + 2;
  const observeOwnThreads = async ({
    headerHash,
  }: {
    readonly headerHash: string;
  }): Promise<FraudProofOwnThreads> => await reclaim.observe({ headerHash });
  const beforeResolution = async (
    context: FraudProofWorkflowAdapterContext,
    removal: { readonly confirmationDepth: number },
    resolved: () => FraudProofWorkflowObservation,
  ): Promise<FraudProofWorkflowObservation> => {
    const threads = await observeOwnThreads({
      headerHash: headerOf(category, context),
    });
    if (threads.standing !== undefined) {
      // A removal still within rollback recovery may yet be dropped, and
      // the header with it come back live: our thread stays where it is.
      if (removal.confirmationDepth < finalDepth)
        return {
          kind: "pending",
          reason: `${category} header removal awaits release finality before our thread ${threads.standing.threadOutRef} is reclaimed`,
        };
      const unavailable = reclaim.unavailable?.();
      if (unavailable !== undefined)
        return {
          kind: "pending",
          reason: threadReclaimUnavailableReason(
            threads.standing.threadOutRef,
            unavailable,
          ),
        };
      return {
        kind: "action_required",
        action: threadReclaimAction(threads.standing),
      };
    }
    const ours = threadReclaimIntents(context.entries);
    const shallow = threads.spends.find(
      ({ txHash, confirmationDepth }) =>
        ours.has(txHash) && confirmationDepth < finalDepth,
    );
    if (shallow !== undefined)
      return {
        kind: "pending",
        reason: `${category} reclaim ${shallow.txHash} of our thread ${shallow.threadOutRef} awaits release finality`,
      };
    return resolved();
  };
  /** A transaction of ours has landed when it created our standing thread
   * or spent one of ours; otherwise its signed bytes, when recorded, say. */
  const reconcileByThreads = async (
    context: Parameters<FraudProofFamilyWorkflowAdapter["reconcile"]>[0],
    label: string,
  ): ReturnType<FraudProofFamilyWorkflowAdapter["reconcile"]> => {
    const { txHash } = context;
    if (txHash === undefined)
      return {
        kind: "conflict",
        reason: `${category} ${label} intent omitted its transaction hash`,
      };
    const threads = await reclaim.observe({
      headerHash: headerOf(category, context),
    });
    if (
      threads.standing?.threadOutRef.startsWith(`${txHash}#`) === true ||
      threads.spends.some((spend) => spend.txHash === txHash)
    )
      return { kind: "confirmed", txHash };
    return context.signedTransactionCborHex === undefined ||
      reclaim.observeSignedTransaction === undefined
      ? { kind: "pending", txHash }
      : await reconcileSignedWorkflowTransaction({
          transactionHash: txHash,
          signedTransactionCborHex: context.signedTransactionCborHex,
          observe: reclaim.observeSignedTransaction,
          rebroadcast: reclaim.rebroadcastSignedTransaction,
          authorizeResubmission: context.authorizeResubmission,
        });
  };
  const isReclaim = (action: {
    readonly input: Readonly<Record<string, unknown>>;
  }) => isThreadReclaimInput(action.input);
  const adapter: FraudProofFamilyWorkflowAdapter = {
    adapterVersion: FRAUD_PROOF_WORKFLOW_ADAPTER,
    category,
    safety: FRAUD_PROOF_WORKFLOW_SAFETY,
    prepare: async (input) => await base.prepare(input),
    ...(base.validatePreparedArtifact === undefined
      ? {}
      : { validatePreparedArtifact: base.validatePreparedArtifact }),
    ...(base.prepareRaw === undefined ? {} : { prepareRaw: base.prepareRaw }),
    ...(base.validatePreparedRawArtifact === undefined
      ? {}
      : { validatePreparedRawArtifact: base.validatePreparedRawArtifact }),
    observeOwnThreads,
    observe: async (context) => {
      let observed: FraudProofWorkflowObservation;
      try {
        observed = await base.observe(context);
      } catch (cause) {
        if (!isFraudProofResolvedByAnotherRecordError(cause)) throw cause;
        return await beforeResolution(context, cause.resolution, () => {
          throw cause;
        });
      }
      if (
        observed.kind === "completed" &&
        !journalCreatedRecord(
          context.entries,
          observed.terminal.proofToken.createdByTxHash,
        )
      )
        return await beforeResolution(
          context,
          observed.terminal.observedAt,
          () => observed,
        );
      return observed;
    },
    preflight: async (context) => {
      const { action } = context;
      if (!isThreadReclaimInput(action.input))
        return await base.preflight(context);
      const { threadOutRef } = action.input;
      const headerHash = headerOf(category, context);
      const { standing } = await reclaim.observe({ headerHash });
      if (standing?.threadOutRef !== threadOutRef)
        throw new WorkflowActionChangedError(
          `${category} thread ${threadOutRef} no longer stands to reclaim`,
        );
      try {
        return await reclaim.preflight({
          actionId: action.actionId,
          headerHash,
          threadOutRef,
          step: standing.step,
        });
      } catch (cause) {
        if (isWorkflowFundingShortfall(cause))
          throw new FraudProofThreadReclaimUnfundedError({
            actionId: action.actionId,
            threadOutRef,
            cause,
          });
        if (
          cause instanceof WorkflowActionChangedError ||
          isFraudProofL1WaitError(cause)
        )
          throw cause;
        // Any other failure holds the objective under the operator's alarm
        // and is tried again at the next observation.
        throw new WorkflowActionUnbuiltError(
          action.actionId,
          threadReclaimUnavailableReason(
            threadOutRef,
            cause instanceof Error ? cause.message : String(cause),
          ),
        );
      }
    },
    submit: async (context) => {
      if (!isReclaim(context.action)) return await base.submit(context);
      return {
        kind: "submitted",
        txHash: await reclaim.submit({ preflight: context.preflight }),
      };
    },
    reconcile: async (context) => {
      if (!isReclaim(context.action))
        try {
          return await base.reconcile(context);
        } catch (cause) {
          // A move of ours still in flight when the header was removed:
          // settled from our threads, so the reclaim can follow it.
          if (!isFraudProofResolvedByAnotherRecordError(cause)) throw cause;
          return await reconcileByThreads(context, "move");
        }
      return await reconcileByThreads(context, "thread reclaim");
    },
  };
  return Object.freeze(adapter);
};
