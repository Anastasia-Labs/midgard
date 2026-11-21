import type { FraudProofCatalogueCategoryName } from "@al-ft/midgard-sdk/core";

import { WorkflowActionChangedError } from "./action-changed.js";
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
  threadReclaimUnavailableReason,
} from "./own-thread-reclaim.js";
import type {
  FraudProofOwnThreads,
  FraudProofStandingThread,
} from "./raw-l1-family-derivation.own-thread.js";
import type { VerifiedFraudProofReleaseFinalityPolicy } from "./release-finality-policy.js";
import { isFraudProofResolvedByAnotherRecordError } from "./resolved-by-another-record.js";
import { isWorkflowFundingShortfall } from "./workflow-funding-shortfall.js";

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
  /** Why a standing thread of ours is not the cancel's to reclaim, when the
   * family's own route settles it instead. */
  unreclaimable?(thread: FraudProofStandingThread): string | undefined;
  preflight(input: {
    readonly actionId: string;
    readonly headerHash: string;
    readonly threadOutRef: string;
    readonly step: number;
  }): Promise<FraudProofWorkflowPreflight>;
  submit(input: {
    readonly preflight: FraudProofWorkflowPreflight;
  }): Promise<string>;
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
 * Puts the reclaim of our own threads in front of a family's resolution and
 * completion. The header is gone through a removal: against another prover's
 * record (the stage derivation's typed error), or against a completed record,
 * whoever on our key made it. While a thread of ours stands on it, the run
 * waits for the removal to pass rollback recovery, and its next action is
 * then that thread's cancel; while a cancel this process sent is still within
 * rollback recovery, the run waits. Only then does the resolution or the
 * completion reach the run. Every other action is the family's own. The
 * adapter also reports our threads, so the orchestrator can refuse any
 * resolution while one still stands.
 *
 * A cancel sent before a restart and then rolled back leaves its thread
 * standing after the run has ended; the watcher's own-thread sweep reclaims
 * it from the chain.
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
  // The cancels this process sent, by transaction hash.
  const sentReclaims = new Set<string>();
  // One read of our threads per observation pass: the check before a
  // resolution and the orchestrator's guard after it read the same view. A
  // pass ends when the adapter is next asked to observe or act, or once the
  // guard has read it.
  const passes = new Map<string, Promise<FraudProofOwnThreads>>();
  const passThreads = (headerHash: string) => {
    const open = passes.get(headerHash);
    if (open !== undefined) return open;
    const threads = reclaim.observe({ headerHash });
    passes.set(headerHash, threads);
    return threads;
  };
  const endPass = (context: FraudProofWorkflowAdapterContext) => {
    if (context.identity.target.kind === "state_queue_header")
      passes.delete(context.identity.target.headerHash);
  };
  const observeOwnThreads = async ({
    headerHash,
  }: {
    readonly headerHash: string;
  }): Promise<FraudProofOwnThreads> => {
    try {
      return await passThreads(headerHash);
    } finally {
      passes.delete(headerHash);
    }
  };
  const beforeResolution = async (
    context: FraudProofWorkflowAdapterContext,
    removal: { readonly confirmationDepth: number },
    resolved: () => FraudProofWorkflowObservation,
  ): Promise<FraudProofWorkflowObservation> => {
    const threads = await passThreads(headerOf(category, context));
    if (threads.standing !== undefined) {
      const unreclaimable = reclaim.unreclaimable?.(threads.standing);
      if (unreclaimable !== undefined)
        return {
          kind: "pending",
          reason: threadReclaimUnavailableReason(
            threads.standing.threadOutRef,
            unreclaimable,
          ),
        };
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
    const shallow = threads.spends.find(
      ({ txHash, confirmationDepth }) =>
        sentReclaims.has(txHash) && confirmationDepth < finalDepth,
    );
    if (shallow !== undefined)
      return {
        kind: "pending",
        reason: `${category} reclaim ${shallow.txHash} of our thread ${shallow.threadOutRef} awaits release finality`,
      };
    return resolved();
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
    // The base names its own actions' alarm; this wrapper's own pause on
    // the shortfall itself.
    ...(base.unfunded === undefined
      ? {}
      : {
          unfunded: async (context) =>
            isReclaim(context.action)
              ? context.shortfall
              : ((await base.unfunded?.(context)) ?? context.shortfall),
        }),
    observeOwnThreads,
    observe: async (context) => {
      endPass(context);
      let observed: FraudProofWorkflowObservation;
      try {
        observed = await base.observe(context);
      } catch (cause) {
        if (!isFraudProofResolvedByAnotherRecordError(cause)) throw cause;
        return await beforeResolution(context, cause.resolution, () => {
          throw cause;
        });
      }
      // A thread of ours left standing beside a completed record, whoever
      // on our key made it, comes back to our wallet first.
      if (observed.kind === "completed")
        return await beforeResolution(
          context,
          observed.terminal.observedAt,
          () => observed,
        );
      return observed;
    },
    preflight: async (context) => {
      endPass(context);
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
      endPass(context);
      if (!isReclaim(context.action)) return await base.submit(context);
      sentReclaims.add(context.preflight.txHash);
      return {
        kind: "submitted",
        txHash: await reclaim.submit({ preflight: context.preflight }),
      };
    },
  };
  return Object.freeze(adapter);
};
