/** The reclaim of our own thread in front of the validation-trace dispute.
 * Init and Open are the dispute's only stages that read the challenged
 * block; when the next is one of them, the header's authenticated L1 view
 * decides whether the block still stands. Removed against a record not
 * ours, the run resolves by that record only after our thread at the
 * opener comes back to our wallet through the opener's Cancel. A thread of
 * ours past the opener keeps the route it has: the pending claim's
 * VerifySource and the case's close need no block. The adapter lists every
 * thread of ours on the header, so the orchestrator holds any resolution
 * while one of them still stands. */
import type { LucidEvolution } from "@lucid-evolution/lucid";

import type { ResolvedProverSigner } from "../runtime.js";
import { cursorFamilyObservation } from "../workflow/cursor-family-state.js";
import type { FraudProofFamilyL1ObservationPort } from "../workflow/family-l1-observation.js";
import type { FraudProofSignedTransactionRecovery } from "../workflow/l1-source.js";
import type {
  FraudProofFamilyWorkflowAdapter,
  FraudProofWorkflowAction,
  FraudProofWorkflowAdapterContext,
} from "../workflow/orchestrator.fraud-proof-family-workflow-adapter.js";
import { createLinearCancelReclaimPort } from "../workflow/own-thread-reclaim.linear-cancel-port.js";
import { withOwnThreadReclaim } from "../workflow/own-thread-reclaim.with-own-thread-reclaim.js";
import type { FraudProofOwnThreads } from "../workflow/raw-l1-family-derivation.own-thread.js";
import type { VerifiedFraudProofReleaseFinalityPolicy } from "../workflow/release-finality-policy.js";
import {
  parseValidationTraceAction,
  VALIDATION_TRACE_DISPUTE_ACTION,
} from "./actions.js";
import { VALIDATION_TRACE_REMOVAL_SPEC } from "./removal.js";
import type { ValidationTraceDisputeDeployment } from "./transactions.js";

const CATEGORY = "validationTraceDispute" as const;

/** The opener: the first of the family's thread positions. */
const OPENER_STEP = 1;

const headerOf = (context: FraudProofWorkflowAdapterContext): string => {
  if (context.identity.target.kind !== "state_queue_header")
    throw new Error(
      "validation-trace thread reclaim needs a state-queue block",
    );
  return context.identity.target.headerHash;
};

/** Whether `action` is a dispute stage that reads the challenged block. */
const readsBlock = (action: FraudProofWorkflowAction): boolean => {
  if (action.input.schemaVersion !== VALIDATION_TRACE_DISPUTE_ACTION)
    return false;
  const { stage } = parseValidationTraceAction(action);
  return stage === "init" || stage === "open";
};

export const withValidationTraceOwnThreadReclaim = ({
  base,
  lucid,
  signer,
  deployment,
  l1For,
  signedRecovery,
  releaseFinality,
}: {
  readonly base: FraudProofFamilyWorkflowAdapter;
  readonly lucid: LucidEvolution;
  readonly signer: ResolvedProverSigner;
  readonly deployment: ValidationTraceDisputeDeployment;
  /** The family's authenticated raw-L1 observation of one challenged
   * block, the one the removal reads. */
  readonly l1For: (
    headerHash: string,
  ) => Pick<
    FraudProofFamilyL1ObservationPort<typeof CATEGORY>,
    "observe" | "observeOwnThreads"
  >;
  readonly signedRecovery: FraudProofSignedTransactionRecovery;
  readonly releaseFinality: Pick<
    VerifiedFraudProofReleaseFinalityPolicy,
    "policy"
  >;
}): FraudProofFamilyWorkflowAdapter => {
  const ownThreads = async (
    headerHash: string,
  ): Promise<FraudProofOwnThreads> => {
    const observe = l1For(headerHash).observeOwnThreads;
    if (observe === undefined)
      throw new Error("validation-trace raw L1 cannot list our threads");
    return await observe({ headerHash });
  };
  // Before a stage that reads the block: a removal against another record
  // surfaces as the derivation's resolution, and a removal against a
  // sibling run's record on our key as its terminal.
  const surfaced: FraudProofFamilyWorkflowAdapter = {
    ...base,
    observe: async (context) => {
      const observed = await base.observe(context);
      if (observed.kind !== "action_required" || !readsBlock(observed.action))
        return observed;
      const headerHash = headerOf(context);
      const { provenance, stage } = await l1For(headerHash).observe({
        headerHash,
      });
      if (stage.kind !== "removed") return observed;
      return cursorFamilyObservation({
        spec: VALIDATION_TRACE_REMOVAL_SPEC,
        headerHash,
        provenance,
        stage,
      });
    },
  };
  const opener = deployment.chain.opener;
  const { contracts, category, witnessReferenceScripts } = deployment.init;
  const wrapped = withOwnThreadReclaim({
    category: CATEGORY,
    base: surfaced,
    releaseFinality,
    reclaim: createLinearCancelReclaimPort({
      family: "validation-trace dispute",
      lucid,
      signer,
      categoryId: category.categoryId,
      contracts: () => ({
        steps: [opener],
        computationThread: contracts.computationThread,
      }),
      stepReferences: [deployment.references.opener],
      witnessReferenceScripts: witnessReferenceScripts ?? {},
      l1: {
        // Only a thread at the opener is ours to cancel; one past it is
        // settled by the dispute's own stages.
        observeOwnThreads: async ({ headerHash }) => {
          const { standing, spends } = await ownThreads(headerHash);
          return standing?.step === OPENER_STEP
            ? { standing, spends }
            : { spends };
        },
        observeSignedTransaction: async (input) =>
          await signedRecovery.observeSignedTransaction(input),
        rebroadcastSignedTransaction: async (input) =>
          await signedRecovery.rebroadcastSignedTransaction(input),
      },
    }),
  });
  return Object.freeze({
    ...wrapped,
    observeOwnThreads: async ({
      headerHash,
    }: {
      readonly headerHash: string;
    }) => await ownThreads(headerHash),
  });
};
