/** The reclaim of our own thread in front of the validation-trace dispute.
 * Init and Open are the dispute's only stages that read the challenged
 * block; when the next is one of them, the header's authenticated L1 view
 * decides whether the block still stands. Removed against a record not
 * ours, the run resolves by that record only after our thread at the
 * opener comes back to our wallet through the opener's Cancel. A thread of
 * ours past the opener keeps the route it has: the pending claim's
 * VerifySource and the case's close need no block, and until one of them
 * spends it the run holds under the reclaim alarm, saying so. The adapter
 * lists every thread of ours on the header, so the orchestrator holds any
 * resolution while one of them still stands. */
import type { LucidEvolution } from "@lucid-evolution/lucid";

import type { ResolvedProverSigner } from "../runtime.js";
import { cursorFamilyObservation } from "../workflow/cursor-family-state.js";
import type {
  FraudProofFamilyL1ObservationPort,
  FraudProofFamilyL1View,
} from "../workflow/family-l1-observation.js";
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

/** Why a thread of ours past the opener is held, not cancelled. */
export const PAST_THE_OPENER =
  "it stands past the dispute opener, where no cancel reaches it; the pending claim's VerifySource or the case's close settles it";

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
    "observe" | "observeOwnThreads" | "observeView"
  >;
  readonly signedRecovery: FraudProofSignedTransactionRecovery;
  readonly releaseFinality: Pick<
    VerifiedFraudProofReleaseFinalityPolicy,
    "policy"
  >;
}): FraudProofFamilyWorkflowAdapter => {
  // One capture of the header per observation pass: the check before Init
  // or Open, the reclaim's read of our threads and the orchestrator's guard
  // after it all read the same view. A pass ends when the adapter is next
  // asked to observe or act, or once the guard has read it.
  const passes = new Map<string, Promise<FraudProofFamilyL1View>>();
  const viewOf = (headerHash: string) => {
    const open = passes.get(headerHash);
    if (open !== undefined) return open;
    const port = l1For(headerHash);
    if (port.observeView === undefined) return undefined;
    const view = port.observeView({ headerHash });
    passes.set(headerHash, view);
    return view;
  };
  const endPass = (context: FraudProofWorkflowAdapterContext) => {
    if (context.identity.target.kind === "state_queue_header")
      passes.delete(context.identity.target.headerHash);
  };
  const ownThreads = async (
    headerHash: string,
  ): Promise<FraudProofOwnThreads> => {
    const view = viewOf(headerHash);
    if (view !== undefined) return (await view).ownThreads();
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
      const view = viewOf(headerHash);
      const { provenance, stage } =
        view === undefined
          ? await l1For(headerHash).observe({ headerHash })
          : await (await view).observe();
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
  const cancel = createLinearCancelReclaimPort({
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
      observeOwnThreads: async ({ headerHash }) => await ownThreads(headerHash),
      observeSignedTransaction: async (input) =>
        await signedRecovery.observeSignedTransaction(input),
      rebroadcastSignedTransaction: async (input) =>
        await signedRecovery.rebroadcastSignedTransaction(input),
    },
  });
  const wrapped = withOwnThreadReclaim({
    category: CATEGORY,
    base: surfaced,
    releaseFinality,
    reclaim: {
      ...cancel,
      // Only a thread at the opener is ours to cancel; one past it is
      // settled by the dispute's own stages, and the hold says so.
      unreclaimable: ({ step }) =>
        step === OPENER_STEP ? undefined : PAST_THE_OPENER,
    },
  });
  return Object.freeze({
    ...wrapped,
    observe: async (context) => {
      endPass(context);
      return await wrapped.observe(context);
    },
    preflight: async (context) => {
      endPass(context);
      return await wrapped.preflight(context);
    },
    submit: async (context) => {
      endPass(context);
      return await wrapped.submit(context);
    },
    reconcile: async (context) => {
      endPass(context);
      return await wrapped.reconcile(context);
    },
  } satisfies FraudProofFamilyWorkflowAdapter);
};
