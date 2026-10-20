/** The convicted block's removal. Once our case's fraud-proof record is on
 * chain, the block and its descendants leave the state queue the way every
 * other family's do: the shared cursor-family remove stage over the family's
 * authenticated raw-L1 observation, the shared state-queue mutation lease,
 * and the shared remove-fraudulent-block builder. */
import * as SDK from "@al-ft/midgard-sdk";
import type { LucidEvolution } from "@lucid-evolution/lucid";

import type { StateQueueMutationLeaseCoordinator } from "../remove-fraudulent-block.js";
import type { ResolvedProverSigner } from "../runtime.js";
import {
  createCursorFamilyWorkflowAdapter,
  CURSOR_FAMILY_TRANSACTION_PORT,
} from "../workflow/cursor-family-adapter.js";
import {
  captureCursorRemoval,
  cursorFamilyActionInput,
} from "../workflow/cursor-family-runtime.js";
import {
  CURSOR_FAMILY_ACTION,
  type CursorFamilySpec,
} from "../workflow/cursor-family-state.js";
import type { FraudProofFamilyL1ObservationPort } from "../workflow/family-l1-observation.js";
import type { FraudProofWorkflowAdapterContext } from "../workflow/orchestrator.fraud-proof-family-workflow-adapter.js";
import type {
  FraudProofFamilyWorkflowAdapter,
  FraudProofWorkflowAction,
  FraudProofWorkflowObservation,
} from "../workflow/orchestrator.js";
import type { FraudProofRawL1FamilyDefinition } from "../workflow/raw-l1-family-derivation.js";
import type { ValidationTraceDisputeDeployment } from "./transactions.js";

const CATEGORY = "validationTraceDispute" as const;

/**
 * Our case convicted and its fraud-proof record is on chain, but the
 * authenticated L1 view the removal acts on has not yet seen it, so the
 * workflow waits.
 */
export const VALIDATION_TRACE_CASE_CONVICTED =
  "validation-trace-case-convicted";

/**
 * The thread token's path: Init leaves it at the opener, Open moves it to the
 * pending claim at the source, VerifySource into the case, and the case's
 * close burns it and mints the fraud-proof record.
 */
export const VALIDATION_TRACE_REMOVAL_SPEC: CursorFamilySpec<typeof CATEGORY> =
  Object.freeze({
    category: CATEGORY,
    stepCount: 3,
    successors: Object.freeze({
      1: Object.freeze([2] as const),
      2: Object.freeze([3] as const),
      3: Object.freeze(["proof_token"] as const),
    }),
  });

/**
 * The case datum read as a thread datum: its bounty claimant, the challenger
 * VerifySource copied from the pending claim, is the thread's fraud prover.
 * Field order is the case datum's; only that field's name differs.
 */
const CaseAsThreadDatumSchema: typeof SDK.CaseSchema = (() => {
  const [constructor] = SDK.CaseSchema.anyOf;
  return {
    ...SDK.CaseSchema,
    anyOf: [
      {
        ...constructor,
        fields: constructor.fields.map((field: { readonly title?: string }) =>
          field.title === "bounty_claimant"
            ? { ...field, title: "fraud_prover" }
            : field,
        ),
      },
    ],
  } as typeof SDK.CaseSchema;
})();

/** The family's computation steps, as its raw-L1 observation reads them. */
export const validationTraceComputationSteps = (
  chain: ValidationTraceDisputeDeployment["chain"],
): FraudProofRawL1FamilyDefinition["computationThread"]["steps"] =>
  Object.freeze([
    {
      role: "computation_thread_step_01",
      address: chain.opener.spendingScriptAddress,
      datumSchema: SDK.FraudProofComputationThreadStepDatumSchema,
    },
    {
      role: "computation_thread_step_02",
      address: chain.source.spendingScriptAddress,
      datumSchema: SDK.PendingValidationClaimDatumSchema,
    },
    {
      role: "computation_thread_step_03",
      address: chain.case.spendingScriptAddress,
      datumSchema: CaseAsThreadDatumSchema,
    },
  ]);

/** What the removal of a convicted block needs beyond the dispute itself. */
export type ValidationTraceRemovalConfig = Readonly<{
  /** The family's authenticated raw-L1 observation of one challenged block. */
  l1For: (
    headerHash: string,
  ) => FraudProofFamilyL1ObservationPort<typeof CATEGORY>;
  stateQueueMutationLeaseCoordinator: StateQueueMutationLeaseCoordinator;
  /** The deployment info the shared removal resolves its contracts from. */
  deploymentInfo: unknown;
  /** The release economics' prover reward the removal pays us. */
  fraudProverRewardLovelace: bigint;
}>;

/** Whether `action` is the shared remove stage rather than a dispute step. */
const isRemoval = (action: FraudProofWorkflowAction): boolean =>
  action.input.schemaVersion === CURSOR_FAMILY_ACTION;

/**
 * The convicted block's removal: one shared cursor-family adapter per
 * challenged block, driving only its remove stage. `convicted` is what the
 * dispute observes once our record is on chain; `route` hands the remove
 * stage's preflight, submit and reconcile, which carry the lease, to it.
 */
export const createValidationTraceRemoval = ({
  lucid,
  signer,
  deployment,
  removal,
}: {
  readonly lucid: LucidEvolution;
  readonly signer: ResolvedProverSigner;
  readonly deployment: ValidationTraceDisputeDeployment;
  readonly removal: ValidationTraceRemovalConfig;
}) => {
  const adapters = new Map<string, FraudProofFamilyWorkflowAdapter>();
  const adapterFor = (headerHash: string) => {
    const existing = adapters.get(headerHash);
    if (existing !== undefined) return existing;
    const adapter = createCursorFamilyWorkflowAdapter({
      spec: VALIDATION_TRACE_REMOVAL_SPEC,
      l1: removal.l1For(headerHash),
      stateQueueMutationLeaseCoordinator:
        removal.stateQueueMutationLeaseCoordinator,
      transactions: {
        portVersion: CURSOR_FAMILY_TRANSACTION_PORT,
        category: CATEGORY,
        prepare: async () => {
          throw new Error(
            "validation-trace removal prepares no artifact of its own",
          );
        },
        capture: async ({ action }) => {
          const input = cursorFamilyActionInput({ category: CATEGORY, action });
          if (input.stage !== "remove")
            throw new Error(
              `validation-trace removal received a ${input.stage} action`,
            );
          return await captureCursorRemoval({
            category: CATEGORY,
            lucid,
            blueprint: deployment.blueprint,
            deploymentInfo: removal.deploymentInfo,
            network: deployment.network,
            signer,
            headerHash,
            input,
            stateQueueMutationLeaseCoordinator:
              removal.stateQueueMutationLeaseCoordinator,
            fraudProverRewardLovelace: removal.fraudProverRewardLovelace,
          });
        },
      },
    });
    adapters.set(headerHash, adapter);
    return adapter;
  };
  const forContext = (context: FraudProofWorkflowAdapterContext) => {
    if (context.identity.target.kind !== "state_queue_header")
      throw new Error("validation-trace removal needs a state-queue block");
    return adapterFor(context.identity.target.headerHash);
  };
  return {
    /** With our record on chain, the shared remove stage decides: it removes
     * the block, or reports the removal complete. An L1 view that still
     * shows the dispute has not yet seen the record. */
    convicted: async (
      context: FraudProofWorkflowAdapterContext,
      record: Readonly<{ unit: string; outRef: string }>,
    ): Promise<FraudProofWorkflowObservation> => {
      const observed = await forContext(context).observe(context);
      return observed.kind === "action_required" &&
        !(
          isRemoval(observed.action) && observed.action.input.stage === "remove"
        )
        ? {
            kind: "pending",
            reason: `${VALIDATION_TRACE_CASE_CONVICTED}: fraud-proof record ${record.unit} at ${record.outRef}; the authenticated L1 view has not yet seen it`,
          }
        : observed;
    },
    route: (
      dispute: FraudProofFamilyWorkflowAdapter,
    ): FraudProofFamilyWorkflowAdapter => ({
      ...dispute,
      preflight: async (context) =>
        isRemoval(context.action)
          ? await forContext(context).preflight(context)
          : await dispute.preflight(context),
      submit: async (context) =>
        isRemoval(context.action)
          ? await forContext(context).submit(context)
          : await dispute.submit(context),
      reconcile: async (context) =>
        isRemoval(context.action)
          ? await forContext(context).reconcile(context)
          : await dispute.reconcile(context),
    }),
  };
};
