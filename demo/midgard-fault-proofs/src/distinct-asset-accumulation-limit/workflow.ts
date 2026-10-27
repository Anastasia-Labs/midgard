/** The `distinctAssetAccumulationLimit` family definition and its manifest-bound
 * runner. */
import { FraudProofComputationThreadStepDatum } from "@al-ft/midgard-sdk";

import type { RetainedDaPayloadSource } from "../transition-trace/fetch.js";
import type { CompleteCanonicalReplayContext } from "../workflow/complete-replay.js";
import { DISTINCT_ASSET_ACCUMULATION_LIMIT_COMPLETE_CANONICAL_REPLAY } from "../workflow/complete-replay.js";
import { parseComputationThreadStage } from "../workflow/computation-thread-steps.js";
import { type CursorFamilyTransactionPort } from "../workflow/cursor-family-adapter.js";
import { cursorFamilyActionInput } from "../workflow/cursor-family-runtime.js";
import { DISTINCT_ASSET_ACCUMULATION_LIMIT_CURSOR_SPEC } from "../workflow/cursor-family-spec.js";
import { type FraudProofWorkflowDeploymentBinding } from "../workflow/deployment-manifest-binding.js";
import {
  defineFamily,
  type FamilyAssemblyContext,
  type LinearFamilyPrerequisiteInput,
} from "../workflow/family-definition.js";
import {
  type FraudProofFamilyL1ObservationPort,
  observeFraudProofWorkflowHeader,
} from "../workflow/family-l1-observation.js";
import { type FieldCarriageRequirement } from "../workflow/field-carriage-prerequisite.js";
import type { FraudProofWorkflowJournalStore } from "../workflow/journal.js";
import { assembleManifestBoundFamilyWorkflow } from "../workflow/manifest-bound-family-assembly.js";
import {
  createFraudProofWorkflowRegistry,
  type FraudProofFamilyWorkflowAdapter,
  type FraudProofWorkflowRunResult,
  type FraudProofWorkflowTerminalVerifier,
  runFraudProofWorkflowFromRetainedDa,
} from "../workflow/orchestrator.js";
import type { FraudProofReleaseFinalityAuthority } from "../workflow/release-finality-policy.js";
import { DISTINCT_ASSET_ACCUMULATION_LIMIT_FIELDS } from "./custody.js";
import {
  distinctAssetAccumulationLimitSourceInclusion,
  planDistinctAssetAccumulationLimitArtifact,
} from "./workflow.plan.js";
import {
  createDistinctAssetAccumulationLimitTransactionPort,
  distinctAssetAccumulationLimitRoleAt,
  distinctAssetAccumulationLimitWorkflowContracts,
  type ManifestBoundDistinctAssetAccumulationLimitWorkflowConfig,
} from "./workflow.transaction-port.js";

export type ManifestBoundDistinctAssetAccumulationLimitWorkflow = Readonly<{
  replayContext?: CompleteCanonicalReplayContext;
  binding: FraudProofWorkflowDeploymentBinding<"distinctAssetAccumulationLimit">;
  l1: FraudProofFamilyL1ObservationPort<"distinctAssetAccumulationLimit">;
  transactions: CursorFamilyTransactionPort<"distinctAssetAccumulationLimit">;
  adapter: FraudProofFamilyWorkflowAdapter;
  terminalVerifier: FraudProofWorkflowTerminalVerifier;
  releaseFinalityAuthority: FraudProofReleaseFinalityAuthority;
}>;

const WITNESS_ROLES = [
  "computationThreadMint",
  "fraudProofMint",
  "phasMembershipWithdraw",
  "chunkedVerifyWithdraw",
] as const;

type Context = FamilyAssemblyContext<
  "distinctAssetAccumulationLimit",
  (typeof WITNESS_ROLES)[number],
  true,
  12
>;

/** The custody role an action spends; `null` for init, removal, step-01 and
 * the terminal, which open no committed field. */
const custodyRole = (input: LinearFamilyPrerequisiteInput) => {
  const stage = cursorFamilyActionInput({
    category: "distinctAssetAccumulationLimit",
    action: input.action,
  }).stage;
  const step = parseComputationThreadStage(stage);
  if (step === undefined) return null;
  const role = distinctAssetAccumulationLimitRoleAt(step);
  return role === "step01" || role === "terminal" ? null : role;
};

const planned = (context: Context, input: LinearFamilyPrerequisiteInput) =>
  planDistinctAssetAccumulationLimitArtifact({
    artifact: input.artifact,
    contracts: distinctAssetAccumulationLimitWorkflowContracts(
      context.binding,
      context.certificate,
    ),
    prover: context.signer.paymentKeyHash,
  });

/** The carriage of `field` ahead of any action whose role opens it. */
const fieldCarriage = (field: number) => ({
  requirementForAction: async (
    context: Context,
    input: LinearFamilyPrerequisiteInput,
  ) => {
    const role = custodyRole(input);
    if (role === null) return null;
    const plan = await planned(context, input);
    if (!plan.fieldsOf(role).includes(field)) return null;
    const { material } = plan.admitted;
    return {
      planned: plan.proof.plan(field).openingPlan,
      compactCbor: material.bound.compact_cbor,
      witnessSetCompactCbor:
        material.material.proofSource.witnessSetCompactCbor.toString("hex"),
      certificate: {
        policyId: context.certificate.policyId,
        mintingScript: context.certificate.mintingScript,
        referenceScriptUtxo: context.references.fieldPreimageCertificateMint,
      },
    } satisfies FieldCarriageRequirement;
  },
});

const S = FraudProofComputationThreadStepDatum;

export const DISTINCT_ASSET_ACCUMULATION_LIMIT_FAMILY_DEFINITION = defineFamily(
  {
    category: "distinctAssetAccumulationLimit",
    stepDatumSchemas: [S, S, S, S, S, S, S, S, S, S, S, S],
    witnessRoles: WITNESS_ROLES,
    fieldPreimageCertificate: true,
    replayer: () => DISTINCT_ASSET_ACCUMULATION_LIMIT_COMPLETE_CANONICAL_REPLAY,
    adapter: {
      kind: "cursor",
      spec: DISTINCT_ASSET_ACCUMULATION_LIMIT_CURSOR_SPEC,
      stepContractNames: [
        "fraudProofDistinctAssetAccumulationLimit",
        "fraudProofDistinctAssetAccumulationLimitBindEvent",
        "fraudProofDistinctAssetAccumulationLimitBindPreState",
        "fraudProofDistinctAssetAccumulationLimitFamilyEntry",
        "fraudProofDistinctAssetAccumulationLimitInputs",
        "fraudProofDistinctAssetAccumulationLimitInputsResolve",
        "fraudProofDistinctAssetAccumulationLimitInputRank",
        "fraudProofDistinctAssetAccumulationLimitOutputsGrammar",
        "fraudProofDistinctAssetAccumulationLimitOutputs",
        "fraudProofDistinctAssetAccumulationLimitMintGrammar",
        "fraudProofDistinctAssetAccumulationLimitMint",
        "fraudProofDistinctAssetAccumulationLimitTerminal",
      ],
      transactionPort: (context) =>
        createDistinctAssetAccumulationLimitTransactionPort({
          ...context,
          contracts: distinctAssetAccumulationLimitWorkflowContracts(
            context.binding,
            context.certificate,
          ),
        }),
    },
    fieldCarriage: DISTINCT_ASSET_ACCUMULATION_LIMIT_FIELDS.map(fieldCarriage),
    proofChunk: async (context, input) => {
      if (input.action.input.stage !== "step_01") return null;
      const { admitted } = await planned(context, input);
      return admitted.coordinate.sourceKind === 0
        ? (await distinctAssetAccumulationLimitSourceInclusion(admitted))
            .txMembershipProofCbor
        : null;
    },
  },
);

export const createManifestBoundDistinctAssetAccumulationLimitWorkflow = (
  config: ManifestBoundDistinctAssetAccumulationLimitWorkflowConfig,
): Promise<ManifestBoundDistinctAssetAccumulationLimitWorkflow> =>
  assembleManifestBoundFamilyWorkflow(
    DISTINCT_ASSET_ACCUMULATION_LIMIT_FAMILY_DEFINITION,
    config,
  );

export const runOrResumeManifestBoundDistinctAssetAccumulationLimitWorkflow =
  async ({
    workflow,
    sources,
    journal,
  }: {
    readonly workflow: ManifestBoundDistinctAssetAccumulationLimitWorkflow;
    readonly sources: readonly RetainedDaPayloadSource[];
    readonly journal: FraudProofWorkflowJournalStore;
  }): Promise<FraudProofWorkflowRunResult> =>
    await runFraudProofWorkflowFromRetainedDa({
      deploymentFingerprint: workflow.binding.deploymentFingerprint,
      observation: await observeFraudProofWorkflowHeader(workflow.l1, {
        headerHash: workflow.binding.definition.headerHash,
      }),
      sources,
      replayer: DISTINCT_ASSET_ACCUMULATION_LIMIT_COMPLETE_CANONICAL_REPLAY,
      replayContext: workflow.replayContext,
      registry: createFraudProofWorkflowRegistry({
        adapters: [workflow.adapter],
        launchScope: ["distinctAssetAccumulationLimit"],
      }),
      journal,
      terminalVerifier: workflow.terminalVerifier,
      releaseFinalityAuthority: workflow.releaseFinalityAuthority,
    });
