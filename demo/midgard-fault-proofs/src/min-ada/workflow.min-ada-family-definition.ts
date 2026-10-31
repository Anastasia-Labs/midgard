import {
  FraudProofComputationThreadStepDatum,
  MinAdaStep02DatumSchema,
  MinAdaStep03DatumSchema,
  MinAdaStep04DatumSchema,
} from "@al-ft/midgard-sdk";

import { parseContractDeploymentReferenceScriptAuthPolicyId } from "../inspect-contracts.js";
import type { RetainedDaPayloadSource } from "../transition-trace/fetch.js";
import type { FaultProofWitnessReferenceScripts } from "../witness-reference-scripts.js";
import { MIN_ADA_COMPLETE_CANONICAL_REPLAY } from "../workflow/complete-replay.js";
import { defineFamily } from "../workflow/family-definition.js";
import { observeFraudProofWorkflowHeader } from "../workflow/family-l1-observation.js";
import { type FieldCarriageRequirement } from "../workflow/field-carriage-prerequisite.js";
import type { FraudProofWorkflowJournalStore } from "../workflow/journal.js";
import { assembleManifestBoundFamilyWorkflow } from "../workflow/manifest-bound-family-assembly.js";
import {
  createFraudProofWorkflowRegistry,
  type FraudProofWorkflowRunResult,
  runFraudProofWorkflowFromRetainedDa,
} from "../workflow/orchestrator.js";
import type { MinAdaContracts } from "./contracts.js";
import {
  type BoundConfig,
  isForced,
  type MinAdaWorkflowReferenceScripts,
  txFieldPlan,
} from "./workflow.resolve-field.js";
import {
  type AssemblyContext,
  boundConfigs,
  type ManifestBoundMinAdaWorkflow,
  type ManifestBoundMinAdaWorkflowConfig,
  transactionPort,
} from "./workflow.transaction-port.js";
import { admitMinAdaWorkflowArtifact as admitMinAdaArtifact } from "./workflow-artifact.js";
import { MIN_ADA_CURSOR_SPEC } from "./workflow-spec.js";

const boundFor = (context: AssemblyContext): BoundConfig => {
  const existing = boundConfigs.get(context);
  if (existing !== undefined) return existing;
  const { binding, certificate } = context;
  const chain = binding.resolvedContracts.contracts.minAda;
  const stateQueuePolicyId = binding.resolvedContracts.stateQueuePolicyId;
  if (
    chain === undefined ||
    stateQueuePolicyId === undefined ||
    certificate === null
  ) {
    throw new Error("min-ada manifest omitted required contracts");
  }
  const contracts: MinAdaContracts = Object.freeze({
    steps: chain.steps,
    yields: chain.yields,
    computationThread: binding.resolvedContracts.contracts.computationThread,
    fraudProof: {
      policyId: binding.resolvedContracts.contracts.fraudProof.policyId,
      mintingScript:
        binding.resolvedContracts.contracts.fraudProof.mintingScript,
      spendingScriptAddress:
        binding.resolvedContracts.contracts.fraudProof.spendingScriptAddress,
    },
    hubOraclePolicyId: binding.resolvedContracts.hubOraclePolicyId,
    stateQueuePolicyId,
    fieldPreimageCertificatePolicyId: certificate.policyId,
    referenceScriptAuthPolicyId:
      parseContractDeploymentReferenceScriptAuthPolicyId(
        binding.deploymentInfo,
        "reference-script-auth minting",
      ),
  });
  const references: MinAdaWorkflowReferenceScripts = {
    ...context.references,
    yields: { tx: context.auxiliaryReferences.tx! },
  };
  const bound: BoundConfig = {
    binding,
    lucid: context.lucid,
    signer: context.signer,
    contracts,
    references,
    stateQueueMutationLeaseCoordinator:
      context.stateQueueMutationLeaseCoordinator,
  };

  boundConfigs.set(context, bound);
  return bound;
};

export const MIN_ADA_FAMILY_DEFINITION = defineFamily<
  "minAda",
  keyof FaultProofWitnessReferenceScripts,
  true,
  4
>({
  category: "minAda",
  stepDatumSchemas: [
    FraudProofComputationThreadStepDatum,
    MinAdaStep02DatumSchema,
    MinAdaStep03DatumSchema,
    MinAdaStep04DatumSchema,
  ],
  witnessRoles: [
    "computationThreadMint",
    "fraudProofMint",
    "phasMembershipWithdraw",
    "chunkedVerifyWithdraw",
    "pexcludesWithdraw",
  ],
  fieldPreimageCertificate: true,
  replayer: () => MIN_ADA_COMPLETE_CANONICAL_REPLAY,
  auxiliaryReferenceScripts: {
    tx: "fraudProofMinAdaStep02TxWithdraw",
  },
  adapter: {
    kind: "cursor",
    spec: MIN_ADA_CURSOR_SPEC,
    stepContractNames: [
      "fraudProofMinAda",
      "fraudProofMinAdaStep02",
      "fraudProofMinAdaStep03",
      "fraudProofMinAdaStep04",
    ],
    transactionPort: (context) => {
      return transactionPort(boundFor(context));
    },
  },
  fieldCarriage: [
    {
      requirementForAction: async (context, { action, artifact }) => {
        const { certificate, references } = context;

        if (action.input.stage !== "step_02") return null;
        const admitted = await admitMinAdaArtifact(artifact);
        return {
          planned: txFieldPlan(admitted, context.signer.paymentKeyHash),
          compactCbor: admitted.prepared.nativeTxCompactCbor,
          certificate: {
            policyId: certificate.policyId,
            mintingScript: certificate.mintingScript,
            referenceScriptUtxo: references.fieldPreimageCertificateMint,
          },
        } satisfies FieldCarriageRequirement;
      },
    },
  ],
  proofChunk: async (_context, { action, artifact }) => {
    const admitted = await admitMinAdaArtifact(artifact);
    return action.input.stage === "step_01" && !isForced(admitted)
      ? admitted.prepared.txInclusion.txMembershipProofCbor
      : null;
  },
});

export const createManifestBoundMinAdaWorkflow = async (
  config: ManifestBoundMinAdaWorkflowConfig,
): Promise<ManifestBoundMinAdaWorkflow> =>
  await assembleManifestBoundFamilyWorkflow(MIN_ADA_FAMILY_DEFINITION, {
    ...config,
    auxiliaryReferenceScripts: config.referenceScripts.yields,
  });

export const runOrResumeManifestBoundMinAdaWorkflow = async ({
  workflow,
  sources,
  journal,
}: {
  readonly workflow: ManifestBoundMinAdaWorkflow;
  readonly sources: readonly RetainedDaPayloadSource[];
  readonly journal: FraudProofWorkflowJournalStore;
}): Promise<FraudProofWorkflowRunResult> =>
  await runFraudProofWorkflowFromRetainedDa({
    deploymentFingerprint: workflow.binding.deploymentFingerprint,
    observation: await observeFraudProofWorkflowHeader(workflow.l1, {
      headerHash: workflow.binding.definition.headerHash,
    }),
    sources,
    replayer: MIN_ADA_COMPLETE_CANONICAL_REPLAY,
    registry: createFraudProofWorkflowRegistry({
      adapters: [workflow.adapter],
      launchScope: ["minAda"],
    }),
    journal,
    terminalVerifier: workflow.terminalVerifier,
    releaseFinalityAuthority: workflow.releaseFinalityAuthority,
  });
