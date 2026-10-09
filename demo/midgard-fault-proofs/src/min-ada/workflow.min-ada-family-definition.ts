import {
  FraudProofComputationThreadStepDatum,
  MinAdaStep02DatumSchema,
  MinAdaStep03DatumSchema,
  MinAdaStep04DatumSchema,
  MinAdaStep05DatumSchema,
} from "@al-ft/midgard-sdk";

import { fetchCanonicalBlockEvidence } from "../evidence/canonical-block-evidence.js";
import { parseContractDeploymentReferenceScriptAuthPolicyId } from "../inspect-contracts.js";
import type { RetainedDaPayloadSource } from "../transition-trace/fetch.js";
import type { FaultProofWitnessReferenceScripts } from "../witness-reference-scripts.js";
import {
  MIN_ADA_COMPLETE_CANONICAL_REPLAY,
  requireCompleteCanonicalReplayDecision,
} from "../workflow/complete-replay.js";
import { defineFamily } from "../workflow/family-definition.js";
import { observeFraudProofWorkflowHeader } from "../workflow/family-l1-observation.js";
import { type FieldCarriageRequirement } from "../workflow/field-carriage-prerequisite.js";
import type { FraudProofWorkflowJournalStore } from "../workflow/journal.js";
import { assembleManifestBoundFamilyWorkflow } from "../workflow/manifest-bound-family-assembly.js";
import {
  createFraudProofWorkflowRegistry,
  type FraudProofWorkflowRunResult,
  runFraudProofWorkflow,
} from "../workflow/orchestrator.js";
import type { MinAdaContracts } from "./contracts.js";
import {
  type BoundConfig,
  isForced,
  isTx,
  type MinAdaWorkflowReferenceScripts,
  txFieldPlan,
} from "./workflow.resolve-field.js";
import {
  type AssemblyContext,
  boundConfigs,
  type ManifestBoundMinAdaWorkflow,
  type ManifestBoundMinAdaWorkflowConfig,
  type MinAdaRuntime,
  transactionPort,
} from "./workflow.transaction-port.js";
import { admitMinAdaWorkflowArtifact as admitMinAdaArtifact } from "./workflow-artifact.js";
import { MIN_ADA_CURSOR_SPEC } from "./workflow-spec.js";

const boundFor = (context: AssemblyContext): BoundConfig => {
  const existing = boundConfigs.get(context);
  if (existing !== undefined) return existing;
  const { binding, certificate } = context;
  const { replayContext } = context.runtime;
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
    yields: {
      tx: context.auxiliaryReferences.tx!,
      utxo: context.auxiliaryReferences.utxo!,
    },
  };
  const bound: BoundConfig = {
    binding,
    lucid: context.lucid,
    signer: context.signer,
    contracts,
    references,
    ...(replayContext === undefined ? {} : { replayContext }),
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
  5,
  MinAdaRuntime
>({
  category: "minAda",
  stepDatumSchemas: [
    FraudProofComputationThreadStepDatum,
    MinAdaStep02DatumSchema,
    MinAdaStep03DatumSchema,
    MinAdaStep04DatumSchema,
    MinAdaStep05DatumSchema,
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
    utxo: "fraudProofMinAdaStep02UtxoWithdraw",
  },
  adapter: {
    kind: "cursor",
    spec: MIN_ADA_CURSOR_SPEC,
    stepContractNames: [
      "fraudProofMinAda",
      "fraudProofMinAdaStep02",
      "fraudProofMinAdaStep03",
      "fraudProofMinAdaStep04",
      "fraudProofMinAdaStep05",
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
        if (!isTx(admitted)) return null;
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
    return action.input.stage === "step_01" &&
      isTx(admitted) &&
      !isForced(admitted)
      ? admitted.prepared.txInclusion.txMembershipProofCbor
      : action.input.stage === "step_02" && !isTx(admitted)
        ? admitted.prepared.postMembershipProofCbor
        : action.input.stage === "step_04" && !isTx(admitted)
          ? admitted.prepared.predecessorNonMembershipProofCbor
          : null;
  },
  extend: (context) =>
    context.runtime.replayContext === undefined
      ? {}
      : { replayContext: context.runtime.replayContext },
});

export const createManifestBoundMinAdaWorkflow = async (
  config: ManifestBoundMinAdaWorkflowConfig,
): Promise<ManifestBoundMinAdaWorkflow> => {
  const runtime: MinAdaRuntime =
    config.replayContext === undefined
      ? {}
      : { replayContext: config.replayContext };
  const workflow = await assembleManifestBoundFamilyWorkflow(
    MIN_ADA_FAMILY_DEFINITION,
    { ...config, auxiliaryReferenceScripts: config.referenceScripts.yields },
    runtime,
  );
  return workflow as unknown as ManifestBoundMinAdaWorkflow;
};

export const runOrResumeManifestBoundMinAdaWorkflow = async ({
  workflow,
  sources,
  journal,
}: {
  readonly workflow: ManifestBoundMinAdaWorkflow;
  readonly sources: readonly RetainedDaPayloadSource[];
  readonly journal: FraudProofWorkflowJournalStore;
}): Promise<FraudProofWorkflowRunResult> => {
  const observation = await observeFraudProofWorkflowHeader(workflow.l1, {
    headerHash: workflow.binding.definition.headerHash,
  });
  const evidence = await fetchCanonicalBlockEvidence({
    observation,
    sources,
    minimumConfirmationDepth: 1,
  });
  const replayer = MIN_ADA_COMPLETE_CANONICAL_REPLAY;
  const { replayContext } = workflow;
  const decision = await replayer.replay(evidence, replayContext);
  const detections = requireCompleteCanonicalReplayDecision({
    evidence,
    replayer,
    decision,
    ...(replayContext === undefined ? {} : { context: replayContext }),
  });
  return await runFraudProofWorkflow({
    deploymentFingerprint: workflow.binding.deploymentFingerprint,
    evidence,
    detections,
    registry: createFraudProofWorkflowRegistry({
      adapters: [workflow.adapter],
      launchScope: ["minAda"],
    }),
    journal,
    terminalVerifier: workflow.terminalVerifier,
    releaseFinalityAuthority: workflow.releaseFinalityAuthority,
  });
};
