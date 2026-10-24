import {
  FraudProofComputationThreadStepDatum,
  NativeScriptInvalidStep02DatumSchema,
  NativeScriptInvalidStep03DatumSchema,
  NativeScriptInvalidStep04DatumSchema,
  NativeScriptInvalidStep05DatumSchema,
} from "@al-ft/midgard-sdk";

import type { RetainedDaPayloadSource } from "../transition-trace/fetch.js";
import { NATIVE_SCRIPT_INVALID_COMPLETE_CANONICAL_REPLAY } from "../workflow/complete-replay.js";
import { cursorFamilyActionInput } from "../workflow/cursor-family-runtime.js";
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
import type { NativeScriptInvalidContracts } from "./contracts.js";
import { scriptFieldPlan, signerFieldPlan } from "./workflow.resolve-field.js";
import {
  type ManifestBoundNativeScriptInvalidWorkflow,
  type ManifestBoundNativeScriptInvalidWorkflowConfig,
  transactionPort,
} from "./workflow.transaction-port.js";
import { admitNativeScriptInvalidWorkflowArtifact } from "./workflow-artifact.js";
import { NATIVE_SCRIPT_INVALID_CURSOR_SPEC } from "./workflow-spec.js";

export const NATIVE_SCRIPT_INVALID_FAMILY_DEFINITION = defineFamily({
  category: "nativeScriptInvalid",
  stepDatumSchemas: [
    FraudProofComputationThreadStepDatum,
    NativeScriptInvalidStep02DatumSchema,
    NativeScriptInvalidStep03DatumSchema,
    NativeScriptInvalidStep04DatumSchema,
    NativeScriptInvalidStep05DatumSchema,
  ],
  witnessRoles: [
    "computationThreadMint",
    "fraudProofMint",
    "phasMembershipWithdraw",
    "chunkedVerifyWithdraw",
    "pexcludesWithdraw",
  ],
  fieldPreimageCertificate: true,
  replayer: () => NATIVE_SCRIPT_INVALID_COMPLETE_CANONICAL_REPLAY,
  adapter: {
    kind: "cursor",
    spec: NATIVE_SCRIPT_INVALID_CURSOR_SPEC,
    stepContractNames: [
      "fraudProofNativeScriptInvalid",
      "fraudProofNativeScriptInvalidStep02",
      "fraudProofNativeScriptInvalidStep03",
      "fraudProofNativeScriptInvalidStep04",
      "fraudProofNativeScriptInvalidStep05",
    ],
    transactionPort: (context) => {
      const { binding, certificate } = context;
      const chain = binding.resolvedContracts.contracts.nativeScriptInvalid;
      const stateQueuePolicyId = binding.resolvedContracts.stateQueuePolicyId;
      if (
        chain === undefined ||
        stateQueuePolicyId === undefined ||
        certificate === null
      ) {
        throw new Error(
          "native-script-invalid manifest omitted required contracts",
        );
      }

      const contracts: NativeScriptInvalidContracts = Object.freeze({
        steps: chain.steps,
        computationThread:
          binding.resolvedContracts.contracts.computationThread,
        fraudProof: {
          policyId: binding.resolvedContracts.contracts.fraudProof.policyId,
          mintingScript:
            binding.resolvedContracts.contracts.fraudProof.mintingScript,
          spendingScriptAddress:
            binding.resolvedContracts.contracts.fraudProof
              .spendingScriptAddress,
        },
        hubOraclePolicyId: binding.resolvedContracts.hubOraclePolicyId,
        stateQueuePolicyId,
        fieldPreimageCertificatePolicyId: certificate.policyId,
      });

      return transactionPort({ ...context, contracts });
    },
  },
  fieldCarriage: [
    {
      requirementForAction: async (context, { action, artifact }) => {
        const { certificate, references } = context;

        const input = cursorFamilyActionInput({
          category: "nativeScriptInvalid",
          action,
        });
        const admitted =
          await admitNativeScriptInvalidWorkflowArtifact(artifact);
        const planned =
          input.stage === "step_02"
            ? scriptFieldPlan(admitted, context.signer.paymentKeyHash)
            : input.stage === "step_03" || input.stage === "step_04"
              ? signerFieldPlan(admitted, context.signer.paymentKeyHash)
              : null;
        if (planned === null) return null;
        return {
          planned,
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
    if (action.input.stage !== "step_01") return null;
    const admitted = await admitNativeScriptInvalidWorkflowArtifact(artifact);
    return admitted.prepared.txInclusion?.txMembershipProofCbor ?? null;
  },
});

export const createManifestBoundNativeScriptInvalidWorkflow = (
  config: ManifestBoundNativeScriptInvalidWorkflowConfig,
): Promise<ManifestBoundNativeScriptInvalidWorkflow> =>
  assembleManifestBoundFamilyWorkflow(
    NATIVE_SCRIPT_INVALID_FAMILY_DEFINITION,
    config,
  );

export const runOrResumeManifestBoundNativeScriptInvalidWorkflow = async ({
  workflow,
  sources,
  journal,
}: {
  readonly workflow: ManifestBoundNativeScriptInvalidWorkflow;
  readonly sources: readonly RetainedDaPayloadSource[];
  readonly journal: FraudProofWorkflowJournalStore;
}): Promise<FraudProofWorkflowRunResult> =>
  await runFraudProofWorkflowFromRetainedDa({
    deploymentFingerprint: workflow.binding.deploymentFingerprint,
    observation: await observeFraudProofWorkflowHeader(workflow.l1, {
      headerHash: workflow.binding.definition.headerHash,
    }),
    sources,
    replayer: NATIVE_SCRIPT_INVALID_COMPLETE_CANONICAL_REPLAY,
    registry: createFraudProofWorkflowRegistry({
      adapters: [workflow.adapter],
      launchScope: ["nativeScriptInvalid"],
    }),
    journal,
    terminalVerifier: workflow.terminalVerifier,
    releaseFinalityAuthority: workflow.releaseFinalityAuthority,
  });
