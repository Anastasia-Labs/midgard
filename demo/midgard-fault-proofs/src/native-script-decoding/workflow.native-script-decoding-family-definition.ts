import {
  FraudProofComputationThreadStepDatum,
  NativeScriptDecodingStep02DatumSchema,
  NativeScriptDecodingStep03AdvanceOrCloseDatumSchema,
  NativeScriptDecodingStep03BindDescriptorDatumSchema,
  NativeScriptDecodingStep03OpenSubjectDatumSchema,
  NativeScriptDecodingStep04DatumSchema,
} from "@al-ft/midgard-sdk";

import type { RetainedDaPayloadSource } from "../transition-trace/fetch.js";
import {
  type CompleteCanonicalReplayContext,
  NATIVE_SCRIPT_DECODING_COMPLETE_CANONICAL_REPLAY,
} from "../workflow/complete-replay.js";
import { type CursorFamilyTransactionPort } from "../workflow/cursor-family-adapter.js";
import { cursorFamilyActionInput } from "../workflow/cursor-family-runtime.js";
import { NATIVE_SCRIPT_DECODING_CURSOR_SPEC } from "../workflow/cursor-family-spec.js";
import { type FraudProofWorkflowDeploymentBinding } from "../workflow/deployment-manifest-binding.js";
import { defineFamily } from "../workflow/family-definition.js";
import { type FraudProofFamilyL1ObservationPort } from "../workflow/family-l1-observation.js";
import { observeFraudProofWorkflowHeader } from "../workflow/family-l1-observation.js";
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
import { admitNativeScriptDecodingWorkflowArtifact } from "./artifact.js";
import type { NativeScriptDecodingContracts } from "./contracts.js";
import {
  createNativeScriptDecodingTransactionPort,
  type ManifestBoundNativeScriptDecodingWorkflowConfig,
  namesField,
  sourceInclusion,
  subjectPlan,
} from "./workflow.create-native-script-decoding-transaction-port.js";

export type ManifestBoundNativeScriptDecodingWorkflow = Readonly<{
  replayContext?: CompleteCanonicalReplayContext;
  binding: FraudProofWorkflowDeploymentBinding<"nativeScriptDecoding">;
  l1: FraudProofFamilyL1ObservationPort<"nativeScriptDecoding">;
  transactions: CursorFamilyTransactionPort<"nativeScriptDecoding">;
  adapter: FraudProofFamilyWorkflowAdapter;
  terminalVerifier: FraudProofWorkflowTerminalVerifier;
  releaseFinalityAuthority: FraudProofReleaseFinalityAuthority;
}>;

export const NATIVE_SCRIPT_DECODING_FAMILY_DEFINITION = defineFamily({
  category: "nativeScriptDecoding",
  stepDatumSchemas: [
    FraudProofComputationThreadStepDatum,
    NativeScriptDecodingStep02DatumSchema,
    NativeScriptDecodingStep03OpenSubjectDatumSchema,
    NativeScriptDecodingStep03BindDescriptorDatumSchema,
    NativeScriptDecodingStep03AdvanceOrCloseDatumSchema,
    NativeScriptDecodingStep04DatumSchema,
  ],
  witnessRoles: [
    "computationThreadMint",
    "fraudProofMint",
    "phasMembershipWithdraw",
    "chunkedVerifyWithdraw",
    "pexcludesWithdraw",
  ],
  fieldPreimageCertificate: true,
  replayer: () => NATIVE_SCRIPT_DECODING_COMPLETE_CANONICAL_REPLAY,
  adapter: {
    kind: "cursor",
    spec: NATIVE_SCRIPT_DECODING_CURSOR_SPEC,
    stepContractNames: [
      "fraudProofNativeScriptDecoding",
      "fraudProofNativeScriptDecodingStep02",
      "fraudProofNativeScriptDecodingStep03OpenSubject",
      "fraudProofNativeScriptDecodingStep03BindDescriptor",
      "fraudProofNativeScriptDecodingStep03AdvanceOrClose",
      "fraudProofNativeScriptDecodingStep04",
    ],
    transactionPort: (context) => {
      const { binding, certificate } = context;
      const chain = binding.resolvedContracts.contracts.nativeScriptDecoding;
      const stateQueuePolicyId = binding.resolvedContracts.stateQueuePolicyId;
      if (
        chain === undefined ||
        stateQueuePolicyId === undefined ||
        certificate === null
      ) {
        throw new Error(
          "native-script-decoding manifest omitted required contracts",
        );
      }

      const contracts: NativeScriptDecodingContracts = Object.freeze({
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

      return createNativeScriptDecodingTransactionPort({
        ...context,
        contracts,
      });
    },
  },
  fieldCarriage: [
    {
      requirementForAction: async (context, { action, artifact }) => {
        const { certificate, references } = context;

        const input = cursorFamilyActionInput({
          category: "nativeScriptDecoding",
          action,
        });
        const admitted =
          await admitNativeScriptDecodingWorkflowArtifact(artifact);
        const planned =
          input.stage === "step_03" && namesField(admitted)
            ? subjectPlan(admitted, context.signer.paymentKeyHash)
            : null;
        if (planned === null) return null;
        return {
          planned,
          compactCbor:
            admitted.material.proofSource.compactCbor.toString("hex"),
          witnessSetCompactCbor:
            admitted.material.proofSource.witnessSetCompactCbor.toString("hex"),
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
    const admitted = await admitNativeScriptDecodingWorkflowArtifact(artifact);
    return admitted.coordinate.sourceKind === 0
      ? (await sourceInclusion(admitted)).txMembershipProofCbor
      : null;
  },
});

export const createManifestBoundNativeScriptDecodingWorkflow = (
  config: ManifestBoundNativeScriptDecodingWorkflowConfig,
): Promise<ManifestBoundNativeScriptDecodingWorkflow> =>
  assembleManifestBoundFamilyWorkflow(
    NATIVE_SCRIPT_DECODING_FAMILY_DEFINITION,
    config,
  );

export const runOrResumeManifestBoundNativeScriptDecodingWorkflow = async ({
  workflow,
  sources,
  journal,
}: {
  readonly workflow: ManifestBoundNativeScriptDecodingWorkflow;
  readonly sources: readonly RetainedDaPayloadSource[];
  readonly journal: FraudProofWorkflowJournalStore;
}): Promise<FraudProofWorkflowRunResult> =>
  await runFraudProofWorkflowFromRetainedDa({
    deploymentFingerprint: workflow.binding.deploymentFingerprint,
    observation: await observeFraudProofWorkflowHeader(workflow.l1, {
      headerHash: workflow.binding.definition.headerHash,
    }),
    sources,
    replayer: NATIVE_SCRIPT_DECODING_COMPLETE_CANONICAL_REPLAY,
    replayContext: workflow.replayContext,
    registry: createFraudProofWorkflowRegistry({
      adapters: [workflow.adapter],
      launchScope: ["nativeScriptDecoding"],
    }),
    journal,
    terminalVerifier: workflow.terminalVerifier,
    releaseFinalityAuthority: workflow.releaseFinalityAuthority,
  });
