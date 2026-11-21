import { type RetainedDaPayloadSource } from "../transition-trace/fetch.js";
import { type FraudProofWorkflowJournalStore } from "../workflow/journal.js";
import {
  assembleBoundManifestBoundFamilyWorkflow,
  bindManifestBoundFamilyWorkflow,
} from "../workflow/manifest-bound-family-assembly.assemble-bound-manifest-bound-family-workflow.js";
import {
  createCanonicalFamilyArtifactPort,
  executeManifestBoundFamilyRecovery,
  type ManifestBoundFamilyWorkflowWithRecovery,
  withManifestBoundFamilyRecovery,
} from "../workflow/manifest-bound-family-recovery.js";
import { scriptDecodeDeploymentFromReferences } from "../workflow/script-decode-prerequisite.deployment.js";
import { type WalletOnlySend } from "../workflow/wallet-only-send-gate.js";
import {
  createOutputReferenceScriptDecodingBoundConfig,
  OUTPUT_REFERENCE_SCRIPT_DECODING_WORKFLOW,
} from "./authenticated-workflow.create-output-reference-script-decoding-bound-config.js";
import { prepareOutputReferenceScriptDecodingRecoveryMaterial } from "./authenticated-workflow.derive-output-reference-script-decoding-authenticated-source.js";
import {
  type ManifestBoundOutputReferenceScriptDecodingInstallation,
  type ManifestBoundOutputReferenceScriptDecodingWorkflowConfig,
  OUTPUT_REFERENCE_SCRIPT_DECODING_FAMILY_DEFINITION,
} from "./authenticated-workflow.output-reference-script-decoding-family-definition.js";

export const createOutputReferenceScriptDecodingRecoveryAdapter = (
  workflow: ManifestBoundOutputReferenceScriptDecodingInstallation,
) => {
  const { config } = workflow;
  const material = createCanonicalFamilyArtifactPort(
    async ({ evidence, classification }) =>
      await prepareOutputReferenceScriptDecodingRecoveryMaterial(
        evidence,
        classification.selected.detectionId,
      ),
  );
  const assembled = assembleBoundManifestBoundFamilyWorkflow(
    OUTPUT_REFERENCE_SCRIPT_DECODING_FAMILY_DEFINITION,
    workflow.deployment,
    { config, material },
  );
  return { ...assembled };
};

export type ManifestBoundOutputReferenceScriptDecodingWorkflow =
  ManifestBoundFamilyWorkflowWithRecovery<
    ManifestBoundOutputReferenceScriptDecodingInstallation,
    ReturnType<typeof createOutputReferenceScriptDecodingRecoveryAdapter>
  >;

/** Production installation factory; no evidence object is accepted here. */
export const createManifestBoundOutputReferenceScriptDecodingWorkflow = async (
  input: ManifestBoundOutputReferenceScriptDecodingWorkflowConfig,
): Promise<ManifestBoundOutputReferenceScriptDecodingWorkflow> => {
  const deployment = await bindManifestBoundFamilyWorkflow(
    OUTPUT_REFERENCE_SCRIPT_DECODING_FAMILY_DEFINITION,
    {
      ...input,
      referenceScripts: {
        steps: [
          input.referenceScripts.step01,
          input.referenceScripts.step02,
          input.referenceScripts.step03,
          input.referenceScripts.step04,
          input.referenceScripts.step05,
          input.referenceScripts.step06,
        ],
        witnesses: input.referenceScripts.witnesses,
        fieldPreimageCertificateMint:
          input.referenceScripts.fieldPreimageCertificateMint,
        fieldGrammarCertificateSpend:
          input.referenceScripts.fieldGrammarCertificateSpend,
      },
    },
  );
  const config = createOutputReferenceScriptDecodingBoundConfig(
    input,
    deployment.binding,
    scriptDecodeDeploymentFromReferences({
      blueprint: deployment.binding.blueprint,
      network: deployment.binding.network,
      references: deployment.auxiliaryReferences,
    }),
  );
  const l1 = deployment.l1;
  return withManifestBoundFamilyRecovery(
    Object.freeze({
      deployment,
      workflowVersion: OUTPUT_REFERENCE_SCRIPT_DECODING_WORKFLOW,
      config,
      binding: config.binding,
      l1,
      stateQueueMutationLeaseCoordinator:
        input.stateQueueMutationLeaseCoordinator,
      decisionDigest: input.decisionDigest,
    }),
    createOutputReferenceScriptDecodingRecoveryAdapter,
  );
};

export const executeManifestBoundOutputReferenceScriptDecodingWorkflow =
  async ({
    workflow,
    sources,
    journal,
    walletOnlySend,
  }: {
    readonly workflow: ManifestBoundOutputReferenceScriptDecodingWorkflow;
    readonly sources: readonly RetainedDaPayloadSource[];
    readonly journal: FraudProofWorkflowJournalStore;
    readonly walletOnlySend: WalletOnlySend;
  }) =>
    await executeManifestBoundFamilyRecovery({
      ...workflow,
      sources,
      journal,
      walletOnlySend,
    });
