import { type RetainedDaPayloadSource } from "../transition-trace/fetch.js";
import { type FraudProofWorkflowJournalStore } from "../workflow/journal.js";
import { assembleBoundManifestBoundFamilyWorkflow } from "../workflow/manifest-bound-family-assembly.js";
import {
  createCanonicalFamilyArtifactPort,
  executeManifestBoundFamilyRecovery,
} from "../workflow/manifest-bound-family-recovery.js";
import { prepareOutputReferenceScriptDecodingRecoveryMaterial } from "./authenticated-workflow.derive-output-reference-script-decoding-authenticated-source.js";
import {
  type ManifestBoundOutputReferenceScriptDecodingWorkflow,
  OUTPUT_REFERENCE_SCRIPT_DECODING_FAMILY_DEFINITION,
} from "./authenticated-workflow.output-reference-script-decoding-family-definition.js";

export const createOutputReferenceScriptDecodingRecoveryAdapter = (
  workflow: ManifestBoundOutputReferenceScriptDecodingWorkflow,
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

export const executeManifestBoundOutputReferenceScriptDecodingWorkflow =
  async ({
    workflow,
    sources,
    journal,
  }: {
    readonly workflow: ManifestBoundOutputReferenceScriptDecodingWorkflow;
    readonly sources: readonly RetainedDaPayloadSource[];
    readonly journal: FraudProofWorkflowJournalStore;
  }) =>
    await executeManifestBoundFamilyRecovery({
      ...workflow,
      sources,
      journal,
      ...createOutputReferenceScriptDecodingRecoveryAdapter(workflow),
    });
