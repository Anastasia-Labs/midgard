import { fetchCanonicalBlockEvidence } from "../evidence/canonical-block-evidence.js";
import { type RetainedDaPayloadSource } from "../transition-trace/fetch.js";
import { observeFraudProofWorkflowHeader } from "../workflow/family-l1-observation.js";
import { type FraudProofWorkflowJournalStore } from "../workflow/journal.js";
import { assembleBoundManifestBoundFamilyWorkflow } from "../workflow/manifest-bound-family-assembly.js";
import {
  createCanonicalFamilyArtifactPort,
  executeManifestBoundFamilyRecovery,
} from "../workflow/manifest-bound-family-recovery.js";
import { createOutputReferenceScriptDecodingRawL1StageResolver } from "./authenticated-workflow.create-output-reference-script-decoding-raw-l1-stage-resolver.js";
import {
  deriveOutputReferenceScriptDecodingAuthenticatedSource,
  outputReferenceScriptDecodingStageFromL1,
  prepareOutputReferenceScriptDecodingRecoveryMaterial,
} from "./authenticated-workflow.derive-output-reference-script-decoding-authenticated-source.js";
import {
  createManifestBoundOutputReferenceScriptDecodingRuntime,
  type ManifestBoundOutputReferenceScriptDecodingWorkflow,
  OUTPUT_REFERENCE_SCRIPT_DECODING_FAMILY_DEFINITION,
} from "./authenticated-workflow.output-reference-script-decoding-family-definition.js";
import { detectOutputReferenceScriptDecodingCompleteReplay } from "./output-reference-script-decoding.js";
import {
  type OutputReferenceScriptDecodingJournal,
  type OutputReferenceScriptDecodingStage,
} from "./workflow.js";

/**
 * Watcher-facing runner. Evidence is always reconstructed from authenticated
 * L1 plus public retained DA; unknown/caller-authored evidence fields fail.
 */
export const runOrResumeManifestBoundOutputReferenceScriptDecodingWorkflow =
  async (input: {
    readonly workflow: ManifestBoundOutputReferenceScriptDecodingWorkflow;
    readonly sources: readonly RetainedDaPayloadSource[];
    readonly journal: OutputReferenceScriptDecodingJournal;
  }): Promise<OutputReferenceScriptDecodingStage> => {
    if (Object.keys(input).sort().join(",") !== "journal,sources,workflow") {
      throw new Error(
        "outputReferenceScriptDecoding runner rejects caller-authored evidence inputs",
      );
    }
    const headerHash = input.workflow.binding.definition.headerHash;
    const observation = await observeFraudProofWorkflowHeader(
      input.workflow.l1,
      { headerHash },
    );
    const canonical = await fetchCanonicalBlockEvidence({
      observation,
      sources: input.sources,
    });
    const findings =
      detectOutputReferenceScriptDecodingCompleteReplay(canonical);
    if (findings.length !== 1)
      throw new Error(
        `outputReferenceScriptDecoding public replay yielded ${findings.length.toString()} exact findings`,
      );
    const evidence = findings[0]!;
    const source = await deriveOutputReferenceScriptDecodingAuthenticatedSource(
      {
        block: canonical,
        evidence,
      },
    );
    const runtime = createManifestBoundOutputReferenceScriptDecodingRuntime({
      config: input.workflow.config,
      journal: input.journal,
      observe: async () =>
        outputReferenceScriptDecodingStageFromL1(
          (await input.workflow.l1.observe({ headerHash })).stage,
        ),
      resolveStage: createOutputReferenceScriptDecodingRawL1StageResolver({
        config: input.workflow.config,
        l1: input.workflow.l1,
        source,
      }),
      stateQueueMutationLeaseCoordinator:
        input.workflow.stateQueueMutationLeaseCoordinator,
    });
    return await runtime.runOrResume(evidence);
  };

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
