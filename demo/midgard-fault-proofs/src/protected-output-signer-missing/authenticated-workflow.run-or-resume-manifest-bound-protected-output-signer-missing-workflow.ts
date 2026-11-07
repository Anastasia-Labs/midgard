import { fetchCanonicalBlockEvidence } from "../evidence/canonical-block-evidence.js";
import { type RetainedDaPayloadSource } from "../transition-trace/fetch.js";
import { observeFraudProofWorkflowHeader } from "../workflow/family-l1-observation.js";
import { type FraudProofWorkflowJournalStore } from "../workflow/journal.js";
import { assembleBoundManifestBoundFamilyWorkflow } from "../workflow/manifest-bound-family-assembly.js";
import {
  createCanonicalFamilyArtifactPort,
  executeManifestBoundFamilyRecovery,
} from "../workflow/manifest-bound-family-recovery.js";
import { createProtectedOutputSignerMissingRawL1StageResolver } from "./authenticated-workflow.create-protected-output-signer-missing-raw-l1-stage-resolver.js";
import {
  deriveProtectedOutputSignerMissingAuthenticatedSource,
  prepareProtectedOutputSignerMissingRecoveryMaterial,
  protectedOutputSignerStageFromL1,
} from "./authenticated-workflow.derive-protected-output-signer-missing-authenticated-source.js";
import {
  createManifestBoundProtectedOutputSignerMissingRuntime,
  type ManifestBoundProtectedOutputSignerMissingWorkflow,
  PROTECTED_OUTPUT_SIGNER_MISSING_FAMILY_DEFINITION,
} from "./authenticated-workflow.protected-output-signer-missing-family-definition.js";
import { detectProtectedOutputSignerMissingCompleteReplay } from "./protected-output-signer-missing.js";
import {
  type ProtectedOutputSignerJournal,
  type ProtectedOutputSignerStage,
} from "./workflow.js";

/**
 * Watcher-facing runner. Evidence is always reconstructed from authenticated
 * L1 plus public retained DA; unknown/caller-authored evidence fields fail.
 */
export const runOrResumeManifestBoundProtectedOutputSignerMissingWorkflow =
  async (input: {
    readonly workflow: ManifestBoundProtectedOutputSignerMissingWorkflow;
    readonly sources: readonly RetainedDaPayloadSource[];
    readonly journal: ProtectedOutputSignerJournal;
  }): Promise<ProtectedOutputSignerStage> => {
    if (Object.keys(input).sort().join(",") !== "journal,sources,workflow") {
      throw new Error(
        "protectedOutputSignerMissing runner rejects caller-authored evidence inputs",
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
      detectProtectedOutputSignerMissingCompleteReplay(canonical);
    if (findings.length !== 1)
      throw new Error(
        `protectedOutputSignerMissing public replay yielded ${findings.length.toString()} exact findings`,
      );
    const evidence = findings[0]!;
    const source = await deriveProtectedOutputSignerMissingAuthenticatedSource({
      block: canonical,
      evidence,
    });
    const runtime = createManifestBoundProtectedOutputSignerMissingRuntime({
      config: input.workflow.config,
      journal: input.journal,
      observe: async () =>
        protectedOutputSignerStageFromL1(
          (await input.workflow.l1.observe({ headerHash })).stage,
        ),
      resolveStage: createProtectedOutputSignerMissingRawL1StageResolver({
        config: input.workflow.config,
        l1: input.workflow.l1,
        source,
      }),
    });
    return await runtime.runOrResume(evidence);
  };

export const createProtectedOutputSignerMissingRecoveryAdapter = (
  workflow: ManifestBoundProtectedOutputSignerMissingWorkflow,
) => {
  const { config } = workflow;
  const material = createCanonicalFamilyArtifactPort(
    async ({ evidence, classification }) =>
      await prepareProtectedOutputSignerMissingRecoveryMaterial(
        evidence,
        classification.selected.detectionId,
      ),
  );
  const assembled = assembleBoundManifestBoundFamilyWorkflow(
    PROTECTED_OUTPUT_SIGNER_MISSING_FAMILY_DEFINITION,
    workflow.deployment,
    { config, material },
  );
  return { ...assembled };
};

export const executeManifestBoundProtectedOutputSignerMissingWorkflow = async ({
  workflow,
  sources,
  journal,
}: {
  readonly workflow: ManifestBoundProtectedOutputSignerMissingWorkflow;
  readonly sources: readonly RetainedDaPayloadSource[];
  readonly journal: FraudProofWorkflowJournalStore;
}) =>
  await executeManifestBoundFamilyRecovery({
    ...workflow,
    sources,
    journal,
    ...createProtectedOutputSignerMissingRecoveryAdapter(workflow),
  });
