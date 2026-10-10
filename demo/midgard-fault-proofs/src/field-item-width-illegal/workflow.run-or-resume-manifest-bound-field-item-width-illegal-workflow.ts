import { fetchCanonicalBlockEvidence } from "../evidence/canonical-block-evidence.js";
import type { StateQueueMutationLeaseCoordinator } from "../remove-fraudulent-block.js";
import { type RetainedDaPayloadSource } from "../transition-trace/fetch.js";
import { type FraudProofWorkflowDeploymentBinding } from "../workflow/deployment-manifest-binding.js";
import { type FamilyDeploymentContext } from "../workflow/family-definition.js";
import { type FraudProofFamilyL1ObservationPort } from "../workflow/family-l1-observation.js";
import { observeFraudProofWorkflowHeader } from "../workflow/family-l1-observation.js";
import { type FraudProofWorkflowJournalStore } from "../workflow/journal.js";
import type { FraudProofL1Source } from "../workflow/l1-source.js";
import {
  assembleBoundManifestBoundFamilyWorkflow,
  bindManifestBoundFamilyWorkflow,
} from "../workflow/manifest-bound-family-assembly.js";
import {
  createCanonicalFamilyArtifactPort,
  executeManifestBoundFamilyRecovery,
} from "../workflow/manifest-bound-family-recovery.js";
import {
  type FieldItemWidthJournal,
  type FieldItemWidthStage,
} from "./field-item-width-illegal.js";
import {
  createFieldItemWidthIllegalBoundConfig,
  createFieldItemWidthIllegalRawL1StageResolver,
  FIELD_ITEM_WIDTH_ILLEGAL_WORKFLOW,
  type FieldItemWidthIllegalRuntimeLoader,
  type LoadManifestBoundFieldItemWidthIllegalConfig,
  type ManifestBoundFieldItemWidthIllegalConfig,
} from "./workflow.create-field-item-width-illegal-raw-l1-stage-resolver.js";
import {
  deriveFieldItemWidthIllegalAuthenticatedSource,
  deriveFieldItemWidthIllegalEvidenceFromCanonicalBlock,
  fieldItemWidthStageFromL1,
  prepareFieldItemWidthIllegalRecoveryMaterial,
} from "./workflow.derive-field-item-width-illegal-authenticated-source.js";
import {
  createManifestBoundFieldItemWidthIllegalRuntime,
  FIELD_ITEM_WIDTH_ILLEGAL_FAMILY_DEFINITION,
  loadManifestBoundFieldItemWidthIllegalConfig,
} from "./workflow.field-item-width-illegal-family-definition.js";

export const loadFieldItemWidthIllegalRuntime = async (
  input: FieldItemWidthIllegalRuntimeLoader,
) => {
  const config = await loadManifestBoundFieldItemWidthIllegalConfig(
    input.config,
  );
  return createManifestBoundFieldItemWidthIllegalRuntime({
    config,
    journal: input.journal,
    observe: input.observe,
    resolveStage: input.resolveStage,
  });
};

export type ManifestBoundFieldItemWidthIllegalWorkflowConfig =
  LoadManifestBoundFieldItemWidthIllegalConfig &
    Readonly<{
      l1Source: FraudProofL1Source;
      decisionDigest: string;
      stateQueueMutationLeaseCoordinator: StateQueueMutationLeaseCoordinator;
    }>;

export type ManifestBoundFieldItemWidthIllegalWorkflow = Readonly<{
  deployment: FamilyDeploymentContext<
    "fieldItemWidthIllegal",
    "computationThreadMint" | "fraudProofMint" | "phasMembershipWithdraw",
    true,
    3
  >;
  workflowVersion: typeof FIELD_ITEM_WIDTH_ILLEGAL_WORKFLOW;
  config: ManifestBoundFieldItemWidthIllegalConfig;
  binding: FraudProofWorkflowDeploymentBinding<"fieldItemWidthIllegal">;
  l1: FraudProofFamilyL1ObservationPort<"fieldItemWidthIllegal">;
  stateQueueMutationLeaseCoordinator: StateQueueMutationLeaseCoordinator;
  decisionDigest: string;
}>;

/** Production installation factory; no evidence object is accepted here. */
export const createManifestBoundFieldItemWidthIllegalWorkflow = async (
  input: ManifestBoundFieldItemWidthIllegalWorkflowConfig,
): Promise<ManifestBoundFieldItemWidthIllegalWorkflow> => {
  const deployment = await bindManifestBoundFamilyWorkflow(
    FIELD_ITEM_WIDTH_ILLEGAL_FAMILY_DEFINITION,
    {
      ...input,
      referenceScripts: {
        steps: [
          input.referenceScripts.step01,
          input.referenceScripts.step02,
          input.referenceScripts.step03,
        ],
        witnesses: input.referenceScripts.witnesses,
        fieldPreimageCertificateMint:
          input.referenceScripts.fieldPreimageCertificateMint,
      },
    },
  );
  const config = createFieldItemWidthIllegalBoundConfig(
    input,
    deployment.binding,
  );
  const l1 = deployment.l1;
  return Object.freeze({
    deployment,
    workflowVersion: FIELD_ITEM_WIDTH_ILLEGAL_WORKFLOW,
    config,
    binding: config.binding,
    l1,
    stateQueueMutationLeaseCoordinator:
      input.stateQueueMutationLeaseCoordinator,
    decisionDigest: input.decisionDigest,
  });
};

/**
 * Watcher-facing runner. Evidence is always reconstructed from authenticated
 * L1 plus public retained DA; unknown/caller-authored evidence fields fail.
 */
export const runOrResumeManifestBoundFieldItemWidthIllegalWorkflow =
  async (input: {
    readonly workflow: ManifestBoundFieldItemWidthIllegalWorkflow;
    readonly sources: readonly RetainedDaPayloadSource[];
    readonly journal: FieldItemWidthJournal;
  }): Promise<FieldItemWidthStage> => {
    if (Object.keys(input).sort().join(",") !== "journal,sources,workflow") {
      throw new Error(
        "fieldItemWidthIllegal runner rejects caller-authored evidence inputs",
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
    const evidence =
      deriveFieldItemWidthIllegalEvidenceFromCanonicalBlock(canonical);
    const source = await deriveFieldItemWidthIllegalAuthenticatedSource({
      block: canonical,
      evidence,
    });
    const runtime = createManifestBoundFieldItemWidthIllegalRuntime({
      config: input.workflow.config,
      journal: input.journal,
      observe: async () =>
        fieldItemWidthStageFromL1(
          (await input.workflow.l1.observe({ headerHash })).stage,
        ),
      resolveStage: createFieldItemWidthIllegalRawL1StageResolver({
        config: input.workflow.config,
        l1: input.workflow.l1,
        source,
      }),
    });
    return await runtime.runOrResume(evidence);
  };

export const createFieldItemWidthIllegalRecoveryAdapter = (
  workflow: ManifestBoundFieldItemWidthIllegalWorkflow,
) => {
  const { config } = workflow;
  const material = createCanonicalFamilyArtifactPort(
    async ({ evidence, classification }) =>
      await prepareFieldItemWidthIllegalRecoveryMaterial(
        evidence,
        classification.selected.detectionId,
      ),
  );
  const assembled = assembleBoundManifestBoundFamilyWorkflow(
    FIELD_ITEM_WIDTH_ILLEGAL_FAMILY_DEFINITION,
    workflow.deployment,
    { config, material },
  );
  return { ...assembled };
};

export const executeManifestBoundFieldItemWidthIllegalWorkflow = async ({
  workflow,
  sources,
  journal,
}: {
  readonly workflow: ManifestBoundFieldItemWidthIllegalWorkflow;
  readonly sources: readonly RetainedDaPayloadSource[];
  readonly journal: FraudProofWorkflowJournalStore;
}) =>
  await executeManifestBoundFamilyRecovery({
    ...workflow,
    sources,
    journal,
    ...createFieldItemWidthIllegalRecoveryAdapter(workflow),
  });
