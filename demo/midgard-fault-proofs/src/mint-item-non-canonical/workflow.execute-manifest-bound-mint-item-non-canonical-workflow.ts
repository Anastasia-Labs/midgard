import { fetchCanonicalBlockEvidence } from "../evidence/canonical-block-evidence.js";
import type { StateQueueMutationLeaseCoordinator } from "../remove-fraudulent-block.js";
import { type RetainedDaPayloadSource } from "../transition-trace/fetch.js";
import { type FraudProofWorkflowDeploymentBinding } from "../workflow/deployment-manifest-binding.js";
import { type FamilyDeploymentContext } from "../workflow/family-definition.js";
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
  type MintItemEvidence,
  type MintItemJournal,
  type MintItemStage,
  runMintItemProof,
} from "./mint-item-non-canonical.js";
import { createManifestBoundMintItemNonCanonicalSubmission } from "./workflow.create-manifest-bound-mint-item-non-canonical-submission.js";
import {
  createMintItemNonCanonicalRawL1StageResolver,
  deriveMintItemNonCanonicalAuthenticatedSource,
  type MintItemNonCanonicalRuntimeLoader,
  mintItemStageFromL1,
  prepareMintItemNonCanonicalRecoveryMaterial,
} from "./workflow.derive-mint-item-non-canonical-authenticated-source.js";
import {
  createMintItemNonCanonicalBoundConfig,
  deriveMintItemNonCanonicalEvidenceFromCanonicalBlock,
  type LoadManifestBoundMintItemNonCanonicalConfig,
  loadManifestBoundMintItemNonCanonicalConfig,
  type ManifestBoundMintItemNonCanonicalConfig,
  MINT_ITEM_NON_CANONICAL_WORKFLOW,
} from "./workflow.load-manifest-bound-mint-item-non-canonical-config.js";
import { MINT_ITEM_NON_CANONICAL_FAMILY_DEFINITION } from "./workflow.mint-item-non-canonical-family-definition.js";

export const loadMintItemNonCanonicalRuntime = async (
  input: MintItemNonCanonicalRuntimeLoader,
) => {
  const config = await loadManifestBoundMintItemNonCanonicalConfig(
    input.config,
  );
  return createManifestBoundMintItemNonCanonicalRuntime({
    config,
    journal: input.journal,
    observe: input.observe,
    resolveStage: input.resolveStage,
  });
};

export const createManifestBoundMintItemNonCanonicalRuntime = ({
  config,
  journal,
  observe,
  resolveStage,
}: {
  readonly config: ManifestBoundMintItemNonCanonicalConfig;
  readonly journal: MintItemJournal;
  readonly observe: MintItemNonCanonicalRuntimeLoader["observe"];
  readonly resolveStage: MintItemNonCanonicalRuntimeLoader["resolveStage"];
}) => {
  const submission = createManifestBoundMintItemNonCanonicalSubmission({
    config,
    observe,
    resolveStage,
  });
  return Object.freeze({
    runtimeVersion: MINT_ITEM_NON_CANONICAL_WORKFLOW,
    config,
    runOrResume: async (evidence: MintItemEvidence) =>
      await runMintItemProof({
        evidence,
        journal,
        submission,
      }),
  });
};

export type ManifestBoundMintItemNonCanonicalWorkflowConfig =
  LoadManifestBoundMintItemNonCanonicalConfig &
    Readonly<{
      l1Source: FraudProofL1Source;
      decisionDigest: string;
      stateQueueMutationLeaseCoordinator: StateQueueMutationLeaseCoordinator;
    }>;

type MintItemNonCanonicalDeployment = FamilyDeploymentContext<
  "mintItemNonCanonical",
  "computationThreadMint" | "fraudProofMint" | "phasMembershipWithdraw",
  true,
  4
>;

export type ManifestBoundMintItemNonCanonicalWorkflow = Readonly<{
  deployment: MintItemNonCanonicalDeployment;
  workflowVersion: typeof MINT_ITEM_NON_CANONICAL_WORKFLOW;
  config: ManifestBoundMintItemNonCanonicalConfig;
  binding: FraudProofWorkflowDeploymentBinding<"mintItemNonCanonical">;
  l1: MintItemNonCanonicalDeployment["l1"];
  stateQueueMutationLeaseCoordinator: StateQueueMutationLeaseCoordinator;
  decisionDigest: string;
}>;

/** Production installation factory; no evidence object is accepted here. */
export const createManifestBoundMintItemNonCanonicalWorkflow = async (
  input: ManifestBoundMintItemNonCanonicalWorkflowConfig,
): Promise<ManifestBoundMintItemNonCanonicalWorkflow> => {
  const deployment = await bindManifestBoundFamilyWorkflow(
    MINT_ITEM_NON_CANONICAL_FAMILY_DEFINITION,
    {
      ...input,
      referenceScripts: {
        steps: [
          input.referenceScripts.step01,
          input.referenceScripts.step02,
          input.referenceScripts.step03,
          input.referenceScripts.step04,
        ],
        witnesses: input.referenceScripts.witnesses,
        fieldPreimageCertificateMint:
          input.referenceScripts.fieldPreimageCertificateMint,
      },
    },
  );
  const config = createMintItemNonCanonicalBoundConfig(
    input,
    deployment.binding,
  );
  const l1 = deployment.l1;
  return Object.freeze({
    deployment,
    workflowVersion: MINT_ITEM_NON_CANONICAL_WORKFLOW,
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
export const runOrResumeManifestBoundMintItemNonCanonicalWorkflow =
  async (input: {
    readonly workflow: ManifestBoundMintItemNonCanonicalWorkflow;
    readonly sources: readonly RetainedDaPayloadSource[];
    readonly journal: MintItemJournal;
  }): Promise<MintItemStage> => {
    if (Object.keys(input).sort().join(",") !== "journal,sources,workflow") {
      throw new Error(
        "mintItemNonCanonical runner rejects caller-authored evidence inputs",
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
      deriveMintItemNonCanonicalEvidenceFromCanonicalBlock(canonical);
    const source = await deriveMintItemNonCanonicalAuthenticatedSource({
      block: canonical,
      evidence,
    });
    const runtime = createManifestBoundMintItemNonCanonicalRuntime({
      config: input.workflow.config,
      journal: input.journal,
      observe: async () =>
        mintItemStageFromL1(
          (await input.workflow.l1.observe({ headerHash })).stage,
        ),
      resolveStage: createMintItemNonCanonicalRawL1StageResolver({
        config: input.workflow.config,
        l1: input.workflow.l1,
        source,
      }),
    });
    return await runtime.runOrResume(evidence);
  };

export const createMintItemNonCanonicalRecoveryAdapter = (
  workflow: ManifestBoundMintItemNonCanonicalWorkflow,
) => {
  const material = createCanonicalFamilyArtifactPort(
    async ({ evidence, classification }) =>
      await prepareMintItemNonCanonicalRecoveryMaterial(
        evidence,
        classification.selected.detectionId,
      ),
  );
  return assembleBoundManifestBoundFamilyWorkflow(
    MINT_ITEM_NON_CANONICAL_FAMILY_DEFINITION,
    workflow.deployment,
    { config: workflow.config, material },
  );
};

/** The shared retained-DA runner over this family's cursor definition. */
export const executeManifestBoundMintItemNonCanonicalWorkflow = async ({
  workflow,
  sources,
  journal,
}: {
  readonly workflow: ManifestBoundMintItemNonCanonicalWorkflow;
  readonly sources: readonly RetainedDaPayloadSource[];
  readonly journal: FraudProofWorkflowJournalStore;
}) =>
  await executeManifestBoundFamilyRecovery({
    ...workflow,
    sources,
    journal,
    ...createMintItemNonCanonicalRecoveryAdapter(workflow),
  });
