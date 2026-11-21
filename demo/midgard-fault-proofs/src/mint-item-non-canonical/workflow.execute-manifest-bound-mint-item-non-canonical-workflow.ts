import type { StateQueueMutationLeaseCoordinator } from "../remove-fraudulent-block.js";
import { type RetainedDaPayloadSource } from "../transition-trace/fetch.js";
import { type FraudProofWorkflowDeploymentBinding } from "../workflow/deployment-manifest-binding.js";
import { type FamilyDeploymentContext } from "../workflow/family-definition.js";
import type { FraudProofL1Source } from "../workflow/l1-source.js";
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
import type { WorkflowRun } from "../workflow/workflow-run.js";
import { prepareMintItemNonCanonicalRecoveryMaterial } from "./workflow.derive-mint-item-non-canonical-authenticated-source.js";
import {
  createMintItemNonCanonicalBoundConfig,
  type LoadManifestBoundMintItemNonCanonicalConfig,
  type ManifestBoundMintItemNonCanonicalConfig,
  MINT_ITEM_NON_CANONICAL_WORKFLOW,
} from "./workflow.load-manifest-bound-mint-item-non-canonical-config.js";
import { MINT_ITEM_NON_CANONICAL_FAMILY_DEFINITION } from "./workflow.mint-item-non-canonical-family-definition.js";

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

/** The bound deployment a workflow's recovery surface is assembled over. */
export type ManifestBoundMintItemNonCanonicalInstallation = Readonly<{
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
  return withManifestBoundFamilyRecovery(
    Object.freeze({
      deployment,
      workflowVersion: MINT_ITEM_NON_CANONICAL_WORKFLOW,
      config,
      binding: config.binding,
      l1,
      stateQueueMutationLeaseCoordinator:
        input.stateQueueMutationLeaseCoordinator,
      decisionDigest: input.decisionDigest,
    }),
    createMintItemNonCanonicalRecoveryAdapter,
  );
};

export const createMintItemNonCanonicalRecoveryAdapter = (
  workflow: ManifestBoundMintItemNonCanonicalInstallation,
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

export type ManifestBoundMintItemNonCanonicalWorkflow =
  ManifestBoundFamilyWorkflowWithRecovery<
    ManifestBoundMintItemNonCanonicalInstallation,
    ReturnType<typeof createMintItemNonCanonicalRecoveryAdapter>
  >;

/** The shared retained-DA runner over this family's cursor definition. */
export const executeManifestBoundMintItemNonCanonicalWorkflow = async ({
  workflow,
  sources,
  run,
}: {
  readonly workflow: ManifestBoundMintItemNonCanonicalWorkflow;
  readonly sources: readonly RetainedDaPayloadSource[];
  readonly run: WorkflowRun;
}) =>
  await executeManifestBoundFamilyRecovery({
    ...workflow,
    sources,
    run,
  });
