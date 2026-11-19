import type { StateQueueMutationLeaseCoordinator } from "../remove-fraudulent-block.js";
import { type RetainedDaPayloadSource } from "../transition-trace/fetch.js";
import { type FraudProofWorkflowDeploymentBinding } from "../workflow/deployment-manifest-binding.js";
import { type FamilyDeploymentContext } from "../workflow/family-definition.js";
import { type FraudProofFamilyL1ObservationPort } from "../workflow/family-l1-observation.js";
import { type FraudProofWorkflowJournalStore } from "../workflow/journal.js";
import type { FraudProofL1Source } from "../workflow/l1-source.js";
import {
  assembleBoundManifestBoundFamilyWorkflow,
  bindManifestBoundFamilyWorkflow,
} from "../workflow/manifest-bound-family-assembly.js";
import {
  createCanonicalFamilyArtifactPort,
  executeManifestBoundFamilyRecovery,
  type ManifestBoundFamilyWorkflowWithRecovery,
  withManifestBoundFamilyRecovery,
} from "../workflow/manifest-bound-family-recovery.js";
import {
  createFieldItemWidthIllegalBoundConfig,
  FIELD_ITEM_WIDTH_ILLEGAL_WORKFLOW,
  type LoadManifestBoundFieldItemWidthIllegalConfig,
  type ManifestBoundFieldItemWidthIllegalConfig,
} from "./workflow.create-field-item-width-illegal-raw-l1-stage-resolver.js";
import { prepareFieldItemWidthIllegalRecoveryMaterial } from "./workflow.derive-field-item-width-illegal-authenticated-source.js";
import { FIELD_ITEM_WIDTH_ILLEGAL_FAMILY_DEFINITION } from "./workflow.field-item-width-illegal-family-definition.js";

export type ManifestBoundFieldItemWidthIllegalWorkflowConfig =
  LoadManifestBoundFieldItemWidthIllegalConfig &
    Readonly<{
      l1Source: FraudProofL1Source;
      decisionDigest: string;
      stateQueueMutationLeaseCoordinator: StateQueueMutationLeaseCoordinator;
    }>;

/** The bound deployment a workflow's recovery surface is assembled over. */
type ManifestBoundFieldItemWidthIllegalInstallation = Readonly<{
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
        fieldGrammarCertificateSpend:
          input.referenceScripts.fieldGrammarCertificateSpend,
      },
    },
  );
  const config = createFieldItemWidthIllegalBoundConfig(
    input,
    deployment.binding,
  );
  const l1 = deployment.l1;
  return withManifestBoundFamilyRecovery(
    Object.freeze({
      deployment,
      workflowVersion: FIELD_ITEM_WIDTH_ILLEGAL_WORKFLOW,
      config,
      binding: config.binding,
      l1,
      stateQueueMutationLeaseCoordinator:
        input.stateQueueMutationLeaseCoordinator,
      decisionDigest: input.decisionDigest,
    }),
    createFieldItemWidthIllegalRecoveryAdapter,
  );
};

export const createFieldItemWidthIllegalRecoveryAdapter = (
  workflow: ManifestBoundFieldItemWidthIllegalInstallation,
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

export type ManifestBoundFieldItemWidthIllegalWorkflow =
  ManifestBoundFamilyWorkflowWithRecovery<
    ManifestBoundFieldItemWidthIllegalInstallation,
    ReturnType<typeof createFieldItemWidthIllegalRecoveryAdapter>
  >;

export const executeManifestBoundFieldItemWidthIllegalWorkflow = async ({
  workflow,
  sources,
  journal,
}: {
  readonly workflow: ManifestBoundFieldItemWidthIllegalWorkflow;
  readonly sources: readonly RetainedDaPayloadSource[];
  readonly journal: FraudProofWorkflowJournalStore;
}) =>
  await executeManifestBoundFamilyRecovery({ ...workflow, sources, journal });
