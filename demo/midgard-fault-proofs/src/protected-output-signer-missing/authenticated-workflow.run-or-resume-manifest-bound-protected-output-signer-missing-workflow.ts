import { type RetainedDaPayloadSource } from "../transition-trace/fetch.js";
import { type FraudProofWorkflowJournalStore } from "../workflow/journal.js";
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
  createProtectedOutputSignerMissingBoundConfig,
  PROTECTED_OUTPUT_SIGNER_MISSING_WORKFLOW,
} from "./authenticated-workflow.create-protected-output-signer-missing-bound-config.js";
import { prepareProtectedOutputSignerMissingRecoveryMaterial } from "./authenticated-workflow.derive-protected-output-signer-missing-authenticated-source.js";
import {
  type ManifestBoundProtectedOutputSignerMissingInstallation,
  type ManifestBoundProtectedOutputSignerMissingWorkflowConfig,
  PROTECTED_OUTPUT_SIGNER_MISSING_FAMILY_DEFINITION,
} from "./authenticated-workflow.protected-output-signer-missing-family-definition.js";

export const createProtectedOutputSignerMissingRecoveryAdapter = (
  workflow: ManifestBoundProtectedOutputSignerMissingInstallation,
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

export type ManifestBoundProtectedOutputSignerMissingWorkflow =
  ManifestBoundFamilyWorkflowWithRecovery<
    ManifestBoundProtectedOutputSignerMissingInstallation,
    ReturnType<typeof createProtectedOutputSignerMissingRecoveryAdapter>
  >;

/** Production installation factory; no evidence object is accepted here. */
export const createManifestBoundProtectedOutputSignerMissingWorkflow = async (
  input: ManifestBoundProtectedOutputSignerMissingWorkflowConfig,
): Promise<ManifestBoundProtectedOutputSignerMissingWorkflow> => {
  const deployment = await bindManifestBoundFamilyWorkflow(
    PROTECTED_OUTPUT_SIGNER_MISSING_FAMILY_DEFINITION,
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
        fieldGrammarCertificateSpend:
          input.referenceScripts.fieldGrammarCertificateSpend,
      },
    },
  );
  const config = createProtectedOutputSignerMissingBoundConfig(
    input,
    deployment.binding,
  );
  const l1 = deployment.l1;
  return withManifestBoundFamilyRecovery(
    Object.freeze({
      deployment,
      workflowVersion: PROTECTED_OUTPUT_SIGNER_MISSING_WORKFLOW,
      config,
      binding: config.binding,
      l1,
      stateQueueMutationLeaseCoordinator:
        input.stateQueueMutationLeaseCoordinator,
      decisionDigest: input.decisionDigest,
    }),
    createProtectedOutputSignerMissingRecoveryAdapter,
  );
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
  await executeManifestBoundFamilyRecovery({ ...workflow, sources, journal });
