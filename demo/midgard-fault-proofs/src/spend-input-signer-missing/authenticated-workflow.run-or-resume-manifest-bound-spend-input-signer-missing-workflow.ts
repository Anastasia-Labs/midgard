import type { RetainedDaPayloadSource } from "../transition-trace/fetch.js";
import { completeCanonicalReplayPredecessorLedger } from "../workflow/complete-replay.admit-complete-canonical-replay-predecessor.js";
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
import {
  createSpendInputSignerMissingBoundConfig,
  SPEND_INPUT_SIGNER_MISSING_WORKFLOW,
} from "./authenticated-workflow.create-spend-input-signer-missing-bound-config.js";
import { prepareSpendInputSignerMissingRecoveryMaterial } from "./authenticated-workflow.derive-spend-input-signer-missing-authenticated-source.js";
import {
  type ManifestBoundSpendInputSignerMissingInstallation,
  type ManifestBoundSpendInputSignerMissingWorkflowConfig,
  SPEND_INPUT_SIGNER_MISSING_FAMILY_DEFINITION,
} from "./authenticated-workflow.spend-input-signer-missing-family-definition.js";

/** Production installation factory; no evidence object is accepted here. */
export const createManifestBoundSpendInputSignerMissingWorkflow = async (
  input: ManifestBoundSpendInputSignerMissingWorkflowConfig,
): Promise<ManifestBoundSpendInputSignerMissingWorkflow> => {
  const deployment = await bindManifestBoundFamilyWorkflow(
    SPEND_INPUT_SIGNER_MISSING_FAMILY_DEFINITION,
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
  const config = createSpendInputSignerMissingBoundConfig(
    input,
    deployment.binding,
  );
  const l1 = deployment.l1;
  return withManifestBoundFamilyRecovery(
    Object.freeze({
      deployment,
      workflowVersion: SPEND_INPUT_SIGNER_MISSING_WORKFLOW,
      config,
      binding: config.binding,
      l1,
      stateQueueMutationLeaseCoordinator:
        input.stateQueueMutationLeaseCoordinator,
      decisionDigest: input.decisionDigest,
      ...(input.replayContext === undefined
        ? {}
        : { replayContext: input.replayContext }),
    }),
    createSpendInputSignerMissingRecoveryAdapter,
  );
};

export const createSpendInputSignerMissingRecoveryAdapter = (
  workflow: ManifestBoundSpendInputSignerMissingInstallation,
) => {
  const { config } = workflow;
  const material = createCanonicalFamilyArtifactPort(
    async ({ evidence: canonical, classification }) =>
      await prepareSpendInputSignerMissingRecoveryMaterial(
        canonical,
        classification.selected.detectionId,
        completeCanonicalReplayPredecessorLedger({
          evidence: canonical,
          context: workflow.replayContext,
        }),
      ),
  );
  return assembleBoundManifestBoundFamilyWorkflow(
    SPEND_INPUT_SIGNER_MISSING_FAMILY_DEFINITION,
    workflow.deployment,
    { config, material },
  );
};

export type ManifestBoundSpendInputSignerMissingWorkflow =
  ManifestBoundFamilyWorkflowWithRecovery<
    ManifestBoundSpendInputSignerMissingInstallation,
    ReturnType<typeof createSpendInputSignerMissingRecoveryAdapter>
  >;

export const executeManifestBoundSpendInputSignerMissingWorkflow = async ({
  workflow,
  sources,
  run,
}: {
  readonly workflow: ManifestBoundSpendInputSignerMissingWorkflow;
  readonly sources: readonly RetainedDaPayloadSource[];
  readonly run: WorkflowRun;
}) =>
  await executeManifestBoundFamilyRecovery({
    ...workflow,
    sources,
    run,
  });
