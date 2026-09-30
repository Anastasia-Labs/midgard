import {
  type CanonicalBlockEvidence,
  fetchCanonicalBlockEvidence,
} from "../evidence/canonical-block-evidence.js";
import { deriveResolvedOutputPriorLedgerReplayFromHistoricalCorpus } from "../resolved-output-non-canonical/resolved-output-non-canonical.js";
import { type RetainedDaPayloadSource } from "../transition-trace/fetch.js";
import { admitCompleteCanonicalReplayHistoricalCorpus } from "../workflow/complete-replay.js";
import { observeFraudProofWorkflowHeader } from "../workflow/family-l1-observation.js";
import { resolveHistoricalNativeScriptCorpus } from "../workflow/historical-native-script-corpus.js";
import { type FraudProofWorkflowJournalStore } from "../workflow/journal.js";
import {
  assembleBoundManifestBoundFamilyWorkflow,
  bindManifestBoundFamilyWorkflow,
} from "../workflow/manifest-bound-family-assembly.js";
import {
  createCanonicalFamilyArtifactPort,
  executeManifestBoundFamilyRecovery,
} from "../workflow/manifest-bound-family-recovery.js";
import {
  createSpendInputSignerMissingBoundConfig,
  SPEND_INPUT_SIGNER_MISSING_WORKFLOW,
} from "./authenticated-workflow.create-spend-input-signer-missing-bound-config.js";
import { createSpendInputSignerMissingRawL1StageResolver } from "./authenticated-workflow.create-spend-input-signer-missing-raw-l1-stage-resolver.js";
import {
  deriveSpendInputSignerMissingAuthenticatedSource,
  prepareSpendInputSignerMissingRecoveryMaterial,
  spendInputSignerStageFromL1,
} from "./authenticated-workflow.derive-spend-input-signer-missing-authenticated-source.js";
import {
  createManifestBoundSpendInputSignerMissingRuntime,
  type ManifestBoundSpendInputSignerMissingWorkflow,
  type ManifestBoundSpendInputSignerMissingWorkflowConfig,
  SPEND_INPUT_SIGNER_MISSING_FAMILY_DEFINITION,
} from "./authenticated-workflow.spend-input-signer-missing-family-definition.js";
import { detectSpendInputSignerMissingCompleteReplay } from "./spend-input-signer-missing.js";
import {
  type SpendInputSignerJournal,
  type SpendInputSignerStage,
} from "./workflow.js";

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
          input.referenceScripts.step05,
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
  return Object.freeze({
    deployment,
    workflowVersion: SPEND_INPUT_SIGNER_MISSING_WORKFLOW,
    config,
    binding: config.binding,
    l1,
    stateQueueMutationLeaseCoordinator:
      input.stateQueueMutationLeaseCoordinator,
    decisionDigest: input.decisionDigest,
    historicalCheckpointStore: input.historicalCheckpointStore,
    historicalSource: input.historicalSource,
  });
};

/**
 * Watcher-facing runner. Evidence is always reconstructed from authenticated
 * L1 plus public retained DA; unknown/caller-authored evidence fields fail.
 */
export const runOrResumeManifestBoundSpendInputSignerMissingWorkflow =
  async (input: {
    readonly workflow: ManifestBoundSpendInputSignerMissingWorkflow;
    readonly sources: readonly RetainedDaPayloadSource[];
    readonly journal: SpendInputSignerJournal;
  }): Promise<SpendInputSignerStage> => {
    if (Object.keys(input).sort().join(",") !== "journal,sources,workflow") {
      throw new Error(
        "spendInputSignerMissing runner rejects caller-authored evidence inputs",
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
    const corpus = await resolveHistoricalNativeScriptCorpus({
      deploymentFingerprint: input.workflow.binding.deploymentFingerprint,
      checkpointStore: input.workflow.historicalCheckpointStore,
      historySource: input.workflow.historicalSource,
      currentEvidence: canonical,
      sources: input.sources,
    });
    const priorLedger =
      await deriveResolvedOutputPriorLedgerReplayFromHistoricalCorpus({
        block: canonical,
        corpus,
      });
    const findings = detectSpendInputSignerMissingCompleteReplay({
      block: canonical,
      priorLedger,
    });
    if (findings.length !== 1)
      throw new Error(
        `spendInputSignerMissing public replay yielded ${findings.length.toString()} exact findings`,
      );
    const evidence = findings[0]!;
    const source = await deriveSpendInputSignerMissingAuthenticatedSource({
      block: canonical,
      evidence,
    });
    const runtime = createManifestBoundSpendInputSignerMissingRuntime({
      config: input.workflow.config,
      journal: input.journal,
      observe: async () =>
        spendInputSignerStageFromL1(
          (await input.workflow.l1.observe({ headerHash })).stage,
        ),
      resolveStage: createSpendInputSignerMissingRawL1StageResolver({
        config: input.workflow.config,
        l1: input.workflow.l1,
        source,
      }),
    });
    return await runtime.runOrResume(evidence);
  };

export const createSpendInputSignerMissingRecoveryAdapter = (
  workflow: ManifestBoundSpendInputSignerMissingWorkflow,
  sources: readonly RetainedDaPayloadSource[],
) => {
  const { config, binding } = workflow;
  let currentHistory:
    | {
        evidence: CanonicalBlockEvidence;
        corpus: Awaited<ReturnType<typeof resolveHistoricalNativeScriptCorpus>>;
      }
    | undefined;
  const resolveReplayContext = async (canonical: CanonicalBlockEvidence) => {
    const corpus = await resolveHistoricalNativeScriptCorpus({
      deploymentFingerprint: binding.deploymentFingerprint,
      checkpointStore: workflow.historicalCheckpointStore,
      historySource: workflow.historicalSource,
      currentEvidence: canonical,
      sources,
    });
    currentHistory = { evidence: canonical, corpus };
    return {
      historicalCorpus: admitCompleteCanonicalReplayHistoricalCorpus({
        evidence: canonical,
        corpus,
      }),
    };
  };
  const material = createCanonicalFamilyArtifactPort(
    async ({ evidence: canonical, classification }) => {
      if (currentHistory?.evidence !== canonical)
        throw new Error(
          "spendInputSignerMissing requires its exact admitted historical replay context",
        );
      return await prepareSpendInputSignerMissingRecoveryMaterial(
        canonical,
        classification.selected.detectionId,
        currentHistory.corpus,
      );
    },
  );
  const assembled = assembleBoundManifestBoundFamilyWorkflow(
    SPEND_INPUT_SIGNER_MISSING_FAMILY_DEFINITION,
    workflow.deployment,
    { config, material },
  );
  return { ...assembled, resolveReplayContext };
};

export const executeManifestBoundSpendInputSignerMissingWorkflow = async ({
  workflow,
  sources,
  journal,
}: {
  readonly workflow: ManifestBoundSpendInputSignerMissingWorkflow;
  readonly sources: readonly RetainedDaPayloadSource[];
  readonly journal: FraudProofWorkflowJournalStore;
}) =>
  await executeManifestBoundFamilyRecovery({
    ...workflow,
    sources,
    journal,
    ...createSpendInputSignerMissingRecoveryAdapter(workflow, sources),
  });
