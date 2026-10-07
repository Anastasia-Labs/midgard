import { decodeMidgardFieldPreimage } from "@al-ft/midgard-core";
import { type FraudProofCatalogueCategoryName } from "@al-ft/midgard-sdk";

import { type CanonicalBlockEvidence } from "../evidence/canonical-block-evidence.js";
import { planFaultProofFieldOpening } from "../field-opening.js";
import type { StateQueueMutationLeaseCoordinator } from "../remove-fraudulent-block.js";
import { type RetainedDaPayloadSource } from "../transition-trace/fetch.js";
import { admitCompleteCanonicalReplayHistoricalCorpus } from "../workflow/complete-replay.js";
import {
  CURSOR_FAMILY_TRANSACTION_PORT,
  type CursorFamilyTransactionPort,
} from "../workflow/cursor-family-adapter.js";
import {
  captureCursorRemoval,
  cursorFamilyActionInput,
  cursorStringField,
} from "../workflow/cursor-family-runtime.js";
import { type FraudProofWorkflowDeploymentBinding } from "../workflow/deployment-manifest-binding.js";
import {
  type FamilyDeploymentContext,
  type LinearFamilyPrerequisiteInput,
} from "../workflow/family-definition.js";
import { type FraudProofFamilyL1ObservationPort } from "../workflow/family-l1-observation.js";
import type { FieldCarriageRequirement } from "../workflow/field-carriage-prerequisite.js";
import {
  type HistoricalNativeScriptCheckpointStore,
  type HistoricalNativeScriptHistorySource,
  resolveHistoricalNativeScriptCorpus,
} from "../workflow/historical-native-script-corpus.js";
import type { LocalKupmiosHttpOgmiosSourceConfig } from "../workflow/local-kupmios-http-ogmios-source.js";
import { createCanonicalFamilyArtifactPort } from "../workflow/manifest-bound-family-recovery.js";
import {
  captureLocallyEvaluatedTransaction,
  workflowTransactionInputOutRefs,
} from "../workflow/transaction-boundary.js";
import { createManifestBoundResolvedOutputNonCanonicalSubmission } from "./authenticated-workflow.create-manifest-bound-resolved-output-non-canonical-submission.js";
import {
  createResolvedOutputNonCanonicalRawL1StageResolver,
  deriveResolvedOutputNonCanonicalAuthenticatedSource,
  type ResolvedOutputNonCanonicalRuntimeLoader,
} from "./authenticated-workflow.derive-resolved-output-non-canonical-authenticated-source.js";
import {
  type LoadManifestBoundResolvedOutputNonCanonicalConfig,
  type ManifestBoundResolvedOutputNonCanonicalConfig,
  RESOLVED_OUTPUT_NON_CANONICAL_WORKFLOW,
} from "./authenticated-workflow.resolved-output-non-canonical-config-from-binding.js";
import { createResolvedOutputNonCanonicalCentralJournalAdapter } from "./central-journal.js";
import {
  deriveResolvedOutputPriorLedgerReplayFromHistoricalCorpus,
  detectResolvedOutputNonCanonicalCompleteReplay,
  type ResolvedOutputEvidence,
  resolvedOutputEvidenceIdentity,
} from "./resolved-output-non-canonical.js";
import {
  nextResolvedOutputAction,
  type ResolvedOutputJournal,
  type ResolvedOutputStage,
} from "./workflow.js";

export const createManifestBoundResolvedOutputNonCanonicalRuntime = ({
  config,
  journal,
  observe,
  resolveStage,
  centralJournal,
  stateQueueMutationLeaseCoordinator,
}: {
  readonly config: ManifestBoundResolvedOutputNonCanonicalConfig;
  readonly journal: ResolvedOutputJournal;
  readonly observe: ResolvedOutputNonCanonicalRuntimeLoader["observe"];
  readonly resolveStage: ResolvedOutputNonCanonicalRuntimeLoader["resolveStage"];
  readonly centralJournal?: ReturnType<
    typeof createResolvedOutputNonCanonicalCentralJournalAdapter
  >;
  readonly stateQueueMutationLeaseCoordinator?: StateQueueMutationLeaseCoordinator;
}) => {
  const submission = createManifestBoundResolvedOutputNonCanonicalSubmission({
    config,
    observe: async (identity) => {
      const observed = await observe(identity);
      await centralJournal?.reconcile(observed);
      return observed;
    },
    resolveStage,
    centralJournal,
    stateQueueMutationLeaseCoordinator,
  });
  return Object.freeze({
    runtimeVersion: RESOLVED_OUTPUT_NON_CANONICAL_WORKFLOW,
    config,
    runOrResume: async (evidence: ResolvedOutputEvidence) => {
      const identity = resolvedOutputEvidenceIdentity(evidence);
      for (;;) {
        const stage = await submission.observe(identity);
        const action = nextResolvedOutputAction(stage);
        if (action === "done") return stage;
        const result = await submission.submit(action, evidence);
        await journal.append({
          sequence: (await journal.load(identity)).length,
          identity,
          stage: result.stage,
          action,
          phase: "submitted",
          txHash: result.txHash,
          outputReference: result.outputReference,
        });
      }
    },
  });
};

export type ManifestBoundResolvedOutputNonCanonicalWorkflowConfig =
  LoadManifestBoundResolvedOutputNonCanonicalConfig &
    Readonly<{
      source: Omit<LocalKupmiosHttpOgmiosSourceConfig, "releaseFinality">;
      decisionDigest: string;
      stateQueueMutationLeaseCoordinator: StateQueueMutationLeaseCoordinator;
      historicalNativeScriptCheckpointStore: HistoricalNativeScriptCheckpointStore;
      historicalNativeScriptHistorySource: HistoricalNativeScriptHistorySource;
    }>;

export type ManifestBoundResolvedOutputNonCanonicalWorkflow = Readonly<{
  deployment: Deployment;
  workflowVersion: typeof RESOLVED_OUTPUT_NON_CANONICAL_WORKFLOW;
  config: ManifestBoundResolvedOutputNonCanonicalConfig;
  binding: FraudProofWorkflowDeploymentBinding<"resolvedOutputNonCanonical">;
  l1: FraudProofFamilyL1ObservationPort<FraudProofCatalogueCategoryName>;
  stateQueueMutationLeaseCoordinator: StateQueueMutationLeaseCoordinator;
  decisionDigest: string;
  historicalNativeScriptCheckpointStore: HistoricalNativeScriptCheckpointStore;
  historicalNativeScriptHistorySource: HistoricalNativeScriptHistorySource;
}>;

export const resolvedOutputStageFromL1 = (
  stage: Awaited<
    ReturnType<
      FraudProofFamilyL1ObservationPort<FraudProofCatalogueCategoryName>["observe"]
    >
  >["stage"],
): ResolvedOutputStage => {
  switch (stage.kind) {
    case "not_started":
      return "none";
    case "step":
      if (stage.step === 1) return "step01";
      if (stage.step === 2) return "step02";
      if (stage.step === 3) return "step03";
      if (stage.step === 4) return "reconstructing";
      if (stage.step === 5) return "step05";
      throw new Error(
        "resolvedOutputNonCanonical L1 stage exceeds five-step topology",
      );
    case "proof_token":
      return "proven";
    case "removed":
      return "removed";
  }
};

/** Material re-derived from admitted canonical evidence before durable encoding. */
export const prepareResolvedOutputNonCanonicalRecoveryMaterial = async (
  canonical: CanonicalBlockEvidence,
  detectionId: string,
  corpus: Awaited<ReturnType<typeof resolveHistoricalNativeScriptCorpus>>,
) => {
  const priorLedger =
    await deriveResolvedOutputPriorLedgerReplayFromHistoricalCorpus({
      block: canonical,
      corpus: corpus,
    });
  const findings = detectResolvedOutputNonCanonicalCompleteReplay({
    block: canonical,
    priorLedger,
  });
  if (findings.length !== 1)
    throw new Error(
      "resolvedOutputNonCanonical public replay requires one exact finding",
    );
  const evidence = findings[0]!;
  const source = await deriveResolvedOutputNonCanonicalAuthenticatedSource({
    block: canonical,
    evidence,
  });
  return {
    category: "resolvedOutputNonCanonical" as const,
    headerHash: canonical.headerHash,
    detectionId,
    evidence,
    source,
  };
};

export const createResolvedOutputNonCanonicalRecoveryPorts = (
  workflow: ManifestBoundResolvedOutputNonCanonicalWorkflow,
  sources: readonly RetainedDaPayloadSource[],
) => {
  const { config, binding, l1, stateQueueMutationLeaseCoordinator } = workflow;
  const category = "resolvedOutputNonCanonical";
  let currentHistory:
    | {
        evidence: CanonicalBlockEvidence;
        corpus: Awaited<ReturnType<typeof resolveHistoricalNativeScriptCorpus>>;
      }
    | undefined;
  const resolveReplayContext = async (canonical: CanonicalBlockEvidence) => {
    const corpus = await resolveHistoricalNativeScriptCorpus({
      deploymentFingerprint: binding.deploymentFingerprint,
      checkpointStore: workflow.historicalNativeScriptCheckpointStore,
      historySource: workflow.historicalNativeScriptHistorySource,
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
          "resolvedOutputNonCanonical requires its exact admitted historical replay context",
        );
      return await prepareResolvedOutputNonCanonicalRecoveryMaterial(
        canonical,
        classification.selected.detectionId,
        currentHistory.corpus,
      );
    },
  );
  const transactions: CursorFamilyTransactionPort<typeof category> = {
    portVersion: CURSOR_FAMILY_TRANSACTION_PORT,
    category,
    prepare: material.prepare,
    validatePreparedArtifact: material.validatePreparedArtifact,
    capture: async ({ action, artifact }) => {
      const input = cursorFamilyActionInput({ category, action });
      if (input.stage === "remove")
        return await captureCursorRemoval({
          category,
          lucid: config.lucid,
          blueprint: binding.blueprint,
          deploymentInfo: binding.deploymentInfo,
          network: binding.network,
          signer: config.signer,
          headerHash: binding.definition.headerHash,
          input,
          stateQueueMutationLeaseCoordinator,
          fraudProverRewardLovelace: BigInt(
            binding.releaseEconomics.policy.fraudProverRewardLovelace,
          ),
        });
      const admitted = material.require(artifact);
      const actions = {
        init: "submitInit",
        step_01: "submitStep01",
        step_02: "submitStep02",
        step_03: "submitStep03",
        step_04: "submitReconstruction",
        step_05: "submitStep05",
      } as const;
      const familyAction = actions[input.stage as keyof typeof actions];
      if (familyAction === undefined)
        throw new Error(
          `${category} cursor action is outside its exact topology`,
        );
      const transaction = await captureLocallyEvaluatedTransaction(
        async (preSubmitBoundary) => {
          const submission =
            createManifestBoundResolvedOutputNonCanonicalSubmission({
              config,
              preSubmitBoundary,
              observe: async () =>
                resolvedOutputStageFromL1(
                  (
                    await l1.observe({
                      headerHash: binding.definition.headerHash,
                    })
                  ).stage,
                ),
              resolveStage: createResolvedOutputNonCanonicalRawL1StageResolver({
                config,
                l1,
                source: admitted.source,
              }),
            });
          await submission.submit(familyAction, admitted.evidence);
        },
      );
      if (
        input.stage !== "init" &&
        !workflowTransactionInputOutRefs(transaction.signed).includes(
          cursorStringField(input, "threadOutRef"),
        )
      )
        throw new Error(
          `${category} captured transaction changed its authenticated thread input`,
        );
      return { transaction };
    },
  };
  const requirementForAction = ({
    action,
    artifact,
  }: LinearFamilyPrerequisiteInput): FieldCarriageRequirement | null => {
    if (action.input.stage !== "step_02") return null;
    const { evidence, source } = material.require(artifact);
    const certificate = binding.fieldPreimageCertificate;
    if (certificate === null)
      throw new Error(`${category} omitted field certificate authority`);
    return {
      planned: planFaultProofFieldOpening({
        anchorSourceKind: evidence.subject.source_kind === 1n ? 1n : 0n,
        fieldIndex: evidence.coordinate.sourceKind,
        anchorTxId: evidence.subject.transaction_id,
        nativeTxCompactCbor: source.nativeTxCompactCbor,
        itemCbors: decodeMidgardFieldPreimage(
          Buffer.from(evidence.inputFieldPreimageHex, "hex"),
        ),
        owner: config.signer.paymentKeyHash,
        publish: true,
        label: `${category} field opening`,
      }),
      compactCbor: source.nativeTxCompactCbor,
      witnessSetCompactCbor: source.witnessSetCompactCbor,
      certificate: {
        policyId: certificate.policyId,
        mintingScript: certificate.mintingScript,
        referenceScriptUtxo:
          config.referenceScripts.fieldPreimageCertificateMint,
      },
    };
  };
  return { transactions, requirementForAction, resolveReplayContext };
};

export const WITNESS_ROLES = [
  "computationThreadMint",
  "fraudProofMint",
  "phasMembershipWithdraw",
] as const;

type Deployment = FamilyDeploymentContext<
  "resolvedOutputNonCanonical",
  (typeof WITNESS_ROLES)[number],
  true,
  5
>;

export type RunContext = Readonly<{
  workflow: ManifestBoundResolvedOutputNonCanonicalWorkflow;
  sources: readonly RetainedDaPayloadSource[];
}>;
