import {
  type FraudProofCatalogueCategoryName,
  FraudProofComputationThreadStepDatum,
} from "@al-ft/midgard-sdk";

import type { StateQueueMutationLeaseCoordinator } from "../remove-fraudulent-block.js";
import { SPEND_INPUT_SIGNER_MISSING_COMPLETE_CANONICAL_REPLAY } from "../workflow/complete-replay.js";
import { CURSOR_FAMILY_TRANSACTION_PORT } from "../workflow/cursor-family-adapter.js";
import {
  captureCursorRemoval,
  cursorFamilyActionInput,
  cursorStringField,
} from "../workflow/cursor-family-runtime.js";
import {
  assertManifestBoundWorkflowSigner,
  bindFraudProofWorkflowDeployment,
} from "../workflow/deployment-manifest-binding.js";
import {
  defineFamily,
  type FamilyDeploymentContext,
} from "../workflow/family-definition.js";
import { type FraudProofFamilyL1ObservationPort } from "../workflow/family-l1-observation.js";
import {
  type HistoricalNativeScriptCheckpointStore,
  type HistoricalNativeScriptHistorySource,
} from "../workflow/historical-native-script-corpus.js";
import type { LocalKupmiosHttpOgmiosSourceConfig } from "../workflow/local-kupmios-http-ogmios-source.js";
import {
  captureLocallyEvaluatedTransaction,
  workflowTransactionInputOutRefs,
} from "../workflow/transaction-boundary.js";
import { createManifestBoundSpendInputSignerMissingSubmission } from "./authenticated-workflow.create-manifest-bound-spend-input-signer-missing-submission.js";
import {
  createSpendInputSignerMissingBoundConfig,
  type LoadManifestBoundSpendInputSignerMissingConfig,
  type ManifestBoundSpendInputSignerMissingConfig,
  SPEND_INPUT_SIGNER_MISSING_MANIFEST_CONTRACTS,
  SPEND_INPUT_SIGNER_MISSING_WORKFLOW,
  type SpendInputSignerMissingDeploymentBinding,
  type SpendInputSignerMissingRuntimeLoader,
} from "./authenticated-workflow.create-spend-input-signer-missing-bound-config.js";
import { createSpendInputSignerMissingRawL1StageResolver } from "./authenticated-workflow.create-spend-input-signer-missing-raw-l1-stage-resolver.js";
import {
  type SpendInputSignerMissingAssemblyRuntime,
  spendInputSignerStageFromL1,
} from "./authenticated-workflow.derive-spend-input-signer-missing-authenticated-source.js";
import { createSpendInputSignerMissingCentralJournalAdapter } from "./central-journal.js";
import {
  planSpendInputSignerInputOpening,
  planSpendInputSignerWitnessOpening,
} from "./field-plans.js";
import {
  SpendInputSignerStep02DatumSchema,
  SpendInputSignerStep03DatumSchema,
  SpendInputSignerStep04DatumSchema,
  SpendInputSignerStep05DatumSchema,
} from "./schemas.js";
import { type SpendInputSignerMissingEvidence } from "./spend-input-signer-missing.js";
import {
  nextSpendInputSignerAction,
  type SpendInputSignerJournal,
  spendInputSignerWorkflowEvidenceIdentity,
} from "./workflow.js";
import { SPEND_INPUT_SIGNER_MISSING_CURSOR_SPEC } from "./workflow-spec.js";

export const loadManifestBoundSpendInputSignerMissingConfig = async (
  input: LoadManifestBoundSpendInputSignerMissingConfig,
): Promise<ManifestBoundSpendInputSignerMissingConfig> => {
  const binding = await bindFraudProofWorkflowDeployment({
    manifest: input.manifest,
    blueprintJson: input.blueprintJson,
    deploymentInfo: input.deploymentInfo,
    category: "spendInputSignerMissing",
    headerHash: input.headerHash,
    proverCredential: input.signer.paymentKeyHash,
    stepDatumSchemas:
      SPEND_INPUT_SIGNER_MISSING_FAMILY_DEFINITION.stepDatumSchemas,
  });
  assertManifestBoundWorkflowSigner({
    network: binding.network,
    address: input.signer.address,
    paymentKeyHash: input.signer.paymentKeyHash,
  });

  return createSpendInputSignerMissingBoundConfig(input, binding);
};

export const createManifestBoundSpendInputSignerMissingRuntime = ({
  config,
  journal,
  observe,
  resolveStage,
  centralJournal,
  stateQueueMutationLeaseCoordinator,
}: {
  readonly config: ManifestBoundSpendInputSignerMissingConfig;
  readonly journal: SpendInputSignerJournal;
  readonly observe: SpendInputSignerMissingRuntimeLoader["observe"];
  readonly resolveStage: SpendInputSignerMissingRuntimeLoader["resolveStage"];
  readonly centralJournal?: ReturnType<
    typeof createSpendInputSignerMissingCentralJournalAdapter
  >;
  readonly stateQueueMutationLeaseCoordinator?: StateQueueMutationLeaseCoordinator;
}) => {
  const submission = createManifestBoundSpendInputSignerMissingSubmission({
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
    runtimeVersion: SPEND_INPUT_SIGNER_MISSING_WORKFLOW,
    config,
    runOrResume: async (evidence: SpendInputSignerMissingEvidence) => {
      const identity = spendInputSignerWorkflowEvidenceIdentity(evidence);
      for (;;) {
        const stage = await submission.observe(identity);
        const action = nextSpendInputSignerAction(stage);
        if (action === "done") return stage;
        if (action === "cancel")
          throw new Error(
            "spendInputSignerMissing automatic runner cannot synthesize cancellation",
          );
        const result = await submission.submit(action, evidence);
        await journal.append({
          sequence: (await journal.load(identity)).length,
          identity,
          sourceStage: stage,
          targetStage: result.stage,
          action,
          phase: "submitted",
          txHash: result.txHash,
        });
      }
    },
  });
};

export type ManifestBoundSpendInputSignerMissingWorkflowConfig =
  LoadManifestBoundSpendInputSignerMissingConfig &
    Readonly<{
      source: Omit<LocalKupmiosHttpOgmiosSourceConfig, "releaseFinality">;
      decisionDigest: string;
      stateQueueMutationLeaseCoordinator: StateQueueMutationLeaseCoordinator;
      historicalNativeScriptCheckpointStore: HistoricalNativeScriptCheckpointStore;
      historicalNativeScriptHistorySource: HistoricalNativeScriptHistorySource;
    }>;

export type ManifestBoundSpendInputSignerMissingWorkflow = Readonly<{
  deployment: FamilyDeploymentContext<
    "spendInputSignerMissing",
    "computationThreadMint" | "fraudProofMint" | "phasMembershipWithdraw",
    true,
    5
  >;
  workflowVersion: typeof SPEND_INPUT_SIGNER_MISSING_WORKFLOW;
  config: ManifestBoundSpendInputSignerMissingConfig;
  binding: SpendInputSignerMissingDeploymentBinding;
  l1: FraudProofFamilyL1ObservationPort<FraudProofCatalogueCategoryName>;
  stateQueueMutationLeaseCoordinator: StateQueueMutationLeaseCoordinator;
  decisionDigest: string;
  historicalNativeScriptCheckpointStore: HistoricalNativeScriptCheckpointStore;
  historicalNativeScriptHistorySource: HistoricalNativeScriptHistorySource;
}>;

export const SPEND_INPUT_SIGNER_MISSING_FAMILY_DEFINITION = defineFamily<
  "spendInputSignerMissing",
  "computationThreadMint" | "fraudProofMint" | "phasMembershipWithdraw",
  true,
  5,
  SpendInputSignerMissingAssemblyRuntime
>({
  category: "spendInputSignerMissing",
  stepDatumSchemas: [
    FraudProofComputationThreadStepDatum,
    SpendInputSignerStep02DatumSchema,
    SpendInputSignerStep03DatumSchema,
    SpendInputSignerStep04DatumSchema,
    SpendInputSignerStep05DatumSchema,
  ],
  witnessRoles: [
    "computationThreadMint",
    "fraudProofMint",
    "phasMembershipWithdraw",
  ],
  fieldPreimageCertificate: true,
  replayer: () => SPEND_INPUT_SIGNER_MISSING_COMPLETE_CANONICAL_REPLAY,
  adapter: {
    kind: "cursor",
    spec: SPEND_INPUT_SIGNER_MISSING_CURSOR_SPEC,
    stepContractNames: [
      SPEND_INPUT_SIGNER_MISSING_MANIFEST_CONTRACTS.step01,
      SPEND_INPUT_SIGNER_MISSING_MANIFEST_CONTRACTS.step02,
      SPEND_INPUT_SIGNER_MISSING_MANIFEST_CONTRACTS.step03,
      SPEND_INPUT_SIGNER_MISSING_MANIFEST_CONTRACTS.step04,
      SPEND_INPUT_SIGNER_MISSING_MANIFEST_CONTRACTS.step05,
    ],
    transactionPort: (context) => {
      const category = "spendInputSignerMissing";
      const { config, material } = context.runtime;
      const { binding, l1, stateQueueMutationLeaseCoordinator } = context;
      return {
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
            step_04: "submitScan",
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
                createManifestBoundSpendInputSignerMissingSubmission({
                  config,
                  preSubmitBoundary,
                  observe: async () =>
                    spendInputSignerStageFromL1(
                      (
                        await l1.observe({
                          headerHash: binding.definition.headerHash,
                        })
                      ).stage,
                    ),
                  resolveStage: createSpendInputSignerMissingRawL1StageResolver(
                    {
                      config,
                      l1,
                      source: admitted.source,
                    },
                  ),
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
    },
  },
  fieldCarriage: [
    {
      requirementForAction: (context, { action, artifact }) => {
        const category = "spendInputSignerMissing";
        const { config, material } = context.runtime;
        const { binding } = context;
        if (
          action.input.stage !== "step_02" &&
          action.input.stage !== "step_03" &&
          action.input.stage !== "step_04"
        )
          return null;
        const { evidence, source } = material.require(artifact);
        const certificate = binding.fieldPreimageCertificate;
        if (certificate === null)
          throw new Error(`${category} omitted field certificate authority`);
        return {
          planned:
            action.input.stage === "step_02"
              ? planSpendInputSignerInputOpening({
                  evidence,
                  nativeTxCompactCbor: source.nativeTxCompactCbor,
                  owner: config.signer.paymentKeyHash,
                  publish: true,
                })
              : planSpendInputSignerWitnessOpening({
                  evidence,
                  nativeTxCompactCbor: source.nativeTxCompactCbor,
                  witnessSetCompactCbor: source.witnessSetCompactCbor,
                  owner: config.signer.paymentKeyHash,
                  publish: true,
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
      },
    },
  ],
});
