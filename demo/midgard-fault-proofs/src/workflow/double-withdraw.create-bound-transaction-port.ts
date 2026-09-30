import { DOUBLE_WITHDRAW_VIOLATION_ID } from "@al-ft/midgard-sdk";
import type { LucidEvolution } from "@lucid-evolution/lucid";

import type { DoubleWithdrawContracts } from "../double-withdraw/contracts.js";
import { submitDoubleWithdrawInit } from "../double-withdraw/submit-double-withdraw-init.js";
import { submitDoubleWithdrawStep01 } from "../double-withdraw/submit-double-withdraw-step-01.js";
import { submitDoubleWithdrawStep02 } from "../double-withdraw/submit-double-withdraw-step-02.js";
import {
  admitCanonicalEvidenceForProofBuild,
  type CanonicalEvidenceBuilderInput,
} from "../evidence/prepare-from-evidence.js";
import { prepareDoubleWithdrawFromCommittedLeaves } from "../prepare-double-withdraw.js";
import {
  type StateQueueMutationLease,
  type StateQueueMutationLeaseCoordinator,
  submitRemoveFraudulentBlock,
} from "../remove-fraudulent-block.js";
import type { ResolvedProverSigner } from "../runtime.js";
import type { CanonicalBlockClassification } from "./classification.js";
import type { FraudProofWorkflowDeploymentBinding } from "./deployment-manifest-binding.js";
import {
  admitDoubleWithdrawArtifact,
  detectionIdForPrepared,
  DOUBLE_WITHDRAW_ARTIFACT,
  type DoubleWithdrawArtifact,
  record,
  selectedPairFromClassification,
} from "./double-withdraw.parse-artifact.js";
import {
  type LinearFamilyAssemblyContext,
  type LinearFamilyReferenceScripts,
  type ManifestBoundLinearFamilyWorkflow,
  type ManifestBoundLinearFamilyWorkflowConfig,
} from "./family-definition.js";
import { normalizeJournalJson } from "./journal.js";
import {
  LINEAR_FAMILY_TRANSACTION_PORT,
  type LinearFamilyTransactionPort,
} from "./linear-family-adapter.js";
import { type FraudProofWorkflowAction } from "./orchestrator.js";
import {
  captureLocallyEvaluatedTransaction,
  workflowTransactionInputOutRefs,
  workflowTransactionReferenceInputOutRefs,
} from "./transaction-boundary.js";

const prepareArtifactFromEvidence = async ({
  evidence,
  classification,
}: CanonicalEvidenceBuilderInput & {
  readonly classification: Extract<
    CanonicalBlockClassification,
    { readonly decision: "fault_detected" }
  > & { readonly category: "doubleWithdraw" };
}): Promise<DoubleWithdrawArtifact> => {
  const admitted = admitCanonicalEvidenceForProofBuild(evidence);
  if (
    classification.headerHash !== admitted.headerHash ||
    classification.selected.violationId !== DOUBLE_WITHDRAW_VIOLATION_ID
  ) {
    throw new Error(
      "double-withdraw classification differs from canonical evidence",
    );
  }
  const entries = evidence.reconstruction.rootData.withdrawals.entries.map(
    ({ key, value }) => ({
      keyCbor: key.toString("hex"),
      valueCbor: value.toString("hex"),
    }),
  );
  const selected = selectedPairFromClassification(classification);
  if (
    entries[selected.firstLeafIndex]?.keyCbor !== selected.firstKeyCbor ||
    entries[selected.secondLeafIndex]?.keyCbor !== selected.secondKeyCbor
  ) {
    throw new Error(
      "double-withdraw classification keys differ from the committed leaves",
    );
  }
  const prepared = await prepareDoubleWithdrawFromCommittedLeaves({
    headerHash: admitted.headerHash,
    committedWithdrawalsRoot: evidence.header.withdrawalsRoot,
    withdrawalCount: evidence.header.withdrawalCount,
    entries: entries.map(({ keyCbor, valueCbor }) => [keyCbor, valueCbor]),
    firstWithdrawalIdCbor: selected.firstKeyCbor,
    secondWithdrawalIdCbor: selected.secondKeyCbor,
  });
  if (
    classification.selected.position !== BigInt(prepared.secondLeaf.index) ||
    classification.selected.detectionId !== detectionIdForPrepared(prepared)
  ) {
    throw new Error(
      "double-withdraw classification changed its deterministic committed pair",
    );
  }
  const artifact = normalizeJournalJson({
    schemaVersion: DOUBLE_WITHDRAW_ARTIFACT,
    headerHash: admitted.headerHash,
    committedWithdrawalsRoot: evidence.header.withdrawalsRoot,
    withdrawalCount: entries.length,
    firstLeafIndex: prepared.firstLeaf.index,
    secondLeafIndex: prepared.secondLeaf.index,
    entries,
  }) as DoubleWithdrawArtifact;
  await admitDoubleWithdrawArtifact(artifact);
  return Object.freeze(artifact);
};

export const WITNESS_ROLES = [
  "computationThreadMint",
  "fraudProofMint",
  "phasMembershipWithdraw",
] as const;

export type DoubleWithdrawWorkflowReferenceScripts =
  LinearFamilyReferenceScripts<
    "doubleWithdraw",
    (typeof WITNESS_ROLES)[number],
    false
  >;

export type AssemblyContext = LinearFamilyAssemblyContext<
  "doubleWithdraw",
  (typeof WITNESS_ROLES)[number],
  false
>;

export type BoundDoubleWithdrawTransactionsConfig = Readonly<{
  lucid: LucidEvolution;
  blueprint: unknown;
  network: FraudProofWorkflowDeploymentBinding<"doubleWithdraw">["network"];
  signer: ResolvedProverSigner;
  headerHash: string;
  contracts: DoubleWithdrawContracts;
  category: FraudProofWorkflowDeploymentBinding<"doubleWithdraw">["resolvedContracts"]["category"];
  catalogue: FraudProofWorkflowDeploymentBinding<"doubleWithdraw">["catalogue"];
  referenceScripts: DoubleWithdrawWorkflowReferenceScripts;
  stateQueueMutationLeaseCoordinator: StateQueueMutationLeaseCoordinator;
  fraudProverRewardLovelace: bigint;
  deploymentInfo: unknown;
}>;

export type DoubleWithdrawBuilderSet = Readonly<{
  init: typeof submitDoubleWithdrawInit;
  step01: typeof submitDoubleWithdrawStep01;
  step02: typeof submitDoubleWithdrawStep02;
  remove: typeof submitRemoveFraudulentBlock;
}>;

export const productionBuilders: DoubleWithdrawBuilderSet = Object.freeze({
  init: submitDoubleWithdrawInit,
  step01: submitDoubleWithdrawStep01,
  step02: submitDoubleWithdrawStep02,
  remove: submitRemoveFraudulentBlock,
});

const requiredAction = (
  action: FraudProofWorkflowAction,
): Readonly<Record<string, unknown>> => {
  const input = record(action.input, "double-withdraw workflow action");
  if (
    input.schemaVersion !== "midgard-production-linear-family-action-v1" ||
    input.category !== "doubleWithdraw" ||
    typeof input.stage !== "string"
  ) {
    throw new Error("double-withdraw workflow action changed identity");
  }
  return input;
};

const stringField = (
  input: Readonly<Record<string, unknown>>,
  name: string,
): string => {
  const value = input[name];
  if (typeof value !== "string") {
    throw new Error(`double-withdraw workflow action omitted ${name}`);
  }
  return value;
};

export const createBoundTransactionPort = ({
  config,
  builders,
}: {
  readonly config: BoundDoubleWithdrawTransactionsConfig;
  readonly builders: DoubleWithdrawBuilderSet;
}): LinearFamilyTransactionPort<"doubleWithdraw"> => ({
  portVersion: LINEAR_FAMILY_TRANSACTION_PORT,
  category: "doubleWithdraw",
  prepare: async ({ evidence, classification }) =>
    await prepareArtifactFromEvidence({ evidence, classification }),
  capture: async ({ action, artifact }) => {
    const admitted = await admitDoubleWithdrawArtifact(artifact);
    if (admitted.artifact.headerHash !== config.headerHash) {
      throw new Error(
        "double-withdraw artifact targets a different manifest-bound header",
      );
    }
    const input = requiredAction(action);
    if (input.stage === "init") {
      const transaction = await captureLocallyEvaluatedTransaction(
        async (preSubmitBoundary) => {
          await builders.init({
            lucid: config.lucid,
            blueprint: config.blueprint,
            network: config.network,
            contracts: config.contracts,
            category: config.category,
            catalogue: config.catalogue,
            signer: config.signer,
            fraudulentBlockOutRef: stringField(input, "stateQueueBlockOutRef"),
            fraudulentHeaderHash: config.headerHash,
            witnessReferenceScripts: config.referenceScripts.witnesses,
            preSubmitBoundary,
            awaitConfirmation: false,
          });
        },
      );
      return Object.freeze({ transaction });
    }
    if (input.stage === "step_01") {
      const transaction = await captureLocallyEvaluatedTransaction(
        async (preSubmitBoundary) => {
          await builders.step01({
            lucid: config.lucid,
            contracts: config.contracts,
            categoryId: config.category.categoryId,
            network: config.network,
            signer: config.signer,
            threadOutRef: stringField(input, "threadOutRef"),
            stateQueueBlockOutRef: stringField(input, "stateQueueBlockOutRef"),
            inclusion: admitted.firstInclusion,
            referenceScriptUtxo: config.referenceScripts.steps[0],
            preSubmitBoundary,
            awaitConfirmation: false,
          });
        },
      );
      return Object.freeze({ transaction });
    }
    if (input.stage === "step_02") {
      const transaction = await captureLocallyEvaluatedTransaction(
        async (preSubmitBoundary) => {
          await builders.step02({
            lucid: config.lucid,
            contracts: config.contracts,
            categoryId: config.category.categoryId,
            network: config.network,
            signer: config.signer,
            threadOutRef: stringField(input, "threadOutRef"),
            stateQueueBlockOutRef: stringField(input, "stateQueueBlockOutRef"),
            inclusion: admitted.secondInclusion,
            referenceScriptUtxo: config.referenceScripts.steps[1],
            witnessReferenceScripts: config.referenceScripts.witnesses,
            preSubmitBoundary,
            awaitConfirmation: false,
          });
        },
      );
      return Object.freeze({ transaction });
    }
    if (input.stage === "remove") {
      let mutationLease: StateQueueMutationLease | undefined;
      const retainingCoordinator: StateQueueMutationLeaseCoordinator = {
        acquire: async () => {
          const acquired =
            await config.stateQueueMutationLeaseCoordinator.acquire();
          mutationLease = acquired;
          return acquired;
        },
      };
      const nextRemovalOutRef = stringField(input, "nextRemovalOutRef");
      const fraudProofOutRef = stringField(input, "fraudProofOutRef");
      const transaction = await captureLocallyEvaluatedTransaction(
        async (boundary) => {
          await builders.remove({
            lucid: config.lucid,
            blueprint: config.blueprint,
            deploymentInfo: config.deploymentInfo,
            network: config.network,
            signer: config.signer,
            fraudCategory: "doubleWithdraw",
            fraudulentHeaderHash: config.headerHash,
            requireReferenceScripts: true,
            stateQueueMutationLeaseCoordinator: retainingCoordinator,
            fraudProverRewardLovelace: config.fraudProverRewardLovelace,
            preSubmitBoundary: async (built) => {
              if (
                !workflowTransactionInputOutRefs(built.signed).includes(
                  nextRemovalOutRef,
                )
              ) {
                throw new Error(
                  "double-withdraw removal does not consume the authenticated next queue input",
                );
              }
              if (
                !workflowTransactionReferenceInputOutRefs(
                  built.signed,
                ).includes(fraudProofOutRef)
              ) {
                throw new Error(
                  "double-withdraw removal does not reference the authenticated retained proof token",
                );
              }
              await boundary(built);
            },
          });
        },
      );
      return Object.freeze({
        transaction,
        ...(mutationLease === undefined ? {} : { mutationLease }),
      });
    }
    throw new Error(
      `double-withdraw workflow action has unsupported stage ${String(input.stage)}`,
    );
  },
});

export type ManifestBoundDoubleWithdrawWorkflowConfig =
  ManifestBoundLinearFamilyWorkflowConfig<
    "doubleWithdraw",
    (typeof WITNESS_ROLES)[number],
    false
  >;

export type ManifestBoundDoubleWithdrawWorkflow =
  ManifestBoundLinearFamilyWorkflow<"doubleWithdraw", false>;
