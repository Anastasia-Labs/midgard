import {
  PROOF_THREAD_SOURCE_KIND_ACCEPTED_V1,
  PROOF_THREAD_SOURCE_KIND_FORCED_V1,
} from "@al-ft/midgard-sdk";
import type { LucidEvolution, Network, UTxO } from "@lucid-evolution/lucid";

import { requireLinearFaultThreadUtxoV1 } from "../linear-fault-family-v1.js";
import type { StateQueueMutationLeaseCoordinator } from "../remove-fraudulent-block.js";
import type { ResolvedProverSigner } from "../runtime.js";
import { submitInit } from "../submit-init.js";
import type { SubmitStep01TxInclusion } from "../submit-step-01.js";
import type { FaultProofWitnessReferenceScriptsV1 } from "../witness-reference-scripts-v1.js";
import {
  captureProductionCursorRemovalV1,
  type ProductionCursorFamilyActionInputV1,
} from "../workflow/production-cursor-family-runtime-v1.js";
import {
  captureLocallyEvaluatedTransactionV1,
  type LocallyEvaluatedTransactionV1,
} from "../workflow/transaction-boundary-v1.js";
import type { DistinctAssetAccumulationContractsV1 } from "./contracts-v1.js";
import {
  DISTINCT_ASSET_ACCUMULATION_LIMIT_CATEGORY_V1,
  type DistinctAssetAccumulationEvidenceV1,
  type DistinctAssetAccumulationFindingV1,
} from "./family-v1.js";
import type { DistinctAssetFoldActionV1 } from "./submit-fold-v1.js";
import { submitDistinctAssetAccumulationFoldV1 } from "./submit-fold-v1.js";
import {
  submitDistinctAssetAccumulationStep01AcceptedV1,
  submitDistinctAssetAccumulationStep01ForcedV1,
} from "./submit-step-01-v1.js";
import {
  type DistinctAssetAccumulatorAuthenticationV1,
  submitDistinctAssetAccumulationStep02V1,
} from "./submit-step-02-v1.js";
import { submitDistinctAssetAccumulationStep06V1 } from "./submit-step-06-v1.js";

export type DistinctAssetAccumulationWorkflowReferencesV1 = Readonly<{
  steps: readonly [UTxO, UTxO, UTxO, UTxO, UTxO, UTxO];
  witnesses: Required<FaultProofWitnessReferenceScriptsV1>;
}>;
export type DistinctAssetAccumulationActuationArtifactV1 = Readonly<{
  headerHash: string;
  finding: DistinctAssetAccumulationFindingV1;
  evidence: DistinctAssetAccumulationEvidenceV1;
  accepted?: Readonly<{
    txInclusion: SubmitStep01TxInclusion;
    validationTracesRoot: string;
    validationTraceCount: bigint;
  }>;
  forcedSource?: Readonly<{
    header: Readonly<Record<string, unknown>>;
    [field: string]: unknown;
  }>;
  authentication: DistinctAssetAccumulatorAuthenticationV1;
  folds: readonly [
    DistinctAssetFoldActionV1,
    DistinctAssetFoldActionV1,
    DistinctAssetFoldActionV1,
  ];
}>;
export type DistinctAssetAccumulationActuatorActionV1 =
  | Readonly<{ stage: "init"; stateQueueBlockOutRef: string }>
  | Readonly<{
      stage: "step01";
      threadOutRef: string;
      stateQueueBlockOutRef: string;
    }>
  | Readonly<{ stage: "step02"; threadOutRef: string }>
  | Readonly<{
      stage: "fold";
      stepIndex: 2 | 3 | 4;
      threadOutRef: string;
    }>
  | Readonly<{ stage: "step06"; threadOutRef: string }>
  | Readonly<{
      stage: "remove";
      nextRemovalOutRef: string;
      fraudProofOutRef: string;
    }>;
export type DistinctAssetAccumulationCapturedActionV1 = Readonly<{
  transaction: LocallyEvaluatedTransactionV1;
  mutationLease?: Awaited<
    ReturnType<StateQueueMutationLeaseCoordinator["acquire"]>
  >;
}>;

export type DistinctAssetAccumulationActuatorConfigV1 = Readonly<{
  lucid: LucidEvolution;
  blueprint: unknown;
  deploymentInfo: unknown;
  network: Network;
  signer: ResolvedProverSigner;
  categoryId: string;
  contracts: DistinctAssetAccumulationContractsV1;
  references: DistinctAssetAccumulationWorkflowReferencesV1;
  stateQueueMutationLeaseCoordinator: StateQueueMutationLeaseCoordinator;
  fraudProofSpendingScriptHash: string;
  fraudProverRewardLovelace: bigint;
}>;

const capture = async (
  submit: Parameters<typeof captureLocallyEvaluatedTransactionV1>[0],
): Promise<DistinctAssetAccumulationCapturedActionV1> =>
  Object.freeze({
    transaction: await captureLocallyEvaluatedTransactionV1(submit),
  });

/**
 * Family actuator. Builders stop after local UPLC evaluation and signing;
 * the durable production workflow remains the sole submit authority.
 */
export const createDistinctAssetAccumulationActuatorV1 = (
  config: DistinctAssetAccumulationActuatorConfigV1,
) => {
  if (config.categoryId !== "00000035")
    throw new Error("distinctAssetAccumulationLimit category id changed");
  return Object.freeze({
    capture: async ({
      action,
      artifact,
    }: {
      readonly action: DistinctAssetAccumulationActuatorActionV1;
      readonly artifact: DistinctAssetAccumulationActuationArtifactV1;
    }): Promise<DistinctAssetAccumulationCapturedActionV1> => {
      if (!/^[0-9a-f]{64}$/u.test(artifact.headerHash))
        throw new Error(
          "distinctAssetAccumulationLimit artifact header changed",
        );
      if (
        artifact.finding.subject.transaction_id !==
          artifact.evidence.finding.subject.transaction_id ||
        JSON.stringify(artifact.finding.coordinate) !==
          JSON.stringify(artifact.evidence.finding.coordinate)
      )
        throw new Error(
          "distinctAssetAccumulationLimit artifact finding changed",
        );
      if (action.stage === "init")
        return await capture(async (preSubmitBoundary) => {
          await submitInit({
            lucid: config.lucid,
            blueprint: config.blueprint,
            deploymentInfo: config.deploymentInfo,
            network: config.network,
            signer: config.signer,
            fraudCategory:
              DISTINCT_ASSET_ACCUMULATION_LIMIT_CATEGORY_V1 as never,
            fraudulentBlockOutRef: action.stateQueueBlockOutRef,
            fraudulentHeaderHash: artifact.headerHash,
            witnessReferenceScripts: config.references.witnesses,
            preSubmitBoundary,
            awaitConfirmation: false,
          });
        });
      if (action.stage === "step01") {
        const accepted = artifact.accepted;
        if ((accepted === undefined) === (artifact.forcedSource === undefined))
          throw new Error(
            "distinctAssetAccumulationLimit source is absent or ambiguous",
          );
        if (
          (accepted !== undefined &&
            artifact.finding.subject.source_kind !==
              PROOF_THREAD_SOURCE_KIND_ACCEPTED_V1) ||
          (artifact.forcedSource !== undefined &&
            artifact.finding.subject.source_kind !==
              PROOF_THREAD_SOURCE_KIND_FORCED_V1)
        )
          throw new Error(
            "distinctAssetAccumulationLimit source polarity changed",
          );
        if (accepted !== undefined)
          return await capture(async (preSubmitBoundary) => {
            const { threadUtxo, threadToken } =
              await requireLinearFaultThreadUtxoV1({
                lucid: config.lucid,
                contracts: config.contracts,
                categoryId: config.categoryId,
                family: "distinct-asset-accumulation-limit",
                stepIndex: 0,
                threadOutRef: action.threadOutRef,
              });
            await submitDistinctAssetAccumulationStep01AcceptedV1({
              lucid: config.lucid,
              blueprint: config.blueprint,
              network: config.network,
              contracts: config.contracts,
              signer: config.signer,
              finding: artifact.finding,
              threadUtxo,
              threadToken,
              stateQueueBlockOutRef: action.stateQueueBlockOutRef,
              txInclusion: accepted.txInclusion,
              validationTracesRoot: accepted.validationTracesRoot,
              validationTraceCount: accepted.validationTraceCount,
              referenceScriptUtxo: config.references.steps[0],
              witnessReferenceScripts: config.references.witnesses,
              preSubmitBoundary,
              awaitConfirmation: false,
            });
          });
        if (artifact.forcedSource === undefined)
          throw new Error(
            "distinctAssetAccumulationLimit forced source disappeared",
          );
        return await capture(async (preSubmitBoundary) => {
          await submitDistinctAssetAccumulationStep01ForcedV1({
            lucid: config.lucid,
            contracts: config.contracts,
            categoryId: config.categoryId,
            signer: config.signer,
            threadOutRef: action.threadOutRef,
            finding: artifact.finding,
            forcedSource: artifact.forcedSource!,
            referenceScriptUtxo: config.references.steps[0],
            preSubmitBoundary,
            awaitConfirmation: false,
          });
        });
      }
      if (action.stage === "step02")
        return await capture(async (preSubmitBoundary) => {
          await submitDistinctAssetAccumulationStep02V1({
            lucid: config.lucid,
            contracts: config.contracts,
            categoryId: config.categoryId,
            signer: config.signer,
            threadOutRef: action.threadOutRef,
            authentication: artifact.authentication,
            referenceScriptUtxo: config.references.steps[1],
            preSubmitBoundary,
            awaitConfirmation: false,
          });
        });
      if (action.stage === "fold")
        return await capture(async (preSubmitBoundary) => {
          await submitDistinctAssetAccumulationFoldV1({
            lucid: config.lucid,
            contracts: config.contracts,
            categoryId: config.categoryId,
            signer: config.signer,
            threadOutRef: action.threadOutRef,
            stepIndex: action.stepIndex,
            action: artifact.folds[action.stepIndex - 2]!,
            referenceScriptUtxo: config.references.steps[action.stepIndex],
            preSubmitBoundary,
            awaitConfirmation: false,
          });
        });
      if (action.stage === "step06")
        return await capture(async (preSubmitBoundary) => {
          await submitDistinctAssetAccumulationStep06V1({
            lucid: config.lucid,
            contracts: config.contracts,
            categoryId: config.categoryId,
            signer: config.signer,
            threadOutRef: action.threadOutRef,
            evidence: artifact.evidence,
            referenceScriptUtxo: config.references.steps[5],
            witnessReferenceScripts: config.references.witnesses,
            preSubmitBoundary,
            awaitConfirmation: false,
          });
        });
      return await captureProductionCursorRemovalV1({
        category: {
          name: DISTINCT_ASSET_ACCUMULATION_LIMIT_CATEGORY_V1,
          categoryId: config.categoryId,
          firstStepDeploymentEntry: "fraudProofDistinctAssetAccumulationLimit",
          firstStepScriptHash: config.contracts.steps[0].spendingScriptHash,
          fraudProof: {
            policyId: config.contracts.fraudProof.policyId,
            spendingScriptHash: config.fraudProofSpendingScriptHash,
            spendingScriptAddress:
              config.contracts.fraudProof.spendingScriptAddress,
          },
        } as never,
        lucid: config.lucid,
        blueprint: config.blueprint,
        deploymentInfo: config.deploymentInfo,
        network: config.network,
        signer: config.signer,
        headerHash: artifact.headerHash,
        input: {
          schemaVersion: "midgard-production-cursor-family-action-v1",
          category: DISTINCT_ASSET_ACCUMULATION_LIMIT_CATEGORY_V1,
          stage: "remove",
          nextRemovalOutRef: action.nextRemovalOutRef,
          fraudProofOutRef: action.fraudProofOutRef,
        } as ProductionCursorFamilyActionInputV1,
        stateQueueMutationLeaseCoordinator:
          config.stateQueueMutationLeaseCoordinator,
        fraudProverRewardLovelace: config.fraudProverRewardLovelace,
      });
    },
  });
};
