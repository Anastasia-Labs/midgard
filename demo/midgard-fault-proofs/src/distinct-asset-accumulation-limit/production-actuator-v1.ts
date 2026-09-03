import {
  PROOF_THREAD_SOURCE_KIND_ACCEPTED,
  PROOF_THREAD_SOURCE_KIND_FORCED,
} from "@al-ft/midgard-sdk";
import type { LucidEvolution, Network, UTxO } from "@lucid-evolution/lucid";

import { requireLinearFaultThreadUtxo } from "../linear-fault-family-v1.js";
import type { StateQueueMutationLeaseCoordinator } from "../remove-fraudulent-block.js";
import type { ResolvedProverSigner } from "../runtime.js";
import { submitInit } from "../submit-init.js";
import type { SubmitStep01TxInclusion } from "../submit-step-01.js";
import type { FaultProofWitnessReferenceScripts } from "../witness-reference-scripts-v1.js";
import {
  captureCursorRemoval,
  type CursorFamilyActionInput,
} from "../workflow/production-cursor-family-runtime-v1.js";
import {
  captureLocallyEvaluatedTransaction,
  type LocallyEvaluatedTransaction,
} from "../workflow/transaction-boundary-v1.js";
import type { DistinctAssetAccumulationContracts } from "./contracts-v1.js";
import {
  DISTINCT_ASSET_ACCUMULATION_LIMIT_CATEGORY,
  type DistinctAssetAccumulationEvidence,
  type DistinctAssetAccumulationFinding,
} from "./family-v1.js";
import type { DistinctAssetFoldAction } from "./submit-fold-v1.js";
import { submitDistinctAssetAccumulationFold } from "./submit-fold-v1.js";
import {
  submitDistinctAssetAccumulationStep01Accepted,
  submitDistinctAssetAccumulationStep01Forced,
} from "./submit-step-01-v1.js";
import {
  type DistinctAssetAccumulatorAuthentication,
  submitDistinctAssetAccumulationStep02,
} from "./submit-step-02-v1.js";
import { submitDistinctAssetAccumulationStep06 } from "./submit-step-06-v1.js";

export type DistinctAssetAccumulationWorkflowReferences = Readonly<{
  steps: readonly [UTxO, UTxO, UTxO, UTxO, UTxO, UTxO];
  witnesses: Required<FaultProofWitnessReferenceScripts>;
}>;
export type DistinctAssetAccumulationActuationArtifact = Readonly<{
  headerHash: string;
  finding: DistinctAssetAccumulationFinding;
  evidence: DistinctAssetAccumulationEvidence;
  accepted?: Readonly<{
    txInclusion: SubmitStep01TxInclusion;
    validationTracesRoot: string;
    validationTraceCount: bigint;
  }>;
  forcedSource?: Readonly<{
    header: Readonly<Record<string, unknown>>;
    [field: string]: unknown;
  }>;
  authentication: DistinctAssetAccumulatorAuthentication;
  folds: readonly [
    DistinctAssetFoldAction,
    DistinctAssetFoldAction,
    DistinctAssetFoldAction,
  ];
}>;
export type DistinctAssetAccumulationActuatorAction =
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
export type DistinctAssetAccumulationCapturedAction = Readonly<{
  transaction: LocallyEvaluatedTransaction;
  mutationLease?: Awaited<
    ReturnType<StateQueueMutationLeaseCoordinator["acquire"]>
  >;
}>;

export type DistinctAssetAccumulationActuatorConfig = Readonly<{
  lucid: LucidEvolution;
  blueprint: unknown;
  deploymentInfo: unknown;
  network: Network;
  signer: ResolvedProverSigner;
  categoryId: string;
  contracts: DistinctAssetAccumulationContracts;
  references: DistinctAssetAccumulationWorkflowReferences;
  stateQueueMutationLeaseCoordinator: StateQueueMutationLeaseCoordinator;
  fraudProofSpendingScriptHash: string;
  fraudProverRewardLovelace: bigint;
}>;

const capture = async (
  submit: Parameters<typeof captureLocallyEvaluatedTransaction>[0],
): Promise<DistinctAssetAccumulationCapturedAction> =>
  Object.freeze({
    transaction: await captureLocallyEvaluatedTransaction(submit),
  });

/**
 * Family actuator. Builders stop after local UPLC evaluation and signing;
 * the durable production workflow remains the sole submit authority.
 */
export const createDistinctAssetAccumulationActuator = (
  config: DistinctAssetAccumulationActuatorConfig,
) => {
  if (config.categoryId !== "00000035")
    throw new Error("distinctAssetAccumulationLimit category id changed");
  return Object.freeze({
    capture: async ({
      action,
      artifact,
    }: {
      readonly action: DistinctAssetAccumulationActuatorAction;
      readonly artifact: DistinctAssetAccumulationActuationArtifact;
    }): Promise<DistinctAssetAccumulationCapturedAction> => {
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
            fraudCategory: DISTINCT_ASSET_ACCUMULATION_LIMIT_CATEGORY as never,
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
              PROOF_THREAD_SOURCE_KIND_ACCEPTED) ||
          (artifact.forcedSource !== undefined &&
            artifact.finding.subject.source_kind !==
              PROOF_THREAD_SOURCE_KIND_FORCED)
        )
          throw new Error(
            "distinctAssetAccumulationLimit source polarity changed",
          );
        if (accepted !== undefined)
          return await capture(async (preSubmitBoundary) => {
            const { threadUtxo, threadToken } =
              await requireLinearFaultThreadUtxo({
                lucid: config.lucid,
                contracts: config.contracts,
                categoryId: config.categoryId,
                family: "distinct-asset-accumulation-limit",
                stepIndex: 0,
                threadOutRef: action.threadOutRef,
              });
            await submitDistinctAssetAccumulationStep01Accepted({
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
          await submitDistinctAssetAccumulationStep01Forced({
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
          await submitDistinctAssetAccumulationStep02({
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
          await submitDistinctAssetAccumulationFold({
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
          await submitDistinctAssetAccumulationStep06({
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
      return await captureCursorRemoval({
        category: {
          name: DISTINCT_ASSET_ACCUMULATION_LIMIT_CATEGORY,
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
          category: DISTINCT_ASSET_ACCUMULATION_LIMIT_CATEGORY,
          stage: "remove",
          nextRemovalOutRef: action.nextRemovalOutRef,
          fraudProofOutRef: action.fraudProofOutRef,
        } as CursorFamilyActionInput,
        stateQueueMutationLeaseCoordinator:
          config.stateQueueMutationLeaseCoordinator,
        fraudProverRewardLovelace: config.fraudProverRewardLovelace,
      });
    },
  });
};
