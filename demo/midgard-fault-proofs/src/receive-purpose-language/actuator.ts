import type {
  ForcedInclusionTxV1,
  Header,
  OutputReference,
  RootMembershipProof,
} from "@al-ft/midgard-sdk";
import type { LucidEvolution, Network, UTxO } from "@lucid-evolution/lucid";

import type { StateQueueMutationLeaseCoordinator } from "../remove-fraudulent-block.js";
import type { ResolvedProverSigner } from "../runtime.js";
import { submitInit } from "../submit-init.js";
import type { SubmitStep01TxInclusion } from "../submit-step-01.js";
import type { FaultProofWitnessReferenceScripts } from "../witness-reference-scripts.js";
import {
  captureCursorRemoval,
  type CursorFamilyActionInput,
} from "../workflow/cursor-family-runtime.js";
import {
  captureLocallyEvaluatedTransaction,
  type LocallyEvaluatedTransaction,
} from "../workflow/transaction-boundary.js";
import type { ReceivePurposeLanguageContracts } from "./contracts.js";
import type { ReceivePurposeLanguageEvidence } from "./family.js";
import {
  submitReceivePurposeLanguageStep01Accepted,
  submitReceivePurposeLanguageStep01Forced,
} from "./submit-step-01.js";
import type { ReceivePurposeLanguageAuthentication } from "./submit-step-02.js";
import { submitReceivePurposeLanguageStep02 } from "./submit-step-02.js";
import { submitReceivePurposeLanguageStep03 } from "./submit-step-03.js";

export type ReceivePurposeLanguageArtifact = Readonly<{
  headerHash: string;
  header: Header;
  evidence: ReceivePurposeLanguageEvidence;
  authentication: ReceivePurposeLanguageAuthentication;
  acceptedInclusion?: SubmitStep01TxInclusion;
  forcedMembership?: RootMembershipProof<OutputReference, ForcedInclusionTxV1>;
}>;
export type ReceivePurposeLanguageWorkflowReferences = Readonly<{
  steps: readonly [UTxO, UTxO, UTxO];
  witnesses: Required<FaultProofWitnessReferenceScripts>;
}>;
export type ReceivePurposeLanguageActuatorAction =
  | Readonly<{ stage: "init"; stateQueueBlockOutRef: string }>
  | Readonly<{
      stage: "step_01";
      threadOutRef: string;
      stateQueueBlockOutRef: string;
    }>
  | Readonly<{ stage: "step_02"; threadOutRef: string }>
  | Readonly<{ stage: "step_03"; threadOutRef: string }>
  | Readonly<{
      stage: "remove";
      nextRemovalOutRef: string;
      fraudProofOutRef: string;
    }>;
export type ReceivePurposeLanguageCapturedAction = Readonly<{
  transaction: LocallyEvaluatedTransaction;
  mutationLease?: Awaited<
    ReturnType<StateQueueMutationLeaseCoordinator["acquire"]>
  >;
}>;
export type BoundReceivePurposeLanguageActuatorConfig = Readonly<{
  binding: Readonly<{
    blueprint: unknown;
    deploymentInfo: unknown;
    network: Network;
    definition: Readonly<{ headerHash: string }>;
    releaseEconomics: Readonly<{
      policy: Readonly<{ fraudProverRewardLovelace: string }>;
    }>;
    resolvedContracts: Readonly<{
      category: Readonly<{ categoryId: string }>;
      contracts: Readonly<{
        fraudProof: Readonly<{ spendingScriptHash: string }>;
      }>;
    }>;
  }>;
  lucid: LucidEvolution;
  signer: ResolvedProverSigner;
  contracts: ReceivePurposeLanguageContracts;
  references: ReceivePurposeLanguageWorkflowReferences;
  stateQueueMutationLeaseCoordinator: StateQueueMutationLeaseCoordinator;
}>;
const captured = async (
  submit: Parameters<typeof captureLocallyEvaluatedTransaction>[0],
): Promise<ReceivePurposeLanguageCapturedAction> =>
  Object.freeze({
    transaction: await captureLocallyEvaluatedTransaction(submit),
  });

/** Family-owned local-evaluation boundary; callers cannot submit around intent journaling. */
export const createReceivePurposeLanguageActuator = (
  config: BoundReceivePurposeLanguageActuatorConfig,
) =>
  Object.freeze({
    capture: async ({
      action,
      artifact,
    }: {
      action: ReceivePurposeLanguageActuatorAction;
      artifact: ReceivePurposeLanguageArtifact;
    }): Promise<ReceivePurposeLanguageCapturedAction> => {
      if (artifact.headerHash !== config.binding.definition.headerHash)
        throw new Error("receivePurposeLanguage artifact changed bound header");
      const categoryId = config.binding.resolvedContracts.category.categoryId;
      if (action.stage === "init")
        return await captured(async (preSubmitBoundary) => {
          await submitInit({
            lucid: config.lucid,
            blueprint: config.binding.blueprint,
            deploymentInfo: config.binding.deploymentInfo,
            network: config.binding.network,
            signer: config.signer,
            fraudCategory: "receivePurposeLanguage" as never,
            fraudulentBlockOutRef: action.stateQueueBlockOutRef,
            fraudulentHeaderHash: artifact.headerHash,
            witnessReferenceScripts: config.references.witnesses,
            preSubmitBoundary,
            awaitConfirmation: false,
          });
        });
      if (action.stage === "step_01")
        return await captured(async (preSubmitBoundary) => {
          const common = {
            lucid: config.lucid,
            contracts: config.contracts,
            categoryId,
            signer: config.signer,
            threadOutRef: action.threadOutRef,
            header: artifact.header,
            executionIndex: BigInt(artifact.evidence.finding.executionIndex),
            referenceScriptUtxo: config.references.steps[0],
            preSubmitBoundary,
            awaitConfirmation: false,
          } as const;
          if (artifact.acceptedInclusion !== undefined)
            await submitReceivePurposeLanguageStep01Accepted({
              ...common,
              blueprint: config.binding.blueprint,
              network: config.binding.network,
              stateQueueBlockOutRef: action.stateQueueBlockOutRef,
              txInclusion: artifact.acceptedInclusion,
              witnessReferenceScripts: config.references.witnesses,
            });
          else if (artifact.forcedMembership !== undefined)
            await submitReceivePurposeLanguageStep01Forced({
              ...common,
              membership: artifact.forcedMembership,
            });
          else
            throw new Error(
              "receivePurposeLanguage artifact omitted exact source",
            );
        });
      if (action.stage === "step_02")
        return await captured(async (preSubmitBoundary) => {
          await submitReceivePurposeLanguageStep02({
            lucid: config.lucid,
            contracts: config.contracts,
            categoryId,
            signer: config.signer,
            threadOutRef: action.threadOutRef,
            evidence: artifact.evidence,
            authentication: artifact.authentication,
            referenceScriptUtxo: config.references.steps[1],
            preSubmitBoundary,
            awaitConfirmation: false,
          });
        });
      if (action.stage === "step_03")
        return await captured(async (preSubmitBoundary) => {
          await submitReceivePurposeLanguageStep03({
            lucid: config.lucid,
            contracts: config.contracts,
            categoryId,
            signer: config.signer,
            threadOutRef: action.threadOutRef,
            evidence: artifact.evidence,
            referenceScriptUtxo: config.references.steps[2],
            witnessReferenceScripts: config.references.witnesses,
            preSubmitBoundary,
            awaitConfirmation: false,
          });
        });
      return await captureCursorRemoval({
        category: {
          name: "receivePurposeLanguage",
          categoryId,
          firstStepDeploymentEntry: "fraudProofReceivePurposeLanguage",
          firstStepScriptHash: config.contracts.steps[0].spendingScriptHash,
          fraudProof: {
            policyId: config.contracts.fraudProof.policyId,
            spendingScriptHash:
              config.binding.resolvedContracts.contracts.fraudProof
                .spendingScriptHash,
            spendingScriptAddress:
              config.contracts.fraudProof.spendingScriptAddress,
          },
        } as never,
        lucid: config.lucid,
        blueprint: config.binding.blueprint,
        deploymentInfo: config.binding.deploymentInfo,
        network: config.binding.network,
        signer: config.signer,
        headerHash: artifact.headerHash,
        input: {
          schemaVersion: "midgard-production-cursor-family-action-v1",
          category: "receivePurposeLanguage",
          stage: "remove",
          nextRemovalOutRef: action.nextRemovalOutRef,
          fraudProofOutRef: action.fraudProofOutRef,
        } as CursorFamilyActionInput,
        stateQueueMutationLeaseCoordinator:
          config.stateQueueMutationLeaseCoordinator,
        fraudProverRewardLovelace: BigInt(
          config.binding.releaseEconomics.policy.fraudProverRewardLovelace,
        ),
      });
    },
  });
