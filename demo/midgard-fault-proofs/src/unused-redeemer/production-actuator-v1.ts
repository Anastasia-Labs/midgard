import type {
  ForcedInclusionTxV1,
  HeaderV1,
  OutputReference,
  RootMembershipProof,
} from "@al-ft/midgard-sdk";
import type { LucidEvolution, Network, UTxO } from "@lucid-evolution/lucid";

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
import type { UnusedRedeemerContractsV1 } from "./contracts-v1.js";
import type { UnusedRedeemerEvidenceV1 } from "./family-v1.js";
import { submitUnusedRedeemerCancelV1 } from "./submit-cancel-v1.js";
import { submitUnusedRedeemerStep01AcceptedV1 } from "./submit-step-01-v1.js";
import { submitUnusedRedeemerStep01ForcedV1 } from "./submit-step-01-v1.js";
import type { UnusedRedeemerAuthenticationV1 } from "./submit-step-02-v1.js";
import { submitUnusedRedeemerStep02V1 } from "./submit-step-02-v1.js";
import { submitUnusedRedeemerStep02aV1 } from "./submit-step-02a-v1.js";
import { submitUnusedRedeemerStep02bV1 } from "./submit-step-02b-v1.js";
import { submitUnusedRedeemerStep02cV1 } from "./submit-step-02c-v1.js";
import { submitUnusedRedeemerStep03V1 } from "./submit-step-03-v1.js";
import { submitUnusedRedeemerStep04V1 } from "./submit-step-04-v1.js";
import { submitUnusedRedeemerStep05V1 } from "./submit-step-05-v1.js";
import { submitUnusedRedeemerStep06V1 } from "./submit-step-06-v1.js";

export type UnusedRedeemerProductionArtifactV1 = Readonly<{
  headerHash: string;
  header: HeaderV1;
  evidence: UnusedRedeemerEvidenceV1;
  authentication: UnusedRedeemerAuthenticationV1;
  acceptedInclusion?: SubmitStep01TxInclusion;
  forcedMembership?: RootMembershipProof<OutputReference, ForcedInclusionTxV1>;
}>;

export type UnusedRedeemerWorkflowReferencesV1 = Readonly<{
  steps: readonly [UTxO, UTxO, UTxO, UTxO, UTxO, UTxO, UTxO, UTxO, UTxO];
  witnesses: Required<FaultProofWitnessReferenceScriptsV1>;
}>;

export type UnusedRedeemerActuatorActionV1 =
  | Readonly<{ stage: "init"; stateQueueBlockOutRef: string }>
  | Readonly<{
      stage: "step_01";
      threadOutRef: string;
      stateQueueBlockOutRef: string;
    }>
  | Readonly<{ stage: "step_02"; threadOutRef: string }>
  | Readonly<{ stage: "step_02a"; threadOutRef: string }>
  | Readonly<{ stage: "step_02b"; threadOutRef: string }>
  | Readonly<{ stage: "step_02c"; threadOutRef: string }>
  | Readonly<{ stage: "step_03"; threadOutRef: string }>
  | Readonly<{ stage: "step_04"; threadOutRef: string }>
  | Readonly<{ stage: "step_05"; threadOutRef: string }>
  | Readonly<{ stage: "step_06"; threadOutRef: string }>
  | Readonly<{
      stage: "cancel";
      threadOutRef: string;
      stepIndex: number;
    }>
  | Readonly<{
      stage: "remove";
      nextRemovalOutRef: string;
      fraudProofOutRef: string;
    }>;

export type UnusedRedeemerCapturedActionV1 = Readonly<{
  transaction: LocallyEvaluatedTransactionV1;
  mutationLease?: Awaited<
    ReturnType<StateQueueMutationLeaseCoordinator["acquire"]>
  >;
}>;

export type BoundUnusedRedeemerActuatorConfigV1 = Readonly<{
  // Family-local structural binding until the protected catalogue serial pass.
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
  contracts: UnusedRedeemerContractsV1;
  references: UnusedRedeemerWorkflowReferencesV1;
  stateQueueMutationLeaseCoordinator: StateQueueMutationLeaseCoordinator;
}>;

const captured = async (
  submit: Parameters<typeof captureLocallyEvaluatedTransactionV1>[0],
): Promise<UnusedRedeemerCapturedActionV1> =>
  Object.freeze({
    transaction: await captureLocallyEvaluatedTransactionV1(submit),
  });

/** Package-owned locally evaluated transaction actuator. */
export const createUnusedRedeemerActuatorV1 = (
  config: BoundUnusedRedeemerActuatorConfigV1,
) =>
  Object.freeze({
    capture: async ({
      action,
      artifact,
    }: {
      action: UnusedRedeemerActuatorActionV1;
      artifact: UnusedRedeemerProductionArtifactV1;
    }): Promise<UnusedRedeemerCapturedActionV1> => {
      if (artifact.headerHash !== config.binding.definition.headerHash)
        throw new Error("unusedRedeemer artifact changed bound header");
      const categoryId = config.binding.resolvedContracts.category.categoryId;
      if (action.stage === "init")
        return await captured(async (preSubmitBoundary) => {
          await submitInit({
            lucid: config.lucid,
            blueprint: config.binding.blueprint,
            deploymentInfo: config.binding.deploymentInfo,
            network: config.binding.network,
            signer: config.signer,
            fraudCategory: "unusedRedeemer" as never,
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
            redeemerIndex: BigInt(artifact.evidence.finding.redeemerIndex),
            referenceScriptUtxo: config.references.steps[0],
            preSubmitBoundary,
            awaitConfirmation: false,
          } as const;
          if (artifact.acceptedInclusion !== undefined)
            await submitUnusedRedeemerStep01AcceptedV1({
              ...common,
              blueprint: config.binding.blueprint,
              network: config.binding.network,
              stateQueueBlockOutRef: action.stateQueueBlockOutRef,
              txInclusion: artifact.acceptedInclusion,
              witnessReferenceScripts: config.references.witnesses,
            });
          else if (artifact.forcedMembership !== undefined)
            await submitUnusedRedeemerStep01ForcedV1({
              ...common,
              membership: artifact.forcedMembership,
            });
          else throw new Error("unusedRedeemer artifact omitted exact source");
        });
      const linear = async (
        operation: (
          preSubmitBoundary: Parameters<
            typeof captureLocallyEvaluatedTransactionV1
          >[0] extends (boundary: infer T) => unknown
            ? T
            : never,
        ) => Promise<unknown>,
      ) => await captured(operation);
      if (action.stage === "step_02")
        return await linear(async (preSubmitBoundary) => {
          await submitUnusedRedeemerStep02V1({
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
      if (action.stage === "step_02a")
        return await linear(async (preSubmitBoundary) => {
          await submitUnusedRedeemerStep02aV1({
            lucid: config.lucid,
            contracts: config.contracts,
            categoryId,
            signer: config.signer,
            threadOutRef: action.threadOutRef,
            authentication: artifact.authentication,
            referenceScriptUtxo: config.references.steps[2],
            preSubmitBoundary,
            awaitConfirmation: false,
          });
        });
      if (action.stage === "step_02b")
        return await linear(async (preSubmitBoundary) => {
          await submitUnusedRedeemerStep02bV1({
            lucid: config.lucid,
            contracts: config.contracts,
            categoryId,
            signer: config.signer,
            threadOutRef: action.threadOutRef,
            authentication: artifact.authentication,
            referenceScriptUtxo: config.references.steps[3],
            preSubmitBoundary,
            awaitConfirmation: false,
          });
        });
      if (action.stage === "step_02c")
        return await linear(async (preSubmitBoundary) => {
          await submitUnusedRedeemerStep02cV1({
            lucid: config.lucid,
            contracts: config.contracts,
            categoryId,
            signer: config.signer,
            threadOutRef: action.threadOutRef,
            authentication: artifact.authentication,
            referenceScriptUtxo: config.references.steps[4],
            preSubmitBoundary,
            awaitConfirmation: false,
          });
        });
      if (action.stage === "step_03")
        return await linear(async (preSubmitBoundary) => {
          await submitUnusedRedeemerStep03V1({
            lucid: config.lucid,
            contracts: config.contracts,
            categoryId,
            signer: config.signer,
            threadOutRef: action.threadOutRef,
            authentication: artifact.authentication,
            referenceScriptUtxo: config.references.steps[5],
            preSubmitBoundary,
            awaitConfirmation: false,
          });
        });
      if (action.stage === "step_04")
        return await linear(async (preSubmitBoundary) => {
          await submitUnusedRedeemerStep04V1({
            lucid: config.lucid,
            contracts: config.contracts,
            categoryId,
            signer: config.signer,
            threadOutRef: action.threadOutRef,
            evidence: artifact.evidence,
            authentication: artifact.authentication,
            referenceScriptUtxo: config.references.steps[6],
            preSubmitBoundary,
            awaitConfirmation: false,
          });
        });
      if (action.stage === "step_05")
        return await linear(async (preSubmitBoundary) => {
          await submitUnusedRedeemerStep05V1({
            lucid: config.lucid,
            contracts: config.contracts,
            categoryId,
            signer: config.signer,
            threadOutRef: action.threadOutRef,
            evidence: artifact.evidence,
            referenceScriptUtxo: config.references.steps[7],
            preSubmitBoundary,
            awaitConfirmation: false,
          });
        });
      if (action.stage === "step_06")
        return await linear(async (preSubmitBoundary) => {
          await submitUnusedRedeemerStep06V1({
            lucid: config.lucid,
            contracts: config.contracts,
            categoryId,
            signer: config.signer,
            threadOutRef: action.threadOutRef,
            evidence: artifact.evidence,
            referenceScriptUtxo: config.references.steps[8],
            witnessReferenceScripts: config.references.witnesses,
            preSubmitBoundary,
            awaitConfirmation: false,
          });
        });
      if (action.stage === "cancel") {
        if (
          !Number.isSafeInteger(action.stepIndex) ||
          action.stepIndex < 0 ||
          action.stepIndex > 8
        )
          throw new Error("unusedRedeemer cancellation step changed");
        return await linear(async (preSubmitBoundary) => {
          await submitUnusedRedeemerCancelV1({
            lucid: config.lucid,
            contracts: config.contracts,
            categoryId,
            signer: config.signer,
            threadOutRef: action.threadOutRef,
            referenceScriptUtxo: config.references.steps[action.stepIndex]!,
            witnessReferenceScripts: config.references.witnesses,
            preSubmitBoundary,
            awaitConfirmation: false,
          });
        });
      }
      return await captureProductionCursorRemovalV1({
        category: {
          name: "unusedRedeemer",
          categoryId,
          firstStepDeploymentEntry: "fraudProofUnusedRedeemer",
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
          category: "unusedRedeemer" as never,
          stage: "remove",
          nextRemovalOutRef: action.nextRemovalOutRef,
          fraudProofOutRef: action.fraudProofOutRef,
        } as ProductionCursorFamilyActionInputV1,
        stateQueueMutationLeaseCoordinator:
          config.stateQueueMutationLeaseCoordinator,
        fraudProverRewardLovelace: BigInt(
          config.binding.releaseEconomics.policy.fraudProverRewardLovelace,
        ),
      });
    },
  });
