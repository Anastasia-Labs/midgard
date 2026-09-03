import type {
  ForcedInclusionTx,
  Header,
  OutputReference,
  RootMembershipProof,
} from "@al-ft/midgard-sdk";
import type { LucidEvolution, Network, UTxO } from "@lucid-evolution/lucid";

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
import type { ExecutionSourceScriptDecodingContracts } from "./contracts-v1.js";
import type { ExecutionSourceScriptDecodingEvidence } from "./family-v1.js";
import { submitExecutionSourceScriptDecodingStep01Accepted } from "./submit-step-01-v1.js";
import { submitExecutionSourceScriptDecodingStep01Forced } from "./submit-step-01-v1.js";
import type { ExecutionSourceAuthenticationData } from "./submit-step-02-v1.js";
import { submitExecutionSourceScriptDecodingStep02 } from "./submit-step-02-v1.js";
import { submitExecutionSourceScriptDecodingStep03 } from "./submit-step-03-v1.js";
import { submitExecutionSourceScriptDecodingStep04 } from "./submit-step-04-v1.js";
import { submitExecutionSourceScriptDecodingStep05 } from "./submit-step-05-v1.js";

export type ExecutionSourceScriptDecodingArtifact = Readonly<{
  headerHash: string;
  header: Header;
  evidence: ExecutionSourceScriptDecodingEvidence;
  authentication: ExecutionSourceAuthenticationData;
  acceptedInclusion?: SubmitStep01TxInclusion;
  forcedMembership?: RootMembershipProof<OutputReference, ForcedInclusionTx>;
}>;

export type ExecutionSourceScriptDecodingWorkflowReferences = Readonly<{
  steps: readonly [UTxO, UTxO, UTxO, UTxO, UTxO];
  witnesses: Required<FaultProofWitnessReferenceScripts>;
}>;

export type ExecutionSourceScriptDecodingActuatorAction =
  | Readonly<{ stage: "init"; stateQueueBlockOutRef: string }>
  | Readonly<{
      stage: "step_01";
      threadOutRef: string;
      stateQueueBlockOutRef: string;
    }>
  | Readonly<{ stage: "step_02"; threadOutRef: string }>
  | Readonly<{ stage: "step_03"; threadOutRef: string }>
  | Readonly<{ stage: "scan"; threadOutRef: string }>
  | Readonly<{ stage: "finalize"; threadOutRef: string }>
  | Readonly<{
      stage: "remove";
      nextRemovalOutRef: string;
      fraudProofOutRef: string;
    }>;

export type ExecutionSourceScriptDecodingCapturedAction = Readonly<{
  transaction: LocallyEvaluatedTransaction;
  mutationLease?: Awaited<
    ReturnType<StateQueueMutationLeaseCoordinator["acquire"]>
  >;
}>;

export type BoundExecutionSourceScriptDecodingActuatorConfig = Readonly<{
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
  contracts: ExecutionSourceScriptDecodingContracts;
  references: ExecutionSourceScriptDecodingWorkflowReferences;
  stateQueueMutationLeaseCoordinator: StateQueueMutationLeaseCoordinator;
}>;

const captured = async (
  submit: Parameters<typeof captureLocallyEvaluatedTransaction>[0],
): Promise<ExecutionSourceScriptDecodingCapturedAction> =>
  Object.freeze({
    transaction: await captureLocallyEvaluatedTransaction(submit),
  });

/** Package-owned locally evaluated transaction actuator. */
export const createExecutionSourceScriptDecodingActuator = (
  config: BoundExecutionSourceScriptDecodingActuatorConfig,
) =>
  Object.freeze({
    capture: async ({
      action,
      artifact,
    }: {
      action: ExecutionSourceScriptDecodingActuatorAction;
      artifact: ExecutionSourceScriptDecodingArtifact;
    }): Promise<ExecutionSourceScriptDecodingCapturedAction> => {
      if (artifact.headerHash !== config.binding.definition.headerHash)
        throw new Error(
          "executionSourceScriptDecoding artifact changed bound header",
        );
      const categoryId = config.binding.resolvedContracts.category.categoryId;
      if (action.stage === "init")
        return await captured(async (preSubmitBoundary) => {
          await submitInit({
            lucid: config.lucid,
            blueprint: config.binding.blueprint,
            deploymentInfo: config.binding.deploymentInfo,
            network: config.binding.network,
            signer: config.signer,
            fraudCategory: "executionSourceScriptDecoding" as never,
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
            await submitExecutionSourceScriptDecodingStep01Accepted({
              ...common,
              blueprint: config.binding.blueprint,
              network: config.binding.network,
              stateQueueBlockOutRef: action.stateQueueBlockOutRef,
              txInclusion: artifact.acceptedInclusion,
              witnessReferenceScripts: config.references.witnesses,
            });
          else if (artifact.forcedMembership !== undefined)
            await submitExecutionSourceScriptDecodingStep01Forced({
              ...common,
              membership: artifact.forcedMembership,
            });
          else
            throw new Error(
              "executionSourceScriptDecoding artifact omitted exact source",
            );
        });
      const linear = async (
        operation: (
          preSubmitBoundary: Parameters<
            typeof captureLocallyEvaluatedTransaction
          >[0] extends (boundary: infer T) => unknown
            ? T
            : never,
        ) => Promise<unknown>,
      ) => await captured(operation);
      if (action.stage === "step_02")
        return await linear(async (preSubmitBoundary) => {
          await submitExecutionSourceScriptDecodingStep02({
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
        return await linear(async (preSubmitBoundary) => {
          await submitExecutionSourceScriptDecodingStep03({
            lucid: config.lucid,
            contracts: config.contracts,
            categoryId,
            signer: config.signer,
            threadOutRef: action.threadOutRef,
            evidence: artifact.evidence,
            referenceScriptUtxo: config.references.steps[2],
            preSubmitBoundary,
            awaitConfirmation: false,
          });
        });
      if (action.stage === "scan")
        return await linear(async (preSubmitBoundary) => {
          await submitExecutionSourceScriptDecodingStep04({
            lucid: config.lucid,
            contracts: config.contracts,
            categoryId,
            signer: config.signer,
            threadOutRef: action.threadOutRef,
            evidence: artifact.evidence,
            referenceScriptUtxo: config.references.steps[3],
            preSubmitBoundary,
            awaitConfirmation: false,
          });
        });
      if (action.stage === "finalize")
        return await linear(async (preSubmitBoundary) => {
          await submitExecutionSourceScriptDecodingStep05({
            lucid: config.lucid,
            contracts: config.contracts,
            categoryId,
            signer: config.signer,
            threadOutRef: action.threadOutRef,
            evidence: artifact.evidence,
            referenceScriptUtxo: config.references.steps[4],
            witnessReferenceScripts: config.references.witnesses,
            preSubmitBoundary,
            awaitConfirmation: false,
          });
        });
      return await captureCursorRemoval({
        category: {
          name: "executionSourceScriptDecoding",
          categoryId,
          firstStepDeploymentEntry: "fraudProofExecutionSourceScriptDecoding",
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
          category: "executionSourceScriptDecoding" as never,
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
