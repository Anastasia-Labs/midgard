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
import type { ScriptIntegrityHashMismatchContractsV1 } from "./contracts-v1.js";
import type { ScriptIntegrityHashMismatchEvidenceV1 } from "./family-v1.js";
import type { ScriptIntegrityStageThreeAuthenticationV1 } from "./retained-stage-three-v1.js";
import {
  submitScriptIntegrityHashMismatchCancelV1,
  submitScriptIntegrityHashMismatchStep01AcceptedV1,
  submitScriptIntegrityHashMismatchStep01ForcedV1,
  submitScriptIntegrityHashMismatchStep02V1,
  submitScriptIntegrityHashMismatchStep03V1,
  submitScriptIntegrityHashMismatchStep04V1,
  submitScriptIntegrityHashMismatchStep05V1,
} from "./submit-v1.js";

export type ScriptIntegrityHashMismatchProductionArtifactV1 = Readonly<{
  headerHash: string;
  header: HeaderV1;
  evidence: ScriptIntegrityHashMismatchEvidenceV1;
  authentication: ScriptIntegrityStageThreeAuthenticationV1;
  acceptedInclusion?: SubmitStep01TxInclusion;
  forcedMembership?: RootMembershipProof<OutputReference, ForcedInclusionTxV1>;
}>;
export type ScriptIntegrityHashMismatchWorkflowReferencesV1 = Readonly<{
  steps: readonly [UTxO, UTxO, UTxO, UTxO, UTxO];
  witnesses: Required<FaultProofWitnessReferenceScriptsV1>;
}>;
export type ScriptIntegrityHashMismatchLucidActionV1 =
  | Readonly<{ stage: "init"; stateQueueBlockOutRef: string }>
  | Readonly<{
      stage: "step_01";
      threadOutRef: string;
      stateQueueBlockOutRef: string;
    }>
  | Readonly<{
      stage: "step_02" | "step_03" | "step_04" | "step_05";
      threadOutRef: string;
    }>
  | Readonly<{
      stage: "cancel";
      threadOutRef: string;
      stepIndex: 0 | 1 | 2 | 3 | 4;
    }>
  | Readonly<{
      stage: "remove";
      nextRemovalOutRef: string;
      fraudProofOutRef: string;
    }>;
export type ScriptIntegrityHashMismatchCapturedLucidActionV1 = Readonly<{
  transaction: LocallyEvaluatedTransactionV1;
  mutationLease?: Awaited<
    ReturnType<StateQueueMutationLeaseCoordinator["acquire"]>
  >;
}>;
export type BoundScriptIntegrityHashMismatchLucidActuatorConfigV1 = Readonly<{
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
  contracts: ScriptIntegrityHashMismatchContractsV1;
  references: ScriptIntegrityHashMismatchWorkflowReferencesV1;
  stateQueueMutationLeaseCoordinator: StateQueueMutationLeaseCoordinator;
}>;

const captured = async (
  submit: Parameters<typeof captureLocallyEvaluatedTransactionV1>[0],
): Promise<ScriptIntegrityHashMismatchCapturedLucidActionV1> =>
  Object.freeze({
    transaction: await captureLocallyEvaluatedTransactionV1(submit),
  });

/** Concrete callback-free Lucid actuator for every physical family transition. */
export const createScriptIntegrityHashMismatchLucidActuatorV1 = (
  config: BoundScriptIntegrityHashMismatchLucidActuatorConfigV1,
) =>
  Object.freeze({
    capture: async ({
      action,
      artifact,
    }: {
      action: ScriptIntegrityHashMismatchLucidActionV1;
      artifact: ScriptIntegrityHashMismatchProductionArtifactV1;
    }): Promise<ScriptIntegrityHashMismatchCapturedLucidActionV1> => {
      if (artifact.headerHash !== config.binding.definition.headerHash)
        throw new Error(
          "scriptIntegrityHashMismatch artifact changed bound header",
        );
      const common = {
        lucid: config.lucid,
        contracts: config.contracts,
        categoryId: config.binding.resolvedContracts.category.categoryId,
        signer: config.signer,
        evidence: artifact.evidence,
        awaitConfirmation: false,
      } as const;
      if (action.stage === "init")
        return await captured(async (preSubmitBoundary) => {
          await submitInit({
            lucid: config.lucid,
            blueprint: config.binding.blueprint,
            deploymentInfo: config.binding.deploymentInfo,
            network: config.binding.network,
            signer: config.signer,
            fraudCategory: "scriptIntegrityHashMismatch" as never,
            fraudulentBlockOutRef: action.stateQueueBlockOutRef,
            fraudulentHeaderHash: artifact.headerHash,
            witnessReferenceScripts: config.references.witnesses,
            preSubmitBoundary,
            awaitConfirmation: false,
          });
        });
      if (action.stage === "step_01")
        return await captured(async (preSubmitBoundary) => {
          const args = {
            ...common,
            threadOutRef: action.threadOutRef,
            header: artifact.header,
            referenceScriptUtxo: config.references.steps[0],
            preSubmitBoundary,
          } as const;
          if (artifact.acceptedInclusion !== undefined)
            await submitScriptIntegrityHashMismatchStep01AcceptedV1({
              ...args,
              blueprint: config.binding.blueprint,
              network: config.binding.network,
              stateQueueBlockOutRef: action.stateQueueBlockOutRef,
              txInclusion: artifact.acceptedInclusion,
              witnessReferenceScripts: config.references.witnesses,
            });
          else if (artifact.forcedMembership !== undefined)
            await submitScriptIntegrityHashMismatchStep01ForcedV1({
              ...args,
              membership: artifact.forcedMembership,
            });
          else
            throw new Error(
              "scriptIntegrityHashMismatch artifact omitted exact source",
            );
        });
      if (action.stage === "cancel")
        return await captured(async (preSubmitBoundary) => {
          await submitScriptIntegrityHashMismatchCancelV1({
            ...common,
            threadOutRef: action.threadOutRef,
            referenceScriptUtxo: config.references.steps[action.stepIndex],
            witnessReferenceScripts: config.references.witnesses,
            preSubmitBoundary,
          });
        });
      if (action.stage === "step_02")
        return await captured(async (preSubmitBoundary) => {
          await submitScriptIntegrityHashMismatchStep02V1({
            ...common,
            threadOutRef: action.threadOutRef,
            authentication: artifact.authentication,
            referenceScriptUtxo: config.references.steps[1],
            preSubmitBoundary,
          });
        });
      if (action.stage === "step_03")
        return await captured(async (preSubmitBoundary) => {
          await submitScriptIntegrityHashMismatchStep03V1({
            ...common,
            threadOutRef: action.threadOutRef,
            referenceScriptUtxo: config.references.steps[2],
            preSubmitBoundary,
          });
        });
      if (action.stage === "step_04")
        return await captured(async (preSubmitBoundary) => {
          await submitScriptIntegrityHashMismatchStep04V1({
            ...common,
            threadOutRef: action.threadOutRef,
            referenceScriptUtxo: config.references.steps[3],
            preSubmitBoundary,
          });
        });
      if (action.stage === "step_05")
        return await captured(async (preSubmitBoundary) => {
          await submitScriptIntegrityHashMismatchStep05V1({
            ...common,
            threadOutRef: action.threadOutRef,
            referenceScriptUtxo: config.references.steps[4],
            witnessReferenceScripts: config.references.witnesses,
            preSubmitBoundary,
          });
        });
      if (action.stage !== "remove")
        throw new Error("scriptIntegrityHashMismatch actuator stage changed");
      return await captureProductionCursorRemovalV1({
        category: "scriptIntegrityHashMismatch",
        lucid: config.lucid,
        blueprint: config.binding.blueprint,
        deploymentInfo: config.binding.deploymentInfo,
        network: config.binding.network,
        signer: config.signer,
        headerHash: artifact.headerHash,
        input: {
          schemaVersion: "midgard-production-cursor-family-action-v1",
          category: "scriptIntegrityHashMismatch",
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
