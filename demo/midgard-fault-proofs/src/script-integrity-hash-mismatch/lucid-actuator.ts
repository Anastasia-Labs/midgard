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
import type { ScriptIntegrityHashMismatchContracts } from "./contracts.js";
import type { ScriptIntegrityHashMismatchEvidence } from "./family.js";
import type { ScriptIntegrityStageThreeAuthentication } from "./retained-stage-three.js";
import {
  submitScriptIntegrityHashMismatchCancel,
  submitScriptIntegrityHashMismatchStep01Accepted,
  submitScriptIntegrityHashMismatchStep01Forced,
  submitScriptIntegrityHashMismatchStep02,
  submitScriptIntegrityHashMismatchStep03,
  submitScriptIntegrityHashMismatchStep04,
  submitScriptIntegrityHashMismatchStep05,
} from "./submit.js";

export type ScriptIntegrityHashMismatchArtifact = Readonly<{
  headerHash: string;
  header: Header;
  evidence: ScriptIntegrityHashMismatchEvidence;
  authentication: ScriptIntegrityStageThreeAuthentication;
  acceptedInclusion?: SubmitStep01TxInclusion;
  forcedMembership?: RootMembershipProof<OutputReference, ForcedInclusionTxV1>;
}>;
export type ScriptIntegrityHashMismatchWorkflowReferences = Readonly<{
  steps: readonly [UTxO, UTxO, UTxO, UTxO, UTxO];
  witnesses: Required<FaultProofWitnessReferenceScripts>;
}>;
export type ScriptIntegrityHashMismatchLucidAction =
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
export type ScriptIntegrityHashMismatchCapturedLucidAction = Readonly<{
  transaction: LocallyEvaluatedTransaction;
  mutationLease?: Awaited<
    ReturnType<StateQueueMutationLeaseCoordinator["acquire"]>
  >;
}>;
export type BoundScriptIntegrityHashMismatchLucidActuatorConfig = Readonly<{
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
  contracts: ScriptIntegrityHashMismatchContracts;
  references: ScriptIntegrityHashMismatchWorkflowReferences;
  stateQueueMutationLeaseCoordinator: StateQueueMutationLeaseCoordinator;
}>;

const captured = async (
  submit: Parameters<typeof captureLocallyEvaluatedTransaction>[0],
): Promise<ScriptIntegrityHashMismatchCapturedLucidAction> =>
  Object.freeze({
    transaction: await captureLocallyEvaluatedTransaction(submit),
  });

/** Concrete callback-free Lucid actuator for every physical family transition. */
export const createScriptIntegrityHashMismatchLucidActuator = (
  config: BoundScriptIntegrityHashMismatchLucidActuatorConfig,
) =>
  Object.freeze({
    capture: async ({
      action,
      artifact,
    }: {
      action: ScriptIntegrityHashMismatchLucidAction;
      artifact: ScriptIntegrityHashMismatchArtifact;
    }): Promise<ScriptIntegrityHashMismatchCapturedLucidAction> => {
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
            await submitScriptIntegrityHashMismatchStep01Accepted({
              ...args,
              blueprint: config.binding.blueprint,
              network: config.binding.network,
              stateQueueBlockOutRef: action.stateQueueBlockOutRef,
              txInclusion: artifact.acceptedInclusion,
              witnessReferenceScripts: config.references.witnesses,
            });
          else if (artifact.forcedMembership !== undefined)
            await submitScriptIntegrityHashMismatchStep01Forced({
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
          await submitScriptIntegrityHashMismatchCancel({
            ...common,
            threadOutRef: action.threadOutRef,
            referenceScriptUtxo: config.references.steps[action.stepIndex],
            witnessReferenceScripts: config.references.witnesses,
            preSubmitBoundary,
          });
        });
      if (action.stage === "step_02")
        return await captured(async (preSubmitBoundary) => {
          await submitScriptIntegrityHashMismatchStep02({
            ...common,
            threadOutRef: action.threadOutRef,
            authentication: artifact.authentication,
            referenceScriptUtxo: config.references.steps[1],
            preSubmitBoundary,
          });
        });
      if (action.stage === "step_03")
        return await captured(async (preSubmitBoundary) => {
          await submitScriptIntegrityHashMismatchStep03({
            ...common,
            threadOutRef: action.threadOutRef,
            referenceScriptUtxo: config.references.steps[2],
            preSubmitBoundary,
          });
        });
      if (action.stage === "step_04")
        return await captured(async (preSubmitBoundary) => {
          await submitScriptIntegrityHashMismatchStep04({
            ...common,
            threadOutRef: action.threadOutRef,
            referenceScriptUtxo: config.references.steps[3],
            preSubmitBoundary,
          });
        });
      if (action.stage === "step_05")
        return await captured(async (preSubmitBoundary) => {
          await submitScriptIntegrityHashMismatchStep05({
            ...common,
            threadOutRef: action.threadOutRef,
            referenceScriptUtxo: config.references.steps[4],
            witnessReferenceScripts: config.references.witnesses,
            preSubmitBoundary,
          });
        });
      if (action.stage !== "remove")
        throw new Error("scriptIntegrityHashMismatch actuator stage changed");
      return await captureCursorRemoval({
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
        } as CursorFamilyActionInput,
        stateQueueMutationLeaseCoordinator:
          config.stateQueueMutationLeaseCoordinator,
        fraudProverRewardLovelace: BigInt(
          config.binding.releaseEconomics.policy.fraudProverRewardLovelace,
        ),
      });
    },
  });
