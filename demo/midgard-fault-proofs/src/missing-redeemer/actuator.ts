import type { LucidEvolution, Network, UTxO } from "@lucid-evolution/lucid";

import type { StateQueueMutationLeaseCoordinator } from "../remove-fraudulent-block.js";
import type { ResolvedProverSigner } from "../runtime.js";
import { submitInit } from "../submit-init.js";
import type { FaultProofWitnessReferenceScripts } from "../witness-reference-scripts.js";
import {
  captureCursorRemoval,
  type CursorFamilyActionInput,
} from "../workflow/cursor-family-runtime.js";
import {
  captureLocallyEvaluatedTransaction,
  type LocallyEvaluatedTransaction,
} from "../workflow/transaction-boundary.js";
import type { MissingRedeemerContracts } from "./contracts.js";
import { MISSING_REDEEMER_CATEGORY } from "./family.js";
import {
  admitMissingRedeemerArtifact,
  type MissingRedeemerArtifact,
} from "./replay.js";
import { planMissingRedeemerStagedWalk } from "./staged-plan.js";
import {
  submitMissingRedeemerStep02,
  submitMissingRedeemerStep02a,
  submitMissingRedeemerStep02b,
} from "./submit-authentication.js";
import { submitMissingRedeemerCancel } from "./submit-cancel.js";
import {
  type MissingRedeemerStep03Action,
  submitMissingRedeemerStep03,
  submitMissingRedeemerStep04,
} from "./submit-field-scan.js";
import {
  submitMissingRedeemerStep01Accepted,
  submitMissingRedeemerStep01Forced,
} from "./submit-step-01.js";
import { submitMissingRedeemerStep05 } from "./submit-step-05.js";

export type MissingRedeemerWorkflowReferences = Readonly<{
  steps: readonly [UTxO, UTxO, UTxO, UTxO, UTxO, UTxO, UTxO];
  witnesses: Required<FaultProofWitnessReferenceScripts>;
}>;
export type BoundMissingRedeemerActuatorConfig = Readonly<{
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
  contracts: MissingRedeemerContracts;
  references: MissingRedeemerWorkflowReferences;
  stateQueueMutationLeaseCoordinator: StateQueueMutationLeaseCoordinator;
}>;
export type MissingRedeemerActuatorAction =
  | { readonly stage: "init"; readonly stateQueueBlockOutRef: string }
  | {
      readonly stage: "step_01";
      readonly threadOutRef: string;
      readonly stateQueueBlockOutRef: string;
    }
  | {
      readonly stage: "step_02" | "step_02a" | "step_02b" | "scan" | "finalize";
      readonly threadOutRef: string;
    }
  | {
      readonly stage: "field";
      readonly threadOutRef: string;
      readonly action: MissingRedeemerStep03Action;
    }
  | {
      readonly stage: "cancel";
      readonly threadOutRef: string;
      readonly stepIndex: number;
    }
  | {
      readonly stage: "remove";
      readonly nextRemovalOutRef: string;
      readonly fraudProofOutRef: string;
    };
export type MissingRedeemerCapturedAction = Readonly<{
  transaction: LocallyEvaluatedTransaction;
  mutationLease?: Awaited<
    ReturnType<StateQueueMutationLeaseCoordinator["acquire"]>
  >;
}>;
const capture = async (
  submit: Parameters<typeof captureLocallyEvaluatedTransaction>[0],
): Promise<MissingRedeemerCapturedAction> =>
  Object.freeze({
    transaction: await captureLocallyEvaluatedTransaction(submit),
  });

/** Concrete seven-role actuator; configuration contains infrastructure only. */
export const createMissingRedeemerActuator = (
  config: BoundMissingRedeemerActuatorConfig,
) =>
  Object.freeze({
    capture: async ({
      action,
      artifact,
    }: {
      action: MissingRedeemerActuatorAction;
      artifact: MissingRedeemerArtifact;
    }): Promise<MissingRedeemerCapturedAction> => {
      artifact = admitMissingRedeemerArtifact(artifact);
      if (artifact.headerHash !== config.binding.definition.headerHash)
        throw new Error("missingRedeemer artifact changed bound header");
      const categoryId = config.binding.resolvedContracts.category.categoryId;
      if (action.stage === "init")
        return await capture(async (preSubmitBoundary) => {
          await submitInit({
            lucid: config.lucid,
            blueprint: config.binding.blueprint,
            deploymentInfo: config.binding.deploymentInfo,
            network: config.binding.network,
            signer: config.signer,
            fraudCategory: MISSING_REDEEMER_CATEGORY as never,
            fraudulentBlockOutRef: action.stateQueueBlockOutRef,
            fraudulentHeaderHash: artifact.headerHash,
            witnessReferenceScripts: config.references.witnesses,
            preSubmitBoundary,
            awaitConfirmation: false,
          });
        });
      if (action.stage === "step_01")
        return await capture(async (preSubmitBoundary) => {
          const common = {
            lucid: config.lucid,
            contracts: config.contracts,
            categoryId,
            signer: config.signer,
            threadOutRef: action.threadOutRef,
            header: artifact.header,
            purposeKind: artifact.evidence.purposeKind,
            purposeIndex: artifact.evidence.purposeIndex,
            referenceScriptUtxo: config.references.steps[0],
            preSubmitBoundary,
            awaitConfirmation: false,
          } as const;
          if (artifact.acceptedInclusion !== undefined)
            await submitMissingRedeemerStep01Accepted({
              ...common,
              blueprint: config.binding.blueprint,
              network: config.binding.network,
              stateQueueBlockOutRef: action.stateQueueBlockOutRef,
              txInclusion: artifact.acceptedInclusion,
              witnessReferenceScripts: config.references.witnesses,
            });
          else if (artifact.forcedMembership !== undefined)
            await submitMissingRedeemerStep01Forced({
              ...common,
              membership: artifact.forcedMembership,
            });
          else throw new Error("missingRedeemer artifact omitted exact source");
        });
      const common =
        "threadOutRef" in action
          ? ({
              lucid: config.lucid,
              contracts: config.contracts,
              categoryId,
              signer: config.signer,
              threadOutRef: action.threadOutRef,
              evidence: artifact.evidence,
              authentication: artifact.authentication,
            } as const)
          : null;
      if (
        action.stage === "step_02" ||
        action.stage === "step_02a" ||
        action.stage === "step_02b"
      )
        return await capture(async (preSubmitBoundary) => {
          const index =
            action.stage === "step_02"
              ? 1
              : action.stage === "step_02a"
                ? 2
                : 3;
          const args = {
            ...common!,
            referenceScriptUtxo: config.references.steps[index],
            preSubmitBoundary,
            awaitConfirmation: false,
          };
          if (action.stage === "step_02")
            await submitMissingRedeemerStep02(args);
          else if (action.stage === "step_02a")
            await submitMissingRedeemerStep02a(args);
          else await submitMissingRedeemerStep02b(args);
        });
      const staged = planMissingRedeemerStagedWalk({
        transactionId: artifact.evidence.subject.transaction_id,
        fieldPreimageCbor: artifact.evidence.fieldPreimageHex,
      });
      if (action.stage === "field")
        return await capture(async (preSubmitBoundary) => {
          await submitMissingRedeemerStep03({
            ...common!,
            nativeTxCompactCbor: artifact.nativeTxCompactCbor,
            staged,
            action: action.action,
            referenceScriptUtxo: config.references.steps[4],
            preSubmitBoundary,
            awaitConfirmation: false,
          });
        });
      if (action.stage === "scan")
        return await capture(async (preSubmitBoundary) => {
          await submitMissingRedeemerStep04({
            ...common!,
            nativeTxCompactCbor: artifact.nativeTxCompactCbor,
            staged,
            referenceScriptUtxo: config.references.steps[5],
            preSubmitBoundary,
            awaitConfirmation: false,
          });
        });
      if (action.stage === "finalize")
        return await capture(async (preSubmitBoundary) => {
          await submitMissingRedeemerStep05({
            ...common!,
            referenceScriptUtxo: config.references.steps[6],
            witnessReferenceScripts: config.references.witnesses,
            preSubmitBoundary,
            awaitConfirmation: false,
          });
        });
      if (action.stage === "cancel") {
        if (
          !Number.isSafeInteger(action.stepIndex) ||
          action.stepIndex < 0 ||
          action.stepIndex >= 7
        )
          throw new Error("missingRedeemer cancellation step is invalid");
        return await capture(async (preSubmitBoundary) => {
          await submitMissingRedeemerCancel({
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
      if (action.stage !== "remove")
        throw new Error("missingRedeemer actuator action is unsupported");
      return await captureCursorRemoval({
        category: {
          name: MISSING_REDEEMER_CATEGORY,
          categoryId,
          firstStepDeploymentEntry: "fraudProofMissingRedeemer",
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
          category: MISSING_REDEEMER_CATEGORY,
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
