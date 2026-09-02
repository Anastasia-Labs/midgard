import type { LucidEvolution, Network, UTxO } from "@lucid-evolution/lucid";

import type { StateQueueMutationLeaseCoordinator } from "../remove-fraudulent-block.js";
import type { ResolvedProverSigner } from "../runtime.js";
import { submitInit } from "../submit-init.js";
import type { FaultProofWitnessReferenceScriptsV1 } from "../witness-reference-scripts-v1.js";
import {
  captureProductionCursorRemovalV1,
  type ProductionCursorFamilyActionInputV1,
} from "../workflow/production-cursor-family-runtime-v1.js";
import {
  captureLocallyEvaluatedTransactionV1,
  type LocallyEvaluatedTransactionV1,
} from "../workflow/transaction-boundary-v1.js";
import type { MissingRedeemerContractsV1 } from "./contracts-v1.js";
import { MISSING_REDEEMER_CATEGORY_V1 } from "./family-v1.js";
import {
  admitMissingRedeemerProductionArtifactV1,
  type MissingRedeemerProductionArtifactV1,
} from "./production-replay-v1.js";
import { planMissingRedeemerStagedWalkV1 } from "./staged-plan-v1.js";
import {
  submitMissingRedeemerStep02aV1,
  submitMissingRedeemerStep02bV1,
  submitMissingRedeemerStep02V1,
} from "./submit-authentication-v1.js";
import { submitMissingRedeemerCancelV1 } from "./submit-cancel-v1.js";
import {
  type MissingRedeemerStep03ActionV1,
  submitMissingRedeemerStep03V1,
  submitMissingRedeemerStep04V1,
} from "./submit-field-scan-v1.js";
import {
  submitMissingRedeemerStep01AcceptedV1,
  submitMissingRedeemerStep01ForcedV1,
} from "./submit-step-01-v1.js";
import { submitMissingRedeemerStep05V1 } from "./submit-step-05-v1.js";

export type MissingRedeemerWorkflowReferencesV1 = Readonly<{
  steps: readonly [UTxO, UTxO, UTxO, UTxO, UTxO, UTxO, UTxO];
  witnesses: Required<FaultProofWitnessReferenceScriptsV1>;
}>;
export type BoundMissingRedeemerActuatorConfigV1 = Readonly<{
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
  contracts: MissingRedeemerContractsV1;
  references: MissingRedeemerWorkflowReferencesV1;
  stateQueueMutationLeaseCoordinator: StateQueueMutationLeaseCoordinator;
}>;
export type MissingRedeemerActuatorActionV1 =
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
      readonly action: MissingRedeemerStep03ActionV1;
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
export type MissingRedeemerCapturedActionV1 = Readonly<{
  transaction: LocallyEvaluatedTransactionV1;
  mutationLease?: Awaited<
    ReturnType<StateQueueMutationLeaseCoordinator["acquire"]>
  >;
}>;
const capture = async (
  submit: Parameters<typeof captureLocallyEvaluatedTransactionV1>[0],
): Promise<MissingRedeemerCapturedActionV1> =>
  Object.freeze({
    transaction: await captureLocallyEvaluatedTransactionV1(submit),
  });

/** Concrete seven-role actuator; configuration contains infrastructure only. */
export const createMissingRedeemerActuatorV1 = (
  config: BoundMissingRedeemerActuatorConfigV1,
) =>
  Object.freeze({
    capture: async ({
      action,
      artifact,
    }: {
      action: MissingRedeemerActuatorActionV1;
      artifact: MissingRedeemerProductionArtifactV1;
    }): Promise<MissingRedeemerCapturedActionV1> => {
      artifact = admitMissingRedeemerProductionArtifactV1(artifact);
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
            fraudCategory: MISSING_REDEEMER_CATEGORY_V1 as never,
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
            await submitMissingRedeemerStep01AcceptedV1({
              ...common,
              blueprint: config.binding.blueprint,
              network: config.binding.network,
              stateQueueBlockOutRef: action.stateQueueBlockOutRef,
              txInclusion: artifact.acceptedInclusion,
              witnessReferenceScripts: config.references.witnesses,
            });
          else if (artifact.forcedMembership !== undefined)
            await submitMissingRedeemerStep01ForcedV1({
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
            await submitMissingRedeemerStep02V1(args);
          else if (action.stage === "step_02a")
            await submitMissingRedeemerStep02aV1(args);
          else await submitMissingRedeemerStep02bV1(args);
        });
      const staged = planMissingRedeemerStagedWalkV1({
        transactionId: artifact.evidence.subject.transaction_id,
        fieldPreimageCbor: artifact.evidence.fieldPreimageHex,
      });
      if (action.stage === "field")
        return await capture(async (preSubmitBoundary) => {
          await submitMissingRedeemerStep03V1({
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
          await submitMissingRedeemerStep04V1({
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
          await submitMissingRedeemerStep05V1({
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
          await submitMissingRedeemerCancelV1({
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
      return await captureProductionCursorRemovalV1({
        category: {
          name: MISSING_REDEEMER_CATEGORY_V1,
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
          category: MISSING_REDEEMER_CATEGORY_V1,
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
