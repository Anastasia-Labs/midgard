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
import type { MissingScriptSourceContractsV1 } from "./contracts-v1.js";
import type { MissingScriptSourceEvidenceV1 } from "./family-v1.js";
import { submitMissingScriptSourceStep01AcceptedV1 } from "./submit-step-01-v1.js";
import { submitMissingScriptSourceStep01ForcedV1 } from "./submit-step-01-v1.js";
import type { ExecutionSourceAuthenticationDataV1 } from "./submit-step-02-v1.js";
import { submitMissingScriptSourceStep02V1 } from "./submit-step-02-v1.js";
import { submitMissingScriptSourceStep03V1 } from "./submit-step-03-v1.js";
import { submitMissingScriptSourceStep04V1 } from "./submit-step-04-v1.js";
import { submitMissingScriptSourceStep05V1 } from "./submit-step-05-v1.js";
import { submitMissingScriptSourceStep06V1 } from "./submit-step-06-v1.js";

export type MissingScriptSourceProductionArtifactV1 = Readonly<{
  headerHash: string;
  header: HeaderV1;
  evidence: MissingScriptSourceEvidenceV1;
  authentication: ExecutionSourceAuthenticationDataV1;
  acceptedInclusion?: SubmitStep01TxInclusion;
  forcedMembership?: RootMembershipProof<OutputReference, ForcedInclusionTxV1>;
}>;

export type MissingScriptSourceWorkflowReferencesV1 = Readonly<{
  steps: readonly [UTxO, UTxO, UTxO, UTxO, UTxO, UTxO];
  witnesses: Required<FaultProofWitnessReferenceScriptsV1>;
}>;

export type MissingScriptSourceActuatorActionV1 =
  | Readonly<{ stage: "init"; stateQueueBlockOutRef: string }>
  | Readonly<{
      stage: "step_01";
      threadOutRef: string;
      stateQueueBlockOutRef: string;
    }>
  | Readonly<{ stage: "step_02"; threadOutRef: string }>
  | Readonly<{ stage: "step_03"; threadOutRef: string }>
  | Readonly<{ stage: "scan"; threadOutRef: string }>
  | Readonly<{ stage: "prove"; threadOutRef: string }>
  | Readonly<{ stage: "finalize"; threadOutRef: string }>
  | Readonly<{
      stage: "remove";
      nextRemovalOutRef: string;
      fraudProofOutRef: string;
    }>;

export type MissingScriptSourceCapturedActionV1 = Readonly<{
  transaction: LocallyEvaluatedTransactionV1;
  mutationLease?: Awaited<
    ReturnType<StateQueueMutationLeaseCoordinator["acquire"]>
  >;
}>;

export type BoundMissingScriptSourceActuatorConfigV1 = Readonly<{
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
  contracts: MissingScriptSourceContractsV1;
  references: MissingScriptSourceWorkflowReferencesV1;
  stateQueueMutationLeaseCoordinator: StateQueueMutationLeaseCoordinator;
}>;

const captured = async (
  submit: Parameters<typeof captureLocallyEvaluatedTransactionV1>[0],
): Promise<MissingScriptSourceCapturedActionV1> =>
  Object.freeze({
    transaction: await captureLocallyEvaluatedTransactionV1(submit),
  });

/** Package-owned locally evaluated transaction actuator. */
export const createMissingScriptSourceActuatorV1 = (
  config: BoundMissingScriptSourceActuatorConfigV1,
) =>
  Object.freeze({
    capture: async ({
      action,
      artifact,
    }: {
      action: MissingScriptSourceActuatorActionV1;
      artifact: MissingScriptSourceProductionArtifactV1;
    }): Promise<MissingScriptSourceCapturedActionV1> => {
      if (artifact.headerHash !== config.binding.definition.headerHash)
        throw new Error("missingScriptSource artifact changed bound header");
      const categoryId = config.binding.resolvedContracts.category.categoryId;
      if (action.stage === "init")
        return await captured(async (preSubmitBoundary) => {
          await submitInit({
            lucid: config.lucid,
            blueprint: config.binding.blueprint,
            deploymentInfo: config.binding.deploymentInfo,
            network: config.binding.network,
            signer: config.signer,
            fraudCategory: "missingScriptSource" as never,
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
            purposeKind: artifact.evidence.finding.purposeKind,
            purposeIndex: BigInt(artifact.evidence.finding.purposeIndex),
            referenceScriptUtxo: config.references.steps[0],
            preSubmitBoundary,
            awaitConfirmation: false,
          } as const;
          if (artifact.acceptedInclusion !== undefined)
            await submitMissingScriptSourceStep01AcceptedV1({
              ...common,
              blueprint: config.binding.blueprint,
              network: config.binding.network,
              stateQueueBlockOutRef: action.stateQueueBlockOutRef,
              txInclusion: artifact.acceptedInclusion,
              witnessReferenceScripts: config.references.witnesses,
            });
          else if (artifact.forcedMembership !== undefined)
            await submitMissingScriptSourceStep01ForcedV1({
              ...common,
              membership: artifact.forcedMembership,
            });
          else
            throw new Error(
              "missingScriptSource artifact omitted exact source",
            );
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
          await submitMissingScriptSourceStep02V1({
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
          await submitMissingScriptSourceStep03V1({
            lucid: config.lucid,
            contracts: config.contracts,
            categoryId,
            signer: config.signer,
            threadOutRef: action.threadOutRef,
            evidence: artifact.evidence,
            authentication: artifact.authentication,
            referenceScriptUtxo: config.references.steps[2],
            preSubmitBoundary,
            awaitConfirmation: false,
          });
        });
      if (action.stage === "scan")
        return await linear(async (preSubmitBoundary) => {
          await submitMissingScriptSourceStep04V1({
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
      if (action.stage === "prove")
        return await linear(async (preSubmitBoundary) => {
          await submitMissingScriptSourceStep05V1({
            lucid: config.lucid,
            contracts: config.contracts,
            categoryId,
            signer: config.signer,
            threadOutRef: action.threadOutRef,
            evidence: artifact.evidence,
            referenceScriptUtxo: config.references.steps[4],
            preSubmitBoundary,
            awaitConfirmation: false,
          });
        });
      if (action.stage === "finalize")
        return await linear(async (preSubmitBoundary) => {
          await submitMissingScriptSourceStep06V1({
            lucid: config.lucid,
            contracts: config.contracts,
            categoryId,
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
          name: "missingScriptSource",
          categoryId,
          firstStepDeploymentEntry: "fraudProofMissingScriptSource",
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
          category: "missingScriptSource" as never,
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
