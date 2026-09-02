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
import type { UnusedScriptWitnessContractsV1 } from "./contracts-v1.js";
import type { UnusedScriptWitnessEvidenceV1 } from "./family-v1.js";
import { submitUnusedScriptWitnessStep01AcceptedV1 } from "./submit-step-01-v1.js";
import { submitUnusedScriptWitnessStep01ForcedV1 } from "./submit-step-01-v1.js";
import type { UnusedScriptWitnessAuthenticationV1 } from "./submit-step-02-v1.js";
import { submitUnusedScriptWitnessStep02V1 } from "./submit-step-02-v1.js";
import { submitUnusedScriptWitnessStep03V1 } from "./submit-step-03-v1.js";
import { submitUnusedScriptWitnessStep04V1 } from "./submit-step-04-v1.js";
import { submitUnusedScriptWitnessStep05V1 } from "./submit-step-05-v1.js";
import { submitUnusedScriptWitnessStep06V1 } from "./submit-step-06-v1.js";

export type UnusedScriptWitnessProductionArtifactV1 = Readonly<{
  headerHash: string;
  header: HeaderV1;
  evidence: UnusedScriptWitnessEvidenceV1;
  authentication: UnusedScriptWitnessAuthenticationV1;
  acceptedInclusion?: SubmitStep01TxInclusion;
  forcedMembership?: RootMembershipProof<OutputReference, ForcedInclusionTxV1>;
}>;

export type UnusedScriptWitnessWorkflowReferencesV1 = Readonly<{
  steps: readonly [UTxO, UTxO, UTxO, UTxO, UTxO, UTxO];
  witnesses: Required<FaultProofWitnessReferenceScriptsV1>;
}>;

export type UnusedScriptWitnessActuatorActionV1 =
  | Readonly<{ stage: "init"; stateQueueBlockOutRef: string }>
  | Readonly<{
      stage: "step_01";
      threadOutRef: string;
      stateQueueBlockOutRef: string;
    }>
  | Readonly<{ stage: "step_02"; threadOutRef: string }>
  | Readonly<{ stage: "step_03"; threadOutRef: string }>
  | Readonly<{ stage: "step_04"; threadOutRef: string }>
  | Readonly<{ stage: "step_05"; threadOutRef: string }>
  | Readonly<{ stage: "step_06"; threadOutRef: string }>
  | Readonly<{
      stage: "remove";
      nextRemovalOutRef: string;
      fraudProofOutRef: string;
    }>;

export type UnusedScriptWitnessCapturedActionV1 = Readonly<{
  transaction: LocallyEvaluatedTransactionV1;
  mutationLease?: Awaited<
    ReturnType<StateQueueMutationLeaseCoordinator["acquire"]>
  >;
}>;

export type BoundUnusedScriptWitnessActuatorConfigV1 = Readonly<{
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
  contracts: UnusedScriptWitnessContractsV1;
  references: UnusedScriptWitnessWorkflowReferencesV1;
  stateQueueMutationLeaseCoordinator: StateQueueMutationLeaseCoordinator;
}>;

const captured = async (
  submit: Parameters<typeof captureLocallyEvaluatedTransactionV1>[0],
): Promise<UnusedScriptWitnessCapturedActionV1> =>
  Object.freeze({
    transaction: await captureLocallyEvaluatedTransactionV1(submit),
  });

/** Package-owned locally evaluated transaction actuator. */
export const createUnusedScriptWitnessActuatorV1 = (
  config: BoundUnusedScriptWitnessActuatorConfigV1,
) =>
  Object.freeze({
    capture: async ({
      action,
      artifact,
    }: {
      action: UnusedScriptWitnessActuatorActionV1;
      artifact: UnusedScriptWitnessProductionArtifactV1;
    }): Promise<UnusedScriptWitnessCapturedActionV1> => {
      if (artifact.headerHash !== config.binding.definition.headerHash)
        throw new Error("unusedScriptWitness artifact changed bound header");
      const categoryId = config.binding.resolvedContracts.category.categoryId;
      if (action.stage === "init")
        return await captured(async (preSubmitBoundary) => {
          await submitInit({
            lucid: config.lucid,
            blueprint: config.binding.blueprint,
            deploymentInfo: config.binding.deploymentInfo,
            network: config.binding.network,
            signer: config.signer,
            fraudCategory: "unusedScriptWitness" as never,
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
            scriptIndex: BigInt(artifact.evidence.finding.scriptIndex),
            referenceScriptUtxo: config.references.steps[0],
            preSubmitBoundary,
            awaitConfirmation: false,
          } as const;
          if (artifact.acceptedInclusion !== undefined)
            await submitUnusedScriptWitnessStep01AcceptedV1({
              ...common,
              blueprint: config.binding.blueprint,
              network: config.binding.network,
              stateQueueBlockOutRef: action.stateQueueBlockOutRef,
              txInclusion: artifact.acceptedInclusion,
              witnessReferenceScripts: config.references.witnesses,
            });
          else if (artifact.forcedMembership !== undefined)
            await submitUnusedScriptWitnessStep01ForcedV1({
              ...common,
              membership: artifact.forcedMembership,
            });
          else
            throw new Error(
              "unusedScriptWitness artifact omitted exact source",
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
          await submitUnusedScriptWitnessStep02V1({
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
          await submitUnusedScriptWitnessStep03V1({
            lucid: config.lucid,
            contracts: config.contracts,
            categoryId,
            signer: config.signer,
            threadOutRef: action.threadOutRef,
            referenceScriptUtxo: config.references.steps[2],
            preSubmitBoundary,
            awaitConfirmation: false,
          });
        });
      if (action.stage === "step_04")
        return await linear(async (preSubmitBoundary) => {
          await submitUnusedScriptWitnessStep04V1({
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
      if (action.stage === "step_05")
        return await linear(async (preSubmitBoundary) => {
          await submitUnusedScriptWitnessStep05V1({
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
      if (action.stage === "step_06")
        return await linear(async (preSubmitBoundary) => {
          await submitUnusedScriptWitnessStep06V1({
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
          name: "unusedScriptWitness",
          categoryId,
          firstStepDeploymentEntry: "fraudProofUnusedScriptWitness",
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
          category: "unusedScriptWitness" as never,
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
