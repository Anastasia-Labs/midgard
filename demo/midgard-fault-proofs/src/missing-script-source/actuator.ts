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
import type { MissingScriptSourceContracts } from "./contracts.js";
import type { MissingScriptSourceEvidence } from "./family.js";
import { submitMissingScriptSourceStep01Accepted } from "./submit-step-01.js";
import { submitMissingScriptSourceStep01Forced } from "./submit-step-01.js";
import type { ExecutionSourceAuthenticationData } from "./submit-step-02.js";
import { submitMissingScriptSourceStep02 } from "./submit-step-02.js";
import { submitMissingScriptSourceStep03 } from "./submit-step-03.js";
import { submitMissingScriptSourceStep04 } from "./submit-step-04.js";
import { submitMissingScriptSourceStep05 } from "./submit-step-05.js";
import { submitMissingScriptSourceStep06 } from "./submit-step-06.js";

export type MissingScriptSourceArtifact = Readonly<{
  headerHash: string;
  header: Header;
  evidence: MissingScriptSourceEvidence;
  authentication: ExecutionSourceAuthenticationData;
  acceptedInclusion?: SubmitStep01TxInclusion;
  forcedMembership?: RootMembershipProof<OutputReference, ForcedInclusionTxV1>;
}>;

export type MissingScriptSourceWorkflowReferences = Readonly<{
  steps: readonly [UTxO, UTxO, UTxO, UTxO, UTxO, UTxO];
  witnesses: Required<FaultProofWitnessReferenceScripts>;
}>;

export type MissingScriptSourceActuatorAction =
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

export type MissingScriptSourceCapturedAction = Readonly<{
  transaction: LocallyEvaluatedTransaction;
  mutationLease?: Awaited<
    ReturnType<StateQueueMutationLeaseCoordinator["acquire"]>
  >;
}>;

export type BoundMissingScriptSourceActuatorConfig = Readonly<{
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
  contracts: MissingScriptSourceContracts;
  references: MissingScriptSourceWorkflowReferences;
  stateQueueMutationLeaseCoordinator: StateQueueMutationLeaseCoordinator;
}>;

const captured = async (
  submit: Parameters<typeof captureLocallyEvaluatedTransaction>[0],
): Promise<MissingScriptSourceCapturedAction> =>
  Object.freeze({
    transaction: await captureLocallyEvaluatedTransaction(submit),
  });

/** Package-owned locally evaluated transaction actuator. */
export const createMissingScriptSourceActuator = (
  config: BoundMissingScriptSourceActuatorConfig,
) =>
  Object.freeze({
    capture: async ({
      action,
      artifact,
    }: {
      action: MissingScriptSourceActuatorAction;
      artifact: MissingScriptSourceArtifact;
    }): Promise<MissingScriptSourceCapturedAction> => {
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
            await submitMissingScriptSourceStep01Accepted({
              ...common,
              blueprint: config.binding.blueprint,
              network: config.binding.network,
              stateQueueBlockOutRef: action.stateQueueBlockOutRef,
              txInclusion: artifact.acceptedInclusion,
              witnessReferenceScripts: config.references.witnesses,
            });
          else if (artifact.forcedMembership !== undefined)
            await submitMissingScriptSourceStep01Forced({
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
            typeof captureLocallyEvaluatedTransaction
          >[0] extends (boundary: infer T) => unknown
            ? T
            : never,
        ) => Promise<unknown>,
      ) => await captured(operation);
      if (action.stage === "step_02")
        return await linear(async (preSubmitBoundary) => {
          await submitMissingScriptSourceStep02({
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
          await submitMissingScriptSourceStep03({
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
          await submitMissingScriptSourceStep04({
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
          await submitMissingScriptSourceStep05({
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
          await submitMissingScriptSourceStep06({
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
      return await captureCursorRemoval({
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
        } as CursorFamilyActionInput,
        stateQueueMutationLeaseCoordinator:
          config.stateQueueMutationLeaseCoordinator,
        fraudProverRewardLovelace: BigInt(
          config.binding.releaseEconomics.policy.fraudProverRewardLovelace,
        ),
      });
    },
  });
