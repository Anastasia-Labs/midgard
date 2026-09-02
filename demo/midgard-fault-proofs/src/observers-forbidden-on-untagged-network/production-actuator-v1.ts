import {
  decodeMidgardFieldPreimageV1,
  decodeMidgardNativeTxCompactV1,
} from "@al-ft/midgard-core";
import { Proof } from "@al-ft/midgard-sdk";
import { Data, type LucidEvolution, type UTxO } from "@lucid-evolution/lucid";

import { planFaultProofFieldOpeningV1 } from "../field-opening-v1.js";
import { requireLinearFaultThreadUtxoV1 } from "../linear-fault-family-v1.js";
import type { StateQueueMutationLeaseCoordinator } from "../remove-fraudulent-block.js";
import type { ResolvedProverSigner } from "../runtime.js";
import { submitInit } from "../submit-init.js";
import { nativeTxFromCoreCompact } from "../submit-step-01.js";
import type { FaultProofWitnessReferenceScriptsV1 } from "../witness-reference-scripts-v1.js";
import type { FraudProofWorkflowDeploymentBindingV1 } from "../workflow/deployment-manifest-binding-v1.js";
import {
  captureProductionCursorRemovalV1,
  type ProductionCursorFamilyActionInputV1,
} from "../workflow/production-cursor-family-runtime-v1.js";
import type { ProductionFieldCarriageRequirementV1 } from "../workflow/production-field-carriage-prerequisite-v1.js";
import {
  captureLocallyEvaluatedTransactionV1,
  type LocallyEvaluatedTransactionV1,
} from "../workflow/transaction-boundary-v1.js";
import type { ObserversForbiddenContractsV1 } from "./contracts-v1.js";
import { OBSERVERS_FORBIDDEN_ON_UNTAGGED_NETWORK_CATEGORY_V1 } from "./family-v1.js";
import {
  admitProductionObserversForbiddenArtifactV1,
  ObserversForbiddenForcedSourcePayloadV1Schema,
} from "./production-artifact-v1.js";
import {
  submitObserversForbiddenStep01AcceptedV1,
  submitObserversForbiddenStep01ForcedV1,
} from "./submit-step-01-v1.js";
import { submitObserversForbiddenStep02V1 } from "./submit-step-02-v1.js";

export type ObserversForbiddenWorkflowReferenceScriptsV1 = Readonly<{
  steps: readonly [UTxO, UTxO];
  witnesses: Required<FaultProofWitnessReferenceScriptsV1>;
  fieldPreimageCertificateMint: UTxO;
}>;

export type BoundObserversForbiddenActuatorConfigV1 = Readonly<{
  // Structurally identical deployment binding; the central category union is
  // intentionally protected until the serial integration pass.
  binding: FraudProofWorkflowDeploymentBindingV1<"observersForbiddenOnUntaggedNetwork">;
  lucid: LucidEvolution;
  signer: ResolvedProverSigner;
  contracts: ObserversForbiddenContractsV1;
  references: ObserversForbiddenWorkflowReferenceScriptsV1;
  stateQueueMutationLeaseCoordinator: StateQueueMutationLeaseCoordinator;
}>;

export type ObserversForbiddenActuatorActionV1 =
  | Readonly<{ stage: "init"; stateQueueBlockOutRef: string }>
  | Readonly<{
      stage: "step_01";
      threadOutRef: string;
      stateQueueBlockOutRef: string;
    }>
  | Readonly<{ stage: "step_02"; threadOutRef: string }>
  | Readonly<{
      stage: "remove";
      nextRemovalOutRef: string;
      fraudProofOutRef: string;
    }>;

export type ObserversForbiddenCapturedActionV1 = Readonly<{
  transaction: LocallyEvaluatedTransactionV1;
  mutationLease?: Awaited<
    ReturnType<StateQueueMutationLeaseCoordinator["acquire"]>
  >;
}>;

export const observersForbiddenFieldRequirementV1 = ({
  action,
  artifact,
  owner,
  certificate,
}: {
  readonly action: ObserversForbiddenActuatorActionV1;
  readonly artifact: unknown;
  readonly owner: string;
  readonly certificate: ProductionFieldCarriageRequirementV1["certificate"];
}): ProductionFieldCarriageRequirementV1 | null => {
  if (action.stage !== "step_02") return null;
  const admitted = admitProductionObserversForbiddenArtifactV1(artifact);
  return {
    planned: planFaultProofFieldOpeningV1({
      fieldIndex: 3,
      anchorTxId: admitted.artifact.transactionId,
      nativeTxCompactCbor: admitted.artifact.nativeTxCompactCbor,
      itemCbors:
        admitted.evidence.observerCount === 0
          ? []
          : decodeMidgardFieldPreimageV1(
              Buffer.from(admitted.artifact.fieldPreimageCbor, "hex"),
            ),
      owner,
      publish: true,
      label: "observersForbidden production field 3",
    }),
    compactCbor: admitted.artifact.nativeTxCompactCbor,
    witnessSetCompactCbor: admitted.artifact.witnessSetCompactCbor,
    certificate,
  };
};

const inclusionFromArtifact = (
  admitted: ReturnType<typeof admitProductionObserversForbiddenArtifactV1>,
) => {
  const artifact = admitted.artifact;
  return Object.freeze({
    nativeTxId: artifact.transactionId,
    nativeTx: nativeTxFromCoreCompact(
      decodeMidgardNativeTxCompactV1(
        Buffer.from(artifact.nativeTxCompactCbor, "hex"),
      ),
    ),
    nativeTxCompactCbor: artifact.nativeTxCompactCbor,
    l2TransactionSourceCbor: artifact.l2TransactionSourceCbor,
    transactionsPhasRoot: artifact.transactionsPhasRoot,
    txMembershipProof: Data.from(artifact.transactionMembershipCbor, Proof),
    txMembershipProofCbor: artifact.transactionMembershipCbor,
  });
};

const capture = async (
  submit: Parameters<typeof captureLocallyEvaluatedTransactionV1>[0],
): Promise<ObserversForbiddenCapturedActionV1> =>
  Object.freeze({
    transaction: await captureLocallyEvaluatedTransactionV1(submit),
  });

/** Package-owned transaction actuator; callers receive no submission callback. */
export const createObserversForbiddenActuatorV1 = (
  config: BoundObserversForbiddenActuatorConfigV1,
) =>
  Object.freeze({
    capture: async ({
      action,
      artifact,
    }: {
      readonly action: ObserversForbiddenActuatorActionV1;
      readonly artifact: unknown;
    }): Promise<ObserversForbiddenCapturedActionV1> => {
      const admitted = admitProductionObserversForbiddenArtifactV1(artifact);
      if (admitted.artifact.headerHash !== config.binding.definition.headerHash)
        throw new Error("observersForbidden artifact changed bound header");
      const categoryId = config.binding.resolvedContracts.category.categoryId;
      if (action.stage === "init")
        return await capture(async (preSubmitBoundary) => {
          await submitInit({
            lucid: config.lucid,
            blueprint: config.binding.blueprint,
            deploymentInfo: config.binding.deploymentInfo,
            network: config.binding.network,
            signer: config.signer,
            fraudCategory:
              OBSERVERS_FORBIDDEN_ON_UNTAGGED_NETWORK_CATEGORY_V1 as never,
            fraudulentBlockOutRef: action.stateQueueBlockOutRef,
            fraudulentHeaderHash: admitted.artifact.headerHash,
            witnessReferenceScripts: config.references.witnesses,
            preSubmitBoundary,
            awaitConfirmation: false,
          });
        });
      if (action.stage === "step_01") {
        const { threadUtxo, threadToken } =
          await requireLinearFaultThreadUtxoV1({
            lucid: config.lucid,
            contracts: config.contracts,
            categoryId,
            family: "observers-forbidden-on-untagged-network",
            stepIndex: 0,
            threadOutRef: action.threadOutRef,
          });
        return await capture(async (preSubmitBoundary) => {
          const common = {
            lucid: config.lucid,
            contracts: config.contracts,
            signer: config.signer,
            finding: admitted.evidence,
            referenceScriptUtxo: config.references.steps[0],
            preSubmitBoundary,
            awaitConfirmation: false,
          } as const;
          if (admitted.artifact.sourceKind === "accepted")
            await submitObserversForbiddenStep01AcceptedV1({
              ...common,
              blueprint: config.binding.blueprint,
              network: config.binding.network,
              threadUtxo,
              threadToken,
              stateQueueBlockOutRef: action.stateQueueBlockOutRef,
              txInclusion: inclusionFromArtifact(admitted),
              witnessReferenceScripts: config.references.witnesses,
            });
          else
            await submitObserversForbiddenStep01ForcedV1({
              ...common,
              categoryId,
              threadOutRef: action.threadOutRef,
              forcedSource: Data.from(
                admitted.artifact.forcedSourceCbor,
                ObserversForbiddenForcedSourcePayloadV1Schema as never,
              ) as Readonly<Record<string, unknown>>,
            });
        });
      }
      if (action.stage === "step_02")
        return await capture(async (preSubmitBoundary) => {
          await submitObserversForbiddenStep02V1({
            lucid: config.lucid,
            contracts: config.contracts,
            categoryId,
            signer: config.signer,
            threadOutRef: action.threadOutRef,
            evidence: admitted.evidence,
            nativeTxCompactCbor: admitted.artifact.nativeTxCompactCbor,
            referenceScriptUtxo: config.references.steps[1],
            witnessReferenceScripts: config.references.witnesses,
            preSubmitBoundary,
            awaitConfirmation: false,
          });
        });
      return await captureProductionCursorRemovalV1({
        category: {
          name: OBSERVERS_FORBIDDEN_ON_UNTAGGED_NETWORK_CATEGORY_V1,
          categoryId,
          firstStepDeploymentEntry:
            "fraudProofObserversForbiddenOnUntaggedNetwork",
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
        headerHash: admitted.artifact.headerHash,
        input: {
          schemaVersion: "midgard-production-cursor-family-action-v1",
          category: OBSERVERS_FORBIDDEN_ON_UNTAGGED_NETWORK_CATEGORY_V1,
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
