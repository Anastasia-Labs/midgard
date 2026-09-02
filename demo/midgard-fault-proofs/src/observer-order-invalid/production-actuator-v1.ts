import { decodeMidgardNativeTxCompactV1 } from "@al-ft/midgard-core";
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
import type { ObserverOrderInvalidContractsV1 } from "./contracts-v1.js";
import { OBSERVER_ORDER_INVALID_CATEGORY_V1 } from "./family-v1.js";
import {
  admitProductionObserverOrderInvalidArtifactV1,
  ObserverOrderInvalidForcedSourcePayloadV1Schema,
} from "./production-artifact-v1.js";
import {
  submitObserverOrderInvalidStep01AcceptedV1,
  submitObserverOrderInvalidStep01ForcedV1,
} from "./submit-step-01-v1.js";
import {
  type ObserverOrderInvalidStep02ActionV1,
  submitObserverOrderInvalidStep02V1,
} from "./submit-step-02-v1.js";
import { submitObserverOrderInvalidStep03V1 } from "./submit-step-03-v1.js";
import { submitObserverOrderInvalidStep04V1 } from "./submit-step-04-v1.js";

export type ObserverOrderInvalidWorkflowReferenceScriptsV1 = Readonly<{
  steps: readonly [UTxO, UTxO, UTxO, UTxO];
  witnesses: Required<FaultProofWitnessReferenceScriptsV1>;
  fieldPreimageCertificateMint: UTxO;
}>;

export type BoundObserverOrderInvalidActuatorConfigV1 = Readonly<{
  binding: FraudProofWorkflowDeploymentBindingV1<"observerOrderInvalid">;
  lucid: LucidEvolution;
  signer: ResolvedProverSigner;
  contracts: ObserverOrderInvalidContractsV1;
  references: ObserverOrderInvalidWorkflowReferenceScriptsV1;
  stateQueueMutationLeaseCoordinator: StateQueueMutationLeaseCoordinator;
}>;

export type ObserverOrderInvalidActuatorActionV1 =
  | Readonly<{ stage: "init"; stateQueueBlockOutRef: string }>
  | Readonly<{
      stage: "step_01";
      threadOutRef: string;
      stateQueueBlockOutRef: string;
    }>
  | Readonly<{
      stage: "step_02";
      threadOutRef: string;
      action: ObserverOrderInvalidStep02ActionV1;
    }>
  | Readonly<{
      stage: "step_03";
      threadOutRef: string;
      walkOrdinal: number;
    }>
  | Readonly<{ stage: "step_04"; threadOutRef: string }>
  | Readonly<{
      stage: "remove";
      nextRemovalOutRef: string;
      fraudProofOutRef: string;
    }>;

export type ObserverOrderInvalidCapturedActionV1 = Readonly<{
  transaction: LocallyEvaluatedTransactionV1;
  mutationLease?: Awaited<
    ReturnType<StateQueueMutationLeaseCoordinator["acquire"]>
  >;
}>;

/** Package-owned prerequisite plan used before every field-consuming action. */
export const observerOrderInvalidFieldRequirementV1 = ({
  action,
  artifact,
  owner,
  certificate,
}: {
  readonly action: ObserverOrderInvalidActuatorActionV1;
  readonly artifact: unknown;
  readonly owner: string;
  readonly certificate: ProductionFieldCarriageRequirementV1["certificate"];
}): ProductionFieldCarriageRequirementV1 | null => {
  if (action.stage !== "step_02" && action.stage !== "step_03") return null;
  const admitted = admitProductionObserverOrderInvalidArtifactV1(artifact);
  return {
    planned: planFaultProofFieldOpeningV1({
      fieldIndex: 3,
      anchorTxId: admitted.artifact.transactionId,
      nativeTxCompactCbor: admitted.artifact.nativeTxCompactCbor,
      itemCbors: admitted.staged.items,
      owner,
      publish: true,
      label: "observerOrderInvalid production field 3",
    }),
    compactCbor: admitted.artifact.nativeTxCompactCbor,
    witnessSetCompactCbor: admitted.artifact.witnessSetCompactCbor,
    certificate,
  };
};

const inclusionFromArtifact = (
  admitted: ReturnType<typeof admitProductionObserverOrderInvalidArtifactV1>,
) => {
  const artifact = admitted.artifact;
  const decoded = decodeMidgardNativeTxCompactV1(
    Buffer.from(artifact.nativeTxCompactCbor, "hex"),
  );
  return Object.freeze({
    nativeTxId: artifact.transactionId,
    nativeTx: nativeTxFromCoreCompact(decoded),
    nativeTxCompactCbor: artifact.nativeTxCompactCbor,
    l2TransactionSourceCbor: artifact.l2TransactionSourceCbor,
    transactionsPhasRoot: artifact.transactionsPhasRoot,
    txMembershipProof: Data.from(artifact.transactionMembershipCbor, Proof),
    txMembershipProofCbor: artifact.transactionMembershipCbor,
  });
};

const capture = async (
  submit: Parameters<typeof captureLocallyEvaluatedTransactionV1>[0],
): Promise<ObserverOrderInvalidCapturedActionV1> =>
  Object.freeze({
    transaction: await captureLocallyEvaluatedTransactionV1(submit),
  });

/**
 * Concrete family actuator. Every proof builder reaches the local-evaluation
 * boundary with `awaitConfirmation:false`; the durable workflow owns the only
 * subsequent submit authority.
 */
export const createObserverOrderInvalidActuatorV1 = (
  config: BoundObserverOrderInvalidActuatorConfigV1,
) =>
  Object.freeze({
    capture: async ({
      action,
      artifact,
    }: {
      readonly action: ObserverOrderInvalidActuatorActionV1;
      readonly artifact: unknown;
    }): Promise<ObserverOrderInvalidCapturedActionV1> => {
      const admitted = admitProductionObserverOrderInvalidArtifactV1(artifact);
      if (admitted.artifact.headerHash !== config.binding.definition.headerHash)
        throw new Error("observerOrderInvalid artifact changed bound header");
      const categoryId = config.binding.resolvedContracts.category.categoryId;
      if (action.stage === "init")
        return await capture(async (preSubmitBoundary) => {
          await submitInit({
            lucid: config.lucid,
            blueprint: config.binding.blueprint,
            deploymentInfo: config.binding.deploymentInfo,
            network: config.binding.network,
            signer: config.signer,
            fraudCategory: OBSERVER_ORDER_INVALID_CATEGORY_V1,
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
            family: "observer-order-invalid",
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
            await submitObserverOrderInvalidStep01AcceptedV1({
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
            await submitObserverOrderInvalidStep01ForcedV1({
              ...common,
              categoryId,
              threadOutRef: action.threadOutRef,
              forcedSource: Data.from(
                admitted.artifact.forcedSourceCbor,
                ObserverOrderInvalidForcedSourcePayloadV1Schema as never,
              ) as Readonly<Record<string, unknown>>,
            });
        });
      }
      if (action.stage === "step_02")
        return await capture(async (preSubmitBoundary) => {
          await submitObserverOrderInvalidStep02V1({
            lucid: config.lucid,
            contracts: config.contracts,
            categoryId,
            signer: config.signer,
            threadOutRef: action.threadOutRef,
            evidence: admitted.evidence,
            nativeTxCompactCbor: admitted.artifact.nativeTxCompactCbor,
            staged: admitted.staged,
            action: action.action,
            referenceScriptUtxo: config.references.steps[1],
            preSubmitBoundary,
            awaitConfirmation: false,
          });
        });
      if (action.stage === "step_03")
        return await capture(async (preSubmitBoundary) => {
          await submitObserverOrderInvalidStep03V1({
            lucid: config.lucid,
            contracts: config.contracts,
            categoryId,
            signer: config.signer,
            threadOutRef: action.threadOutRef,
            evidence: admitted.evidence,
            nativeTxCompactCbor: admitted.artifact.nativeTxCompactCbor,
            staged: admitted.staged,
            walkOrdinal: action.walkOrdinal,
            referenceScriptUtxo: config.references.steps[2],
            preSubmitBoundary,
            awaitConfirmation: false,
          });
        });
      if (action.stage === "step_04")
        return await capture(async (preSubmitBoundary) => {
          await submitObserverOrderInvalidStep04V1({
            lucid: config.lucid,
            contracts: config.contracts,
            categoryId,
            signer: config.signer,
            threadOutRef: action.threadOutRef,
            evidence: admitted.evidence,
            referenceScriptUtxo: config.references.steps[3],
            witnessReferenceScripts: config.references.witnesses,
            preSubmitBoundary,
            awaitConfirmation: false,
          });
        });
      return await captureProductionCursorRemovalV1({
        category: OBSERVER_ORDER_INVALID_CATEGORY_V1,
        lucid: config.lucid,
        blueprint: config.binding.blueprint,
        deploymentInfo: config.binding.deploymentInfo,
        network: config.binding.network,
        signer: config.signer,
        headerHash: admitted.artifact.headerHash,
        input: {
          schemaVersion: "midgard-production-cursor-family-action-v1",
          category: OBSERVER_ORDER_INVALID_CATEGORY_V1,
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
