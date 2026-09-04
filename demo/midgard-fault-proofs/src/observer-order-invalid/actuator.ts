import { decodeMidgardNativeTxCompact } from "@al-ft/midgard-core";
import { Proof } from "@al-ft/midgard-sdk";
import { Data, type LucidEvolution, type UTxO } from "@lucid-evolution/lucid";

import { planFaultProofFieldOpening } from "../field-opening.js";
import { requireLinearFaultThreadUtxo } from "../linear-fault-family.js";
import type { StateQueueMutationLeaseCoordinator } from "../remove-fraudulent-block.js";
import type { ResolvedProverSigner } from "../runtime.js";
import { submitInit } from "../submit-init.js";
import { nativeTxFromCoreCompact } from "../submit-step-01.js";
import type { FaultProofWitnessReferenceScripts } from "../witness-reference-scripts.js";
import {
  captureCursorRemoval,
  type CursorFamilyActionInput,
} from "../workflow/cursor-family-runtime.js";
import type { FraudProofWorkflowDeploymentBinding } from "../workflow/deployment-manifest-binding.js";
import type { FieldCarriageRequirement } from "../workflow/field-carriage-prerequisite.js";
import {
  captureLocallyEvaluatedTransaction,
  type LocallyEvaluatedTransaction,
} from "../workflow/transaction-boundary.js";
import {
  admitObserverOrderInvalidArtifact,
  ObserverOrderInvalidForcedSourcePayloadSchema,
} from "./artifact.js";
import type { ObserverOrderInvalidContracts } from "./contracts.js";
import { OBSERVER_ORDER_INVALID_CATEGORY } from "./family.js";
import {
  submitObserverOrderInvalidStep01Accepted,
  submitObserverOrderInvalidStep01Forced,
} from "./submit-step-01.js";
import {
  type ObserverOrderInvalidStep02Action,
  submitObserverOrderInvalidStep02,
} from "./submit-step-02.js";
import { submitObserverOrderInvalidStep03 } from "./submit-step-03.js";
import { submitObserverOrderInvalidStep04 } from "./submit-step-04.js";

export type ObserverOrderInvalidWorkflowReferenceScripts = Readonly<{
  steps: readonly [UTxO, UTxO, UTxO, UTxO];
  witnesses: Required<FaultProofWitnessReferenceScripts>;
  fieldPreimageCertificateMint: UTxO;
}>;

export type BoundObserverOrderInvalidActuatorConfig = Readonly<{
  binding: FraudProofWorkflowDeploymentBinding<"observerOrderInvalid">;
  lucid: LucidEvolution;
  signer: ResolvedProverSigner;
  contracts: ObserverOrderInvalidContracts;
  references: ObserverOrderInvalidWorkflowReferenceScripts;
  stateQueueMutationLeaseCoordinator: StateQueueMutationLeaseCoordinator;
}>;

export type ObserverOrderInvalidActuatorAction =
  | Readonly<{ stage: "init"; stateQueueBlockOutRef: string }>
  | Readonly<{
      stage: "step_01";
      threadOutRef: string;
      stateQueueBlockOutRef: string;
    }>
  | Readonly<{
      stage: "step_02";
      threadOutRef: string;
      action: ObserverOrderInvalidStep02Action;
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

export type ObserverOrderInvalidCapturedAction = Readonly<{
  transaction: LocallyEvaluatedTransaction;
  mutationLease?: Awaited<
    ReturnType<StateQueueMutationLeaseCoordinator["acquire"]>
  >;
}>;

/** Package-owned prerequisite plan used before every field-consuming action. */
export const observerOrderInvalidFieldRequirement = ({
  action,
  artifact,
  owner,
  certificate,
}: {
  readonly action: ObserverOrderInvalidActuatorAction;
  readonly artifact: unknown;
  readonly owner: string;
  readonly certificate: FieldCarriageRequirement["certificate"];
}): FieldCarriageRequirement | null => {
  if (action.stage !== "step_02" && action.stage !== "step_03") return null;
  const admitted = admitObserverOrderInvalidArtifact(artifact);
  return {
    planned: planFaultProofFieldOpening({
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
  admitted: ReturnType<typeof admitObserverOrderInvalidArtifact>,
) => {
  const artifact = admitted.artifact;
  const decoded = decodeMidgardNativeTxCompact(
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
  submit: Parameters<typeof captureLocallyEvaluatedTransaction>[0],
): Promise<ObserverOrderInvalidCapturedAction> =>
  Object.freeze({
    transaction: await captureLocallyEvaluatedTransaction(submit),
  });

/**
 * Concrete family actuator. Every proof builder reaches the local-evaluation
 * boundary with `awaitConfirmation:false`; the durable workflow owns the only
 * subsequent submit authority.
 */
export const createObserverOrderInvalidActuator = (
  config: BoundObserverOrderInvalidActuatorConfig,
) =>
  Object.freeze({
    capture: async ({
      action,
      artifact,
    }: {
      readonly action: ObserverOrderInvalidActuatorAction;
      readonly artifact: unknown;
    }): Promise<ObserverOrderInvalidCapturedAction> => {
      const admitted = admitObserverOrderInvalidArtifact(artifact);
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
            fraudCategory: OBSERVER_ORDER_INVALID_CATEGORY,
            fraudulentBlockOutRef: action.stateQueueBlockOutRef,
            fraudulentHeaderHash: admitted.artifact.headerHash,
            witnessReferenceScripts: config.references.witnesses,
            preSubmitBoundary,
            awaitConfirmation: false,
          });
        });
      if (action.stage === "step_01") {
        const { threadUtxo, threadToken } = await requireLinearFaultThreadUtxo({
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
            await submitObserverOrderInvalidStep01Accepted({
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
            await submitObserverOrderInvalidStep01Forced({
              ...common,
              categoryId,
              threadOutRef: action.threadOutRef,
              forcedSource: Data.from(
                admitted.artifact.forcedSourceCbor,
                ObserverOrderInvalidForcedSourcePayloadSchema as never,
              ) as Readonly<Record<string, unknown>>,
            });
        });
      }
      if (action.stage === "step_02")
        return await capture(async (preSubmitBoundary) => {
          await submitObserverOrderInvalidStep02({
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
          await submitObserverOrderInvalidStep03({
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
          await submitObserverOrderInvalidStep04({
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
      return await captureCursorRemoval({
        category: OBSERVER_ORDER_INVALID_CATEGORY,
        lucid: config.lucid,
        blueprint: config.binding.blueprint,
        deploymentInfo: config.binding.deploymentInfo,
        network: config.binding.network,
        signer: config.signer,
        headerHash: admitted.artifact.headerHash,
        input: {
          schemaVersion: "midgard-production-cursor-family-action-v1",
          category: OBSERVER_ORDER_INVALID_CATEGORY,
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
