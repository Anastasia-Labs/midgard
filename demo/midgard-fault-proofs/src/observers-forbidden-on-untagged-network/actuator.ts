import {
  decodeMidgardFieldPreimage,
  decodeMidgardNativeTxCompact,
} from "@al-ft/midgard-core";
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
  admitObserversForbiddenArtifact,
  ObserversForbiddenForcedSourcePayloadSchema,
} from "./artifact.js";
import type { ObserversForbiddenContracts } from "./contracts.js";
import { OBSERVERS_FORBIDDEN_ON_UNTAGGED_NETWORK_CATEGORY } from "./family.js";
import {
  submitObserversForbiddenStep01Accepted,
  submitObserversForbiddenStep01Forced,
} from "./submit-step-01.js";
import { submitObserversForbiddenStep02 } from "./submit-step-02.js";

export type ObserversForbiddenWorkflowReferenceScripts = Readonly<{
  steps: readonly [UTxO, UTxO];
  witnesses: Required<FaultProofWitnessReferenceScripts>;
  fieldPreimageCertificateMint: UTxO;
}>;

export type BoundObserversForbiddenActuatorConfig = Readonly<{
  // Structurally identical deployment binding; the central category union is
  // intentionally protected until the serial integration pass.
  binding: FraudProofWorkflowDeploymentBinding<"observersForbiddenOnUntaggedNetwork">;
  lucid: LucidEvolution;
  signer: ResolvedProverSigner;
  contracts: ObserversForbiddenContracts;
  references: ObserversForbiddenWorkflowReferenceScripts;
  stateQueueMutationLeaseCoordinator: StateQueueMutationLeaseCoordinator;
}>;

export type ObserversForbiddenActuatorAction =
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

export type ObserversForbiddenCapturedAction = Readonly<{
  transaction: LocallyEvaluatedTransaction;
  mutationLease?: Awaited<
    ReturnType<StateQueueMutationLeaseCoordinator["acquire"]>
  >;
}>;

export const observersForbiddenFieldRequirement = ({
  action,
  artifact,
  owner,
  certificate,
}: {
  readonly action: ObserversForbiddenActuatorAction;
  readonly artifact: unknown;
  readonly owner: string;
  readonly certificate: FieldCarriageRequirement["certificate"];
}): FieldCarriageRequirement | null => {
  if (action.stage !== "step_02") return null;
  const admitted = admitObserversForbiddenArtifact(artifact);
  return {
    planned: planFaultProofFieldOpening({
      fieldIndex: 3,
      anchorTxId: admitted.artifact.transactionId,
      nativeTxCompactCbor: admitted.artifact.nativeTxCompactCbor,
      itemCbors:
        admitted.evidence.observerCount === 0
          ? []
          : decodeMidgardFieldPreimage(
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
  admitted: ReturnType<typeof admitObserversForbiddenArtifact>,
) => {
  const artifact = admitted.artifact;
  return Object.freeze({
    nativeTxId: artifact.transactionId,
    nativeTx: nativeTxFromCoreCompact(
      decodeMidgardNativeTxCompact(
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
  submit: Parameters<typeof captureLocallyEvaluatedTransaction>[0],
): Promise<ObserversForbiddenCapturedAction> =>
  Object.freeze({
    transaction: await captureLocallyEvaluatedTransaction(submit),
  });

/** Package-owned transaction actuator; callers receive no submission callback. */
export const createObserversForbiddenActuator = (
  config: BoundObserversForbiddenActuatorConfig,
) =>
  Object.freeze({
    capture: async ({
      action,
      artifact,
    }: {
      readonly action: ObserversForbiddenActuatorAction;
      readonly artifact: unknown;
    }): Promise<ObserversForbiddenCapturedAction> => {
      const admitted = admitObserversForbiddenArtifact(artifact);
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
              OBSERVERS_FORBIDDEN_ON_UNTAGGED_NETWORK_CATEGORY as never,
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
            await submitObserversForbiddenStep01Accepted({
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
            await submitObserversForbiddenStep01Forced({
              ...common,
              categoryId,
              threadOutRef: action.threadOutRef,
              forcedSource: Data.from(
                admitted.artifact.forcedSourceCbor,
                ObserversForbiddenForcedSourcePayloadSchema as never,
              ) as Readonly<Record<string, unknown>>,
            });
        });
      }
      if (action.stage === "step_02")
        return await capture(async (preSubmitBoundary) => {
          await submitObserversForbiddenStep02({
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
      return await captureCursorRemoval({
        category: {
          name: OBSERVERS_FORBIDDEN_ON_UNTAGGED_NETWORK_CATEGORY,
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
          category: OBSERVERS_FORBIDDEN_ON_UNTAGGED_NETWORK_CATEGORY,
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
