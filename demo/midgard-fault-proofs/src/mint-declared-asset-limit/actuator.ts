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
  admitMintDeclaredAssetLimitArtifact,
  MintDeclaredAssetLimitForcedSourcePayloadSchema,
} from "./artifact.js";
import type { MintDeclaredAssetLimitContracts } from "./contracts.js";
import { MINT_DECLARED_ASSET_LIMIT_CATEGORY } from "./family.js";
import {
  submitMintDeclaredAssetLimitStep01Accepted,
  submitMintDeclaredAssetLimitStep01Forced,
} from "./submit-step-01.js";
import {
  type MintDeclaredAssetLimitStep02Action,
  submitMintDeclaredAssetLimitStep02,
} from "./submit-step-02.js";
import { submitMintDeclaredAssetLimitStep03 } from "./submit-step-03.js";
import { submitMintDeclaredAssetLimitStep04 } from "./submit-step-04.js";

export type MintDeclaredAssetLimitWorkflowReferenceScripts = Readonly<{
  steps: readonly [UTxO, UTxO, UTxO, UTxO];
  witnesses: Required<FaultProofWitnessReferenceScripts>;
  fieldPreimageCertificateMint: UTxO;
}>;

export type BoundMintDeclaredAssetLimitActuatorConfig = Readonly<{
  binding: FraudProofWorkflowDeploymentBinding<"mintDeclaredAssetLimit">;
  lucid: LucidEvolution;
  signer: ResolvedProverSigner;
  contracts: MintDeclaredAssetLimitContracts;
  references: MintDeclaredAssetLimitWorkflowReferenceScripts;
  stateQueueMutationLeaseCoordinator: StateQueueMutationLeaseCoordinator;
}>;

export type MintDeclaredAssetLimitActuatorAction =
  | Readonly<{ stage: "init"; stateQueueBlockOutRef: string }>
  | Readonly<{
      stage: "step_01";
      threadOutRef: string;
      stateQueueBlockOutRef: string;
    }>
  | Readonly<{
      stage: "step_02";
      threadOutRef: string;
      action: MintDeclaredAssetLimitStep02Action;
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

export type MintDeclaredAssetLimitCapturedAction = Readonly<{
  transaction: LocallyEvaluatedTransaction;
  mutationLease?: Awaited<
    ReturnType<StateQueueMutationLeaseCoordinator["acquire"]>
  >;
}>;

/** Package-owned prerequisite plan used before every field-consuming action. */
export const mintDeclaredAssetLimitFieldRequirement = ({
  action,
  artifact,
  owner,
  certificate,
}: {
  readonly action: MintDeclaredAssetLimitActuatorAction;
  readonly artifact: unknown;
  readonly owner: string;
  readonly certificate: FieldCarriageRequirement["certificate"];
}): FieldCarriageRequirement | null => {
  if (action.stage !== "step_02" && action.stage !== "step_03") return null;
  const admitted = admitMintDeclaredAssetLimitArtifact(artifact);
  return {
    planned: planFaultProofFieldOpening({
      fieldIndex: 5,
      anchorTxId: admitted.artifact.transactionId,
      nativeTxCompactCbor: admitted.artifact.nativeTxCompactCbor,
      itemCbors: admitted.staged.items,
      owner,
      publish: true,
      label: "mintDeclaredAssetLimit production field 5",
    }),
    compactCbor: admitted.artifact.nativeTxCompactCbor,
    witnessSetCompactCbor: admitted.artifact.witnessSetCompactCbor,
    certificate,
  };
};

const inclusionFromArtifact = (
  admitted: ReturnType<typeof admitMintDeclaredAssetLimitArtifact>,
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
): Promise<MintDeclaredAssetLimitCapturedAction> =>
  Object.freeze({
    transaction: await captureLocallyEvaluatedTransaction(submit),
  });

/**
 * Concrete family actuator. Every proof builder reaches the local-evaluation
 * boundary with `awaitConfirmation:false`; the durable workflow owns the only
 * subsequent submit authority.
 */
export const createMintDeclaredAssetLimitActuator = (
  config: BoundMintDeclaredAssetLimitActuatorConfig,
) =>
  Object.freeze({
    capture: async ({
      action,
      artifact,
    }: {
      readonly action: MintDeclaredAssetLimitActuatorAction;
      readonly artifact: unknown;
    }): Promise<MintDeclaredAssetLimitCapturedAction> => {
      const admitted = admitMintDeclaredAssetLimitArtifact(artifact);
      if (admitted.artifact.headerHash !== config.binding.definition.headerHash)
        throw new Error("mintDeclaredAssetLimit artifact changed bound header");
      const categoryId = config.binding.resolvedContracts.category.categoryId;
      if (action.stage === "init")
        return await capture(async (preSubmitBoundary) => {
          await submitInit({
            lucid: config.lucid,
            blueprint: config.binding.blueprint,
            deploymentInfo: config.binding.deploymentInfo,
            network: config.binding.network,
            signer: config.signer,
            fraudCategory: MINT_DECLARED_ASSET_LIMIT_CATEGORY,
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
          family: "mint-declared-asset-limit",
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
            await submitMintDeclaredAssetLimitStep01Accepted({
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
            await submitMintDeclaredAssetLimitStep01Forced({
              ...common,
              categoryId,
              threadOutRef: action.threadOutRef,
              forcedSource: Data.from(
                admitted.artifact.forcedSourceCbor,
                MintDeclaredAssetLimitForcedSourcePayloadSchema as never,
              ) as Readonly<Record<string, unknown>>,
            });
        });
      }
      if (action.stage === "step_02")
        return await capture(async (preSubmitBoundary) => {
          await submitMintDeclaredAssetLimitStep02({
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
          await submitMintDeclaredAssetLimitStep03({
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
          await submitMintDeclaredAssetLimitStep04({
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
        category: {
          name: MINT_DECLARED_ASSET_LIMIT_CATEGORY,
          categoryId,
          firstStepDeploymentEntry: "fraudProofMintDeclaredAssetLimit",
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
          category: MINT_DECLARED_ASSET_LIMIT_CATEGORY,
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
