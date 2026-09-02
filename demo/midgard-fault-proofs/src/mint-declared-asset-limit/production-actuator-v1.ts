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
import type { MintDeclaredAssetLimitContractsV1 } from "./contracts-v1.js";
import { MINT_DECLARED_ASSET_LIMIT_CATEGORY_V1 } from "./family-v1.js";
import {
  admitProductionMintDeclaredAssetLimitArtifactV1,
  MintDeclaredAssetLimitForcedSourcePayloadV1Schema,
} from "./production-artifact-v1.js";
import {
  submitMintDeclaredAssetLimitStep01AcceptedV1,
  submitMintDeclaredAssetLimitStep01ForcedV1,
} from "./submit-step-01-v1.js";
import {
  type MintDeclaredAssetLimitStep02ActionV1,
  submitMintDeclaredAssetLimitStep02V1,
} from "./submit-step-02-v1.js";
import { submitMintDeclaredAssetLimitStep03V1 } from "./submit-step-03-v1.js";
import { submitMintDeclaredAssetLimitStep04V1 } from "./submit-step-04-v1.js";

export type MintDeclaredAssetLimitWorkflowReferenceScriptsV1 = Readonly<{
  steps: readonly [UTxO, UTxO, UTxO, UTxO];
  witnesses: Required<FaultProofWitnessReferenceScriptsV1>;
  fieldPreimageCertificateMint: UTxO;
}>;

export type BoundMintDeclaredAssetLimitActuatorConfigV1 = Readonly<{
  binding: FraudProofWorkflowDeploymentBindingV1<"mintDeclaredAssetLimit">;
  lucid: LucidEvolution;
  signer: ResolvedProverSigner;
  contracts: MintDeclaredAssetLimitContractsV1;
  references: MintDeclaredAssetLimitWorkflowReferenceScriptsV1;
  stateQueueMutationLeaseCoordinator: StateQueueMutationLeaseCoordinator;
}>;

export type MintDeclaredAssetLimitActuatorActionV1 =
  | Readonly<{ stage: "init"; stateQueueBlockOutRef: string }>
  | Readonly<{
      stage: "step_01";
      threadOutRef: string;
      stateQueueBlockOutRef: string;
    }>
  | Readonly<{
      stage: "step_02";
      threadOutRef: string;
      action: MintDeclaredAssetLimitStep02ActionV1;
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

export type MintDeclaredAssetLimitCapturedActionV1 = Readonly<{
  transaction: LocallyEvaluatedTransactionV1;
  mutationLease?: Awaited<
    ReturnType<StateQueueMutationLeaseCoordinator["acquire"]>
  >;
}>;

/** Package-owned prerequisite plan used before every field-consuming action. */
export const mintDeclaredAssetLimitFieldRequirementV1 = ({
  action,
  artifact,
  owner,
  certificate,
}: {
  readonly action: MintDeclaredAssetLimitActuatorActionV1;
  readonly artifact: unknown;
  readonly owner: string;
  readonly certificate: ProductionFieldCarriageRequirementV1["certificate"];
}): ProductionFieldCarriageRequirementV1 | null => {
  if (action.stage !== "step_02" && action.stage !== "step_03") return null;
  const admitted = admitProductionMintDeclaredAssetLimitArtifactV1(artifact);
  return {
    planned: planFaultProofFieldOpeningV1({
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
  admitted: ReturnType<typeof admitProductionMintDeclaredAssetLimitArtifactV1>,
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
): Promise<MintDeclaredAssetLimitCapturedActionV1> =>
  Object.freeze({
    transaction: await captureLocallyEvaluatedTransactionV1(submit),
  });

/**
 * Concrete family actuator. Every proof builder reaches the local-evaluation
 * boundary with `awaitConfirmation:false`; the durable workflow owns the only
 * subsequent submit authority.
 */
export const createMintDeclaredAssetLimitActuatorV1 = (
  config: BoundMintDeclaredAssetLimitActuatorConfigV1,
) =>
  Object.freeze({
    capture: async ({
      action,
      artifact,
    }: {
      readonly action: MintDeclaredAssetLimitActuatorActionV1;
      readonly artifact: unknown;
    }): Promise<MintDeclaredAssetLimitCapturedActionV1> => {
      const admitted =
        admitProductionMintDeclaredAssetLimitArtifactV1(artifact);
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
            fraudCategory: MINT_DECLARED_ASSET_LIMIT_CATEGORY_V1,
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
            await submitMintDeclaredAssetLimitStep01AcceptedV1({
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
            await submitMintDeclaredAssetLimitStep01ForcedV1({
              ...common,
              categoryId,
              threadOutRef: action.threadOutRef,
              forcedSource: Data.from(
                admitted.artifact.forcedSourceCbor,
                MintDeclaredAssetLimitForcedSourcePayloadV1Schema as never,
              ) as Readonly<Record<string, unknown>>,
            });
        });
      }
      if (action.stage === "step_02")
        return await capture(async (preSubmitBoundary) => {
          await submitMintDeclaredAssetLimitStep02V1({
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
          await submitMintDeclaredAssetLimitStep03V1({
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
          await submitMintDeclaredAssetLimitStep04V1({
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
        category: {
          name: MINT_DECLARED_ASSET_LIMIT_CATEGORY_V1,
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
          category: MINT_DECLARED_ASSET_LIMIT_CATEGORY_V1,
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
