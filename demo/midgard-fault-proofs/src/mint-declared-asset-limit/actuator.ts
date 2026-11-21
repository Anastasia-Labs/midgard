import { decodeMidgardNativeTxCompact } from "@al-ft/midgard-core";
import { Proof } from "@al-ft/midgard-sdk/core";
import { Data, type LucidEvolution, type UTxO } from "@lucid-evolution/lucid";

import { type FaultProofTurn } from "../fault-proof-turn.js";
import { planFaultProofFieldOpening } from "../field-opening.plan-fault-proof-field-opening.js";
import { requireLinearFaultThreadUtxo } from "../linear-fault-family.js";
import type { StateQueueMutationLeaseCoordinator } from "../remove-fraudulent-block.js";
import type { ResolvedProverSigner } from "../runtime.js";
import { nativeTxFromCoreCompact } from "../step-support.js";
import { buildInit } from "../submit-init.submit-init.js";
import type { FaultProofWitnessReferenceScripts } from "../witness-reference-scripts.js";
import {
  captureCursorRemoval,
  type CursorFamilyActionInput,
} from "../workflow/cursor-family-runtime.js";
import type { FraudProofWorkflowDeploymentBinding } from "../workflow/deployment-manifest-binding.js";
import type { FieldCarriageRequirement } from "../workflow/field-carriage-prerequisite.js";
import type { OwnOutputJournal } from "../workflow/own-outputs.js";
import { journaledStepTransaction } from "../workflow/step-transaction.js";
import { type LocallyEvaluatedTransaction } from "../workflow/transaction-boundary.js";
import {
  admitMintDeclaredAssetLimitArtifact,
  MintDeclaredAssetLimitForcedSourcePayloadSchema,
} from "./artifact.js";
import type { MintDeclaredAssetLimitContracts } from "./contracts.js";
import { MINT_DECLARED_ASSET_LIMIT_CATEGORY } from "./family.begin-mint-declared-policy.js";
import {
  buildMintDeclaredAssetLimitStep01Accepted,
  buildMintDeclaredAssetLimitStep01Forced,
} from "./submit-step-01.js";
import {
  buildMintDeclaredAssetLimitStep02,
  type MintDeclaredAssetLimitStep02Action,
} from "./submit-step-02.js";
import { buildMintDeclaredAssetLimitStep03 } from "./submit-step-03.js";
import { buildMintDeclaredAssetLimitStep04 } from "./submit-step-04.js";

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
      stateQueueBlockOutRef: string;
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
      anchorSourceKind: admitted.artifact.sourceKind === "forced" ? 1n : 0n,
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
  built: FaultProofTurn<{ readonly transaction: LocallyEvaluatedTransaction }>,
  label: string,
): Promise<MintDeclaredAssetLimitCapturedAction> =>
  Object.freeze({
    transaction: (await journaledStepTransaction(built, label)).transaction,
  });

/**
 * Concrete family actuator. Every proof builder returns its signed transaction
 * unsubmitted; the durable workflow owns the only submit authority.
 */
export const createMintDeclaredAssetLimitActuator = (
  config: BoundMintDeclaredAssetLimitActuatorConfig,
) =>
  Object.freeze({
    capture: async ({
      action,
      artifact,
      ownOutputs,
    }: {
      readonly action: MintDeclaredAssetLimitActuatorAction;
      readonly artifact: unknown;
      /** The workflow's field-carriage claims, which the steps read carriage by. */
      readonly ownOutputs: OwnOutputJournal;
    }): Promise<MintDeclaredAssetLimitCapturedAction> => {
      const admitted = admitMintDeclaredAssetLimitArtifact(artifact);
      if (admitted.artifact.headerHash !== config.binding.definition.headerHash)
        throw new Error("mintDeclaredAssetLimit artifact changed bound header");
      const categoryId = config.binding.resolvedContracts.category.categoryId;
      if (action.stage === "init")
        return await capture(
          await buildInit({
            lucid: config.lucid,
            blueprint: config.binding.blueprint,
            deploymentInfo: config.binding.deploymentInfo,
            network: config.binding.network,
            signer: config.signer,
            fraudCategory: MINT_DECLARED_ASSET_LIMIT_CATEGORY,
            fraudulentBlockOutRef: action.stateQueueBlockOutRef,
            fraudulentHeaderHash: admitted.artifact.headerHash,
            witnessReferenceScripts: config.references.witnesses,
          }),
          "mint-declared-asset-limit init",
        );
      if (action.stage === "step_01") {
        const { threadUtxo, threadToken } = await requireLinearFaultThreadUtxo({
          lucid: config.lucid,
          contracts: config.contracts,
          categoryId,
          family: "mint-declared-asset-limit",
          stepIndex: 0,
          threadOutRef: action.threadOutRef,
        });
        const common = {
          lucid: config.lucid,
          contracts: config.contracts,
          signer: config.signer,
          finding: admitted.evidence,
          referenceScriptUtxo: config.references.steps[0],
        } as const;
        return await capture(
          admitted.artifact.sourceKind === "accepted"
            ? await buildMintDeclaredAssetLimitStep01Accepted({
                ...common,
                blueprint: config.binding.blueprint,
                network: config.binding.network,
                threadUtxo,
                threadToken,
                stateQueueBlockOutRef: action.stateQueueBlockOutRef,
                txInclusion: inclusionFromArtifact(admitted),
                witnessReferenceScripts: config.references.witnesses,
              })
            : await buildMintDeclaredAssetLimitStep01Forced({
                ...common,
                categoryId,
                threadOutRef: action.threadOutRef,
                forcedSource: Data.from(
                  admitted.artifact.forcedSourceCbor,
                  MintDeclaredAssetLimitForcedSourcePayloadSchema as never,
                ) as Readonly<Record<string, unknown>>,
              }),
          "mint-declared-asset-limit step_01",
        );
      }
      if (action.stage === "step_02")
        return await capture(
          await buildMintDeclaredAssetLimitStep02({
            ownOutputs,
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
          }),
          "mint-declared-asset-limit step_02",
        );
      if (action.stage === "step_03") {
        const staged = admitted.staged;
        if (!("walk" in staged))
          throw new Error(
            "mintDeclaredAssetLimit absent policy closes before the fold",
          );
        return await capture(
          await buildMintDeclaredAssetLimitStep03({
            ownOutputs,
            lucid: config.lucid,
            contracts: config.contracts,
            categoryId,
            signer: config.signer,
            threadOutRef: action.threadOutRef,
            evidence: admitted.evidence,
            nativeTxCompactCbor: admitted.artifact.nativeTxCompactCbor,
            staged,
            walkOrdinal: action.walkOrdinal,
            referenceScriptUtxo: config.references.steps[2],
          }),
          "mint-declared-asset-limit step_03",
        );
      }
      if (action.stage === "step_04")
        return await capture(
          await buildMintDeclaredAssetLimitStep04({
            lucid: config.lucid,
            contracts: config.contracts,
            categoryId,
            signer: config.signer,
            threadOutRef: action.threadOutRef,
            evidence: admitted.evidence,
            referenceScriptUtxo: config.references.steps[3],
            witnessReferenceScripts: config.references.witnesses,
          }),
          "mint-declared-asset-limit step_04",
        );
      // A registered family resolves removal through the canonical catalogue:
      // the deployment manifest's `fraudProofMintDeclaredAssetLimit` entry
      // pins the step-01 script, so no explicit category is described here.
      return await captureCursorRemoval({
        category: MINT_DECLARED_ASSET_LIMIT_CATEGORY,
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
          stateQueueBlockOutRef: action.stateQueueBlockOutRef,
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
