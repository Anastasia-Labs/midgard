import type { LucidEvolution } from "@lucid-evolution/lucid";

import {
  admitMissingSignatureForcedArtifact,
  MISSING_SIGNATURE_FORCED_ARTIFACT,
} from "../missing-signature/forced-artifact.js";
import { buildMissingSignatureForcedAction } from "../missing-signature/submit-forced.js";
import { type StateQueueMutationLeaseCoordinator } from "../remove-fraudulent-block.js";
import type { ResolvedProverSigner } from "../runtime.js";
import { type FraudProofWorkflowDeploymentBinding } from "./deployment-manifest-binding.js";
import { type FraudProofFamilyL1ObservationPort } from "./family-l1-observation.js";
import type { FraudProofL1Source } from "./l1-source.js";
import {
  admitMissingSignatureArtifact,
  type BoundMissingSignatureTransactionsConfig,
  type MissingSignatureBuilderSet,
  type MissingSignatureWorkflowReferenceScripts,
  prepareMissingSignatureWorkflowArtifact,
  requiredAction,
  stringField,
} from "./missing-signature.admit-missing-signature-artifact.js";
import {
  MISSING_SIGNATURE_TRANSACTION_PORT,
  type MissingSignatureCapturedAction,
  type MissingSignatureTransactionPort,
} from "./missing-signature-adapter.js";
import {
  type FraudProofFamilyWorkflowAdapter,
  type FraudProofWorkflowTerminalVerifier,
} from "./orchestrator.js";
import { proofMaterialsAccess } from "./proof-materials-view.js";
import type { FraudProofReleaseFinalityAuthority } from "./release-finality-policy.js";
import { admitRemovalTransaction } from "./removal-admission.js";
import { workflowStepTransaction } from "./step-transaction.js";
import {
  workflowTransactionInputOutRefs,
  workflowTransactionReferenceInputOutRefs,
} from "./transaction-boundary.js";

export const createBoundTransactionPort = ({
  config,
  builders,
}: {
  readonly config: BoundMissingSignatureTransactionsConfig;
  readonly builders: MissingSignatureBuilderSet;
}): MissingSignatureTransactionPort => {
  return {
    portVersion: MISSING_SIGNATURE_TRANSACTION_PORT,
    category: "missingSignature",
    prepare: async ({ evidence, classification }) =>
      await prepareMissingSignatureWorkflowArtifact({
        evidence,
        classification,
      }),
    capture: async ({ action, artifact, holder }) => {
      const forced =
        artifact.schemaVersion === MISSING_SIGNATURE_FORCED_ARTIFACT
          ? await admitMissingSignatureForcedArtifact(artifact)
          : undefined;
      const admitted =
        forced === undefined
          ? await admitMissingSignatureArtifact(artifact)
          : undefined;
      if (
        (forced?.headerHash ?? admitted?.artifact.headerHash) !==
        config.headerHash
      ) {
        throw new Error(
          "missing-signature artifact targets a different manifest-bound header",
        );
      }
      const input = requiredAction(action);
      // Every step reads the field carriage its prerequisite published.
      const materials = proofMaterialsAccess({
        signer: config.signer,
        lucid: config.lucid,
        holder,
      });
      if (input.stage === "init") {
        const transaction = (
          await workflowStepTransaction(
            await builders.init({
              lucid: config.lucid,
              blueprint: config.blueprint,
              network: config.network,
              contracts: config.contracts,
              category: config.category,
              catalogue: config.catalogue,
              signer: config.signer,
              fraudulentBlockOutRef: stringField(
                input,
                "stateQueueBlockOutRef",
              ),
              fraudulentHeaderHash: config.headerHash,
              witnessReferenceScripts: config.referenceScripts.witnesses,
            }),
            "missing-signature init",
          )
        ).transaction;
        return Object.freeze({ transaction });
      }
      if (forced !== undefined && input.stage !== "remove") {
        const refs = config.referenceScripts.forced;
        if (refs === undefined)
          throw new Error(
            "missingSignature: forced reference scripts are not deployed",
          );
        const referenceScriptUtxo =
          input.stage === "step_01"
            ? config.referenceScripts.steps[0]
            : input.stage === "step_05"
              ? refs.bind
              : input.stage === "step_06"
                ? refs.signer
                : input.stage === "step_07"
                  ? refs.witness
                  : undefined;
        if (referenceScriptUtxo === undefined)
          throw new Error(
            "missingSignature: forced artifact entered an accepted stage",
          );
        const transaction = (
          await workflowStepTransaction(
            await buildMissingSignatureForcedAction({
              materials,
              lucid: config.lucid,
              contracts: config.contracts,
              categoryId: config.category.categoryId,
              signer: config.signer,
              threadOutRef: stringField(input, "threadOutRef"),
              prepared: forced,
              referenceScriptUtxo,
              witnessReferenceScripts: config.referenceScripts.witnesses,
              certificateUtxo:
                input.stage === "step_06"
                  ? config.referenceScripts.fieldCertificates?.forcedSigner
                  : config.referenceScripts.fieldCertificates?.forcedWitness,
            }),
            "missing-signature buildMissingSignatureForcedAction",
          )
        ).transaction;
        return Object.freeze({ transaction });
      }
      if (input.stage === "step_01") {
        if (admitted === undefined)
          throw new Error("missingSignature: accepted artifact missing");
        const transaction = (
          await workflowStepTransaction(
            await builders.step01({
              lucid: config.lucid,
              blueprint: config.blueprint,
              network: config.network,
              contracts: config.contracts,
              categoryId: config.category.categoryId,
              signer: config.signer,
              threadOutRef: stringField(input, "threadOutRef"),
              stateQueueBlockOutRef: stringField(
                input,
                "stateQueueBlockOutRef",
              ),
              txInclusion: admitted.txInclusion,
              referenceScriptUtxo: config.referenceScripts.steps[0],
              witnessReferenceScripts: config.referenceScripts.witnesses,
            }),
            "missing-signature step01",
          )
        ).transaction;
        return Object.freeze({ transaction });
      }
      if (input.stage === "step_02") {
        if (admitted === undefined)
          throw new Error("missingSignature: accepted artifact missing");
        const transaction = (
          await workflowStepTransaction(
            await builders.step02({
              materials,
              lucid: config.lucid,
              contracts: config.contracts,
              categoryId: config.category.categoryId,
              signer: config.signer,
              threadOutRef: stringField(input, "threadOutRef"),
              requiredSignerHashes: admitted.requiredSignerHashes,
              nativeTxCompactCbor: admitted.nativeTxCompactCbor,
              badRequiredSignerHashIndex: admitted.accusedRequiredSignerIndex,
              certificateUtxo:
                config.referenceScripts.fieldCertificates?.step02,
              referenceScriptUtxo: config.referenceScripts.steps[1],
            }),
            "missing-signature step02",
          )
        ).transaction;
        return Object.freeze({ transaction });
      }
      if (input.stage === "step_03") {
        if (admitted === undefined)
          throw new Error("missingSignature: accepted artifact missing");
        const transaction = (
          await workflowStepTransaction(
            await builders.step03({
              lucid: config.lucid,
              contracts: config.contracts,
              categoryId: config.category.categoryId,
              signer: config.signer,
              threadOutRef: stringField(input, "threadOutRef"),
              missingRequiredSignerVkey: admitted.resolvedVkey,
              referenceScriptUtxo: config.referenceScripts.steps[2],
            }),
            "missing-signature step03",
          )
        ).transaction;
        return Object.freeze({ transaction });
      }
      if (input.stage === "step_04") {
        if (admitted === undefined)
          throw new Error("missingSignature: accepted artifact missing");
        const transaction = (
          await workflowStepTransaction(
            await builders.step04({
              materials,
              lucid: config.lucid,
              contracts: config.contracts,
              categoryId: config.category.categoryId,
              signer: config.signer,
              threadOutRef: stringField(input, "threadOutRef"),
              addrTxWits: admitted.addrTxWits,
              nativeTxCompactCbor: admitted.nativeTxCompactCbor,
              witnessSetCompact: admitted.witnessSetCompact,
              certificateUtxo:
                config.referenceScripts.fieldCertificates?.step04,
              referenceScriptUtxo: config.referenceScripts.steps[3],
              witnessReferenceScripts: config.referenceScripts.witnesses,
            }),
            "missing-signature step04",
          )
        ).transaction;
        return Object.freeze({ transaction });
      }
      if (input.stage === "remove") {
        const nextRemovalOutRef = stringField(input, "nextRemovalOutRef");
        const fraudProofOutRef = stringField(input, "fraudProofOutRef");
        const { transaction, mutationLease } = await admitRemovalTransaction(
          await builders.remove({
            fraudProofOutRef,
            lucid: config.lucid,
            blueprint: config.blueprint,
            deploymentInfo: config.deploymentInfo,
            network: config.network,
            signer: config.signer,
            fraudCategory: "missingSignature",
            fraudulentHeaderHash: config.headerHash,
            requireReferenceScripts: true,
            stateQueueMutationLeaseCoordinator:
              config.stateQueueMutationLeaseCoordinator,
            fraudProverRewardLovelace: config.fraudProverRewardLovelace,
          }),
          async (built) => {
            if (
              !workflowTransactionInputOutRefs(built.signed).includes(
                nextRemovalOutRef,
              )
            ) {
              throw new Error(
                "missing-signature removal does not consume the authenticated next queue input",
              );
            }
            if (
              !workflowTransactionReferenceInputOutRefs(built.signed).includes(
                fraudProofOutRef,
              )
            ) {
              throw new Error(
                "missing-signature removal does not reference the authenticated retained proof token",
              );
            }
          },
        );
        return Object.freeze({
          transaction,
          ...(mutationLease === undefined ? {} : { mutationLease }),
        }) satisfies MissingSignatureCapturedAction;
      }
      throw new Error(
        `missing-signature workflow action has unsupported stage ${String(input.stage)}`,
      );
    },
  };
};

export type ManifestBoundMissingSignatureWorkflowConfig = Readonly<{
  manifest: unknown;
  blueprintJson: string;
  deploymentInfo: unknown;
  headerHash: string;
  lucid: LucidEvolution;
  signer: ResolvedProverSigner;
  referenceScripts: MissingSignatureWorkflowReferenceScripts;
  l1Source: FraudProofL1Source;
  stateQueueMutationLeaseCoordinator: StateQueueMutationLeaseCoordinator;
}>;

export type ManifestBoundMissingSignatureWorkflow = Readonly<{
  binding: FraudProofWorkflowDeploymentBinding<"missingSignature">;
  l1: FraudProofFamilyL1ObservationPort<"missingSignature">;
  transactions: MissingSignatureTransactionPort;
  adapter: FraudProofFamilyWorkflowAdapter;
  terminalVerifier: FraudProofWorkflowTerminalVerifier;
  releaseFinalityAuthority: FraudProofReleaseFinalityAuthority;
}>;
