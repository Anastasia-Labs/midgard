import type { LucidEvolution } from "@lucid-evolution/lucid";

import {
  admitMissingSignatureForcedArtifact,
  MISSING_SIGNATURE_FORCED_ARTIFACT,
} from "../missing-signature/forced-artifact.js";
import { submitMissingSignatureForcedAction } from "../missing-signature/submit-forced.js";
import {
  type StateQueueMutationLease,
  type StateQueueMutationLeaseCoordinator,
} from "../remove-fraudulent-block.js";
import type { ResolvedProverSigner } from "../runtime.js";
import { type FraudProofWorkflowDeploymentBinding } from "./deployment-manifest-binding.js";
import { type FraudProofFamilyL1ObservationPort } from "./family-l1-observation.js";
import type { LocalKupmiosHttpOgmiosSourceConfig } from "./local-kupmios-http-ogmios-source.js";
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
import type { FraudProofReleaseFinalityAuthority } from "./release-finality-policy.js";
import {
  captureLocallyEvaluatedTransaction,
  workflowTransactionInputOutRefs,
  workflowTransactionReferenceInputOutRefs,
} from "./transaction-boundary.js";

export const createBoundTransactionPort = ({
  config,
  builders,
}: {
  readonly config: BoundMissingSignatureTransactionsConfig;
  readonly builders: MissingSignatureBuilderSet;
}): MissingSignatureTransactionPort => ({
  portVersion: MISSING_SIGNATURE_TRANSACTION_PORT,
  category: "missingSignature",
  prepare: async ({ evidence, classification }) =>
    await prepareMissingSignatureWorkflowArtifact({
      evidence,
      classification,
    }),
  capture: async ({ action, artifact }) => {
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
    if (input.stage === "init") {
      const transaction = await captureLocallyEvaluatedTransaction(
        async (preSubmitBoundary) => {
          await builders.init({
            lucid: config.lucid,
            blueprint: config.blueprint,
            network: config.network,
            contracts: config.contracts,
            category: config.category,
            catalogue: config.catalogue,
            signer: config.signer,
            fraudulentBlockOutRef: stringField(input, "stateQueueBlockOutRef"),
            fraudulentHeaderHash: config.headerHash,
            witnessReferenceScripts: config.referenceScripts.witnesses,
            preSubmitBoundary,
            awaitConfirmation: false,
          });
        },
      );
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
      const transaction = await captureLocallyEvaluatedTransaction(
        async (preSubmitBoundary) => {
          await submitMissingSignatureForcedAction({
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
            preSubmitBoundary,
            awaitConfirmation: false,
          });
        },
      );
      return Object.freeze({ transaction });
    }
    if (input.stage === "step_01") {
      if (admitted === undefined)
        throw new Error("missingSignature: accepted artifact missing");
      const transaction = await captureLocallyEvaluatedTransaction(
        async (preSubmitBoundary) => {
          await builders.step01({
            lucid: config.lucid,
            blueprint: config.blueprint,
            network: config.network,
            contracts: config.contracts,
            categoryId: config.category.categoryId,
            signer: config.signer,
            threadOutRef: stringField(input, "threadOutRef"),
            stateQueueBlockOutRef: stringField(input, "stateQueueBlockOutRef"),
            txInclusion: admitted.txInclusion,
            referenceScriptUtxo: config.referenceScripts.steps[0],
            witnessReferenceScripts: config.referenceScripts.witnesses,
            preSubmitBoundary,
            awaitConfirmation: false,
          });
        },
      );
      return Object.freeze({ transaction });
    }
    if (input.stage === "step_02") {
      if (admitted === undefined)
        throw new Error("missingSignature: accepted artifact missing");
      const transaction = await captureLocallyEvaluatedTransaction(
        async (preSubmitBoundary) => {
          await builders.step02({
            lucid: config.lucid,
            contracts: config.contracts,
            categoryId: config.category.categoryId,
            signer: config.signer,
            threadOutRef: stringField(input, "threadOutRef"),
            requiredSignerHashes: admitted.requiredSignerHashes,
            nativeTxCompactCbor: admitted.nativeTxCompactCbor,
            badRequiredSignerHashIndex: admitted.accusedRequiredSignerIndex,
            certificateUtxo: config.referenceScripts.fieldCertificates?.step02,
            referenceScriptUtxo: config.referenceScripts.steps[1],
            preSubmitBoundary,
            awaitConfirmation: false,
          });
        },
      );
      return Object.freeze({ transaction });
    }
    if (input.stage === "step_03") {
      if (admitted === undefined)
        throw new Error("missingSignature: accepted artifact missing");
      const transaction = await captureLocallyEvaluatedTransaction(
        async (preSubmitBoundary) => {
          await builders.step03({
            lucid: config.lucid,
            contracts: config.contracts,
            categoryId: config.category.categoryId,
            signer: config.signer,
            threadOutRef: stringField(input, "threadOutRef"),
            missingRequiredSignerVkey: admitted.resolvedVkey,
            referenceScriptUtxo: config.referenceScripts.steps[2],
            preSubmitBoundary,
            awaitConfirmation: false,
          });
        },
      );
      return Object.freeze({ transaction });
    }
    if (input.stage === "step_04") {
      if (admitted === undefined)
        throw new Error("missingSignature: accepted artifact missing");
      const transaction = await captureLocallyEvaluatedTransaction(
        async (preSubmitBoundary) => {
          await builders.step04({
            lucid: config.lucid,
            contracts: config.contracts,
            categoryId: config.category.categoryId,
            signer: config.signer,
            threadOutRef: stringField(input, "threadOutRef"),
            addrTxWits: admitted.addrTxWits,
            nativeTxCompactCbor: admitted.nativeTxCompactCbor,
            witnessSetCompact: admitted.witnessSetCompact,
            certificateUtxo: config.referenceScripts.fieldCertificates?.step04,
            referenceScriptUtxo: config.referenceScripts.steps[3],
            witnessReferenceScripts: config.referenceScripts.witnesses,
            preSubmitBoundary,
            awaitConfirmation: false,
          });
        },
      );
      return Object.freeze({ transaction });
    }
    if (input.stage === "remove") {
      let mutationLease: StateQueueMutationLease | undefined;
      const retainingCoordinator: StateQueueMutationLeaseCoordinator = {
        acquire: async () => {
          const acquired =
            await config.stateQueueMutationLeaseCoordinator.acquire();
          mutationLease = acquired;
          return acquired;
        },
      };
      const nextRemovalOutRef = stringField(input, "nextRemovalOutRef");
      const fraudProofOutRef = stringField(input, "fraudProofOutRef");
      const transaction = await captureLocallyEvaluatedTransaction(
        async (boundary) => {
          await builders.remove({
            lucid: config.lucid,
            blueprint: config.blueprint,
            deploymentInfo: config.deploymentInfo,
            network: config.network,
            signer: config.signer,
            fraudCategory: "missingSignature",
            fraudulentHeaderHash: config.headerHash,
            requireReferenceScripts: true,
            stateQueueMutationLeaseCoordinator: retainingCoordinator,
            fraudProverRewardLovelace: config.fraudProverRewardLovelace,
            preSubmitBoundary: async (built) => {
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
                !workflowTransactionReferenceInputOutRefs(
                  built.signed,
                ).includes(fraudProofOutRef)
              ) {
                throw new Error(
                  "missing-signature removal does not reference the authenticated retained proof token",
                );
              }
              await boundary(built);
            },
          });
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
});

export type ManifestBoundMissingSignatureWorkflowConfig = Readonly<{
  manifest: unknown;
  blueprintJson: string;
  deploymentInfo: unknown;
  headerHash: string;
  lucid: LucidEvolution;
  signer: ResolvedProverSigner;
  referenceScripts: MissingSignatureWorkflowReferenceScripts;
  source: Omit<LocalKupmiosHttpOgmiosSourceConfig, "releaseFinality">;
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
