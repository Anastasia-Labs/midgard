import {
  decodeMidgardFieldPreimage,
  decodeMidgardNativeTxWitnessSetCompact,
} from "@al-ft/midgard-core";
import { PROOF_THREAD_SOURCE_KIND_ACCEPTED } from "@al-ft/midgard-sdk";

import type { StateQueueMutationLeaseCoordinator } from "../remove-fraudulent-block.js";
import { submitRemoveFraudulentBlock } from "../remove-fraudulent-block.js";
import type { FraudProofPreSubmitBoundary } from "../workflow/transaction-boundary.js";
import { createWitnessScriptDecodingCentralJournalAdapter } from "./central-journal.js";
import { submitWitnessScriptDecodingCancel } from "./submit-cancel.js";
import { submitWitnessScriptDecodingInit } from "./submit-init.js";
import { submitWitnessScriptDecodingStep01Accepted } from "./submit-step-01.js";
import { submitWitnessScriptDecodingStep01Forced } from "./submit-step-01.js";
import { submitWitnessScriptDecodingStep02 } from "./submit-step-02.js";
import { submitWitnessScriptDecodingStep03 } from "./submit-step-03.js";
import { submitWitnessScriptDecodingStep04 } from "./submit-step-04.js";
import {
  type WitnessScriptDecodingAction,
  type WitnessScriptDecodingEvidence,
  witnessScriptDecodingEvidenceIdentity,
  type WitnessScriptDecodingJournalEntry,
} from "./witness-script-decoding.js";
import {
  required,
  type WitnessScriptDecodingRuntimeLoader,
} from "./workflow.detect-witness-script-decoding-complete-replay.js";
import { type ManifestBoundWitnessScriptDecodingConfig } from "./workflow.witness-script-decoding-config-from-binding.js";

export const createManifestBoundWitnessScriptDecodingSubmission = ({
  config,
  observe,
  resolveStage,
  centralJournal,
  preSubmitBoundary,
  stateQueueMutationLeaseCoordinator,
}: {
  readonly config: ManifestBoundWitnessScriptDecodingConfig;
  readonly observe: (
    identity: string,
  ) => Promise<
    Pick<
      WitnessScriptDecodingJournalEntry,
      "stage" | "transactionId" | "outputReference" | "checkpointHash"
    >
  >;
  readonly resolveStage: WitnessScriptDecodingRuntimeLoader["resolveStage"];
  readonly preSubmitBoundary?: FraudProofPreSubmitBoundary;
  readonly centralJournal?: ReturnType<
    typeof createWitnessScriptDecodingCentralJournalAdapter
  >;
  readonly stateQueueMutationLeaseCoordinator?: StateQueueMutationLeaseCoordinator;
}) => ({
  observe,
  submit: async (
    action: Exclude<WitnessScriptDecodingAction, "done">,
    evidence: WitnessScriptDecodingEvidence,
  ) => {
    const familyIdentity = witnessScriptDecodingEvidenceIdentity(evidence);
    const fixedTransition =
      action === "submitInit"
        ? (["none", "step01"] as const)
        : action === "submitStep01"
          ? (["step01", "step02"] as const)
          : action === "submitStep02"
            ? (["step02", "scan"] as const)
            : action === "submitStep04"
              ? (["step04", "proven"] as const)
              : action === "removeDescendants"
                ? (["proven", "removed"] as const)
                : (["scan", "scan"] as const);
    if (action !== "submitScanOrResume")
      await centralJournal?.begin(
        action,
        familyIdentity,
        fixedTransition[0],
        fixedTransition[1],
      );
    const stage = await resolveStage({ action, evidence });
    const boundary =
      preSubmitBoundary ??
      centralJournal?.boundary(
        action,
        familyIdentity,
        fixedTransition[0],
        fixedTransition[1],
      );
    if (action === "submitInit") {
      const result = await submitWitnessScriptDecodingInit({
        lucid: config.lucid,
        blueprint: config.binding.blueprint,
        network: config.binding.network,
        contracts: config.contracts,
        category: config.binding.resolvedContracts.category,
        catalogue: config.binding.catalogue,
        signer: config.signer,
        fraudulentBlockOutRef: stage.fraudulentBlockOutRef,
        fraudulentHeaderHash: config.binding.definition.headerHash,
        witnessReferenceScripts: config.referenceScripts.witnesses,
        preSubmitBoundary: boundary,
      });
      return {
        stage: "step01" as const,
        transactionId: result.txHash,
        outputReference: result.nextThreadOutRef,
        checkpointHash: null,
      };
    }
    if (action === "submitStep01") {
      const subject = evidence.finding.subject;
      if (subject.source_kind === PROOF_THREAD_SOURCE_KIND_ACCEPTED) {
        const result = await submitWitnessScriptDecodingStep01Accepted({
          lucid: config.lucid,
          blueprint: config.binding.blueprint,
          network: config.binding.network,
          contracts: config.contracts,
          categoryId: config.binding.resolvedContracts.category.categoryId,
          signer: config.signer,
          threadOutRef: required(stage.threadOutRef, "step01 thread out-ref"),
          stateQueueBlockOutRef: required(
            stage.stateQueueBlockOutRef,
            "state-queue block out-ref",
          ),
          txInclusion: required(stage.acceptedInclusion, "accepted inclusion"),
          scriptIndex: BigInt(evidence.finding.scriptIndex),
          referenceScriptUtxo: config.referenceScripts.step01,
          witnessReferenceScripts: config.referenceScripts.witnesses,
          preSubmitBoundary: boundary,
        });
        return {
          stage: "step02" as const,
          transactionId: result.txHash,
          outputReference: result.nextThreadOutRef,
          checkpointHash: null,
        };
      }
      const result = await submitWitnessScriptDecodingStep01Forced({
        lucid: config.lucid,
        contracts: config.contracts,
        categoryId: config.binding.resolvedContracts.category.categoryId,
        signer: config.signer,
        threadOutRef: required(stage.threadOutRef, "step01 thread out-ref"),
        header: required(stage.forcedHeader, "forced header"),
        membership: required(stage.forcedMembership, "forced membership"),
        direction: required(stage.forcedDirection, "forced direction"),
        witnessSetHash: evidence.finding.witnessSetHash,
        scriptIndex: BigInt(evidence.finding.scriptIndex),
        referenceScriptUtxo: config.referenceScripts.step01,
        preSubmitBoundary: boundary,
      });
      return {
        stage: "step02" as const,
        transactionId: result.txHash,
        outputReference: result.nextThreadOutRef,
        checkpointHash: null,
      };
    }
    if (action === "submitStep02") {
      const auxiliaryHashes: string[] = [];
      const witnessSet = decodeMidgardNativeTxWitnessSetCompact(
        Buffer.from(
          required(stage.witnessSetCompactCbor, "witness-set compact CBOR"),
          "hex",
        ),
      );
      const result = await submitWitnessScriptDecodingStep02({
        lucid: config.lucid,
        network: config.binding.network,
        contracts: config.contracts,
        categoryId: config.binding.resolvedContracts.category.categoryId,
        signer: config.signer,
        threadOutRef: required(stage.threadOutRef, "step02 thread out-ref"),
        evidence,
        nativeTxCompactCbor: required(
          stage.nativeTxCompactCbor,
          "native transaction compact CBOR",
        ),
        witnessSetCompactCbor: required(
          stage.witnessSetCompactCbor,
          "witness-set compact CBOR",
        ),
        witnessSet: {
          addr_tx_wits_hash: witnessSet.addrTxWitsHash.toString("hex"),
          script_tx_wits_hash: witnessSet.scriptTxWitsHash.toString("hex"),
          redeemer_tx_wits_hash: witnessSet.redeemerTxWitsHash.toString("hex"),
        },
        scriptWitnessItems: decodeMidgardFieldPreimage(
          Buffer.from(evidence.fieldPreimageHex, "hex"),
        ),
        publishCarriage: evidence.carriage !== "Inline",
        publishedCarriageUtxos: stage.publishedCarriageUtxos,
        certificateUtxo: stage.certificateUtxo,
        certificateReferenceScriptUtxo:
          config.referenceScripts.fieldPreimageCertificateMint,
        publicationPreSubmitBoundary:
          preSubmitBoundary ??
          centralJournal?.auxiliaryBoundary(
            "publication",
            familyIdentity,
            "step02",
            auxiliaryHashes,
          ),
        certificatePreSubmitBoundary:
          preSubmitBoundary ??
          centralJournal?.auxiliaryBoundary(
            "certificate",
            familyIdentity,
            "step02",
            auxiliaryHashes,
          ),
        onCarriageReady:
          centralJournal === undefined
            ? undefined
            : async () => {
                for (const txHash of auxiliaryHashes)
                  await centralJournal.confirmAuxiliary(txHash);
              },
        referenceScriptUtxo: config.referenceScripts.step02,
        preSubmitBoundary: boundary,
      });
      return {
        stage: "scan" as const,
        transactionId: result.txHash,
        outputReference: result.nextThreadOutRef,
        checkpointHash: result.scanState.checkpoint_hash,
      };
    }
    if (action === "submitScanOrResume") {
      const result = await submitWitnessScriptDecodingStep03({
        lucid: config.lucid,
        contracts: config.contracts,
        categoryId: config.binding.resolvedContracts.category.categoryId,
        signer: config.signer,
        threadOutRef: required(stage.threadOutRef, "scan thread out-ref"),
        evidence,
        referenceScriptUtxo: config.referenceScripts.step03,
        preSubmitBoundaryForResult:
          preSubmitBoundary !== undefined
            ? async () => preSubmitBoundary
            : centralJournal === undefined
              ? undefined
              : async (closed) => {
                  const target = closed ? "step04" : "scan";
                  await centralJournal.begin(
                    action,
                    familyIdentity,
                    "scan",
                    target,
                  );
                  return centralJournal.boundary(
                    action,
                    familyIdentity,
                    "scan",
                    target,
                  );
                },
      });
      return {
        stage: result.closed ? ("step04" as const) : ("scan" as const),
        transactionId: result.txHash,
        outputReference: result.nextThreadOutRef,
        checkpointHash: result.scanState.checkpoint_hash,
      };
    }
    if (action === "submitStep04") {
      const result = await submitWitnessScriptDecodingStep04({
        lucid: config.lucid,
        contracts: config.contracts,
        categoryId: config.binding.resolvedContracts.category.categoryId,
        signer: config.signer,
        threadOutRef: required(stage.threadOutRef, "step04 thread out-ref"),
        evidence,
        referenceScriptUtxo: config.referenceScripts.step04,
        witnessReferenceScripts: config.referenceScripts.witnesses,
        preSubmitBoundary: boundary,
      });
      return {
        stage: "proven" as const,
        transactionId: result.txHash,
        outputReference: null,
        checkpointHash: null,
      };
    }
    const result = await submitRemoveFraudulentBlock({
      lucid: config.lucid,
      blueprint: config.binding.blueprint,
      deploymentInfo: config.binding.deploymentInfo,
      network: config.binding.network,
      signer: config.signer,
      fraudCategory: "witnessScriptDecoding",
      fraudulentHeaderHash: config.binding.definition.headerHash,
      requireReferenceScripts: true,
      stateQueueMutationLeaseCoordinator:
        stateQueueMutationLeaseCoordinator ??
        (() => {
          throw new Error(
            "witnessScriptDecoding production removal requires a state-queue mutation lease coordinator",
          );
        })(),
      awaitConfirmation: true,
      validFrom: stage.validFrom,
      validTo: stage.validTo,
      preSubmitBoundary: boundary,
    });
    return {
      stage: "removed" as const,
      transactionId: result.txHash,
      outputReference: null,
      checkpointHash: null,
    };
  },
  cancel: async (
    current: "step01" | "step02" | "scan" | "step04",
    evidence: WitnessScriptDecodingEvidence,
  ) => {
    const stage = await resolveStage({ action: "cancel", evidence });
    const index =
      current === "step01"
        ? 0
        : current === "step02"
          ? 1
          : current === "scan"
            ? 2
            : 3;
    const result = await submitWitnessScriptDecodingCancel({
      lucid: config.lucid,
      contracts: config.contracts,
      categoryId: config.binding.resolvedContracts.category.categoryId,
      signer: config.signer,
      threadOutRef: required(stage.threadOutRef, "cancel thread out-ref"),
      referenceScriptUtxo: [
        config.referenceScripts.step01,
        config.referenceScripts.step02,
        config.referenceScripts.step03,
        config.referenceScripts.step04,
      ][index]!,
      witnessReferenceScripts: config.referenceScripts.witnesses,
    });
    return {
      stage: "cancelled" as const,
      transactionId: result.txHash,
      outputReference: null,
      checkpointHash: null,
    };
  },
});
