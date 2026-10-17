import {
  PROOF_THREAD_SOURCE_KIND_ACCEPTED,
  PROOF_THREAD_SOURCE_KIND_FORCED,
} from "@al-ft/midgard-sdk";

import { submitCommittedFieldShapeInit } from "../committed-field-shape/submit-committed-field-shape-init.js";
import type { StateQueueMutationLeaseCoordinator } from "../remove-fraudulent-block.js";
import { submitRemoveFraudulentBlock } from "../remove-fraudulent-block.js";
import type { FraudProofPreSubmitBoundary } from "../workflow/transaction-boundary.js";
import {
  type ManifestBoundOutputReferenceScriptDecodingConfig,
  type OutputReferenceScriptDecodingRuntimeLoader,
  required,
} from "./authenticated-workflow.create-output-reference-script-decoding-bound-config.js";
import {
  outputReferenceScriptDecodingOutputScanTarget,
  outputReferenceScriptDecodingStructuralTarget,
} from "./authenticated-workflow.create-output-reference-script-decoding-raw-l1-stage-resolver.js";
import { createOutputReferenceScriptDecodingCentralJournalAdapter } from "./central-journal.js";
import { type OutputReferenceScriptDecodingEvidence } from "./output-reference-script-decoding.js";
import { submitOutputReferenceScriptDecodingCancel } from "./submit-cancel.js";
import { submitOutputReferenceScriptDecodingStep01Accepted } from "./submit-step-01-accepted.js";
import { submitOutputReferenceScriptDecodingStep01Forced } from "./submit-step-01-forced.js";
import { submitOutputReferenceScriptDecodingStep02 } from "./submit-step-02.js";
import { submitOutputReferenceScriptDecodingStep03 } from "./submit-step-03.js";
import { submitOutputReferenceScriptDecodingStep04 } from "./submit-step-04.js";
import { submitOutputReferenceScriptDecodingStep05 } from "./submit-step-05.js";
import { submitOutputReferenceScriptDecodingStep06 } from "./submit-step-06.js";
import {
  outputReferenceScriptDecodingEvidenceIdentity,
  type OutputReferenceScriptDecodingStage,
} from "./workflow.js";

export const createManifestBoundOutputReferenceScriptDecodingSubmission = ({
  config,
  observe,
  resolveStage,
  centralJournal,
  preSubmitBoundary,
  stateQueueMutationLeaseCoordinator,
}: {
  readonly config: ManifestBoundOutputReferenceScriptDecodingConfig;
  readonly observe: (
    identity: string,
  ) => Promise<OutputReferenceScriptDecodingStage>;
  readonly resolveStage: OutputReferenceScriptDecodingRuntimeLoader["resolveStage"];
  readonly preSubmitBoundary?: FraudProofPreSubmitBoundary;
  readonly centralJournal?: ReturnType<
    typeof createOutputReferenceScriptDecodingCentralJournalAdapter
  >;
  readonly stateQueueMutationLeaseCoordinator?: StateQueueMutationLeaseCoordinator;
}) => ({
  observe,
  submit: async (
    action:
      | "submitInit"
      | "submitStep01"
      | "submitStep02"
      | "submitOutputScan"
      | "submitReferenceBind"
      | "submitStructuralScan"
      | "submitStep06"
      | "removeDescendants",
    evidence: OutputReferenceScriptDecodingEvidence,
  ) => {
    if (evidence.subject.transaction_id.length !== 64)
      throw new Error(
        "outputReferenceScriptDecoding evidence transaction id is not canonical",
      );
    const familyIdentity =
      outputReferenceScriptDecodingEvidenceIdentity(evidence);
    const stage = await resolveStage({ action, evidence });
    const transition =
      action === "submitInit"
        ? (["none", "step01"] as const)
        : action === "submitStep01"
          ? (["step01", "step02"] as const)
          : action === "submitStep02"
            ? (["step02", "outputScan"] as const)
            : action === "submitOutputScan"
              ? ([
                  "outputScan",
                  outputReferenceScriptDecodingOutputScanTarget({
                    stage,
                    evidence,
                  }),
                ] as const)
              : action === "submitReferenceBind"
                ? (["referenceBind", "scan"] as const)
                : action === "submitStructuralScan"
                  ? ([
                      "scan",
                      outputReferenceScriptDecodingStructuralTarget({
                        stage,
                        evidence,
                      }),
                    ] as const)
                  : action === "submitStep06"
                    ? (["step06", "proven"] as const)
                    : (["proven", "removed"] as const);
    await centralJournal?.begin(
      action,
      familyIdentity,
      transition[0],
      transition[1],
    );
    if (action === "submitInit") {
      const result = await submitCommittedFieldShapeInit({
        lucid: config.lucid,
        blueprint: config.binding.blueprint,
        network: config.binding.network,
        contracts: config.contracts as never,
        category: config.binding.resolvedContracts.category,
        catalogue: config.binding.catalogue,
        signer: config.signer,
        fraudulentBlockOutRef: stage.fraudulentBlockOutRef,
        fraudulentHeaderHash: config.binding.definition.headerHash,
        witnessReferenceScripts: config.referenceScripts.witnesses,
        preSubmitBoundary:
          preSubmitBoundary ??
          centralJournal?.boundary(
            action,
            familyIdentity,
            transition[0],
            transition[1],
          ),
      });
      return {
        stage: "step01" as const,
        txHash: result.txHash,
        outputReference: `${result.txHash}#${result.firstStepOutputIndex.toString()}`,
      };
    }
    if (action === "submitStep01") {
      if (evidence.subject.source_kind === PROOF_THREAD_SOURCE_KIND_ACCEPTED) {
        const result = await submitOutputReferenceScriptDecodingStep01Accepted({
          lucid: config.lucid,
          blueprint: config.binding.blueprint,
          network: config.binding.network,
          contracts: config.contracts,
          signer: config.signer,
          finding: evidence,
          threadUtxo: required(stage.threadUtxo, "step01 thread UTxO"),
          threadToken: required(stage.threadToken, "step01 thread token"),
          stateQueueBlockOutRef: required(
            stage.stateQueueBlockOutRef,
            "state-queue block out-ref",
          ),
          txInclusion: required(stage.acceptedInclusion, "accepted inclusion"),
          referenceScriptUtxo: config.referenceScripts.step01,
          witnessReferenceScripts: config.referenceScripts.witnesses,
          preSubmitBoundary:
            preSubmitBoundary ??
            centralJournal?.boundary(
              action,
              familyIdentity,
              transition[0],
              transition[1],
            ),
        });
        return {
          stage: "step02" as const,
          txHash: result.txHash,
          outputReference: result.nextThreadOutRef,
        };
      }
      if (evidence.subject.source_kind !== PROOF_THREAD_SOURCE_KIND_FORCED)
        throw new Error(
          "outputReferenceScriptDecoding evidence source kind is invalid",
        );
      const result = await submitOutputReferenceScriptDecodingStep01Forced({
        lucid: config.lucid,
        contracts: config.contracts,
        categoryId: config.binding.resolvedContracts.category.categoryId,
        signer: config.signer,
        threadOutRef: required(stage.threadOutRef, "step01 thread out-ref"),
        evidence,
        forcedSource: {
          header: required(stage.forcedHeader, "forced header"),
          membership: required(stage.forcedMembership, "forced membership"),
          direction: required(stage.forcedDirection, "forced direction"),
        },
        referenceScriptUtxo: config.referenceScripts.step01,
        preSubmitBoundary:
          preSubmitBoundary ??
          centralJournal?.boundary(
            action,
            familyIdentity,
            transition[0],
            transition[1],
          ),
      });
      return {
        stage: "step02" as const,
        txHash: result.txHash,
        outputReference: result.nextThreadOutRef,
      };
    }
    if (action === "submitStep02") {
      const auxiliaryHashes: string[] = [];
      const result = await submitOutputReferenceScriptDecodingStep02({
        lucid: config.lucid,
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
        publishCarriage: true,
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
        referenceScriptUtxo: config.referenceScripts.step02,
        preSubmitBoundary:
          preSubmitBoundary ??
          centralJournal?.boundary(
            action,
            familyIdentity,
            transition[0],
            transition[1],
          ),
      });
      for (const txHash of auxiliaryHashes)
        await centralJournal?.confirmAuxiliary(txHash);
      return {
        stage: "outputScan" as const,
        txHash: result.txHash,
        outputReference: result.nextThreadOutRef,
      };
    }
    if (action === "submitOutputScan") {
      const result = await submitOutputReferenceScriptDecodingStep03({
        lucid: config.lucid,
        contracts: config.contracts,
        categoryId: config.binding.resolvedContracts.category.categoryId,
        signer: config.signer,
        threadOutRef: required(stage.threadOutRef, "step03 thread out-ref"),
        evidence,
        referenceScriptUtxo: config.referenceScripts.step03,
        preSubmitBoundary:
          preSubmitBoundary ??
          centralJournal?.boundary(
            action,
            familyIdentity,
            transition[0],
            transition[1],
          ),
      });
      return {
        stage: result.terminal
          ? ("referenceBind" as const)
          : ("outputScan" as const),
        txHash: result.txHash,
        outputReference: result.nextThreadOutRef,
      };
    }
    if (action === "submitReferenceBind") {
      const auxiliaryHashes: string[] = [];
      const result = await submitOutputReferenceScriptDecodingStep04({
        lucid: config.lucid,
        contracts: config.contracts,
        categoryId: config.binding.resolvedContracts.category.categoryId,
        signer: config.signer,
        threadOutRef: required(
          stage.threadOutRef,
          "reconstruction thread out-ref",
        ),
        evidence,
        nativeTxCompactCbor: required(
          stage.nativeTxCompactCbor,
          "native transaction compact CBOR",
        ),
        witnessSetCompactCbor: required(
          stage.witnessSetCompactCbor,
          "witness-set compact CBOR",
        ),
        certificateReferenceScriptUtxo:
          config.referenceScripts.fieldPreimageCertificateMint,
        publishCarriage: true,
        publicationPreSubmitBoundary:
          preSubmitBoundary ??
          centralJournal?.auxiliaryBoundary(
            "publication",
            familyIdentity,
            "referenceBind",
            auxiliaryHashes,
          ),
        certificatePreSubmitBoundary:
          preSubmitBoundary ??
          centralJournal?.auxiliaryBoundary(
            "certificate",
            familyIdentity,
            "referenceBind",
            auxiliaryHashes,
          ),
        referenceScriptUtxo: config.referenceScripts.step04,
        preSubmitBoundary:
          preSubmitBoundary ??
          centralJournal?.boundary(
            action,
            familyIdentity,
            transition[0],
            transition[1],
          ),
      });
      for (const txHash of auxiliaryHashes)
        await centralJournal?.confirmAuxiliary(txHash);
      return {
        stage: "scan" as const,
        txHash: result.txHash,
        outputReference: result.nextThreadOutRef,
      };
    }
    if (action === "submitStructuralScan") {
      const result = await submitOutputReferenceScriptDecodingStep05({
        lucid: config.lucid,
        contracts: config.contracts,
        categoryId: config.binding.resolvedContracts.category.categoryId,
        signer: config.signer,
        threadOutRef: required(stage.threadOutRef, "step05 thread out-ref"),
        evidence,
        referenceScriptUtxo: config.referenceScripts.step05,
        preSubmitBoundary:
          preSubmitBoundary ??
          centralJournal?.boundary(
            action,
            familyIdentity,
            transition[0],
            transition[1],
          ),
      });
      return {
        stage: result.closed ? ("step06" as const) : ("scan" as const),
        txHash: result.txHash,
        outputReference: result.nextThreadOutRef,
      };
    }
    if (action === "submitStep06") {
      const result = await submitOutputReferenceScriptDecodingStep06({
        lucid: config.lucid,
        contracts: config.contracts,
        categoryId: config.binding.resolvedContracts.category.categoryId,
        signer: config.signer,
        threadOutRef: required(stage.threadOutRef, "step06 thread out-ref"),
        evidence,
        referenceScriptUtxo: config.referenceScripts.step06,
        witnessReferenceScripts: config.referenceScripts.witnesses,
        preSubmitBoundary:
          preSubmitBoundary ??
          centralJournal?.boundary(
            action,
            familyIdentity,
            transition[0],
            transition[1],
          ),
      });
      return {
        stage: "proven" as const,
        txHash: result.txHash,
        outputReference: null,
      };
    }
    const result = await submitRemoveFraudulentBlock({
      lucid: config.lucid,
      blueprint: config.binding.blueprint,
      deploymentInfo: config.binding.deploymentInfo,
      network: config.binding.network,
      signer: config.signer,
      fraudCategory: "outputReferenceScriptDecoding",
      fraudulentHeaderHash: config.binding.definition.headerHash,
      requireReferenceScripts: true,
      stateQueueMutationLeaseCoordinator:
        stateQueueMutationLeaseCoordinator ??
        (() => {
          throw new Error(
            "outputReferenceScriptDecoding production removal requires a state-queue mutation lease coordinator",
          );
        })(),
      awaitConfirmation: true,
      validFrom: stage.validFrom,
      validTo: stage.validTo,
      preSubmitBoundary:
        preSubmitBoundary ??
        centralJournal?.boundary(
          action,
          familyIdentity,
          transition[0],
          transition[1],
        ),
    });
    return {
      stage: "removed" as const,
      txHash: result.txHash,
      outputReference: null,
    };
  },
  cancel: async (
    current: "step01" | "step02" | "outputScan" | "referenceBind" | "scan",
    evidence: OutputReferenceScriptDecodingEvidence,
  ) => {
    const stage = await resolveStage({
      action: "cancel",
      evidence,
      currentStage: current,
    });
    const index =
      current === "step01"
        ? 0
        : current === "step02"
          ? 1
          : current === "outputScan"
            ? 2
            : current === "referenceBind"
              ? 3
              : 4;
    const result = await submitOutputReferenceScriptDecodingCancel({
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
        config.referenceScripts.step05,
      ][index]!,
      witnessReferenceScripts: config.referenceScripts.witnesses,
      preSubmitBoundary:
        preSubmitBoundary ??
        centralJournal?.boundary(
          "cancel",
          outputReferenceScriptDecodingEvidenceIdentity(evidence),
          current,
          "cancelled",
        ),
    });
    return {
      stage: "cancelled" as const,
      txHash: result.txHash,
      outputReference: null,
    };
  },
});
