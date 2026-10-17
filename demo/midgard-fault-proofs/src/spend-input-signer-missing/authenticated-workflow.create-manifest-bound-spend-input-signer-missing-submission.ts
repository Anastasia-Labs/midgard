import {
  PROOF_THREAD_SOURCE_KIND_ACCEPTED,
  PROOF_THREAD_SOURCE_KIND_FORCED,
} from "@al-ft/midgard-sdk";

import { submitCommittedFieldShapeInit } from "../committed-field-shape/submit-committed-field-shape-init.js";
import type { StateQueueMutationLeaseCoordinator } from "../remove-fraudulent-block.js";
import { submitRemoveFraudulentBlock } from "../remove-fraudulent-block.js";
import type { FraudProofPreSubmitBoundary } from "../workflow/transaction-boundary.js";
import {
  type ManifestBoundSpendInputSignerMissingConfig,
  required,
  type SpendInputSignerMissingRuntimeLoader,
} from "./authenticated-workflow.create-spend-input-signer-missing-bound-config.js";
import { createSpendInputSignerMissingCentralJournalAdapter } from "./central-journal.js";
import { type SpendInputSignerMissingEvidence } from "./spend-input-signer-missing.js";
import { submitSpendInputSignerMissingCancel } from "./submit-cancel.js";
import { submitSpendInputSignerMissingStep01Accepted } from "./submit-step-01-accepted.js";
import { submitSpendInputSignerMissingStep01Forced } from "./submit-step-01-forced.js";
import { submitSpendInputSignerMissingStep02 } from "./submit-step-02.js";
import { submitSpendInputSignerMissingStep03 } from "./submit-step-03.js";
import { submitSpendInputSignerMissingStep04 } from "./submit-step-04.js";
import { submitSpendInputSignerMissingStep05 } from "./submit-step-05.js";
import {
  type SpendInputSignerStage,
  spendInputSignerWorkflowEvidenceIdentity,
} from "./workflow.js";

export const createManifestBoundSpendInputSignerMissingSubmission = ({
  config,
  observe,
  resolveStage,
  centralJournal,
  preSubmitBoundary,
  stateQueueMutationLeaseCoordinator,
}: {
  readonly config: ManifestBoundSpendInputSignerMissingConfig;
  readonly observe: (identity: string) => Promise<SpendInputSignerStage>;
  readonly resolveStage: SpendInputSignerMissingRuntimeLoader["resolveStage"];
  readonly preSubmitBoundary?: FraudProofPreSubmitBoundary;
  readonly centralJournal?: ReturnType<
    typeof createSpendInputSignerMissingCentralJournalAdapter
  >;
  readonly stateQueueMutationLeaseCoordinator?: StateQueueMutationLeaseCoordinator;
}) => ({
  observe,
  submit: async (
    action:
      | "submitInit"
      | "submitStep01"
      | "submitStep02"
      | "submitStep03"
      | "submitScan"
      | "submitStep05"
      | "removeDescendants",
    evidence: SpendInputSignerMissingEvidence,
  ) => {
    if (evidence.subject.transaction_id.length !== 64)
      throw new Error(
        "spendInputSignerMissing evidence transaction id is not canonical",
      );
    const familyIdentity = spendInputSignerWorkflowEvidenceIdentity(evidence);
    const transition =
      action === "submitInit"
        ? (["none", "step01"] as const)
        : action === "submitStep01"
          ? (["step01", "step02"] as const)
          : action === "submitStep02"
            ? // The witness scan continues at step 03; a coordinate that
              // needs no signer closes at step 05 directly.
              evidence.signerRequired
              ? (["step02", "step03"] as const)
              : (["step02", "step05"] as const)
            : action === "submitStep03"
              ? (["step03", "scanning"] as const)
              : action === "submitScan"
                ? (["scanning", "step05"] as const)
                : action === "submitStep05"
                  ? (["step05", "proven"] as const)
                  : (["proven", "removed"] as const);
    await centralJournal?.begin(
      action,
      familyIdentity,
      transition[0],
      transition[1],
    );
    const stage = await resolveStage({ action, evidence });
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
        const result = await submitSpendInputSignerMissingStep01Accepted({
          lucid: config.lucid,
          blueprint: config.binding.blueprint,
          network: config.binding.network,
          contracts: config.contracts,
          signer: config.signer,
          evidence,
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
          "spendInputSignerMissing evidence source kind is invalid",
        );
      const result = await submitSpendInputSignerMissingStep01Forced({
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
      const result = await submitSpendInputSignerMissingStep02({
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
        certificateReferenceScriptUtxo:
          config.referenceScripts.fieldPreimageCertificateMint,
        membershipReferenceScriptUtxo:
          config.referenceScripts.witnesses.phasMembershipWithdraw,
        publicationBoundary:
          preSubmitBoundary ??
          centralJournal?.auxiliaryBoundary(
            "publication",
            familyIdentity,
            "step02",
            auxiliaryHashes,
          ),
        certificateBoundary:
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
        stage: result.stage,
        txHash: result.txHash,
        outputReference: result.nextThreadOutRef,
      };
    }
    if (action === "submitStep03") {
      const auxiliaryHashes: string[] = [];
      const result = await submitSpendInputSignerMissingStep03({
        lucid: config.lucid,
        network: config.binding.network,
        contracts: config.contracts,
        categoryId: config.binding.resolvedContracts.category.categoryId,
        signer: config.signer,
        threadOutRef: required(stage.threadOutRef, "step03 thread out-ref"),
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
        publicationBoundary:
          preSubmitBoundary ??
          centralJournal?.auxiliaryBoundary(
            "publication",
            familyIdentity,
            "step03",
            auxiliaryHashes,
          ),
        certificateBoundary:
          preSubmitBoundary ??
          centralJournal?.auxiliaryBoundary(
            "certificate",
            familyIdentity,
            "step03",
            auxiliaryHashes,
          ),
        onCarriageReady:
          centralJournal === undefined
            ? undefined
            : async () => {
                for (const txHash of auxiliaryHashes)
                  await centralJournal.confirmAuxiliary(txHash);
              },
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
        stage: "scanning" as const,
        txHash: result.txHash,
        outputReference: result.nextThreadOutRef,
      };
    }
    if (action === "submitScan") {
      const auxiliaryHashes: string[] = [];
      const result = await submitSpendInputSignerMissingStep04({
        lucid: config.lucid,
        network: config.binding.network,
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
        publicationBoundary:
          preSubmitBoundary ??
          centralJournal?.auxiliaryBoundary(
            "publication",
            familyIdentity,
            "scanning",
            auxiliaryHashes,
          ),
        certificateBoundary:
          preSubmitBoundary ??
          centralJournal?.auxiliaryBoundary(
            "certificate",
            familyIdentity,
            "scanning",
            auxiliaryHashes,
          ),
        onCarriageReady:
          centralJournal === undefined
            ? undefined
            : async () => {
                for (const txHash of auxiliaryHashes)
                  await centralJournal.confirmAuxiliary(txHash);
              },
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
      return {
        stage: result.stage,
        txHash: result.txHash,
        outputReference: result.nextThreadOutRef,
      };
    }
    if (action === "submitStep05") {
      const result = await submitSpendInputSignerMissingStep05({
        lucid: config.lucid,
        contracts: config.contracts,
        categoryId: config.binding.resolvedContracts.category.categoryId,
        signer: config.signer,
        threadOutRef: required(stage.threadOutRef, "step05 thread out-ref"),
        evidence,
        referenceScriptUtxo: config.referenceScripts.step05,
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
      fraudCategory: "spendInputSignerMissing",
      fraudulentHeaderHash: config.binding.definition.headerHash,
      requireReferenceScripts: true,
      stateQueueMutationLeaseCoordinator:
        stateQueueMutationLeaseCoordinator ??
        (() => {
          throw new Error(
            "spendInputSignerMissing production removal requires a state-queue mutation lease coordinator",
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
    current: "step01" | "step02" | "step03" | "scanning" | "step05",
    evidence: SpendInputSignerMissingEvidence,
  ) => {
    const stage = await resolveStage({ action: "cancel", evidence });
    const index =
      current === "step01"
        ? 0
        : current === "step02"
          ? 1
          : current === "step03"
            ? 2
            : current === "scanning"
              ? 3
              : 4;
    const result = await submitSpendInputSignerMissingCancel({
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
    });
    return {
      stage: "cancelled" as const,
      txHash: result.txHash,
      outputReference: null,
    };
  },
});
