import {
  type FraudProofCatalogueCategoryName,
  PROOF_THREAD_SOURCE_KIND_ACCEPTED,
  PROOF_THREAD_SOURCE_KIND_FORCED,
} from "@al-ft/midgard-sdk";

import { submitCommittedFieldShapeInit } from "../committed-field-shape/submit-committed-field-shape-init.js";
import type { StateQueueMutationLeaseCoordinator } from "../remove-fraudulent-block.js";
import { submitRemoveFraudulentBlock } from "../remove-fraudulent-block.js";
import {
  createOwnOutputJournal,
  type OwnOutputJournal,
} from "../workflow/own-outputs.js";
import type { FraudProofPreSubmitBoundary } from "../workflow/transaction-boundary.js";
import {
  required,
  type ResolvedOutputNonCanonicalRuntimeLoader,
} from "./authenticated-workflow.derive-resolved-output-non-canonical-authenticated-source.js";
import { type ManifestBoundResolvedOutputNonCanonicalConfig } from "./authenticated-workflow.resolved-output-non-canonical-config-from-binding.js";
import { type ResolvedOutputEvidence } from "./resolved-output-non-canonical.js";
import { submitResolvedOutputNonCanonicalCancel } from "./submit-cancel.js";
import { submitResolvedOutputNonCanonicalStep01Accepted } from "./submit-step-01-accepted.js";
import { submitResolvedOutputNonCanonicalStep01Forced } from "./submit-step-01-forced.js";
import { submitResolvedOutputNonCanonicalStep02 } from "./submit-step-02.js";
import { submitResolvedOutputNonCanonicalStep03 } from "./submit-step-03.js";
import { submitResolvedOutputNonCanonicalStep04 } from "./submit-step-04.js";
import { submitResolvedOutputNonCanonicalStep04Datum } from "./submit-step-04-datum.js";
import { submitResolvedOutputNonCanonicalStep05 } from "./submit-step-05.js";
import { type ResolvedOutputStage } from "./workflow.js";

export const createManifestBoundResolvedOutputNonCanonicalSubmission = ({
  config,
  observe,
  resolveStage,
  preSubmitBoundary,
  stateQueueMutationLeaseCoordinator,
  ownOutputs = createOwnOutputJournal(),
}: {
  readonly config: ManifestBoundResolvedOutputNonCanonicalConfig;
  readonly observe: (identity: string) => Promise<ResolvedOutputStage>;
  readonly resolveStage: ResolvedOutputNonCanonicalRuntimeLoader["resolveStage"];
  readonly preSubmitBoundary?: FraudProofPreSubmitBoundary;
  readonly stateQueueMutationLeaseCoordinator?: StateQueueMutationLeaseCoordinator;
  /** Our carriage claims; a fresh in-memory journal by default. */
  readonly ownOutputs?: OwnOutputJournal;
}) => ({
  observe,
  submit: async (
    action:
      | "submitInit"
      | "submitStep01"
      | "submitStep02"
      | "submitStep03"
      | "submitReconstruction"
      | "submitStep04Datum"
      | "submitStep05"
      | "removeDescendants",
    evidence: ResolvedOutputEvidence,
  ) => {
    if (evidence.subject.transaction_id.length !== 64)
      throw new Error(
        "resolvedOutputNonCanonical evidence transaction id is not canonical",
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
        preSubmitBoundary,
      });
      return {
        stage: "step01" as const,
        txHash: result.txHash,
        outputReference: `${result.txHash}#${result.firstStepOutputIndex.toString()}`,
      };
    }
    if (action === "submitStep01") {
      if (evidence.subject.source_kind === PROOF_THREAD_SOURCE_KIND_ACCEPTED) {
        const result = await submitResolvedOutputNonCanonicalStep01Accepted({
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
          preSubmitBoundary,
        });
        return {
          stage: "step02" as const,
          txHash: result.txHash,
          outputReference: result.nextThreadOutRef,
        };
      }
      if (evidence.subject.source_kind !== PROOF_THREAD_SOURCE_KIND_FORCED)
        throw new Error(
          "resolvedOutputNonCanonical evidence source kind is invalid",
        );
      const result = await submitResolvedOutputNonCanonicalStep01Forced({
        lucid: config.lucid,
        contracts: config.contracts,
        categoryId: config.binding.resolvedContracts.category.categoryId,
        signer: config.signer,
        threadOutRef: required(stage.threadOutRef, "step01 thread out-ref"),
        finding: evidence,
        forcedSource: {
          header: required(stage.forcedHeader, "forced header"),
          membership: required(stage.forcedMembership, "forced membership"),
        },
        referenceScriptUtxo: config.referenceScripts.step01,
        preSubmitBoundary,
      });
      return {
        stage: "step02" as const,
        txHash: result.txHash,
        outputReference: result.nextThreadOutRef,
      };
    }
    if (action === "submitStep02") {
      const result = await submitResolvedOutputNonCanonicalStep02({
        ownOutputs,
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
        publishCarriage: evidence.carriage === "RawUtxo",
        publishedCarriageUtxos: stage.publishedCarriageUtxos,
        certificateUtxo: stage.certificateUtxo,
        certificateReferenceScriptUtxo:
          config.referenceScripts.fieldPreimageCertificateMint,
        publicationPreSubmitBoundary: preSubmitBoundary,
        certificatePreSubmitBoundary: preSubmitBoundary,
        referenceScriptUtxo: config.referenceScripts.step02,
        preSubmitBoundary,
      });
      return {
        stage: "step03" as const,
        txHash: result.txHash,
        outputReference: result.nextThreadOutRef,
      };
    }
    if (action === "submitStep03") {
      const result = await submitResolvedOutputNonCanonicalStep03({
        lucid: config.lucid,
        network: config.binding.network,
        contracts: config.contracts,
        categoryId: config.binding.resolvedContracts.category.categoryId,
        signer: config.signer,
        threadOutRef: required(stage.threadOutRef, "step03 thread out-ref"),
        evidence,
        referenceScriptUtxo: config.referenceScripts.step03,
        witnessReferenceScripts: config.referenceScripts.witnesses,
        preSubmitBoundary,
      });
      return {
        stage: "reconstructing" as const,
        txHash: result.txHash,
        outputReference: result.nextThreadOutRef,
      };
    }
    if (action === "submitReconstruction") {
      const result = await submitResolvedOutputNonCanonicalStep04({
        lucid: config.lucid,
        contracts: config.contracts,
        categoryId: config.binding.resolvedContracts.category.categoryId,
        signer: config.signer,
        threadOutRef: required(
          stage.threadOutRef,
          "reconstruction thread out-ref",
        ),
        evidence,
        referenceScriptUtxo: config.referenceScripts.step04,
        preSubmitBoundary,
      });
      return {
        stage: result.nextStage,
        txHash: result.txHash,
        outputReference: result.nextThreadOutRef,
      };
    }
    if (action === "submitStep04Datum") {
      const result = await submitResolvedOutputNonCanonicalStep04Datum({
        lucid: config.lucid,
        contracts: config.contracts,
        categoryId: config.binding.resolvedContracts.category.categoryId,
        signer: config.signer,
        threadOutRef: required(stage.threadOutRef, "datum thread out-ref"),
        evidence,
        referenceScriptUtxo: config.referenceScripts.step04Datum,
        preSubmitBoundary,
      });
      return {
        stage: result.nextStage,
        txHash: result.txHash,
        outputReference: result.nextThreadOutRef,
      };
    }
    if (action === "submitStep05") {
      const result = await submitResolvedOutputNonCanonicalStep05({
        lucid: config.lucid,
        contracts: config.contracts,
        categoryId: config.binding.resolvedContracts.category.categoryId,
        signer: config.signer,
        threadOutRef: required(stage.threadOutRef, "step05 thread out-ref"),
        evidence,
        referenceScriptUtxo: config.referenceScripts.step05,
        witnessReferenceScripts: config.referenceScripts.witnesses,
        preSubmitBoundary,
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
      fraudCategory:
        "resolvedOutputNonCanonical" as FraudProofCatalogueCategoryName,
      fraudulentHeaderHash: config.binding.definition.headerHash,
      requireReferenceScripts: true,
      stateQueueMutationLeaseCoordinator:
        stateQueueMutationLeaseCoordinator ??
        (() => {
          throw new Error(
            "resolvedOutputNonCanonical production removal requires a state-queue mutation lease coordinator",
          );
        })(),
      awaitConfirmation: true,
      validFrom: stage.validFrom,
      validTo: stage.validTo,
      preSubmitBoundary,
    });
    return {
      stage: "removed" as const,
      txHash: result.txHash,
      outputReference: null,
    };
  },
  cancel: async (
    current: "step01" | "step02" | "step03" | "reconstructing" | "step04Datum",
    evidence: ResolvedOutputEvidence,
  ) => {
    const stage = await resolveStage({ action: "cancel", evidence });
    const index =
      current === "step01"
        ? 0
        : current === "step02"
          ? 1
          : current === "step03"
            ? 2
            : current === "reconstructing"
              ? 3
              : 4;
    const result = await submitResolvedOutputNonCanonicalCancel({
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
        config.referenceScripts.step04Datum,
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
