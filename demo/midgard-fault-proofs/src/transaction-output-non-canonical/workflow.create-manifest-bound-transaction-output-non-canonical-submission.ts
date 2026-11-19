import {
  type FraudProofCatalogueCategoryName,
  PROOF_THREAD_SOURCE_KIND_ACCEPTED,
  PROOF_THREAD_SOURCE_KIND_FORCED,
} from "@al-ft/midgard-sdk";

import { buildCommittedFieldShapeInit } from "../committed-field-shape/submit-committed-field-shape-init.js";
import { isPrerequisiteTurn } from "../fault-proof-turn.js";
import type { StateQueueMutationLeaseCoordinator } from "../remove-fraudulent-block.js";
import { buildRemoveFraudulentBlock } from "../remove-fraudulent-block.js";
import {
  createOwnOutputJournal,
  type OwnOutputJournal,
} from "../workflow/own-outputs.js";
import { buildTransactionOutputNonCanonicalCancel } from "./submit-cancel.js";
import { buildTransactionOutputNonCanonicalStep01Accepted } from "./submit-step-01-accepted.js";
import { buildTransactionOutputNonCanonicalStep01Forced } from "./submit-step-01-forced.js";
import { buildTransactionOutputNonCanonicalStep02 } from "./submit-step-02.js";
import { buildTransactionOutputNonCanonicalStep03 } from "./submit-step-03.js";
import { buildTransactionOutputNonCanonicalStep03Datum } from "./submit-step-03-datum.js";
import { buildTransactionOutputNonCanonicalStep04 } from "./submit-step-04.js";
import {
  type TransactionOutputEvidence,
  type TransactionOutputStage,
} from "./transaction-output-non-canonical.js";
import {
  required,
  type TransactionOutputNonCanonicalRuntimeLoader,
} from "./workflow.detect-transaction-output-non-canonical-complete-replay.js";
import { type ManifestBoundTransactionOutputNonCanonicalConfig } from "./workflow.transaction-output-non-canonical-config-from-binding.js";

export const createManifestBoundTransactionOutputNonCanonicalSubmission = ({
  config,
  observe,
  resolveStage,
  stateQueueMutationLeaseCoordinator,
  ownOutputs = createOwnOutputJournal(),
}: {
  readonly config: ManifestBoundTransactionOutputNonCanonicalConfig;
  readonly observe: (identity: string) => Promise<TransactionOutputStage>;
  readonly resolveStage: TransactionOutputNonCanonicalRuntimeLoader["resolveStage"];
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
      | "submitStep03Datum"
      | "submitStep04"
      | "removeDescendants",
    evidence: TransactionOutputEvidence,
  ) => {
    if (evidence.subject.transaction_id.length !== 64)
      throw new Error(
        "transactionOutputNonCanonical evidence transaction id is not canonical",
      );
    const stage = await resolveStage({ action, evidence });
    if (action === "submitInit") {
      const result = await buildCommittedFieldShapeInit({
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
      });
      return {
        transaction: result.transaction,
        stage: "step01" as const,
        txHash: result.transaction.txHash,
        outputReference: `${result.transaction.txHash}#${result.firstStepOutputIndex.toString()}`,
      };
    }
    if (action === "submitStep01") {
      if (evidence.subject.source_kind === PROOF_THREAD_SOURCE_KIND_ACCEPTED) {
        const result = await buildTransactionOutputNonCanonicalStep01Accepted({
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
        });
        return {
          transaction: result.transaction,
          stage: "step02" as const,
          txHash: result.transaction.txHash,
          outputReference: result.nextThreadOutRef,
        };
      }
      if (evidence.subject.source_kind !== PROOF_THREAD_SOURCE_KIND_FORCED)
        throw new Error(
          "transactionOutputNonCanonical evidence source kind is invalid",
        );
      const result = await buildTransactionOutputNonCanonicalStep01Forced({
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
      });
      return {
        transaction: result.transaction,
        stage: "step02" as const,
        txHash: result.transaction.txHash,
        outputReference: result.nextThreadOutRef,
      };
    }
    if (action === "submitStep02") {
      const result = await buildTransactionOutputNonCanonicalStep02({
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
        fieldGrammar: config.fieldGrammar,
        referenceScriptUtxo: config.referenceScripts.step02,
      });
      if (isPrerequisiteTurn(result)) return result;
      return {
        transaction: result.transaction,
        stage: result.nextStage,
        txHash: result.transaction.txHash,
        outputReference: result.nextThreadOutRef,
      };
    }
    if (action === "submitStep03") {
      const result = await buildTransactionOutputNonCanonicalStep03({
        lucid: config.lucid,
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
        publishCarriage: evidence.carriage === "RawUtxo",
        publishedCarriageUtxos: stage.publishedCarriageUtxos,
        certificateUtxo: stage.certificateUtxo,
        certificateReferenceScriptUtxo:
          config.referenceScripts.fieldPreimageCertificateMint,
        referenceScriptUtxo: config.referenceScripts.step03,
      });
      return {
        transaction: result.transaction,
        stage: result.nextStage,
        txHash: result.transaction.txHash,
        outputReference: result.nextThreadOutRef,
      };
    }
    if (action === "submitStep03Datum") {
      const result = await buildTransactionOutputNonCanonicalStep03Datum({
        lucid: config.lucid,
        contracts: config.contracts,
        categoryId: config.binding.resolvedContracts.category.categoryId,
        signer: config.signer,
        threadOutRef: required(
          stage.threadOutRef,
          "step03 datum thread out-ref",
        ),
        evidence,
        referenceScriptUtxo: config.referenceScripts.step03Datum,
      });
      return {
        transaction: result.transaction,
        stage: result.nextStage,
        txHash: result.transaction.txHash,
        outputReference: result.nextThreadOutRef,
      };
    }
    if (action === "submitStep04") {
      const result = await buildTransactionOutputNonCanonicalStep04({
        lucid: config.lucid,
        contracts: config.contracts,
        categoryId: config.binding.resolvedContracts.category.categoryId,
        signer: config.signer,
        threadOutRef: required(stage.threadOutRef, "step04 thread out-ref"),
        evidence,
        referenceScriptUtxo: config.referenceScripts.step04,
        witnessReferenceScripts: config.referenceScripts.witnesses,
      });
      return {
        transaction: result.transaction,
        stage: "proven" as const,
        txHash: result.transaction.txHash,
        outputReference: null,
      };
    }
    const result = await buildRemoveFraudulentBlock({
      lucid: config.lucid,
      blueprint: config.binding.blueprint,
      deploymentInfo: config.binding.deploymentInfo,
      network: config.binding.network,
      signer: config.signer,
      fraudCategory:
        "transactionOutputNonCanonical" as FraudProofCatalogueCategoryName,
      fraudulentHeaderHash: config.binding.definition.headerHash,
      requireReferenceScripts: true,
      stateQueueMutationLeaseCoordinator:
        stateQueueMutationLeaseCoordinator ??
        (() => {
          throw new Error(
            "transactionOutputNonCanonical production removal requires a state-queue mutation lease coordinator",
          );
        })(),
      validFrom: stage.validFrom,
      validTo: stage.validTo,
    });
    return {
      transaction: result.transaction,
      stateQueueMutationLease: result.stateQueueMutationLease,
      // A removal of a successor leaves the fraud proof in place.
      stage:
        result.kind === "remove-target"
          ? ("removed" as const)
          : ("proven" as const),
      txHash: result.transaction.txHash,
      outputReference: null,
    };
  },
  cancel: async (
    current: "step01" | "step02" | "step03" | "step03Datum" | "step04",
    evidence: TransactionOutputEvidence,
  ) => {
    const stage = await resolveStage({ action: "cancel", evidence });
    const index = (
      ["step01", "step02", "step03", "step03Datum", "step04"] as const
    ).indexOf(current);
    const result = await buildTransactionOutputNonCanonicalCancel({
      lucid: config.lucid,
      contracts: config.contracts,
      categoryId: config.binding.resolvedContracts.category.categoryId,
      signer: config.signer,
      threadOutRef: required(stage.threadOutRef, "cancel thread out-ref"),
      referenceScriptUtxo: [
        config.referenceScripts.step01,
        config.referenceScripts.step02,
        config.referenceScripts.step03,
        config.referenceScripts.step03Datum,
        config.referenceScripts.step04,
      ][index]!,
      witnessReferenceScripts: config.referenceScripts.witnesses,
    });
    return {
      transaction: result.transaction,
      stage: "cancelled" as const,
      txHash: result.transaction.txHash,
      outputReference: null,
    };
  },
});
