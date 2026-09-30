import {
  decodeMidgardFieldPreimage,
  deriveMidgardNativeTxFaultEvidenceMaterial,
  midgardFieldCommitment,
} from "@al-ft/midgard-core";
import { deriveMidgardForcedTxFaultEvidenceMaterial } from "@al-ft/midgard-core/codec/forced";
import {
  acceptedVerdictSubject,
  forcedVerdictSubject,
  type FraudProofCatalogueCategoryName,
  PROOF_THREAD_SOURCE_KIND_FORCED,
} from "@al-ft/midgard-sdk";

import { type CanonicalBlockEvidence } from "../evidence/canonical-block-evidence.js";
import { requireLinearFaultThreadUtxo } from "../linear-fault-family.js";
import type { CanonicalViolationDetection } from "../workflow/classification.js";
import {
  acceptedTransactionSubject,
  forcedTransactionSubject,
} from "../workflow/detection-subject.js";
import { type FraudProofFamilyL1ObservationPort } from "../workflow/family-l1-observation.js";
import {
  prepareTransactionOutputEvidence,
  type TransactionOutputEvidence,
  transactionOutputEvidenceCloses,
  type TransactionOutputJournal,
  type TransactionOutputStage,
} from "./transaction-output-non-canonical.js";
import { type TransactionOutputNonCanonicalAuthenticatedSource } from "./workflow.derive-transaction-output-non-canonical-authenticated-source.js";
import {
  type LoadManifestBoundTransactionOutputNonCanonicalConfig,
  type ManifestBoundTransactionOutputNonCanonicalConfig,
  TRANSACTION_OUTPUT_NON_CANONICAL_VIOLATION_ID,
  type TransactionOutputNonCanonicalStage,
} from "./workflow.transaction-output-non-canonical-config-from-binding.js";

/** Complete replay member: scans every accepted field-2 output and exact forced reason. */
export const detectTransactionOutputNonCanonicalCompleteReplay = (
  evidence: CanonicalBlockEvidence,
): readonly CanonicalViolationDetection[] => {
  const accepted = evidence.transactions.flatMap(
    (transaction, transactionIndex) => {
      const material = deriveMidgardNativeTxFaultEvidenceMaterial(
        Buffer.from(transaction.txCbor, "hex"),
      );
      const transactionId = material.transactionId.toString("hex");
      if (transaction.nodeTxId !== transactionId) {
        throw new Error(
          "transactionOutputNonCanonical complete replay transaction identity changed",
        );
      }
      const fieldIndex = 2 as const;
      const fieldPreimage = material.fieldPreimages[fieldIndex]!;
      return decodeMidgardFieldPreimage(fieldPreimage).flatMap(
        (item, itemIndex) => {
          if (item.length > 16_384) return [];
          const prepared = prepareTransactionOutputEvidence({
            finding: {
              subject: acceptedVerdictSubject(transactionId),
              fieldIndex,
              itemIndex,
            },
            fieldPreimage,
            committedFieldHashHex:
              midgardFieldCommitment(fieldPreimage).toString("hex"),
          });
          return prepared.decisiveFaultHolds
            ? [
                {
                  ...acceptedTransactionSubject(transactionId),
                  detectionId: `${TRANSACTION_OUTPUT_NON_CANONICAL_VIOLATION_ID}:${transactionIndex.toString()}:${transactionId}:${fieldIndex.toString()}:${itemIndex.toString()}:${item.length.toString()}`,
                  headerHash: evidence.headerHash,
                  violationId: TRANSACTION_OUTPUT_NON_CANONICAL_VIOLATION_ID,
                  position: BigInt(transactionIndex),
                  diagnostic: `transaction ${transactionId} field ${fieldIndex.toString()} item ${itemIndex.toString()} has illegal width ${item.length.toString()}`,
                },
              ]
            : [];
        },
      );
    },
  );
  const forced = evidence.reconstruction.forcedTransactions.flatMap(
    (transaction, forcedIndex) => {
      if (transaction.value.verdict === "ForcedTxValid") return [];
      const reason = transaction.value.verdict.ForcedTxInvalid.reason;
      if (typeof reason === "string" || !("OutputNonCanonical" in reason)) {
        return [];
      }
      const material = deriveMidgardForcedTxFaultEvidenceMaterial(
        transaction.fullTransactionCbor,
      );
      if (
        material.transactionId.toString("hex") !== transaction.value.tx_id ||
        material.proofSource.compactCbor.toString("hex") !==
          transaction.value.submitted_source.compact_cbor ||
        material.proofSource.witnessSetCompactCbor.toString("hex") !==
          transaction.value.submitted_source.witness_set_compact_cbor ||
        material.proofSource.fieldPreimageLengthsCbor.toString("hex") !==
          transaction.value.submitted_source.field_preimage_lengths_cbor
      ) {
        throw new Error(
          "transactionOutputNonCanonical forced transaction differs from its authenticated leaf",
        );
      }
      const coordinate = reason.OutputNonCanonical;
      const fieldIndex = 2 as const;
      const itemIndex = Number(coordinate.output_index);
      const preimage = material.fieldPreimages[fieldIndex];
      const item =
        preimage === undefined
          ? undefined
          : decodeMidgardFieldPreimage(preimage)[itemIndex];
      if (item === undefined || item.length > 16_384) {
        return [];
      }
      const fieldPreimage = material.fieldPreimages[fieldIndex]!;
      const prepared = prepareTransactionOutputEvidence({
        finding: {
          subject: forcedVerdictSubject({
            transactionId: transaction.value.tx_id,
            sourceKey: transaction.key,
            rejectionReason: reason,
          }),
          fieldIndex,
          itemIndex,
        },
        fieldPreimage,
        committedFieldHashHex:
          midgardFieldCommitment(fieldPreimage).toString("hex"),
      });
      if (!transactionOutputEvidenceCloses(prepared)) return [];
      return [
        {
          ...forcedTransactionSubject(transaction.key),
          detectionId: `${TRANSACTION_OUTPUT_NON_CANONICAL_VIOLATION_ID}:forced:${forcedIndex.toString()}:${transaction.value.tx_id}:${fieldIndex.toString()}:${itemIndex.toString()}:${item.length.toString()}`,
          headerHash: evidence.headerHash,
          violationId: TRANSACTION_OUTPUT_NON_CANONICAL_VIOLATION_ID,
          position: BigInt(forcedIndex),
          diagnostic: `forced transaction ${transaction.value.tx_id} was rejected for legal field ${fieldIndex.toString()} item ${itemIndex.toString()} width ${item.length.toString()}`,
        },
      ];
    },
  );
  return [...accepted, ...forced];
};

export type TransactionOutputNonCanonicalRuntimeLoader = Readonly<{
  config: LoadManifestBoundTransactionOutputNonCanonicalConfig;
  journal: TransactionOutputJournal;
  observe: (identity: string) => Promise<TransactionOutputStage>;
  resolveStage: (input: {
    readonly action:
      | "submitInit"
      | "submitStep01"
      | "submitStep02"
      | "submitStep03"
      | "submitStep04"
      | "removeDescendants"
      | "cancel";
    readonly evidence: TransactionOutputEvidence;
  }) => Promise<TransactionOutputNonCanonicalStage>;
}>;

export const createTransactionOutputNonCanonicalRawL1StageResolver =
  ({
    config,
    l1,
    source,
  }: {
    readonly config: ManifestBoundTransactionOutputNonCanonicalConfig;
    readonly l1: FraudProofFamilyL1ObservationPort<FraudProofCatalogueCategoryName>;
    readonly source: TransactionOutputNonCanonicalAuthenticatedSource;
  }): TransactionOutputNonCanonicalRuntimeLoader["resolveStage"] =>
  async ({ action, evidence }) => {
    const observed = await l1.observe({
      headerHash: config.binding.definition.headerHash,
    });
    const stage = observed.stage;
    if (action === "submitInit") {
      if (stage.kind !== "not_started") {
        throw new Error(
          "transactionOutputNonCanonical init requires raw-L1 not_started",
        );
      }
      return { fraudulentBlockOutRef: stage.stateQueueBlockOutRef };
    }
    if (action === "removeDescendants") {
      if (stage.kind !== "proof_token") {
        throw new Error(
          "transactionOutputNonCanonical removal requires raw-L1 proof token",
        );
      }
      return { fraudulentBlockOutRef: stage.stateQueueBlockOutRef };
    }
    const expectedStep =
      action === "submitStep01"
        ? 1
        : action === "submitStep02"
          ? 2
          : action === "submitStep03"
            ? 3
            : 4;
    if (stage.kind !== "step" || stage.step !== expectedStep) {
      throw new Error(
        `transactionOutputNonCanonical ${action} differs from authenticated raw-L1 stage`,
      );
    }
    const common = {
      fraudulentBlockOutRef: stage.stateQueueBlockOutRef,
      threadOutRef: stage.threadOutRef,
      nativeTxCompactCbor: source.nativeTxCompactCbor,
      witnessSetCompactCbor: source.witnessSetCompactCbor,
    };
    if (action !== "submitStep01") return common;
    if (evidence.subject.source_kind === PROOF_THREAD_SOURCE_KIND_FORCED) {
      return {
        ...common,
        forcedHeader: required(
          source.forcedHeader,
          "authenticated forced header",
        ),
        forcedMembership: required(
          source.forcedMembership,
          "authenticated forced membership",
        ),
        forcedDirection: required(
          source.forcedDirection,
          "authenticated forced direction",
        ),
      };
    }
    const thread = await requireLinearFaultThreadUtxo({
      lucid: config.lucid,
      contracts: config.contracts,
      categoryId: config.binding.resolvedContracts.category.categoryId,
      family: "transaction-output-non-canonical",
      stepIndex: 0,
      threadOutRef: stage.threadOutRef,
    });
    return {
      ...common,
      threadUtxo: thread.threadUtxo,
      threadToken: thread.threadToken,
      stateQueueBlockOutRef: stage.stateQueueBlockOutRef,
      acceptedInclusion: required(
        source.acceptedInclusion,
        "authenticated accepted inclusion",
      ),
    };
  };

export const required = <T>(value: T | undefined, label: string): T => {
  if (value === undefined)
    throw new Error(`transactionOutputNonCanonical missing ${label}`);
  return value;
};
