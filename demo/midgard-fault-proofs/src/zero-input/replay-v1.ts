import {
  deriveMidgardNativeTxFaultEvidenceMaterialV1,
  midgardFieldCommitmentV1,
} from "@al-ft/midgard-core";
import * as SDK from "@al-ft/midgard-sdk";

import type { CanonicalBlockEvidenceV1 } from "../evidence/canonical-block-evidence-v1.js";
import {
  prepareZeroInputEvidenceV1,
  ZERO_INPUT_FIELD_INDEX_V1,
  zeroInputEvidenceClosesV1,
  type ZeroInputEvidenceV1,
} from "./family-v1.js";

export const ZERO_INPUT_VIOLATION_ID_V1 = "zero-input" as const;

export type ZeroInputForcedReplayDetectionV1 = Readonly<{
  detectionId: string;
  headerHash: string;
  violationId: typeof ZERO_INPUT_VIOLATION_ID_V1;
  position: bigint;
  forcedIndex: number;
  transactionId: string;
  evidence: ZeroInputEvidenceV1;
}>;

/**
 * Derives direction, reason, transaction identity, and decisive input count
 * exclusively from an already-authenticated canonical block reconstruction.
 */
export const detectZeroInputForcedReplayV1 = (
  block: CanonicalBlockEvidenceV1,
): readonly ZeroInputForcedReplayDetectionV1[] => {
  const detections: ZeroInputForcedReplayDetectionV1[] = [];
  block.reconstruction.forcedTransactions.forEach(
    (transaction, forcedIndex) => {
      const verdict = transaction.value.verdict;
      if (
        verdict === "ForcedTxValid" ||
        verdict.ForcedTxInvalid.reason !== "EmptyInputs"
      )
        return;
      const material = deriveMidgardNativeTxFaultEvidenceMaterialV1(
        transaction.fullTransactionCbor,
      );
      if (
        material.transactionId.toString("hex") !== transaction.value.tx_id ||
        material.proofSource.compactCbor.toString("hex") !==
          transaction.value.source.compact_cbor ||
        material.proofSource.witnessSetCompactCbor.toString("hex") !==
          transaction.value.source.witness_set_compact_cbor ||
        material.proofSource.fieldPreimageLengthsCbor.toString("hex") !==
          transaction.value.source.field_preimage_lengths_cbor
      )
        throw new Error(
          "zeroInput: forced preimage differs from authenticated leaf",
        );
      const field = material.fieldPreimages[ZERO_INPUT_FIELD_INDEX_V1];
      if (field === undefined) return;
      const evidence = prepareZeroInputEvidenceV1({
        finding: {
          subject: SDK.forcedVerdictSubjectV1({
            transactionId: transaction.value.tx_id,
            sourceKey: transaction.key,
            rejectionReason: verdict.ForcedTxInvalid.reason,
          }),
        },
        inputFieldPreimage: field,
        committedFieldHashHex: midgardFieldCommitmentV1(field).toString("hex"),
      });
      if (!zeroInputEvidenceClosesV1(evidence)) return;
      detections.push(
        Object.freeze({
          detectionId: `${ZERO_INPUT_VIOLATION_ID_V1}:forced:${forcedIndex.toString()}:${transaction.value.tx_id}`,
          headerHash: block.headerHash,
          violationId: ZERO_INPUT_VIOLATION_ID_V1,
          position: BigInt(forcedIndex),
          forcedIndex,
          transactionId: transaction.value.tx_id,
          evidence,
        }),
      );
    },
  );
  return Object.freeze(detections);
};

export const selectCanonicalZeroInputForcedDetectionV1 = (
  detections: readonly ZeroInputForcedReplayDetectionV1[],
): ZeroInputForcedReplayDetectionV1 => {
  if (detections.length === 0)
    throw new Error("zeroInput: no authenticated wrongful rejection");
  return [...detections].sort((left, right) =>
    left.position === right.position
      ? left.detectionId.localeCompare(right.detectionId)
      : left.position < right.position
        ? -1
        : 1,
  )[0]!;
};
