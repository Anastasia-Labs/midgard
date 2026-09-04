import {
  deriveMidgardNativeTxFaultEvidenceMaterial,
  midgardFieldCommitment,
} from "@al-ft/midgard-core";
import * as SDK from "@al-ft/midgard-sdk";

import type { CanonicalBlockEvidence } from "../evidence/canonical-block-evidence.js";
import {
  prepareZeroInputEvidence,
  ZERO_INPUT_FIELD_INDEX,
  type ZeroInputEvidence,
  zeroInputEvidenceCloses,
} from "./family.js";

export const ZERO_INPUT_VIOLATION_ID = "zero-input" as const;

export type ZeroInputForcedReplayDetection = Readonly<{
  detectionId: string;
  headerHash: string;
  violationId: typeof ZERO_INPUT_VIOLATION_ID;
  position: bigint;
  forcedIndex: number;
  transactionId: string;
  evidence: ZeroInputEvidence;
}>;

/**
 * Derives direction, reason, transaction identity, and decisive input count
 * exclusively from an already-authenticated canonical block reconstruction.
 */
export const detectZeroInputForcedReplay = (
  block: CanonicalBlockEvidence,
): readonly ZeroInputForcedReplayDetection[] => {
  const detections: ZeroInputForcedReplayDetection[] = [];
  block.reconstruction.forcedTransactions.forEach(
    (transaction, forcedIndex) => {
      const verdict = transaction.value.verdict;
      if (
        verdict === "ForcedTxValid" ||
        verdict.ForcedTxInvalid.reason !== "EmptyInputs"
      )
        return;
      const material = deriveMidgardNativeTxFaultEvidenceMaterial(
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
      const field = material.fieldPreimages[ZERO_INPUT_FIELD_INDEX];
      if (field === undefined) return;
      const evidence = prepareZeroInputEvidence({
        finding: {
          subject: SDK.forcedVerdictSubject({
            transactionId: transaction.value.tx_id,
            sourceKey: transaction.key,
            rejectionReason: verdict.ForcedTxInvalid.reason,
          }),
        },
        inputFieldPreimage: field,
        committedFieldHashHex: midgardFieldCommitment(field).toString("hex"),
      });
      if (!zeroInputEvidenceCloses(evidence)) return;
      detections.push(
        Object.freeze({
          detectionId: `${ZERO_INPUT_VIOLATION_ID}:forced:${forcedIndex.toString()}:${transaction.value.tx_id}`,
          headerHash: block.headerHash,
          violationId: ZERO_INPUT_VIOLATION_ID,
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

export const selectCanonicalZeroInputForcedDetection = (
  detections: readonly ZeroInputForcedReplayDetection[],
): ZeroInputForcedReplayDetection => {
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
