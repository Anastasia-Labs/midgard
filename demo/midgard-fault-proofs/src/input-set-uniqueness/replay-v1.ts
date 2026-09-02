import {
  decodeMidgardNativeByteListPreimage,
  deriveMidgardNativeTxFaultEvidenceMaterialV1,
} from "@al-ft/midgard-core";
import { forcedVerdictSubjectV1 } from "@al-ft/midgard-sdk";

import type { CanonicalBlockEvidenceV1 } from "../evidence/canonical-block-evidence-v1.js";
import {
  bindForcedDuplicateInputV1,
  type BoundDuplicateInputV1,
  inputSetUnionIsStrictlyIncreasingV1,
} from "./wrongful-rejection-v1.js";

export const INPUT_SET_UNIQUENESS_WRONGFUL_REJECTION_VIOLATION_ID_V1 =
  "input-set-uniqueness-wrongful-rejection" as const;

export type InputSetUniquenessForcedReplayDetectionV1 = Readonly<{
  detectionId: string;
  headerHash: string;
  violationId: typeof INPUT_SET_UNIQUENESS_WRONGFUL_REJECTION_VIOLATION_ID_V1;
  position: bigint;
  forcedIndex: number;
  transactionId: string;
  bound: BoundDuplicateInputV1;
  spendInputItemCbors: readonly string[];
  referenceInputItemCbors: readonly string[];
}>;

/** Complete retained-DA scan; no verdict/evidence callback crosses this API. */
export const detectInputSetUniquenessForcedReplayV1 = (
  block: CanonicalBlockEvidenceV1,
): readonly InputSetUniquenessForcedReplayDetectionV1[] => {
  const detections: InputSetUniquenessForcedReplayDetectionV1[] = [];
  block.reconstruction.forcedTransactions.forEach(
    (transaction, forcedIndex) => {
      const verdict = transaction.value.verdict;
      if (
        verdict === "ForcedTxValid" ||
        typeof verdict.ForcedTxInvalid.reason !== "object" ||
        !("DuplicateInput" in verdict.ForcedTxInvalid.reason)
      ) {
        return;
      }
      const material = deriveMidgardNativeTxFaultEvidenceMaterialV1(
        transaction.fullTransactionCbor,
      );
      const source = transaction.value.source;
      if (
        material.transactionId.toString("hex") !== transaction.value.tx_id ||
        material.proofSource.compactCbor.toString("hex") !==
          source.compact_cbor ||
        material.proofSource.witnessSetCompactCbor.toString("hex") !==
          source.witness_set_compact_cbor ||
        material.proofSource.fieldPreimageLengthsCbor.toString("hex") !==
          source.field_preimage_lengths_cbor
      ) {
        throw new Error(
          "inputSetUniqueness: forced preimage differs from authenticated leaf",
        );
      }
      const items = (fieldIndex: 0 | 1): readonly string[] => {
        const field = material.fieldPreimages[fieldIndex];
        if (field === undefined) {
          throw new Error(
            `inputSetUniqueness: forced field ${fieldIndex.toString()} disappeared`,
          );
        }
        return decodeMidgardNativeByteListPreimage(
          field,
          `inputSetUniqueness forced field ${fieldIndex.toString()}`,
        ).map((item) => Buffer.from(item).toString("hex"));
      };
      const spendInputItemCbors = items(0);
      const referenceInputItemCbors = items(1);
      if (
        !inputSetUnionIsStrictlyIncreasingV1({
          spendInputItemCbors,
          referenceInputItemCbors,
        })
      ) {
        return;
      }
      const bound = bindForcedDuplicateInputV1(
        forcedVerdictSubjectV1({
          transactionId: transaction.value.tx_id,
          sourceKey: transaction.key,
          rejectionReason: verdict.ForcedTxInvalid.reason,
        }),
      );
      const count = (field: bigint) =>
        field === 0n
          ? BigInt(spendInputItemCbors.length)
          : field === 1n
            ? BigInt(referenceInputItemCbors.length)
            : -1n;
      if (
        bound.first_item_index < 0n ||
        bound.second_item_index < 0n ||
        bound.first_item_index >= count(bound.first_field_index) ||
        bound.second_item_index >= count(bound.second_field_index) ||
        bound.first_field_index > bound.second_field_index ||
        (bound.first_field_index === bound.second_field_index &&
          bound.first_item_index >= bound.second_item_index)
      ) {
        return;
      }
      detections.push(
        Object.freeze({
          detectionId: `${INPUT_SET_UNIQUENESS_WRONGFUL_REJECTION_VIOLATION_ID_V1}:forced:${forcedIndex.toString()}:${transaction.value.tx_id}`,
          headerHash: block.headerHash,
          violationId: INPUT_SET_UNIQUENESS_WRONGFUL_REJECTION_VIOLATION_ID_V1,
          position: BigInt(forcedIndex),
          forcedIndex,
          transactionId: transaction.value.tx_id,
          bound,
          spendInputItemCbors: Object.freeze(spendInputItemCbors),
          referenceInputItemCbors: Object.freeze(referenceInputItemCbors),
        }),
      );
    },
  );
  return Object.freeze(detections);
};

export const selectCanonicalInputSetUniquenessForcedDetectionV1 = (
  detections: readonly InputSetUniquenessForcedReplayDetectionV1[],
): InputSetUniquenessForcedReplayDetectionV1 => {
  if (detections.length === 0) {
    throw new Error("inputSetUniqueness: no authenticated wrongful rejection");
  }
  return [...detections].sort((left, right) =>
    left.position === right.position
      ? left.detectionId.localeCompare(right.detectionId)
      : left.position < right.position
        ? -1
        : 1,
  )[0]!;
};
