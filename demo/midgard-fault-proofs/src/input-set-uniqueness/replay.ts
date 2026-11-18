import { decodeMidgardNativeByteListPreimage } from "@al-ft/midgard-core";
import { deriveMidgardForcedTxFaultEvidenceMaterial } from "@al-ft/midgard-core/codec/forced";
import { forcedVerdictSubject } from "@al-ft/midgard-sdk";

import type { CanonicalBlockEvidence } from "../evidence/canonical-block-evidence.js";
import {
  type DetectionSubject,
  forcedTransactionSubject,
} from "../workflow/detection-subject.js";
import {
  bindForcedDuplicateInput,
  type BoundDuplicateInput,
  inputSetPairNamesNoDuplicate,
  inputSetUnionIsStrictlyIncreasing,
} from "./wrongful-rejection.js";

export const INPUT_SET_UNIQUENESS_WRONGFUL_REJECTION_VIOLATION_ID =
  "input-set-uniqueness-wrongful-rejection" as const;

export type InputSetUniquenessForcedReplayDetection = DetectionSubject &
  Readonly<{
    detectionId: string;
    headerHash: string;
    violationId: typeof INPUT_SET_UNIQUENESS_WRONGFUL_REJECTION_VIOLATION_ID;
    position: bigint;
    forcedIndex: number;
    transactionId: string;
    bound: BoundDuplicateInput;
    spendInputItemCbors: readonly string[];
    referenceInputItemCbors: readonly string[];
  }>;

/** Complete retained-DA scan; no verdict/evidence callback crosses this API. */
export const detectInputSetUniquenessForcedReplay = (
  block: CanonicalBlockEvidence,
): readonly InputSetUniquenessForcedReplayDetection[] => {
  const detections: InputSetUniquenessForcedReplayDetection[] = [];
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
      const material = deriveMidgardForcedTxFaultEvidenceMaterial(
        transaction.fullTransactionCbor,
      );
      const source = transaction.value.submitted_source;
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
      const bound = bindForcedDuplicateInput(
        forcedVerdictSubject({
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
      // A pair that cannot name two distinct inputs contradicts the
      // rejection whatever the union holds.
      const namesNoDuplicate = inputSetPairNamesNoDuplicate({
        bound,
        spendCount: BigInt(spendInputItemCbors.length),
        referenceCount: BigInt(referenceInputItemCbors.length),
      });
      if (
        !namesNoDuplicate &&
        (!inputSetUnionIsStrictlyIncreasing({
          spendInputItemCbors,
          referenceInputItemCbors,
        }) ||
          bound.first_item_index < 0n ||
          bound.second_item_index < 0n ||
          bound.first_item_index >= count(bound.first_field_index) ||
          bound.second_item_index >= count(bound.second_field_index) ||
          bound.first_field_index > bound.second_field_index ||
          (bound.first_field_index === bound.second_field_index &&
            bound.first_item_index >= bound.second_item_index))
      ) {
        return;
      }
      detections.push(
        Object.freeze({
          ...forcedTransactionSubject(transaction.key),
          detectionId: `${INPUT_SET_UNIQUENESS_WRONGFUL_REJECTION_VIOLATION_ID}:forced:${forcedIndex.toString()}:${transaction.value.tx_id}`,
          headerHash: block.headerHash,
          violationId: INPUT_SET_UNIQUENESS_WRONGFUL_REJECTION_VIOLATION_ID,
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
