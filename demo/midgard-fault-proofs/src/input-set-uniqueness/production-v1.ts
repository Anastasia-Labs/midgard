import type { CanonicalBlockEvidenceV1 } from "../evidence/canonical-block-evidence-v1.js";
import { buildForcedTransactionLeafMembershipProof } from "../transition-trace/witnesses.js";
import {
  detectInputSetUniquenessForcedReplayV1,
  selectCanonicalInputSetUniquenessForcedDetectionV1,
} from "./replay-v1.js";

/** Callback-free production boundary: authenticated block evidence is the input. */
export type InputSetUniquenessForcedProductionInputV1 = Readonly<{
  block: CanonicalBlockEvidenceV1;
}>;

export const prepareInputSetUniquenessForcedProductionPlanV1 = async ({
  block,
}: InputSetUniquenessForcedProductionInputV1) => {
  const detection = selectCanonicalInputSetUniquenessForcedDetectionV1(
    detectInputSetUniquenessForcedReplayV1(block),
  );
  const transaction =
    block.reconstruction.forcedTransactions[detection.forcedIndex];
  if (
    transaction === undefined ||
    transaction.value.verdict === "ForcedTxValid" ||
    typeof transaction.value.verdict.ForcedTxInvalid.reason !== "object" ||
    !("DuplicateInput" in transaction.value.verdict.ForcedTxInvalid.reason)
  ) {
    throw new Error("inputSetUniqueness: selected forced leaf disappeared");
  }
  const membership = await buildForcedTransactionLeafMembershipProof({
    reconstruction: block.reconstruction,
    eventKey: {
      ForcedTransactionEventKey: { tx_order_id: transaction.key },
    },
  });
  return Object.freeze({
    detectionId: detection.detectionId,
    headerHash: block.headerHash,
    header: block.header,
    membership,
    nativeTxCompactCbor: transaction.value.source.compact_cbor,
    spendInputItemCbors: detection.spendInputItemCbors,
    referenceInputItemCbors: detection.referenceInputItemCbors,
    bound: detection.bound,
  });
};
