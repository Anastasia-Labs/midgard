import type { CanonicalBlockEvidenceV1 } from "../evidence/canonical-block-evidence-v1.js";
import { buildForcedTransactionLeafMembershipProof } from "../transition-trace/witnesses.js";
import { detectInvalidRangeForcedReplayV1 } from "./replay-v1.js";

export type InvalidRangeForcedProductionInputV1 = Readonly<{
  block: CanonicalBlockEvidenceV1;
}>;
export const prepareInvalidRangeForcedProductionPlanV1 = async ({
  block,
}: InvalidRangeForcedProductionInputV1) => {
  const detection = detectInvalidRangeForcedReplayV1(block)[0];
  if (detection === undefined)
    throw new Error("invalidRange: no authenticated wrongful rejection");
  const transaction =
    block.reconstruction.forcedTransactions[detection.forcedIndex]!;
  const membership = await buildForcedTransactionLeafMembershipProof({
    reconstruction: block.reconstruction,
    eventKey: { ForcedTransactionEventKey: { tx_order_id: transaction.key } },
  });
  return Object.freeze({
    detectionId: detection.detectionId,
    headerHash: block.headerHash,
    evidence: detection.evidence,
    nativeTxCompactCbor: transaction.value.source.compact_cbor,
    forcedSource: Object.freeze({
      header: block.header,
      membership,
      direction: 1n,
    }),
  });
};
