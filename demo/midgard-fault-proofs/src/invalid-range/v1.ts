import type { CanonicalBlockEvidence } from "../evidence/canonical-block-evidence.js";
import { buildForcedTransactionLeafMembershipProof } from "../transition-trace/witnesses.js";
import { detectInvalidRangeForcedReplay } from "./replay.js";

export type InvalidRangeForcedInput = Readonly<{
  block: CanonicalBlockEvidence;
}>;
export const prepareInvalidRangeForcedPlan = async ({
  block,
}: InvalidRangeForcedInput) => {
  const detection = detectInvalidRangeForcedReplay(block)[0];
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
