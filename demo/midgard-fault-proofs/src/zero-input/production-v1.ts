import type { CanonicalBlockEvidenceV1 } from "../evidence/canonical-block-evidence-v1.js";
import { buildForcedTransactionLeafMembershipProof } from "../transition-trace/witnesses.js";
import {
  detectZeroInputForcedReplayV1,
  selectCanonicalZeroInputForcedDetectionV1,
} from "./replay-v1.js";

/**
 * The only production input is a canonical block reconstructed from retained
 * DA and an authenticated Cardano header. Verdict, reason, evidence, and the
 * forced-source redeemer payload are all derived below this boundary.
 */
export type ZeroInputForcedProductionInputV1 = Readonly<{
  block: CanonicalBlockEvidenceV1;
}>;

export const prepareZeroInputForcedProductionPlanV1 = async ({
  block,
}: ZeroInputForcedProductionInputV1) => {
  const detection = selectCanonicalZeroInputForcedDetectionV1(
    detectZeroInputForcedReplayV1(block),
  );
  const transaction =
    block.reconstruction.forcedTransactions[detection.forcedIndex];
  if (
    transaction === undefined ||
    transaction.value.verdict === "ForcedTxValid" ||
    transaction.value.verdict.ForcedTxInvalid.reason !== "EmptyInputs"
  )
    throw new Error("zeroInput: selected forced leaf disappeared");
  const membership = await buildForcedTransactionLeafMembershipProof({
    reconstruction: block.reconstruction,
    eventKey: {
      ForcedTransactionEventKey: { tx_order_id: transaction.key },
    },
  });
  return Object.freeze({
    detectionId: detection.detectionId,
    headerHash: block.headerHash,
    finding: Object.freeze({ subject: detection.evidence.subject }),
    evidence: detection.evidence,
    nativeTxCompactCbor: transaction.value.source.compact_cbor,
    forcedSource: Object.freeze({
      header: block.header,
      membership,
      direction: 1n,
    }),
  });
};
