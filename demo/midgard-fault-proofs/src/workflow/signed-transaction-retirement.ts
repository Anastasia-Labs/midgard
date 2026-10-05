import { DEPLOYMENT_MANIFEST_L1_FINALITY } from "@al-ft/midgard-core/deployment-manifest-identity";

import {
  admitFraudProofRawL1Point,
  type FraudProofRawL1Point,
} from "./raw-l1-snapshot.js";

/** Persisted receipt of the authenticated canonical observation that retired exact bytes. */
export type SignedWorkflowTransactionRetirement = Readonly<{
  transactionHash: string;
  canonicalPoint: FraudProofRawL1Point;
  releaseFinalPoint: FraudProofRawL1Point;
  reason: "expired" | "invalidated" | "included";
}>;

export const parseSignedWorkflowTransactionRetirement = (
  value: unknown,
  transactionHash: string,
): SignedWorkflowTransactionRetirement => {
  if (
    value === null ||
    typeof value !== "object" ||
    Array.isArray(value) ||
    Object.keys(value).sort().join(",") !==
      "canonicalPoint,reason,releaseFinalPoint,transactionHash"
  )
    throw new Error("Signed retirement requires its exact canonical receipt");
  const record = value as SignedWorkflowTransactionRetirement;
  const canonicalPoint = admitFraudProofRawL1Point(record.canonicalPoint);
  const releaseFinalPoint = admitFraudProofRawL1Point(record.releaseFinalPoint);
  if (
    record.transactionHash !== transactionHash ||
    !/^[0-9a-f]{64}$/u.test(transactionHash) ||
    (record.reason !== "expired" &&
      record.reason !== "invalidated" &&
      record.reason !== "included") ||
    BigInt(canonicalPoint.blockNo) - BigInt(releaseFinalPoint.blockNo) <=
      BigInt(DEPLOYMENT_MANIFEST_L1_FINALITY.automaticRecoveryMaxDepth) ||
    BigInt(canonicalPoint.slot) <= BigInt(releaseFinalPoint.slot) ||
    canonicalPoint.blockHash === releaseFinalPoint.blockHash
  )
    throw new Error(
      "Signed retirement is not beyond the canonical recovery horizon",
    );
  return Object.freeze({
    transactionHash,
    canonicalPoint,
    releaseFinalPoint,
    reason: record.reason,
  });
};
