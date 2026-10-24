import { computeFraudProofRawL1PointId } from "../../src/workflow/raw-l1-snapshot.js";
import type { SignedTransactionRecoveryObservation } from "../../src/workflow/signed-transaction-reconciliation.js";

export const signedRecoveryObservationFixture = ({
  transactionHash,
  signedTransactionCborHex,
}: Pick<
  SignedTransactionRecoveryObservation,
  "transactionHash" | "signedTransactionCborHex"
>) => {
  const boundary = { slot: "1000", blockNo: "50", blockHash: "ab".repeat(32) };
  const tip = { slot: "10000", blockNo: "2211", blockHash: "cd".repeat(32) };
  const releaseFinalPoint = {
    ...boundary,
    pointId: computeFraudProofRawL1PointId(boundary),
  };
  return (
    status: SignedTransactionRecoveryObservation["status"],
    deep = false,
  ): SignedTransactionRecoveryObservation => ({
    transactionHash,
    signedTransactionCborHex,
    status,
    canonicalPoint: deep
      ? { ...tip, pointId: computeFraudProofRawL1PointId(tip) }
      : releaseFinalPoint,
    releaseFinalPoint,
    inputs: [],
    reason: status,
  });
};
