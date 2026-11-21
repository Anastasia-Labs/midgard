import { CML } from "@lucid-evolution/lucid";

import type { FraudProofRawL1Point } from "./raw-l1-snapshot.js";

export type SignedWorkflowTransaction = Readonly<{
  transactionHash: string;
  signedTransactionCborHex: string;
}>;

export type SignedTransactionRecoveryObservation = SignedWorkflowTransaction &
  Readonly<{
    status:
      | "included"
      | "pending"
      | "rebroadcast"
      | "expired"
      | "invalidated"
      /** Absent and impossible at the tip, but not yet beyond the horizon. */
      | "expired_at_tip"
      | "invalidated_at_tip"
      /** The exact body landed failing phase 2 at `inclusionPoint`: the
       * chain took its collateral, so it can never land valid. */
      | "landed_invalid"
      | "conflict"
      | "unknown";
    inclusionPoint?: FraudProofRawL1Point;
    canonicalPoint: FraudProofRawL1Point;
    releaseFinalPoint: FraudProofRawL1Point;
    inputs: readonly Readonly<{ outRef: string; outputCbor: string }>[];
    reason: string;
  }>;

/** Whether a transaction, where `observed` says it stands, is final: landed
 * at or below the release-final point, never on first sight of landing. The
 * one finality rule for a transaction of ours. */
export const isFinalAtReleaseDepth = (
  observed: SignedTransactionRecoveryObservation,
): boolean =>
  observed.status === "included" &&
  observed.inclusionPoint !== undefined &&
  BigInt(observed.inclusionPoint.blockNo) <=
    BigInt(observed.releaseFinalPoint.blockNo);

export const inspectSignedWorkflowTransaction = (
  input: SignedWorkflowTransaction,
) => {
  if (
    !/^[0-9a-f]{64}$/u.test(input.transactionHash) ||
    !/^(?:[0-9a-f]{2})+$/u.test(input.signedTransactionCborHex)
  )
    throw new Error(
      "Workflow recovery requires exact canonical signed transaction bytes",
    );
  const transaction = CML.Transaction.from_cbor_hex(
    input.signedTransactionCborHex,
  );
  const body = transaction.body();
  if (
    !transaction.is_valid() ||
    CML.hash_transaction(body).to_hex() !== input.transactionHash ||
    transaction.to_cbor_hex() !== input.signedTransactionCborHex
  )
    throw new Error(
      "Workflow recovery signed bytes differ from their durable transaction identity",
    );
  const ordinaryInputs = body.inputs();
  if (ordinaryInputs.len() === 0)
    throw new Error("Workflow recovery transaction has no ordinary inputs");
  const inputs = new Set<string>();
  for (const group of [
    ordinaryInputs,
    body.collateral_inputs(),
    body.reference_inputs(),
  ]) {
    if (group === undefined) continue;
    for (let index = 0; index < group.len(); index += 1) {
      const item = group.get(index);
      inputs.add(
        `${item.transaction_id().to_hex()}#${item.index().toString()}`,
      );
    }
  }
  return {
    transaction,
    body,
    inputOutRefs: [...inputs].sort(),
    expiresAtSlot: body.ttl(),
    validFromSlot: body.validity_interval_start(),
  };
};
