import { CML } from "@lucid-evolution/lucid";

import type { FraudProofWorkflowReconcileResult } from "./orchestrator.js";
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
      | "conflict"
      | "unknown";
    canonicalPoint: FraudProofRawL1Point;
    releaseFinalPoint: FraudProofRawL1Point;
    inputs: readonly Readonly<{ outRef: string; outputCbor: string }>[];
    reason: string;
  }>;

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

/** The family state machine calls this only while its exact predecessor remains current. */
export const reconcileSignedWorkflowTransaction = async ({
  transactionHash,
  signedTransactionCborHex,
  observe,
  rebroadcast,
  authorizeResubmission,
}: {
  readonly transactionHash: string;
  readonly signedTransactionCborHex?: string;
  readonly observe?: (
    input: SignedWorkflowTransaction,
  ) => Promise<SignedTransactionRecoveryObservation>;
  readonly rebroadcast?: (
    input: SignedWorkflowTransaction & {
      readonly authorizeResubmission: (
        input: SignedWorkflowTransaction,
      ) => Promise<void>;
    },
  ) => Promise<string>;
  readonly authorizeResubmission?: (
    input: SignedWorkflowTransaction,
  ) => Promise<void>;
}): Promise<FraudProofWorkflowReconcileResult> => {
  const pending = { kind: "pending", txHash: transactionHash } as const;
  if (signedTransactionCborHex === undefined || observe === undefined)
    return {
      kind: "unknown",
      reason:
        "Recorded signed bytes or canonical recovery source are unavailable",
    };
  const input = { transactionHash, signedTransactionCborHex };
  inspectSignedWorkflowTransaction(input);
  let observed: SignedTransactionRecoveryObservation;
  try {
    observed = await observe(input);
  } catch (cause) {
    // Transport, lag, or changing canonical boundaries cannot establish absence.
    return {
      kind: "unknown",
      reason: `Canonical signed recovery is unresolved: ${String(cause)}`,
    };
  }
  if (
    observed.transactionHash !== transactionHash ||
    observed.signedTransactionCborHex !== signedTransactionCborHex
  )
    throw new Error(
      "Signed recovery observation substituted the durable transaction",
    );
  if (observed.status === "expired") return { kind: "not_found" };
  if (observed.status === "conflict")
    return { kind: "conflict", reason: observed.reason };
  if (observed.status === "unknown")
    return { kind: "unknown", reason: observed.reason };
  if (
    observed.status === "rebroadcast" &&
    (rebroadcast === undefined || authorizeResubmission === undefined)
  )
    return {
      kind: "unknown",
      reason: "Exact recorded transaction requires live rebroadcast authority",
    };
  if (
    observed.status === "rebroadcast" &&
    rebroadcast !== undefined &&
    authorizeResubmission !== undefined
  ) {
    try {
      const submitted = await rebroadcast({ ...input, authorizeResubmission });
      if (submitted !== transactionHash)
        throw new Error("Rebroadcast changed recorded transaction hash");
    } catch (cause) {
      // A rejected authorization submits nothing. An ambiguous acknowledgement
      // retains the same signed intent for the next canonical reconciliation.
      return {
        kind: "unknown",
        reason: `Recorded transaction rebroadcast remains unresolved: ${String(cause)}`,
      };
    }
  }
  return pending;
};
