import type {
  StateQueueMutationLease,
  StateQueueMutationLeaseCoordinator,
  StateQueueMutationLeaseIdentity,
} from "../remove-fraudulent-block.js";
import type { FraudProofFamilyL1ObservationPort } from "./family-l1-observation.js";
import type { FraudProofWorkflowReconcileResult } from "./orchestrator.js";
import {
  reconcileSignedWorkflowTransaction,
  type SignedWorkflowTransaction,
} from "./signed-transaction-reconciliation.js";

/** The shared signed read of a family's recorded attempt. */
export type RecoverUnconfirmedTransaction =
  () => Promise<FraudProofWorkflowReconcileResult>;

/**
 * One reconcile of a state-queue family that may hold the shared queue
 * mutation lease. The family's own state read gets the shared signed read of
 * its recorded attempt, which it calls while its stage is unchanged and the
 * attempt is not on chain: a landing that failed phase 2, an expiry or an
 * invalidation then reaches the workflow from the attempt's own bytes, and a
 * replay renews the lease first. A lease the attempt's durable recovery names
 * is resumed after the read, since a removal that landed may have ended it.
 */
export const reconcileQueueLeaseFamily = async ({
  category,
  txHash,
  recovery,
  coordinator,
  leaseByTxHash,
  l1,
  signedTransactionCborHex,
  authorizeResubmission,
  read,
}: {
  readonly category: string;
  readonly txHash?: string;
  readonly recovery?: StateQueueMutationLeaseIdentity;
  readonly coordinator: StateQueueMutationLeaseCoordinator;
  readonly leaseByTxHash: Map<string, StateQueueMutationLease>;
  readonly l1: Pick<
    FraudProofFamilyL1ObservationPort<never>,
    "observeSignedTransaction" | "rebroadcastSignedTransaction"
  >;
  readonly signedTransactionCborHex?: string;
  readonly authorizeResubmission?: (
    input: SignedWorkflowTransaction,
  ) => Promise<void>;
  readonly read: (
    recoverUnconfirmedTransaction: RecoverUnconfirmedTransaction | undefined,
  ) => Promise<FraudProofWorkflowReconcileResult>;
}): Promise<FraudProofWorkflowReconcileResult> => {
  let lease = txHash === undefined ? undefined : leaseByTxHash.get(txHash);
  const restoreLease = async () => {
    if (lease !== undefined || recovery === undefined) return;
    if (coordinator.resume === undefined) {
      throw new Error(
        `${category} lease coordinator cannot resume durable intent`,
      );
    }
    lease = await coordinator.resume(recovery);
    if (txHash !== undefined) leaseByTxHash.set(txHash, lease);
  };
  const { observeSignedTransaction, rebroadcastSignedTransaction } = l1;
  const result = await read(
    txHash === undefined
      ? undefined
      : () =>
          reconcileSignedWorkflowTransaction({
            transactionHash: txHash,
            signedTransactionCborHex,
            observe:
              observeSignedTransaction === undefined
                ? undefined
                : (input) => observeSignedTransaction.call(l1, input),
            rebroadcast:
              rebroadcastSignedTransaction === undefined
                ? undefined
                : (input) => rebroadcastSignedTransaction.call(l1, input),
            authorizeResubmission:
              authorizeResubmission === undefined
                ? undefined
                : async (signed) => {
                    await restoreLease();
                    await lease?.renew();
                    await authorizeResubmission(signed);
                  },
          }),
  );
  // Canonical reconciliation remains possible after a removal has made its
  // old lease unavailable. A live lease is still required for any replay.
  try {
    await restoreLease();
  } catch (cause) {
    if (result.kind === "pending" || result.kind === "unknown") {
      return {
        kind: "unknown",
        reason: `${category} durable lease cannot resume: ${String(cause)}`,
      };
    }
  }
  if (result.kind === "confirmed") {
    await lease?.release();
    leaseByTxHash.delete(result.txHash);
  } else if (result.kind === "not_found") {
    await lease?.release();
    if (txHash !== undefined) leaseByTxHash.delete(txHash);
  } else if (result.kind === "conflict") {
    await lease?.fail(result.reason);
    if (txHash !== undefined) leaseByTxHash.delete(txHash);
  } else {
    await lease?.renew();
  }
  return result;
};
