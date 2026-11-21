import { formatUnknownError } from "@al-ft/midgard-core";

import type {
  RemoveFraudulentBlockBuild,
  StateQueueMutationLease,
} from "../remove-fraudulent-block.js";
import type { LocallyEvaluatedTransaction } from "./transaction-boundary.js";

/** A built removal a journaled adapter accepted, with the lease it holds. */
export type AdmittedRemovalTransaction = Readonly<{
  transaction: LocallyEvaluatedTransaction;
  mutationLease?: StateQueueMutationLease;
}>;

/**
 * Runs a journaled adapter's check of a built removal. When the check refuses
 * the transaction, the state-queue mutation lease the build took is failed
 * with the refusal before it is rethrown, so no lease outlives a removal that
 * is never submitted. Otherwise returns the transaction and the lease, which
 * the adapter retains across its durable intent and submission.
 */
export const admitRemovalTransaction = async (
  built: RemoveFraudulentBlockBuild,
  check: (transaction: LocallyEvaluatedTransaction) => void | Promise<void>,
): Promise<AdmittedRemovalTransaction> => {
  try {
    await check(built.transaction);
  } catch (error) {
    await built.stateQueueMutationLease?.fail(formatUnknownError(error));
    throw error;
  }
  return Object.freeze({
    transaction: built.transaction,
    ...(built.stateQueueMutationLease === null
      ? {}
      : { mutationLease: built.stateQueueMutationLease }),
  });
};
