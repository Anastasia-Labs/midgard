import {
  computeFraudProofRawL1PointId,
  FraudProofL1CheckpointChangedError,
  type FraudProofRawL1Point,
  type FraudProofRawL1Transaction,
} from "@al-ft/midgard-fault-proofs";
import type { FactStore } from "@al-ft/midgard-l1-follower";
import type { DaAvailabilityReadScope } from "@al-ft/midgard-sdk";

import type { WatcherAuthenticatedStateQueueObservation } from "../indexers/authenticated-state-queue-observation.js";
import { required } from "../l1-follower/fault-proof-l1-source.chain.js";
import type { FollowerRawReads } from "../l1-follower/raw-reads.types.js";

/**
 * The availability actor's L1 reads: the watcher follower's raw reads and
 * the store they read, for the canonical check that closes a capture.
 */
export type WatcherAvailabilityL1 = Readonly<{
  reads: FollowerRawReads;
  store: Pick<FactStore, "pointStatus">;
}>;

/** The raw point of an authenticated observation's native point. */
export const observationPoint = (
  observation: WatcherAuthenticatedStateQueueObservation,
): FraudProofRawL1Point => {
  const { blockHash, slot, blockNo } = observation.nativePoint;
  return {
    blockHash,
    slot,
    blockNo,
    pointId: computeFraudProofRawL1PointId({ blockHash, slot, blockNo }),
  };
};

/** Every read settles before the first failure is thrown. */
export const settleAll = async <T>(
  reads: readonly Promise<T>[],
): Promise<T[]> => {
  const settled = await Promise.allSettled(reads);
  const failed = settled.find(
    (result): result is PromiseRejectedResult => result.status === "rejected",
  );
  if (failed !== undefined) throw failed.reason;
  return settled.map((result) => (result as PromiseFulfilledResult<T>).value);
};

/**
 * Runs `read` against the follower at `point`, inside the scope when one is
 * given. Facts at a canonical point never change, so the reads agree with
 * each other exactly when the point is still canonical after the last one;
 * a point a rollback removed is `FraudProofL1CheckpointChangedError`.
 */
export const readAtPoint = async <T>(
  l1: WatcherAvailabilityL1,
  point: FraudProofRawL1Point,
  scope: DaAvailabilityReadScope | undefined,
  read: () => Promise<T>,
): Promise<T> => {
  scope?.assertCurrent();
  const result = await (scope === undefined ? read() : scope.read(read));
  const status = await l1.store.pointStatus({
    slot: Number(point.slot),
    hash: Buffer.from(point.blockHash, "hex"),
  });
  if (status.kind !== "canonical")
    throw new FraudProofL1CheckpointChangedError(
      `availability read point ${point.pointId} left the canonical chain: ${status.detail}`,
    );
  scope?.assertCurrent();
  return result;
};

/**
 * One stored transaction at its inclusion point, at least
 * `minimumConfirmationDepth` deep. Its bytes are what availability reads;
 * inputs the follower cannot resolve are left out of `resolvedInputs`.
 */
export const rawTransactionAt = async (
  reads: FollowerRawReads,
  txHash: string,
  inclusionPoint: FraudProofRawL1Point,
  minimumConfirmationDepth: number,
): Promise<FraudProofRawL1Transaction> => {
  const { transaction } = required(
    await reads.rawTransaction(txHash, inclusionPoint),
  );
  if (transaction.confirmationDepth < minimumConfirmationDepth)
    throw new Error(
      `L1 transaction ${txHash} is shallower than the required confirmation depth`,
    );
  return transaction;
};

/** The canonical history of one projected unit at `point`, as raw transactions. */
export const unitHistoryTransactions = async (
  reads: FollowerRawReads,
  unit: string,
  point: FraudProofRawL1Point,
  minimumConfirmationDepth: number,
): Promise<FraudProofRawL1Transaction[]> => {
  const history = required(await reads.unitHistoryAtPoint(unit, point));
  return await settleAll(
    history.transactions.map(({ txHash, inclusionPoint }) =>
      rawTransactionAt(reads, txHash, inclusionPoint, minimumConfirmationDepth),
    ),
  );
};
