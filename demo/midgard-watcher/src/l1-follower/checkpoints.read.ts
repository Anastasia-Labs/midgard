import { computeFraudProofRawL1PointId } from "@al-ft/midgard-fault-proofs";
import type { SqlRow, SqlTx } from "@al-ft/midgard-l1-follower";
import * as SDK from "@al-ft/midgard-sdk";

import type { WatcherQueueTransition } from "./checkpoints.js";
import { WATCHER_QUEUE_CHECKPOINTS_TABLE } from "./tables.js";

/**
 * Read-time access to the watcher's transition checkpoints (ticket W1): the
 * rows the checkpoint derivation wrote, turned into SDK checkpoints at a
 * finality depth, with refusals where pruning may have removed rows.
 */

/** The checkpoints of one block, read at a finality depth. */
export type WatcherBlockCheckpoints =
  | Readonly<{
      kind: "ok";
      checkpoints: readonly SDK.StateQueueAuthenticatedReplayCheckpoint[];
      correctionLockWitnesses: readonly SDK.StateQueueCorrectionLockWitness[];
    }>
  | Readonly<{ kind: "failed"; transactionHash: string; failure: string }>
  /** The block is at or below the pruned window: its rows may be partial. */
  | Readonly<{ kind: "beyond_retention"; prunedThroughSlot: number }>;

export type CheckpointReadOptions = Readonly<{
  deploymentIdentityDigest: string;
  stateQueuePolicyId: string;
  finalityDepth: number;
  /** The store cursor's `prunedThroughSlot`. */
  prunedThroughSlot: number;
}>;

/**
 * The SDK checkpoints of the block at `block`, as the authenticated
 * observation of that block records them at `finalityDepth`. A block at or
 * below the pruned window is refused: pins keep some of its rows, so the
 * rest may be gone. Read a pinned transaction with
 * {@link readWatcherTransactionCheckpoint}.
 */
export const readWatcherCheckpoints = async (
  tx: SqlTx,
  block: Readonly<{ slot: number; hash: Buffer; height: number }>,
  options: CheckpointReadOptions,
): Promise<WatcherBlockCheckpoints> => {
  if (block.slot <= options.prunedThroughSlot)
    return {
      kind: "beyond_retention",
      prunedThroughSlot: options.prunedThroughSlot,
    };
  const rows = await tx.query(
    `SELECT tx_hash, transition, failure FROM ${WATCHER_QUEUE_CHECKPOINTS_TABLE} WHERE block_slot = ? AND block_hash = ? ORDER BY tx_index`,
    [block.slot, block.hash],
  );
  return checkpointsOf(rows, block, options);
};

/** One transaction's checkpoint, read at a finality depth. */
export type WatcherTransactionCheckpoint =
  | WatcherBlockCheckpoints
  /** The tx is stored and is not a state-queue transition (or failed phase 2). */
  | Readonly<{ kind: "none" }>
  /** No stored tx has this hash and nothing was pruned: it is not on the chain. */
  | Readonly<{ kind: "unknown" }>;

/**
 * The checkpoint of one transaction, wherever it is stored. A header's
 * history transactions are pinned until the header is merged and k deep,
 * so their checkpoints answer below the pruned window too. A transaction
 * no longer stored after pruning, or stored at or below the pruned window
 * without a row, is `beyond_retention`, never `unknown` or `none`.
 */
export const readWatcherTransactionCheckpoint = async (
  tx: SqlTx,
  txHash: Buffer,
  options: CheckpointReadOptions & Readonly<{ originSlot: number }>,
): Promise<WatcherTransactionCheckpoint> => {
  const rows = await tx.query(
    `SELECT c.tx_hash, c.transition, c.failure, c.block_slot, c.block_hash, c.block_height FROM ${WATCHER_QUEUE_CHECKPOINTS_TABLE} c WHERE c.tx_hash = ?`,
    [txHash],
  );
  const row = rows[0];
  if (row !== undefined)
    return checkpointsOf(
      [row],
      {
        slot: Number(row.block_slot as number | string),
        hash: Buffer.from(row.block_hash as Uint8Array),
        height: Number(row.block_height as number | string),
      },
      options,
    );
  const stored = await tx.query(
    "SELECT block_slot FROM l1_txs WHERE tx_hash = ?",
    [txHash],
  );
  // A stored tx without a row did not touch the queue, unless its row may
  // have been pruned with its block.
  const slot = stored[0]?.block_slot;
  if (
    slot !== undefined &&
    Number(slot as number | string) > options.prunedThroughSlot
  )
    return { kind: "none" };
  return options.prunedThroughSlot > options.originSlot
    ? {
        kind: "beyond_retention",
        prunedThroughSlot: options.prunedThroughSlot,
      }
    : { kind: "unknown" };
};

const checkpointsOf = (
  rows: readonly SqlRow[],
  block: Readonly<{ slot: number; hash: Buffer; height: number }>,
  options: CheckpointReadOptions,
): WatcherBlockCheckpoints => {
  const point = {
    blockHash: block.hash.toString("hex"),
    slot: block.slot.toString(),
    blockNo: block.height.toString(),
  };
  const chainPointId = computeFraudProofRawL1PointId(point);
  const checkpoints: SDK.StateQueueAuthenticatedReplayCheckpoint[] = [];
  for (const row of rows) {
    const txHash = Buffer.from(row.tx_hash as Uint8Array).toString("hex");
    if (row.failure !== null)
      return {
        kind: "failed",
        transactionHash: txHash,
        failure: row.failure as string,
      };
    const stored = JSON.parse(
      row.transition as string,
    ) as WatcherQueueTransition;
    const checkpoint = SDK.deriveStateQueueAuthenticatedReplayCheckpoint({
      deploymentIdentityDigest: options.deploymentIdentityDigest,
      stateQueuePolicyId: options.stateQueuePolicyId,
      transactionHash: stored.transactionHash,
      blockHash: point.blockHash,
      slot: point.slot,
      blockNo: point.blockNo,
      transactionIndex: stored.transactionIndex,
      chainPointId,
      finalityDepth: options.finalityDepth.toString(),
      mintPolicyIds: stored.mintPolicyIds,
      redeemers: stored.redeemers,
      spentInputOutRefs: stored.spentInputOutRefs,
      referenceInputOutRefs: stored.referenceInputOutRefs,
      correctionLockWitness: stored.correctionLockWitness,
      previousQueue: stored.previousQueue,
      nextQueue: stored.nextQueue,
    });
    if (checkpoint === null)
      return {
        kind: "failed",
        transactionHash: txHash,
        failure:
          "state-queue transaction failed authenticated checkpoint derivation",
      };
    checkpoints.push(checkpoint);
  }
  return {
    kind: "ok",
    checkpoints,
    correctionLockWitnesses: checkpoints.map(
      ({ correctionLockWitness: witness }) => witness,
    ),
  };
};
