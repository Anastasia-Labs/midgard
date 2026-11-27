import { createHash } from "node:crypto";

import type { L1NodeTransport } from "@al-ft/l1-node-transport";
import {
  FraudProofL1CheckpointChangedError,
  FraudProofL1UnavailableError,
} from "@al-ft/midgard-fault-proofs/workflow/l1-source";
import type { FactStore, View } from "@al-ft/midgard-l1-follower";
import {
  decodePendingTransactions,
  type PendingTransaction,
} from "@al-ft/midgard-l1-follower/mempool";
import {
  type LedgerTip,
  pendingOutsideFacts,
} from "@al-ft/midgard-l1-follower/provider";

import {
  currentView,
  withCheckpointRetries,
} from "./fault-proof-l1-source.chain.js";

/**
 * The node's mempool at the follower's view: what the fault-proof workflow
 * derives its next action from and builds it against, the ledger at the
 * node's tip with every mempool transaction applied.
 *
 * One read lists the mempool, then reads the node's tip, then the
 * follower's view, and holds only when the view is at that tip. A
 * transaction that landed before the listing is then in the follower's
 * facts, and one that landed after it is listed and in the facts, where
 * the facts win: no transaction of ours is ever absent from both.
 */

/** The follower's view is not at the node's tip: the read retries. */
export class WatcherFollowerNotAtNodeTipError extends FraudProofL1CheckpointChangedError {
  constructor(detail: string) {
    super(detail);
    this.name = "WatcherFollowerNotAtNodeTipError";
  }
}

export type WatcherPendingAtTip = Readonly<{
  /** The follower's view, at the node's tip. */
  view: View;
  /** The node's mempool transactions the facts do not show applied, in mempool order. */
  pending: readonly PendingTransaction[];
}>;

export type WatcherPendingRead = Readonly<{
  /** One consistent read; a view off the node's tip is `WatcherFollowerNotAtNodeTipError`. */
  atTip(): Promise<WatcherPendingAtTip>;
  /**
   * The pending transactions builds read against: the last read's while
   * the follower's view is the one it was read at, else those of a new
   * read under the checkpoint retries.
   */
  latest(): Promise<readonly PendingTransaction[]>;
}>;

const viewKey = (view: View): string =>
  `${view.generation.toString()}:${view.point.slot.toString()}:${view.point.hash.toString("hex")}`;

const listingDigest = (txs: readonly Uint8Array[]): string => {
  const hash = createHash("sha256");
  for (const tx of txs) {
    const length = Buffer.alloc(4);
    length.writeUInt32BE(tx.length);
    hash.update(length).update(tx);
  }
  return hash.digest("hex");
};

/**
 * Runs `attempt` under the checkpoint retries; a view still off the node's
 * tip after them is `FraudProofL1UnavailableError`.
 */
export const withNodeTipRetries = async <T>(
  attempt: () => Promise<T>,
): Promise<T> => {
  try {
    return await withCheckpointRetries(attempt);
  } catch (error) {
    if (error instanceof WatcherFollowerNotAtNodeTipError)
      throw new FraudProofL1UnavailableError(
        `the follower is not at the node's tip: ${error.message}`,
        { cause: error },
      );
    throw error;
  }
};

export const createWatcherPendingRead = (
  input: Readonly<{
    store: FactStore;
    node: Pick<L1NodeTransport, "mempoolTxs">;
    /** The node's ledger tip (local state query). */
    nodeTip: () => Promise<Pick<LedgerTip, "slot" | "hash">>;
  }>,
): WatcherPendingRead => {
  // The decoded, filtered listing of one (view, listing) pair.
  let memo:
    | Readonly<{ key: string; view: string; read: WatcherPendingAtTip }>
    | undefined;
  const atTip = async (): Promise<WatcherPendingAtTip> => {
    const listing = await input.node.mempoolTxs();
    const tip = await input.nodeTip();
    const view = await currentView(input.store);
    if (
      view.point.slot !== tip.slot ||
      view.point.hash.toString("hex") !== tip.hash
    )
      throw new WatcherFollowerNotAtNodeTipError(
        `the follower's view is at slot ${view.point.slot.toString()}, the node's tip at slot ${tip.slot.toString()}`,
      );
    const key = `${viewKey(view)}|${listingDigest(listing)}`;
    if (memo?.key === key) return memo.read;
    const read = Object.freeze({
      view,
      pending: await pendingOutsideFacts(
        input.store,
        decodePendingTransactions(listing),
      ),
    });
    memo = { key, view: viewKey(view), read };
    return read;
  };
  return Object.freeze({
    atTip,
    latest: async () => {
      const last = memo;
      if (last !== undefined) {
        const view = await currentView(input.store);
        if (viewKey(view) === last.view) return last.read.pending;
      }
      return (await withNodeTipRetries(atTip)).pending;
    },
  });
};
