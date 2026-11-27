/**
 * The state the next action is derived from: the ledger at our node's tip
 * with every transaction of its mempool applied in mempool order, which is
 * exactly what the node checks our next transaction against.
 *
 * An output a pending transaction spends (regular inputs only) is gone and
 * an output it creates exists; spent wins. An action whose transaction is in
 * the mempool is therefore no longer required: its effect is in the view,
 * and the next action spends the outputs it creates. A transaction that left
 * the mempool without landing is simply absent, and its action is required
 * again.
 *
 * The view decides what to do next and nothing else. Evidence (terminals,
 * completion, depths, authenticated snapshots and their digests) reads the
 * admitted ledger snapshot alone: a pending transaction has no inclusion
 * point, and the view carries none.
 */
import {
  overlayPending,
  type PendingTransaction,
} from "@al-ft/midgard-l1-follower/mempool";

import type {
  FraudProofRawL1Snapshot,
  FraudProofRawL1Utxo,
} from "./raw-l1-snapshot.compute-fraud-proof-raw-l1-snapshot-evidence-digest.js";

/** The address scopes a derivation reads: a ledger snapshot's, or a view's. */
export type FraudProofRawL1Scopes = Pick<FraudProofRawL1Snapshot, "scopes">;

/** Ledger plus pending, scope by scope. */
export type FraudProofNextView = Readonly<{
  /** Each scope of the ledger snapshot with the pending transactions applied. */
  scopes: FraudProofRawL1Snapshot["scopes"];
  /** The pending transactions, in mempool order. */
  pending: readonly PendingTransaction[];
  /**
   * How new the transaction that created an output of the view is: its
   * inclusion slot for a ledger transaction, and after every ledger
   * transaction, in mempool order, for a pending one. -1 when the snapshot
   * holds no creation for it.
   */
  createdRank: (txHash: string) => bigint;
}>;

/**
 * The next view over an admitted ledger snapshot: every scope minus what the
 * pending transactions spend, plus what they create at the scope's address
 * (only its out-refs, for an out-ref scope), de-duplicated by out-ref.
 */
export const nextViewOf = ({
  snapshot,
  pending,
}: {
  readonly snapshot: FraudProofRawL1Snapshot;
  readonly pending: readonly PendingTransaction[];
}): FraudProofNextView => {
  const ledgerRank = new Map(
    snapshot.transactions.map(
      (transaction) =>
        [transaction.txHash, BigInt(transaction.inclusionPoint.slot)] as const,
    ),
  );
  const newestLedger = [...ledgerRank.values()].reduce(
    (newest, slot) => (slot > newest ? slot : newest),
    -1n,
  );
  const pendingRank = new Map(
    pending.map((tx, index) => [tx.txHash, BigInt(index)] as const),
  );
  const createdRank = (txHash: string): bigint => {
    const ranked = pendingRank.get(txHash);
    if (ranked !== undefined) return newestLedger + 1n + ranked;
    return ledgerRank.get(txHash) ?? -1n;
  };
  if (pending.length === 0)
    return Object.freeze({ scopes: snapshot.scopes, pending, createdRank });
  const { spent, created } = overlayPending(pending);
  const scopes = snapshot.scopes.map((scope) => {
    const wanted =
      scope.outRefs === undefined ? undefined : new Set(scope.outRefs);
    const listed = new Map<string, FraudProofRawL1Utxo>();
    for (const utxo of scope.utxos)
      if (!spent.has(utxo.outRef)) listed.set(utxo.outRef, utxo);
    for (const { outRef, utxo, raw } of created)
      if (
        utxo.address === scope.address &&
        !listed.has(outRef) &&
        (wanted === undefined || wanted.has(outRef))
      )
        listed.set(outRef, raw);
    return Object.freeze({
      ...scope,
      utxos: Object.freeze([...listed.values()]),
    });
  });
  return Object.freeze({ scopes: Object.freeze(scopes), pending, createdRank });
};
