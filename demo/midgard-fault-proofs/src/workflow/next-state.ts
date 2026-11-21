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
import type { PendingTransaction } from "@al-ft/midgard-l1-follower/mempool";
import { CML } from "@lucid-evolution/lucid";

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

/** The outputs of the transaction `txHash`, as raw outputs; frees each. */
const rawOutputsOf = (
  txHash: string,
  outputs: CML.TransactionOutputList,
): readonly FraudProofRawL1Utxo[] =>
  Object.freeze(
    Array.from({ length: outputs.len() }, (_, index) => {
      const output = outputs.get(index);
      const datum = output.datum();
      const inline = datum?.as_datum();
      const scriptRef = output.script_ref();
      try {
        return Object.freeze({
          outRef: `${txHash}#${index.toString()}`,
          outputCbor: output.to_canonical_cbor_hex(),
          datumCbor: inline?.to_canonical_cbor_hex() ?? null,
          referenceScriptCbor: scriptRef?.to_canonical_cbor_hex() ?? null,
        });
      } finally {
        scriptRef?.free();
        inline?.free();
        datum?.free();
        output.free();
      }
    }),
  );

/** The outputs the transaction body `bodyCbor` of `txHash` pays, as raw
 * outputs in the canonical encoding an admitted snapshot holds. */
export const bodyRawOutputs = (
  txHash: string,
  bodyCbor: string,
): readonly FraudProofRawL1Utxo[] => {
  const body = CML.TransactionBody.from_cbor_hex(bodyCbor);
  const outputs = body.outputs();
  try {
    return rawOutputsOf(txHash, outputs);
  } finally {
    outputs.free();
    body.free();
  }
};

/** `pending`'s body and witness set as CBOR, and its outputs as raw outputs. */
export type PendingParts = Readonly<{
  bodyCbor: string;
  witnessSetCbor: string;
  outputs: readonly FraudProofRawL1Utxo[];
}>;

const decodedParts = new WeakMap<PendingTransaction, PendingParts>();

/** `pending`'s body, witness set and outputs, decoded once and freed. */
export const pendingParts = (pending: PendingTransaction): PendingParts => {
  const known = decodedParts.get(pending);
  if (known !== undefined) return known;
  const tx = CML.Transaction.from_cbor_hex(pending.cborHex);
  const body = tx.body();
  const witnesses = tx.witness_set();
  const outputs = body.outputs();
  try {
    const parts = Object.freeze({
      bodyCbor: body.to_cbor_hex(),
      witnessSetCbor: witnesses.to_cbor_hex(),
      outputs: rawOutputsOf(pending.txHash, outputs),
    });
    decodedParts.set(pending, parts);
    return parts;
  } finally {
    outputs.free();
    witnesses.free();
    body.free();
    tx.free();
  }
};

/** `pending`'s outputs as raw outputs. */
export const pendingRawOutputs = (
  pending: PendingTransaction,
): readonly FraudProofRawL1Utxo[] => pendingParts(pending).outputs;

/** `pending`'s transaction body, as CBOR. */
export const pendingBodyCbor = (pending: PendingTransaction): string =>
  pendingParts(pending).bodyCbor;

/** Every out-ref the pending transactions spend. */
export const pendingSpent = (
  pending: readonly PendingTransaction[],
): ReadonlySet<string> => new Set(pending.flatMap((tx) => tx.spends));

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
  const spent = pendingSpent(pending);
  const scopes = snapshot.scopes.map((scope) => {
    const wanted =
      scope.outRefs === undefined ? undefined : new Set(scope.outRefs);
    const listed = new Map<string, FraudProofRawL1Utxo>();
    for (const utxo of scope.utxos)
      if (!spent.has(utxo.outRef)) listed.set(utxo.outRef, utxo);
    for (const tx of pending) {
      const at = tx.creates.filter(
        ({ outRef, utxo }) =>
          utxo.address === scope.address &&
          !spent.has(outRef) &&
          !listed.has(outRef) &&
          (wanted === undefined || wanted.has(outRef)),
      );
      if (at.length === 0) continue;
      const raws = pendingRawOutputs(tx);
      for (const { outRef } of at) {
        const raw = raws.find((candidate) => candidate.outRef === outRef);
        if (raw !== undefined) listed.set(outRef, raw);
      }
    }
    return Object.freeze({
      ...scope,
      utxos: Object.freeze([...listed.values()]),
    });
  });
  return Object.freeze({ scopes: Object.freeze(scopes), pending, createdRank });
};
