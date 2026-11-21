/**
 * The node's mempool as a pending layer over the ledger at its tip.
 *
 * The node checks the next submitted transaction against the ledger with
 * every mempool transaction applied in mempool order, so a read that decides
 * what to build next uses the same state: an input a pending transaction
 * spends (regular inputs only; collateral is consumed only when scripts fail)
 * is gone, and an output it creates exists. Spent wins over created, so an
 * output one pending transaction creates and a later one spends is listed
 * nowhere.
 *
 * A pending transaction has no inclusion point and no depth: it is never
 * ledger evidence and its type carries nothing that could pass for it.
 *
 * Decoding uses the follower's own transaction decoder, so a pending output
 * has exactly the Lucid shape the follower gives the same output once it
 * lands. The only wasm object involved (an address being rendered) is freed
 * before the call returns.
 */
import type { UTxO } from "@lucid-evolution/lucid";

import { decodeTransaction } from "../decode/tx.js";
import { toLucidUtxo } from "../provider/utxo.js";
import type { OutRef } from "../types.js";

/** One output a pending transaction creates. */
export type PendingOutput = Readonly<{
  /** `${txHash}#${index}`. */
  outRef: string;
  /** The full output: value, datum and script reference. */
  utxo: UTxO;
}>;

/** One transaction of the node's mempool, decoded. */
export type PendingTransaction = Readonly<{
  txHash: string;
  /** The full transaction as the mempool listed it. */
  cborHex: string;
  /** Regular inputs, as out-ref labels. */
  spends: readonly string[];
  /** Reference inputs, as out-ref labels. */
  reads: readonly string[];
  creates: readonly PendingOutput[];
  /** The TTL slot, when the transaction sets one. */
  validTo: bigint | undefined;
}>;

/** The ledger with the pending transactions applied. */
export type PendingOverlay = Readonly<{
  /** Every out-ref a pending transaction spends. */
  spent: ReadonlySet<string>;
  /** `ledger`, one address's ledger listing, minus what pending
   * transactions spend, plus what they create at `address`. */
  overAddress: (address: string, ledger: readonly UTxO[]) => readonly UTxO[];
  /** The outputs named by `outRefs` that exist with pending applied: from
   * `ledger` (the ledger's answer for them) or created by a pending
   * transaction, in the order of `outRefs`. */
  overOutRefs: (
    outRefs: readonly string[],
    ledger: readonly UTxO[],
  ) => readonly UTxO[];
}>;

const label = (outRef: OutRef): string =>
  `${outRef.txHash.toString("hex")}#${outRef.index.toString()}`;

const utxoLabel = (utxo: UTxO): string =>
  `${utxo.txHash}#${utxo.outputIndex.toString()}`;

const decodeOne = (bytes: Uint8Array): PendingTransaction => {
  const tx = decodeTransaction(bytes);
  const txHash = tx.hash.toString("hex");
  return Object.freeze({
    txHash,
    cborHex: Buffer.from(bytes).toString("hex"),
    spends: Object.freeze(tx.inputs.map(label)),
    reads: Object.freeze(tx.referenceInputs.map(label)),
    creates: Object.freeze(
      tx.outputs.map((output, index) =>
        Object.freeze({
          outRef: `${txHash}#${index.toString()}`,
          utxo: toLucidUtxo({ txHash: tx.hash, index }, output),
        }),
      ),
    ),
    validTo: tx.invalidAfter ?? undefined,
  });
};

/** The mempool transactions `txs` (each one's full CBOR), decoded in order. */
export const decodePendingTransactions = (
  txs: readonly Uint8Array[],
): readonly PendingTransaction[] => Object.freeze(txs.map(decodeOne));

/** Adds each of `utxos` not already in `into`, keyed by out-ref. */
const addAbsent = (into: Map<string, UTxO>, utxos: readonly UTxO[]): void => {
  for (const utxo of utxos) {
    const key = utxoLabel(utxo);
    if (!into.has(key)) into.set(key, utxo);
  }
};

/** The overlay of `pending`, in mempool order, on any ledger listing. */
export const overlayPending = (
  pending: readonly PendingTransaction[],
): PendingOverlay => {
  const spent: ReadonlySet<string> = new Set(
    pending.flatMap((tx) => tx.spends),
  );
  const created = pending
    .flatMap((tx) => tx.creates)
    .filter((output) => !spent.has(output.outRef));
  return Object.freeze({
    spent,
    overAddress: (address, ledger) => {
      const listed = new Map<string, UTxO>();
      addAbsent(
        listed,
        ledger.filter((utxo) => !spent.has(utxoLabel(utxo))),
      );
      addAbsent(
        listed,
        created
          .filter((output) => output.utxo.address === address)
          .map((output) => output.utxo),
      );
      return [...listed.values()];
    },
    overOutRefs: (outRefs, ledger) => {
      const known = new Map<string, UTxO>();
      addAbsent(known, ledger);
      addAbsent(
        known,
        created.map((output) => output.utxo),
      );
      return [...new Set(outRefs)]
        .filter((outRef) => !spent.has(outRef))
        .flatMap((outRef) => known.get(outRef) ?? []);
    },
  });
};
