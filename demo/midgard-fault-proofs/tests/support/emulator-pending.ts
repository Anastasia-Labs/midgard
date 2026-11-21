/**
 * The Lucid emulator's mempool as our node's mempool: the transactions
 * submitted since the last block, as `decodePendingTransactions` decodes their
 * signed bytes, and the emulator's own provider reads with them applied, so a
 * build resolves (and spends) the outputs they create, as a build over the
 * node's view does.
 *
 * The emulator keeps no transaction bytes, so `submitTx` is layered on the
 * instance to keep each accepted transaction's bytes by hash. Which of them
 * are pending is the emulator's own mempool: its entries are the outputs of
 * the transactions submitted since the last block, in submission order. A
 * block (`awaitBlock`, or `awaitSlot` crossing one) empties it; `drop`
 * empties it without a block, as a node discarding its mempool does.
 *
 * Install it on the emulator after any recorder layered on the same
 * instance's `submitTx` (`recordCrossBlockRawEmulator({ emulator })` calls the
 * prototype's, past any instance layer beneath it), and hand its `pending` to
 * that recorder, which otherwise layers the same reads itself
 * ({@link layerEmulatorMempool}). Restore in reverse order.
 */
import {
  decodePendingTransactions,
  type PendingTransaction,
} from "@al-ft/midgard-l1-follower/mempool";
import { layerMethod } from "@al-ft/midgard-test-support/method-layer";
import {
  type Credential,
  type Emulator,
  getAddressDetails,
  type UTxO,
} from "@lucid-evolution/lucid";

const label = (utxo: UTxO): string =>
  `${utxo.txHash}#${utxo.outputIndex.toString()}`;

export type EmulatorPending = Readonly<{
  /** The mempool's transactions, decoded, in submission order. */
  pending: () => readonly PendingTransaction[];
  /** Discards the mempool without a block: every pending transaction is
   * gone, and the ledger outputs they spent are unspent again. */
  drop: () => void;
  restore: () => void;
}>;

/**
 * The mempool layered over `target`'s provider reads: an emulator instance,
 * or `Emulator.prototype` for every emulator. `signedCbor` answers the bytes
 * of each transaction the emulator accepted, by hash.
 */
export const layerEmulatorMempool = (
  target: Emulator,
  signedCbor: (txHash: string) => string | undefined,
): Readonly<{
  pending: (emulator: Emulator) => readonly PendingTransaction[];
  restore: () => void;
}> => {
  const decoded = new Map<string, PendingTransaction>();
  const pending = (emulator: Emulator): readonly PendingTransaction[] =>
    [
      ...new Set(Object.keys(emulator.mempool).map((key) => key.slice(0, 64))),
    ].map((txHash) => {
      const known = decoded.get(txHash);
      if (known !== undefined) return known;
      const cbor = signedCbor(txHash);
      if (cbor === undefined)
        throw new Error(
          `mempool transaction ${txHash} was submitted before the pending layer`,
        );
      const transaction = decodePendingTransactions([
        Buffer.from(cbor, "hex"),
      ])[0]!;
      decoded.set(txHash, transaction);
      return transaction;
    });
  /** What the mempool creates and does not itself spend. */
  const created = (emulator: Emulator): readonly UTxO[] => {
    const transactions = pending(emulator);
    const spent = new Set(transactions.flatMap((tx) => tx.spends));
    return transactions
      .flatMap((tx) => tx.creates)
      .filter((output) => !spent.has(output.outRef))
      .map((output) => output.utxo);
  };
  const at = (addressOrCredential: string | Credential, utxo: UTxO) =>
    typeof addressOrCredential === "string"
      ? utxo.address === addressOrCredential
      : getAddressDetails(utxo.address).paymentCredential?.hash ===
        addressOrCredential.hash;
  // The emulator's reads already leave out a ledger output a mempool
  // transaction spends; each read adds what the mempool creates.
  const plus = (ledger: readonly UTxO[], extra: readonly UTxO[]): UTxO[] => {
    const known = new Set(ledger.map(label));
    return [...ledger, ...extra.filter((utxo) => !known.has(label(utxo)))];
  };
  const reads = [
    layerMethod(
      target,
      "getUtxos",
      (next) =>
        async function (this: Emulator, addressOrCredential) {
          return plus(
            await next.call(this, addressOrCredential),
            created(this).filter((utxo) => at(addressOrCredential, utxo)),
          );
        },
    ),
    layerMethod(
      target,
      "getUtxosWithUnit",
      (next) =>
        async function (this: Emulator, addressOrCredential, unit) {
          return plus(
            await next.call(this, addressOrCredential, unit),
            created(this).filter(
              (utxo) =>
                at(addressOrCredential, utxo) && (utxo.assets[unit] ?? 0n) > 0n,
            ),
          );
        },
    ),
    layerMethod(
      target,
      "getUtxosByOutRef",
      (next) =>
        async function (this: Emulator, outRefs) {
          const wanted = new Set(
            outRefs.map(
              ({ txHash, outputIndex }) =>
                `${txHash}#${outputIndex.toString()}`,
            ),
          );
          return plus(
            await next.call(this, outRefs),
            created(this).filter((utxo) => wanted.has(label(utxo))),
          );
        },
    ),
    layerMethod(
      target,
      "getUtxoByUnit",
      (next) =>
        async function (this: Emulator, unit) {
          const holders = created(this).filter(
            (utxo) => (utxo.assets[unit] ?? 0n) > 0n,
          );
          if (holders.length === 0) return await next.call(this, unit);
          if (holders.length > 1)
            throw new Error(
              "Unit needs to be an NFT or only held by one address.",
            );
          return holders[0]!;
        },
    ),
  ];
  return Object.freeze({
    pending,
    restore: () => {
      for (const read of [...reads].reverse()) read.restore();
    },
  });
};

export const emulatorPending = (emulator: Emulator): EmulatorPending => {
  const signed = new Map<string, string>();
  const submitted = layerMethod(
    emulator,
    "submitTx",
    (next) =>
      async function (this: Emulator, cbor: string) {
        const txHash = await next.call(this, cbor);
        signed.set(txHash, cbor);
        return txHash;
      },
  );
  const mempool = layerEmulatorMempool(emulator, (txHash) =>
    signed.get(txHash),
  );
  const pending = () => mempool.pending(emulator);
  return Object.freeze({
    pending,
    drop: () => {
      const spends = pending().flatMap((tx) => tx.spends);
      const ledger = emulator.ledger;
      for (const outRef of spends) {
        const entry = ledger[outRef.replace("#", "")];
        if (entry !== undefined) entry.spent = false;
      }
      const history = emulator.transactionHistory;
      for (const [txHash, { status }] of Object.entries(history))
        if (status === "pending") Reflect.deleteProperty(history, txHash);
      emulator.mempool = {};
    },
    restore: () => {
      mempool.restore();
      submitted.restore();
    },
  });
};
