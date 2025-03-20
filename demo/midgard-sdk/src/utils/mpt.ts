import { Trie } from "@aiken-lang/merkle-patricia-forestry";
import { Effect } from "effect";
import { CML } from "@lucid-evolution/lucid";

export const mptFromUTxOs = (utxos: string[]): Effect.Effect<Trie, Error> =>
  Effect.gen(function* () {
    const data = utxos.map((utxoCbor) => {
      const cmlUTxO = CML.TransactionUnspentOutput.from_cbor_hex(utxoCbor);
      return {
        key: cmlUTxO.input().to_cbor_hex(),
        value: cmlUTxO.output().to_cbor_hex(),
      };
    });

    const trie = yield* Effect.tryPromise({
      try: () => Trie.fromList(data),
      catch: (e) => new Error(`${e}`),
    });

    return trie;
  });

export const mptFromTxs = (
  txs: { txHash: string; txCbor: string }[],
): Effect.Effect<Trie, Error> =>
  Effect.gen(function* () {
    const data = txs.map(({ txHash, txCbor }) => {
      return {
        key: txHash,
        value: txCbor,
      };
    });

    const trie = yield* Effect.tryPromise({
      try: () => Trie.fromList(data),
      catch: (e) => new Error(`${e}`),
    });

    return trie;
  });

/**
 * Create a Merkle Patricia Trie (MPT) from a list of items.
 *
 * @param items - An array of items to be included in the trie.
 * @returns An Effect that resolves to the created Trie.
 */
export const mptFromList = <T>(items: T[]): Effect.Effect<Trie, never, never> =>
  Effect.gen(function* () {
    const data = items.map((item) => ({
      key: item,
      value: "",
    }));

    const trie = yield* Effect.promise(() => Trie.fromList(data));
    return trie;
  });
