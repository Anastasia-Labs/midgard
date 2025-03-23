import { Store, Trie } from "@aiken-lang/merkle-patricia-forestry";
import { Effect } from "effect";
import { inspect } from "node:util";

export const mptFromUTxOs = (
  spentUtxos: Uint8Array[],
  producedUtxos: { outputReference: Uint8Array; output: Uint8Array }[],
  ledgerAfterUpdate: { outputReference: Uint8Array; output: Uint8Array }[],
): Effect.Effect<Trie, Error> =>
  Effect.gen(function* () {
    const store = new Store("utxosStore");

    yield* Effect.tryPromise({
      try: () => store.ready(),
      catch: (e) => new Error(`${e}`),
    });

    const trieProgram = Effect.tryPromise({
      try: async () => Trie.load(store),
      catch: (e) => new Error(`${e}`),
    });

    let trie: Trie = yield* Effect.catchAll(trieProgram, (e) =>
      Effect.gen(function* () {
        yield* Effect.logInfo(
          `📁 Failed to load store, starting with a new one... ${e}`,
        );
        return new Trie(store);
      }),
    );

    yield* Effect.logInfo(`🌲 UTxO trie loaded: ${inspect(trie)}`);

    const deletePrograms = spentUtxos.map((outputReference) =>
      Effect.tryPromise({
        try: () => trie.delete(Buffer.from(outputReference)),
        catch: (e) => new Error(`${e}`),
      }),
    );

    const insertPrograms = producedUtxos.map(({ outputReference, output }) =>
      Effect.tryPromise({
        try: () =>
          trie.insert(Buffer.from(outputReference), Buffer.from(output)),
        catch: (e) => new Error(`${e}`),
      }),
    );

    // Silently ignoring failed updates and falling back to `fromList`.
    yield* Effect.catchAll(
      Effect.all([...deletePrograms, ...insertPrograms], { concurrency: 1 }),
      (e) =>
        Effect.gen(function* () {
          yield* Effect.logInfo(
            `😔 Fallback to \`fromList\`... Failed to update trie: ${e}`,
          );
          yield* Effect.try({
            try: async () => {
              trie = await Trie.fromList(
                ledgerAfterUpdate.map(({ outputReference, output }) => ({
                  key: Buffer.from(outputReference),
                  value: Buffer.from(output),
                })),
                store,
              );
            },
            catch: (e) => new Error(`${e}`),
          });
        }),
    );

    yield* Effect.logInfo(`🌲 Updated UTxO trie: ${inspect(trie)}`);

    return trie;
  });

export const mptFromTxs = (
  txs: { txHash: Uint8Array; txCbor: Uint8Array }[],
): Effect.Effect<Trie, Error> =>
  Effect.gen(function* () {
    const store = new Store("txsStore");

    yield* Effect.tryPromise({
      try: () => store.ready(),
      catch: (e) => new Error(`${e}`),
    });

    const trie = new Trie(store);

    yield* Effect.forEach(
      txs,
      ({ txHash, txCbor }) =>
        Effect.tryPromise({
          try: () => trie.insert(Buffer.from(txHash), Buffer.from(txCbor)),
          catch: (e) => new Error(`${e}`),
        }),
      { concurrency: 1 }, // omitting this is equivalent to sequential traversal.
    );

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
