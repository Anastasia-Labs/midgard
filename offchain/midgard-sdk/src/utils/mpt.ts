import { Trie } from "@aiken-lang/merkle-patricia-forestry";
import { Effect } from "effect";

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
