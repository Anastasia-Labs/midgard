/** Replay work counted as the leaves of every Merkle Patricia trie built, at
 * the trie library the replay uses: each root check of a ledger builds a trie
 * the size of that ledger. */
import { Trie } from "@aiken-lang/merkle-patricia-forestry";
import { encodeMidgardSpendInputItem } from "@al-ft/midgard-core";

import type { LedgerEntry } from "../../src/ledger-at.js";

/** A plain output at a script address, holding no value. */
const PLAIN_OUTPUT = Buffer.from(
  "a200581d70aaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaa018200a0",
  "hex",
);

/** `count` predecessor entries no event of a block touches. */
export const filler = (count: number): LedgerEntry[] =>
  Array.from({ length: count }, (_, index) => {
    const txId = Buffer.alloc(32, 0x5a);
    txId.writeUInt32BE(index);
    return {
      outRef: encodeMidgardSpendInputItem({ txId, outputIndex: 0 }),
      output: PLAIN_OUTPUT,
    };
  });

/** Runs `run`, counting the leaves of every trie it builds. */
export const trieLeaves = async <T>(
  run: () => Promise<T>,
): Promise<{ result: T; leaves: number }> => {
  const fromList = Trie.fromList.bind(Trie);
  let leaves = 0;
  Trie.fromList = (async (elements: readonly unknown[]) => {
    leaves += elements.length;
    return await fromList(elements as never);
  }) as typeof Trie.fromList;
  try {
    return { result: await run(), leaves };
  } finally {
    Trie.fromList = fromList;
  }
};
