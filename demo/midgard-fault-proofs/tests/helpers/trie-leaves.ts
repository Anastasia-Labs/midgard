/** Replay work counted as Merkle Patricia trie leaves, at the trie library the
 * replay uses: every leaf of a trie built from a list, and every leaf inserted
 * into a trie. Each root check of a ledger builds a trie the size of that
 * ledger, and each admission of a ledger to a store inserts every entry. */
import { AsyncLocalStorage } from "node:async_hooks";

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

/** The trie node classes, each with its own `insert`. */
const trieClasses = async (): Promise<readonly (typeof Trie)[]> => {
  const leaf = await Trie.fromList([{ key: "0", value: "" }]);
  const branch = await Trie.fromList([
    { key: "0", value: "" },
    { key: "1", value: "" },
  ]);
  return [Trie, leaf.constructor, branch.constructor] as (typeof Trie)[];
};

/** Runs `run`, counting the leaves of every trie it builds, and every leaf it
 * inserts (once, however many nodes the insert passes through). */
export const trieLeaves = async <T>(
  run: () => Promise<T>,
): Promise<{ result: T; leaves: number }> => {
  const classes = await trieClasses();
  const fromList = Trie.fromList.bind(Trie);
  const inserts = classes.map(({ prototype }) => prototype.insert);
  const inserting = new AsyncLocalStorage<true>();
  let leaves = 0;
  Trie.fromList = (async (elements: readonly unknown[]) => {
    leaves += elements.length;
    return await fromList(elements as never);
  }) as typeof Trie.fromList;
  classes.forEach(({ prototype }, index) => {
    prototype.insert = async function (
      this: Trie,
      ...args: Parameters<Trie["insert"]>
    ) {
      if (inserting.getStore() === undefined) leaves += 1;
      return await inserting.run(
        true,
        async () => await inserts[index]!.apply(this, args),
      );
    };
  });
  try {
    return { result: await run(), leaves };
  } finally {
    Trie.fromList = fromList;
    classes.forEach(({ prototype }, index) => {
      prototype.insert = inserts[index]!;
    });
  }
};
