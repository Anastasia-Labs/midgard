/**
 * The ledger store's LevelDB adapter: trie nodes in the node's MPF LevelDB,
 * under the same keys (the node hash, 64 hex digits) and in the same JSON
 * shape the native owner and `MidgardMpf` already read and write, so a node
 * written by either is readable by the other.
 *
 * A held node's reference count is its `__refs__/<hash>` key, as a decimal
 * string; a node key without one is not held. The dead queue is the
 * `__dead__/<hash>` keys, and retained roots are `__retained__/<root>` keys.
 * The adapter never writes or deletes the `__root__` marker.
 *
 * Outputs are not kept here: `readOutputs` is injected, every output counts
 * as held once (`outputRefs` answers 1), and output writes are no-ops.
 *
 * Nothing else may delete node keys this store holds. The store deletes a
 * node once no retained root reaches it, so every root another reader of the
 * database still needs, the `__root__` marker's included, must be retained.
 */
import type {
  LedgerStorage,
  OutRefHex,
  StoredNode,
} from "@al-ft/midgard-fault-proofs/ledger-store";
import type { Level } from "level";

import type { MpfStoredValue } from "./types.js";

export const RETAINED_ROOT_PREFIX = "__retained__/";
const NODE_REFS_PREFIX = "__refs__/";
const DEAD_NODE_PREFIX = "__dead__/";
/** A node key: the node hash. */
const NODE_KEY = /^[0-9a-f]{64}$/;

/** The range of keys under `prefix`, which ends in `/` (the byte before 0). */
const under = (prefix: string) => ({
  gte: prefix,
  lt: `${prefix.slice(0, -1)}0`,
});

export const createLevelLedgerStorage = (input: {
  /** The MPF LevelDB, opened with JSON value encoding. */
  readonly level: Level<string, MpfStoredValue>;
  readonly readOutputs: (
    outRefs: readonly OutRefHex[],
  ) => Promise<(Buffer | undefined)[]>;
}): LedgerStorage => {
  const { level } = input;

  const keysUnder = async (prefix: string, limit = -1): Promise<string[]> => {
    const keys: string[] = [];
    for await (const key of level.keys({ ...under(prefix), limit }))
      keys.push(key.slice(prefix.length));
    return keys;
  };

  const countUnder = async (prefix: string) => (await keysUnder(prefix)).length;

  const put = (key: string, value: MpfStoredValue) => ({
    type: "put" as const,
    key,
    value,
  });
  const del = (key: string) => ({ type: "del" as const, key });

  return {
    readNodes: async (hashes) =>
      hashes.length === 0
        ? []
        : ((await level.getMany([...hashes])) as (StoredNode | undefined)[]),
    readOutputs: async (outRefs) => await input.readOutputs(outRefs),
    nodeRefs: async (hashes) =>
      hashes.length === 0
        ? []
        : (
            await level.getMany(hashes.map((hash) => NODE_REFS_PREFIX + hash))
          ).map((refs) => (refs === undefined ? undefined : Number(refs))),
    outputRefs: async (outRefs) => outRefs.map(() => 1),
    retainedRoots: async () => await keysUnder(RETAINED_ROOT_PREFIX),
    deadNodes: async (limit) => await keysUnder(DEAD_NODE_PREFIX, limit),
    write: async (change) => {
      await level.batch([
        ...[...change.putNodes].flatMap(([hash, { node, refs }]) => [
          put(hash, node as MpfStoredValue),
          put(NODE_REFS_PREFIX + hash, String(refs)),
        ]),
        ...[...change.nodeRefs].map(([hash, refs]) =>
          put(NODE_REFS_PREFIX + hash, String(refs)),
        ),
        ...change.deleteNodes.flatMap((hash) => [
          del(hash),
          del(NODE_REFS_PREFIX + hash),
        ]),
        ...change.addRoots.map((root) =>
          put(RETAINED_ROOT_PREFIX + root, root),
        ),
        ...change.removeRoots.map((root) => del(RETAINED_ROOT_PREFIX + root)),
        ...change.queueDead.map((hash) => put(DEAD_NODE_PREFIX + hash, hash)),
        ...change.unqueueDead.map((hash) => del(DEAD_NODE_PREFIX + hash)),
      ]);
    },
    // `nodes` counts every hash with a node key or a count key. Each node
    // this store holds has both, so a node key left without its count, or a
    // count left without its node key, shows up as a node more than the store
    // holds; so does a node key another writer of the database put there.
    counts: async () => {
      const hashes = new Set<string>();
      for await (const key of level.keys())
        if (NODE_KEY.test(key)) hashes.add(key);
      let nodeRefs = 0;
      for await (const [key, refs] of level.iterator(under(NODE_REFS_PREFIX))) {
        hashes.add(key.slice(NODE_REFS_PREFIX.length));
        nodeRefs += Number(refs);
      }
      return {
        nodes: hashes.size,
        outputs: 0,
        roots: await countUnder(RETAINED_ROOT_PREFIX),
        nodeRefs,
        outputRefs: 0,
        dead: await countUnder(DEAD_NODE_PREFIX),
      };
    },
  };
};
