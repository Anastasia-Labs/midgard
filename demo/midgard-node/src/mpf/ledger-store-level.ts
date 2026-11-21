/**
 * The ledger store's LevelDB adapter: trie nodes in the node's MPF LevelDB,
 * under the same keys (the node hash, 64 hex digits) and in the same JSON
 * shape the native owner and `MidgardMpf` already read and write, so a node
 * written by either is readable by the other.
 *
 * Retained roots are `__retained__/<root hex>` keys. The adapter never writes
 * or deletes the `__root__` marker, and a sweep considers only node keys.
 *
 * Outputs are not kept here: `readOutputs` is injected, and output writes and
 * the output sweep are no-ops.
 *
 * While a sweep runs (`LedgerStore.retain`), nothing else may write this
 * database: the adapter tells records written during the sweep apart only by
 * the writes it made itself. A sweep also deletes every node not reachable
 * from the retained roots, so the retained set must include every root
 * another reader of the database still needs, the `__root__` marker's
 * included.
 */
import type {
  LedgerStorage,
  NodeHash,
  OutRefHex,
  StoredNode,
} from "@al-ft/midgard-fault-proofs/ledger-store";
import type { Level } from "level";

import type { MpfStoredValue } from "./types.js";

export const RETAINED_ROOT_PREFIX = "__retained__/";
/** The first key after every `__retained__/` key. */
const RETAINED_ROOT_END = "__retained__0";

const NODE_KEY = /^[0-9a-f]{64}$/;

export const createLevelLedgerStorage = (input: {
  /** The MPF LevelDB, opened with JSON value encoding. */
  readonly level: Level<string, MpfStoredValue>;
  readonly readOutputs: (
    outRefs: readonly OutRefHex[],
  ) => Promise<(Buffer | undefined)[]>;
}): LedgerStorage => {
  const { level } = input;
  /** Node keys written or kept since the running sweep began. */
  let writtenDuringSweep: Set<NodeHash> | undefined;

  const retainedKeys = async (): Promise<string[]> => {
    const keys: string[] = [];
    for await (const key of level.keys({
      gte: RETAINED_ROOT_PREFIX,
      lt: RETAINED_ROOT_END,
    }))
      keys.push(key);
    return keys;
  };

  return {
    readNodes: async (hashes) =>
      hashes.length === 0
        ? []
        : ((await level.getMany([...hashes])) as (StoredNode | undefined)[]),
    readOutputs: async (outRefs) => await input.readOutputs(outRefs),
    isRetained: async (root) =>
      (await level.get(RETAINED_ROOT_PREFIX + root)) !== undefined,
    writeBlock: async (block) => {
      await level.batch([
        ...[...block.nodes].map(([hash, node]) => ({
          type: "put" as const,
          key: hash,
          value: node as MpfStoredValue,
        })),
        {
          type: "put" as const,
          key: RETAINED_ROOT_PREFIX + block.root,
          value: block.root,
        },
      ]);
      if (writtenDuringSweep === undefined) return;
      for (const hash of block.nodes.keys()) writtenDuringSweep.add(hash);
      for (const hash of block.keptNodes) writtenDuringSweep.add(hash);
    },
    beginSweep: async (retained) => {
      const dropped = (await retainedKeys()).filter(
        (key) => !retained.has(key.slice(RETAINED_ROOT_PREFIX.length)),
      );
      await level.batch([
        ...dropped.map((key) => ({ type: "del" as const, key })),
        ...[...retained].map((root) => ({
          type: "put" as const,
          key: RETAINED_ROOT_PREFIX + root,
          value: root,
        })),
      ]);
      writtenDuringSweep = new Set();
    },
    sweepNodes: async (after, limit) => {
      const written = writtenDuringSweep;
      if (written === undefined) return [];
      const page: NodeHash[] = [];
      for await (const key of level.keys(
        after === undefined ? {} : { gt: after },
      )) {
        if (!NODE_KEY.test(key) || written.has(key)) continue;
        page.push(key);
        if (page.length >= limit) break;
      }
      return page;
    },
    sweepOutputs: async () => [],
    deleteSwept: async (records) => {
      const written = writtenDuringSweep;
      if (written === undefined) throw new Error("no sweep is running");
      const dead = records.nodes.filter((hash) => !written.has(hash));
      if (dead.length > 0)
        await level.batch(dead.map((key) => ({ type: "del" as const, key })));
    },
    endSweep: async () => {
      writtenDuringSweep = undefined;
    },
    counts: async () => {
      let nodes = 0;
      for await (const key of level.keys()) if (NODE_KEY.test(key)) nodes += 1;
      return { nodes, outputs: 0, roots: (await retainedKeys()).length };
    },
  };
};
