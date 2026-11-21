/** The in-memory storage adapter, for tests and short-lived ledgers. */
import type {
  LedgerStorage,
  NodeHash,
  OutRefHex,
  StoredNode,
} from "./storage.js";

type Counted<T> = { readonly value: T; refs: number };

const sumOfRefs = (records: ReadonlyMap<string, Counted<unknown>>) => {
  let total = 0;
  for (const { refs } of records.values()) total += refs;
  return total;
};

export const createMemoryLedgerStorage = (): LedgerStorage => {
  const nodes = new Map<NodeHash, Counted<StoredNode>>();
  const outputs = new Map<OutRefHex, Counted<Buffer>>();
  const roots = new Set<NodeHash>();
  const dead = new Set<NodeHash>();

  const setRefs = <T>(
    records: Map<string, Counted<T>>,
    refs: ReadonlyMap<string, number>,
  ) => {
    for (const [key, count] of refs) {
      const record = records.get(key);
      if (record !== undefined) record.refs = count;
    }
  };

  return {
    readNodes: async (hashes) => hashes.map((hash) => nodes.get(hash)?.value),
    readOutputs: async (outRefs) =>
      outRefs.map((outRef) => outputs.get(outRef)?.value),
    nodeRefs: async (hashes) => hashes.map((hash) => nodes.get(hash)?.refs),
    outputRefs: async (outRefs) =>
      outRefs.map((outRef) => outputs.get(outRef)?.refs),
    isRetained: async (root) => roots.has(root),
    retainedRoots: async () => [...roots],
    deadNodes: async (limit) => {
      const page: NodeHash[] = [];
      for (const hash of dead) {
        if (page.length >= limit) break;
        page.push(hash);
      }
      return page;
    },
    write: async (change) => {
      for (const [hash, { node, refs }] of change.putNodes)
        nodes.set(hash, { value: node, refs });
      setRefs(nodes, change.nodeRefs);
      for (const hash of change.deleteNodes) nodes.delete(hash);
      for (const [outRef, { output, refs }] of change.putOutputs)
        outputs.set(outRef, { value: Buffer.from(output), refs });
      setRefs(outputs, change.outputRefs);
      for (const outRef of change.deleteOutputs) outputs.delete(outRef);
      for (const root of change.addRoots) roots.add(root);
      for (const root of change.removeRoots) roots.delete(root);
      for (const hash of change.queueDead) dead.add(hash);
      for (const hash of change.unqueueDead) dead.delete(hash);
    },
    counts: async () => ({
      nodes: nodes.size,
      outputs: outputs.size,
      roots: roots.size,
      nodeRefs: sumOfRefs(nodes),
      outputRefs: sumOfRefs(outputs),
      dead: dead.size,
    }),
  };
};
