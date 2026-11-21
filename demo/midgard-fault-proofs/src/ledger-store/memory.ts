/** The in-memory storage adapter, for tests and short-lived ledgers. */
import type {
  LedgerStorage,
  NodeHash,
  OutRefHex,
  StoredNode,
} from "./storage.js";

type Stamped<T> = { readonly value: T; epoch: number };

/** Keys of `records` stamped at or before `epoch`, ascending. */
const stampedBefore = <T>(
  records: ReadonlyMap<string, Stamped<T>>,
  epoch: number,
): string[] =>
  [...records]
    .filter(([, record]) => record.epoch <= epoch)
    .map(([key]) => key)
    .sort();

/** The page of `sorted` after `after`. */
const pageAfter = (
  sorted: readonly string[],
  after: string | undefined,
  limit: number,
): string[] => {
  let low = 0;
  let high = sorted.length;
  while (after !== undefined && low < high) {
    const middle = (low + high) >>> 1;
    if (sorted[middle]! <= after) low = middle + 1;
    else high = middle;
  }
  return sorted.slice(low, low + limit);
};

export const createMemoryLedgerStorage = (): LedgerStorage => {
  const nodes = new Map<NodeHash, Stamped<StoredNode>>();
  const outputs = new Map<OutRefHex, Stamped<Buffer>>();
  const roots = new Set<NodeHash>();
  let writeEpoch = 1;
  /** Whether a sweep began and has not ended. */
  let sweepRunning = false;
  let sweep:
    | {
        readonly epoch: number;
        readonly nodes: readonly NodeHash[];
        readonly outputs: readonly OutRefHex[];
      }
    | undefined;

  const stamp = <T>(records: Map<string, Stamped<T>>, key: string) => {
    const record = records.get(key);
    if (record !== undefined) record.epoch = writeEpoch;
  };
  const removeSwept = <T>(
    records: Map<string, Stamped<T>>,
    keys: readonly string[],
  ) => {
    if (sweep === undefined) throw new Error("no sweep is running");
    for (const key of keys)
      if ((records.get(key)?.epoch ?? Infinity) <= sweep.epoch)
        records.delete(key);
  };

  return {
    readNodes: async (hashes) => hashes.map((hash) => nodes.get(hash)?.value),
    readOutputs: async (outRefs) =>
      outRefs.map((outRef) => outputs.get(outRef)?.value),
    isRetained: async (root) => roots.has(root),
    writeBlock: async (block) => {
      for (const [hash, node] of block.nodes)
        nodes.set(hash, { value: node, epoch: writeEpoch });
      for (const [outRef, output] of block.outputs)
        outputs.set(outRef, { value: Buffer.from(output), epoch: writeEpoch });
      for (const hash of block.keptNodes) stamp(nodes, hash);
      for (const outRef of block.keptOutputs) stamp(outputs, outRef);
      roots.add(block.root);
    },
    beginSweep: async (retained) => {
      const epoch = writeEpoch;
      writeEpoch += 1;
      roots.clear();
      for (const root of retained) roots.add(root);
      sweepRunning = true;
      sweep = {
        epoch,
        nodes: stampedBefore(nodes, epoch),
        outputs: stampedBefore(outputs, epoch),
      };
    },
    sweepNodes: async (after, limit) =>
      pageAfter(sweep?.nodes ?? [], after, limit),
    sweepOutputs: async (after, limit) =>
      pageAfter(sweep?.outputs ?? [], after, limit),
    deleteSwept: async (records) => {
      removeSwept(nodes, records.nodes);
      removeSwept(outputs, records.outputs);
    },
    endSweep: async () => {
      sweep = undefined;
      sweepRunning = false;
    },
    unfinishedSweep: async () => (sweepRunning ? new Set(roots) : undefined),
    counts: async () => ({
      nodes: nodes.size,
      outputs: outputs.size,
      roots: roots.size,
    }),
  };
};
