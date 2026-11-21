/**
 * Storage wrappers that stop a sweep partway, for the ledger store's
 * conformance cases.
 */
import type { LedgerStorage } from "../../src/ledger-store/index.js";

/**
 * A storage whose first sweep is held back until `release`. At `"nodes"` the
 * first `sweepNodes` page is held: the sweep has listed its candidates but
 * deleted none of them yet. At `"outputs"` the first empty `sweepOutputs`
 * page is held: the sweep has deleted every output it will delete and has
 * not ended.
 */
export const gatedStorage = (
  storage: LedgerStorage,
  at: "nodes" | "outputs" = "nodes",
) => {
  let reached!: () => void;
  let release!: () => void;
  const atGate = new Promise<void>((resolve) => (reached = resolve));
  const opened = new Promise<void>((resolve) => (release = resolve));
  let held = false;
  const hold = async (page: string[], holds: boolean) => {
    if (holds && !held) {
      held = true;
      reached();
      await opened;
    }
    return page;
  };
  return {
    atGate,
    release: () => release(),
    storage: {
      ...storage,
      sweepNodes: async (after: string | undefined, limit: number) =>
        await hold(await storage.sweepNodes(after, limit), at === "nodes"),
      sweepOutputs: async (after: string | undefined, limit: number) => {
        const page = await storage.sweepOutputs(after, limit);
        return await hold(page, at === "outputs" && page.length === 0);
      },
    } satisfies LedgerStorage,
  };
};

/**
 * A storage whose first sweep dies at its first deletion: it deletes the
 * leaves among the nodes it was given (children before their parents), or
 * nothing at all, then fails, as a process killed between two pages would.
 * Later deletions go through.
 */
export const interruptedStorage = (
  storage: LedgerStorage,
  deletes: "leaves" | "nothing",
): LedgerStorage => {
  let died = false;
  return {
    ...storage,
    deleteSwept: async (records) => {
      if (died) return await storage.deleteSwept(records);
      died = true;
      if (deletes === "leaves") {
        const nodes = await storage.readNodes(records.nodes);
        await storage.deleteSwept({
          nodes: records.nodes.filter(
            (_, index) => nodes[index]?.__kind === "Leaf",
          ),
          outputs: [],
        });
      }
      throw new Error("the process died mid-sweep");
    },
  };
};

/**
 * A storage whose next sweep, once `holeAt(hash)` has named a node, deletes
 * that node as the sweep begins, through the sweep's own deletion: a retained
 * root then reaches a node that is not held.
 */
export const holedStorage = (storage: LedgerStorage) => {
  let hole: string | undefined;
  return {
    holeAt: (hash: string) => {
      hole = hash;
    },
    storage: {
      ...storage,
      beginSweep: async (retained) => {
        await storage.beginSweep(retained);
        if (hole === undefined) return;
        const nodes = [hole];
        hole = undefined;
        await storage.deleteSwept({ nodes, outputs: [] });
      },
    } satisfies LedgerStorage,
  };
};
