/**
 * Storage wrappers that stop a sweep partway, for the ledger store's
 * conformance cases.
 */
import type { LedgerStorage } from "../../src/ledger-store/index.js";

/**
 * A storage whose first `sweepNodes` page is held back until `release`: the
 * sweep has listed its candidates but deleted none of them yet.
 */
export const gatedStorage = (storage: LedgerStorage) => {
  let reached!: () => void;
  let release!: () => void;
  const atGate = new Promise<void>((resolve) => (reached = resolve));
  const opened = new Promise<void>((resolve) => (release = resolve));
  let first = true;
  return {
    atGate,
    release: () => release(),
    storage: {
      ...storage,
      sweepNodes: async (after: string | undefined, limit: number) => {
        const page = await storage.sweepNodes(after, limit);
        if (first) {
          first = false;
          reached();
          await opened;
        }
        return page;
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
