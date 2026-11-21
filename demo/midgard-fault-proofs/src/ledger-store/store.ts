/**
 * `LedgerStore`: ledgers addressed by root over one storage adapter.
 */
import {
  type LedgerEntry,
  ledgerEntryLeaf,
  LedgerRootMismatchError,
} from "../ledger-at.js";
import { buildTrie, EMPTY_LEDGER_ROOT, LedgerTrieStore } from "./engine.js";
import { LedgerNodeMissing, LedgerNotRetained } from "./errors.js";
import {
  type Ledger,
  type LedgerSource,
  LedgerValue,
  overlayNode,
  overlayOutput,
} from "./ledger.js";
import type {
  LedgerStorage,
  NodeHash,
  OutRefHex,
  StoredNode,
} from "./storage.js";

export interface LedgerStore {
  /**
   * The retained ledger with `root`. Refuses at once with
   * `LedgerNotRetained` when `root` was never persisted or a `retain` dropped
   * it; it never walks. The empty ledger is always available.
   */
  at(root: string): Promise<Ledger>;
  /**
   * The ledger of a snapshot, checked against `expectedRoot`
   * (`LedgerRootMismatchError` otherwise). It is held in memory until
   * `persist`.
   */
  admit(
    entries: Iterable<LedgerEntry> | AsyncIterable<LedgerEntry>,
    expectedRoot: string,
  ): Promise<Ledger>;
  /**
   * Writes the nodes reachable from `ledger.root` that the store does not
   * hold, and the outputs of its new leaves, in one atomic write, and retains
   * the root. Nodes of intermediate `apply` steps are never written. Only a
   * ledger obtained from this store (by `at`, `admit` or `apply`) is accepted.
   * A ledger that reaches a node or an output the store no longer holds (one
   * derived from a root a sweep has since dropped) is refused with
   * `LedgerNodeMissing` or `LedgerOutputMissing`, and nothing is written.
   */
  persist(ledger: Ledger): Promise<void>;
  /**
   * Makes `roots` the retained set and deletes every node and output not
   * reachable from it, including a root persisted earlier that `roots`
   * leaves out. Every root in `roots` must be retained already
   * (`LedgerNotRetained` otherwise). Roots persisted after `retain` has begun
   * are kept. A ledger value whose root is dropped stops being readable.
   * Calls run one at a time, in order. A sweep that does not finish (its
   * process dies, or a storage call fails) is finished, from the retained set
   * it began with plus the roots persisted since, before the next `persist`
   * or `retain` of any store over the same storage.
   */
  retain(roots: ReadonlySet<string>): Promise<void>;
}

/** Marks of the running sweep: everything reachable from its retained set,
 * once marking completes. */
type SweepMarks = {
  readonly nodes: Set<NodeHash>;
  readonly outputs: Set<OutRefHex>;
};

const READ_BATCH = 1024;
const SWEEP_PAGE = 4096;

/** A one-at-a-time async lock. */
const exclusive = () => {
  let tail: Promise<unknown> = Promise.resolve();
  return <T>(task: () => Promise<T>): Promise<T> => {
    const run = tail.then(task, task);
    tail = run.catch(() => undefined);
    return run;
  };
};

const toLedgerSource = (storage: LedgerStorage): LedgerSource => ({
  node: async (hash) => (await storage.readNodes([hash]))[0],
  outputs: async (outRefs) => await storage.readOutputs(outRefs),
});

/** The hashes of `batch` not in `seen`, each once; adds them to `seen`. */
const takeUnseen = (
  batch: readonly NodeHash[],
  seen: Set<NodeHash>,
): NodeHash[] => {
  const fresh: NodeHash[] = [];
  for (const hash of batch) {
    if (seen.has(hash)) continue;
    seen.add(hash);
    fresh.push(hash);
  }
  return fresh;
};

const children = (node: StoredNode): NodeHash[] =>
  node.__kind === "Branch"
    ? node.children.filter((child): child is NodeHash => child !== null)
    : [];

async function* allEntries(
  entries: Iterable<LedgerEntry> | AsyncIterable<LedgerEntry>,
): AsyncIterable<LedgerEntry> {
  yield* entries;
}

export const createLedgerStore = (storage: LedgerStorage): LedgerStore => {
  const owner = {};
  const source = toLedgerSource(storage);
  /** Serialises persists against the sweep's epoch switch and deletions. */
  const writeLock = exclusive();
  /** Serialises retains. */
  const retainQueue = exclusive();
  let sweep: SweepMarks | undefined;
  /** Settles once no sweep the storage records as unfinished remains. */
  let recovery: Promise<void> | undefined;

  const retained = async (root: string): Promise<boolean> =>
    root === EMPTY_LEDGER_ROOT || (await storage.isRetained(root));

  const persistLocked = async (ledger: LedgerValue): Promise<void> => {
    const marks = sweep;
    const nodes = new Map<NodeHash, StoredNode>();
    const outputs = new Map<OutRefHex, Buffer>();
    const keptNodes: NodeHash[] = [];
    const keptOutputs: OutRefHex[] = [];
    const seen = new Set<NodeHash>();
    const pending: NodeHash[] = [ledger.root];
    const visit = (node: StoredNode) => {
      pending.push(...children(node));
      if (node.__kind !== "Leaf") return;
      const output = overlayOutput(ledger.overlay, node.key);
      // A leaf the trie moved is a new node whose output the overlay does not
      // carry: it must still be held.
      if (output !== undefined) outputs.set(node.key, output);
      else keptOutputs.push(node.key);
    };
    while (pending.length > 0) {
      const batch = takeUnseen(pending.splice(0, READ_BATCH), seen);
      const held = await storage.readNodes(batch);
      for (const [index, hash] of batch.entries()) {
        const stored = held[index];
        if (stored !== undefined) {
          // A held node is complete below it, unless a running sweep has not
          // marked it: then it and everything under it must be kept.
          if (marks === undefined || marks.nodes.has(hash)) continue;
          keptNodes.push(hash);
          visit(stored);
          continue;
        }
        const node = overlayNode(ledger.overlay, hash);
        if (node === undefined) throw new LedgerNodeMissing(hash);
        nodes.set(hash, node);
        visit(node);
      }
    }
    await storage.writeBlock({
      root: ledger.root,
      nodes,
      outputs,
      keptNodes,
      keptOutputs,
    });
  };

  const mark = async (roots: ReadonlySet<NodeHash>, marks: SweepMarks) => {
    const pending = [...roots];
    while (pending.length > 0) {
      const batch = takeUnseen(pending.splice(0, READ_BATCH), marks.nodes);
      const nodes = await storage.readNodes(batch);
      for (const [index, hash] of batch.entries()) {
        const node = nodes[index];
        if (node === undefined) throw new LedgerNodeMissing(hash);
        if (node.__kind === "Leaf") marks.outputs.add(node.key);
        else
          for (const child of children(node))
            if (!marks.nodes.has(child)) pending.push(child);
      }
    }
  };

  const deleteUnmarked = async (
    list: (after: string | undefined, limit: number) => Promise<string[]>,
    marked: ReadonlySet<string>,
    remove: (dead: readonly string[]) => Promise<void>,
  ) => {
    let after: string | undefined;
    for (;;) {
      const page = await list(after, SWEEP_PAGE);
      if (page.length === 0) return;
      after = page[page.length - 1];
      const dead = page.filter((key) => !marked.has(key));
      if (dead.length > 0) await writeLock(() => remove(dead));
    }
  };

  const sweepTo = async (roots: ReadonlySet<NodeHash>) => {
    const marks: SweepMarks = { nodes: new Set(), outputs: new Set() };
    try {
      await writeLock(async () => {
        await storage.beginSweep(roots);
        sweep = marks;
      });
      await mark(roots, marks);
      await deleteUnmarked(
        (after, limit) => storage.sweepNodes(after, limit),
        marks.nodes,
        (dead) => storage.deleteSwept({ nodes: dead, outputs: [] }),
      );
      await deleteUnmarked(
        (after, limit) => storage.sweepOutputs(after, limit),
        marks.outputs,
        (dead) => storage.deleteSwept({ nodes: [], outputs: dead }),
      );
    } catch (error) {
      // The storage still records the sweep as running, and `sweep` stays
      // set, so a persist in the meantime walks under every held node the
      // sweep has not marked. The next persist or retain finishes it.
      recovery = undefined;
      throw error;
    }
    await writeLock(async () => {
      sweep = undefined;
      await storage.endSweep();
    });
  };

  /** Finishes a sweep the storage records as unfinished. Runs in the retain
   * queue. */
  const finishUnfinishedSweep = async () => {
    const roots = await storage.unfinishedSweep();
    if (roots !== undefined) await sweepTo(roots);
  };

  const recovered = (): Promise<void> => {
    recovery ??= retainQueue(finishUnfinishedSweep).catch((error: unknown) => {
      recovery = undefined;
      throw error;
    });
    return recovery;
  };

  return {
    at: async (root) => {
      if (!(await retained(root))) throw new LedgerNotRetained(root);
      return new LedgerValue(root, owner, undefined, source);
    },

    admit: async (entries, expectedRoot) => {
      const pairs: { key: Buffer; value: Buffer }[] = [];
      const outputs = new Map<OutRefHex, Buffer>();
      for await (const candidate of allEntries(entries)) {
        const { entry, leaf } = ledgerEntryLeaf(candidate, "ledger snapshot");
        const outRef = entry.outRef.toString("hex");
        if (outputs.has(outRef))
          throw new Error(`ledger snapshot: duplicate out-ref ${outRef}`);
        outputs.set(outRef, Buffer.from(entry.output));
        pairs.push({ key: Buffer.from(entry.outRef), value: leaf });
      }
      const store = new LedgerTrieStore(async () => undefined);
      const root = await buildTrie(store, pairs);
      if (root !== expectedRoot)
        throw new LedgerRootMismatchError(
          "ledger snapshot",
          expectedRoot,
          root,
        );
      return new LedgerValue(
        root,
        owner,
        { nodes: store.writes, outputs, parent: undefined },
        source,
      );
    },

    persist: async (ledger) => {
      if (!(ledger instanceof LedgerValue) || ledger.owner !== owner)
        throw new Error("persist: the ledger does not belong to this store");
      if (ledger.root === EMPTY_LEDGER_ROOT) return;
      // A held node is complete below it only once no sweep is unfinished.
      await recovered();
      await writeLock(() => persistLocked(ledger));
    },

    retain: (roots) =>
      retainQueue(async () => {
        await finishUnfinishedSweep();
        const kept = new Set<NodeHash>();
        for (const root of roots) {
          if (!(await retained(root))) throw new LedgerNotRetained(root);
          if (root !== EMPTY_LEDGER_ROOT) kept.add(root);
        }
        await sweepTo(kept);
      }),
  };
};
