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
   * leaves out. Every root persisted after the call is kept by its sweep. A
   * ledger value whose root is dropped stops being readable.
   *
   * The sweep marks root by root. A root whose walk meets a node that is not
   * held is dropped from the retained set, atomically with the end of the
   * sweep, and the sweep completes. Marking reads nodes only: a missing
   * output is not detected.
   *
   * Resolves to `notRetained`: every root of `roots` that is not retained
   * when the sweep ends, whether never persisted, dropped earlier, or dropped
   * by this sweep for a missing node. The empty ledger's root is never
   * listed. A non-empty set calls for an alert.
   *
   * One sweep runs at a time. A call made while an earlier one is still
   * waiting to start replaces it: the newer set is swept, the roots persisted
   * since the earlier call are kept, and both calls resolve with the newer
   * one's result. A call whose sweep has started is never replaced.
   *
   * A sweep that does not finish (its process dies, or a storage call fails)
   * is finished, from the retained set it began with plus the roots persisted
   * since, before the next `persist` or `retain` of this store, or before the
   * first of the next store over the same records.
   */
  retain(
    roots: ReadonlySet<string>,
  ): Promise<{ readonly notRetained: ReadonlySet<string> }>;
}

/** The running sweep's marks: everything reachable from the roots whose walk
 * has completed, and the roots persisted while it runs. */
type SweepMarks = {
  readonly nodes: Set<NodeHash>;
  readonly outputs: Set<OutRefHex>;
  readonly persisted: Set<NodeHash>;
};

type RetainResult = { readonly notRetained: ReadonlySet<string> };

/** A `retain` call whose sweep has not started yet. */
type WaitingRetain = {
  /** The newest set asked for. */
  roots: ReadonlySet<string>;
  /** Roots persisted since the first of the calls it stands for. */
  readonly persisted: Set<NodeHash>;
  readonly result: Promise<RetainResult>;
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

/** Every storage object a store has been created over. */
const ownedStorages = new WeakSet<LedgerStorage>();

/**
 * The store over `storage`. One store per storage object: a second
 * `createLedgerStore` over the same object throws.
 */
export const createLedgerStore = (storage: LedgerStorage): LedgerStore => {
  if (ownedStorages.has(storage))
    throw new Error("createLedgerStore: a store already owns this storage");
  ownedStorages.add(storage);
  const owner = {};
  const source = toLedgerSource(storage);
  /** Serialises persists against the sweep's epoch switch and deletions. */
  const writeLock = exclusive();
  /** Serialises retains. */
  const retainQueue = exclusive();
  /** The retain call that has not started; a newer call replaces its set. */
  let waiting: WaitingRetain | undefined;
  /** Every live retain call's collector of roots persisted since the call. */
  const collectors = new Set<Set<NodeHash>>();
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
    marks?.persisted.add(ledger.root);
    for (const collector of collectors) collector.add(ledger.root);
  };

  /**
   * Marks everything `root` reaches, or nothing when its walk meets a node
   * that is not held; answers whether the walk completed. The walk's marks
   * join `marks` only once it completes, so a persist never relies on a node
   * under a hole, and a later root that reaches the same hole meets it on its
   * own walk. Reads nodes only: a missing output is not detected.
   */
  const markRoot = async (
    root: NodeHash,
    marks: SweepMarks,
  ): Promise<boolean> => {
    const nodes = new Set<NodeHash>();
    const outputs: OutRefHex[] = [];
    const pending = [root];
    while (pending.length > 0) {
      const batch = takeUnseen(
        pending.splice(0, READ_BATCH).filter((hash) => !marks.nodes.has(hash)),
        nodes,
      );
      for (const node of await storage.readNodes(batch)) {
        if (node === undefined) return false;
        if (node.__kind === "Leaf") {
          if (!marks.outputs.has(node.key)) outputs.push(node.key);
        } else
          for (const child of children(node))
            if (!marks.nodes.has(child) && !nodes.has(child))
              pending.push(child);
      }
    }
    for (const hash of nodes) marks.nodes.add(hash);
    for (const outRef of outputs) marks.outputs.add(outRef);
    return true;
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

  /** Sweeps to `roots` plus the roots in `persisted` when the sweep begins. */
  const sweepTo = async (
    roots: ReadonlySet<NodeHash>,
    persisted: ReadonlySet<NodeHash> = new Set(),
  ) => {
    const marks: SweepMarks = {
      nodes: new Set(),
      outputs: new Set(),
      persisted: new Set(),
    };
    const incomplete: NodeHash[] = [];
    try {
      const retainedSet = await writeLock(async () => {
        // Under the lock, so no persist lands between this read and the
        // replacement of the retained set.
        const set = new Set([...roots, ...persisted]);
        await storage.beginSweep(set);
        sweep = marks;
        return set;
      });
      for (const root of retainedSet)
        if (!(await markRoot(root, marks))) incomplete.push(root);
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
      // A root persisted since the sweep began is complete and stays.
      await storage.endSweep(
        new Set(incomplete.filter((root) => !marks.persisted.has(root))),
      );
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

    retain: (roots) => {
      if (waiting !== undefined) {
        waiting.roots = roots;
        return waiting.result;
      }
      const persisted = new Set<NodeHash>();
      collectors.add(persisted);
      const call: WaitingRetain = {
        roots,
        persisted,
        result: retainQueue(async () => {
          waiting = undefined;
          try {
            await finishUnfinishedSweep();
            const kept = new Set<NodeHash>();
            for (const root of call.roots)
              if (
                root !== EMPTY_LEDGER_ROOT &&
                (await storage.isRetained(root))
              )
                kept.add(root);
            await sweepTo(kept, persisted);
            const notRetained = new Set<string>();
            for (const root of call.roots)
              if (!(await retained(root))) notRetained.add(root);
            return { notRetained };
          } finally {
            collectors.delete(persisted);
          }
        }),
      };
      waiting = call;
      return call.result;
    },
  };
};
