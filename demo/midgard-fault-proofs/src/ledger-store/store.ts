/**
 * `LedgerStore`: ledgers addressed by root over one storage adapter.
 *
 * Held nodes and outputs are reference counted (see `LedgerStorage`). A held
 * node's children are all held: a child is deleted only after its last
 * parent is, since each parent's reference is in the child's count. So
 * `persist` never looks below a held node.
 */
import {
  type LedgerEntry,
  ledgerEntryLeaf,
  LedgerRootMismatchError,
} from "../ledger-at.js";
import { buildTrie, EMPTY_LEDGER_ROOT, LedgerTrieStore } from "./engine.js";
import {
  LedgerNodeMissing,
  LedgerNotRetained,
  LedgerOutputMissing,
} from "./errors.js";
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
   * hold, and the outputs of its new leaves, and retains the root, in one
   * atomic write. Nodes of intermediate `apply` steps are never written. Only
   * a ledger obtained from this store (by `at`, `admit` or `apply`) is
   * accepted. Persisting a retained root changes nothing. A ledger that
   * reaches a node or an output the store no longer holds (one derived from a
   * root a `retain` has since dropped) is refused with `LedgerNodeMissing` or
   * `LedgerOutputMissing`, and nothing is written.
   */
  persist(ledger: Ledger): Promise<void>;
  /**
   * Drops every retained root that `roots` does not name, and deletes every
   * node and output no retained root reaches any more. A ledger value whose
   * root is dropped stops being readable. `roots` is read when `retain` is
   * called; changing the set afterwards changes nothing.
   *
   * The retained set is replaced in the order of calls: a `persist` called
   * after `retain` lands after the replacement, so its root is kept. The
   * deletion follows in bounded batches, each one atomic write, with persists
   * landing between them; a node a persist reuses before its batch is kept.
   * A deletion left unfinished (its process died, or a storage call failed)
   * is finished by the next `retain` of any store over the same records;
   * nothing needs to happen before the next `persist`.
   *
   * Resolves to `notRetained`: every root of `roots` that is not retained
   * when the deletion ends, whether never persisted or dropped earlier. The
   * empty ledger's root is never listed. A non-empty set calls for an alert.
   */
  retain(
    roots: ReadonlySet<string>,
  ): Promise<{ readonly notRetained: ReadonlySet<string> }>;
}

const READ_BATCH = 1024;
/** Dead-queue hashes one deletion batch takes. */
const DRAIN_BATCH = 1024;

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

/** A branch's children, one per listing, so a child listed twice counts
 * twice. */
const children = (node: StoredNode): NodeHash[] =>
  node.__kind === "Branch"
    ? node.children.filter((child): child is NodeHash => child !== null)
    : [];

/** `items` in slices of at most `size`. */
function* slices<T>(items: readonly T[], size: number): Iterable<T[]> {
  for (let start = 0; start < items.length; start += size)
    yield items.slice(start, start + size);
}

/** A change under construction; see `LedgerStorageChange`. */
const newChange = () => ({
  putNodes: new Map<NodeHash, { node: StoredNode; refs: number }>(),
  nodeRefs: new Map<NodeHash, number>(),
  deleteNodes: [] as NodeHash[],
  putOutputs: new Map<OutRefHex, { output: Buffer; refs: number }>(),
  outputRefs: new Map<OutRefHex, number>(),
  deleteOutputs: [] as OutRefHex[],
  addRoots: [] as NodeHash[],
  removeRoots: [] as NodeHash[],
  queueDead: [] as NodeHash[],
  unqueueDead: [] as NodeHash[],
});

/** The stored counts of `keys`, keyed, each read once. */
const readCounts = async (
  keys: Iterable<string>,
  read: (keys: readonly string[]) => Promise<(number | undefined)[]>,
): Promise<Map<string, number | undefined>> => {
  const counts = new Map<string, number | undefined>();
  for (const slice of slices([...new Set(keys)], READ_BATCH)) {
    const found = await read(slice);
    slice.forEach((key, index) => counts.set(key, found[index]));
  }
  return counts;
};

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
  /** Serialises every write, and the reads its counts come from. */
  const writeLock = exclusive();
  /**
   * The retained roots, read from storage once. The store is its storage's
   * only writer, so it keeps the set current itself, after each write that
   * changes it succeeds.
   */
  let retainedSet: Promise<Set<NodeHash>> | undefined;
  const retainedRoots = (): Promise<Set<NodeHash>> => {
    retainedSet ??= storage.retainedRoots().then(
      (roots) => new Set(roots),
      (error: unknown) => {
        retainedSet = undefined;
        throw error;
      },
    );
    return retainedSet;
  };

  const retained = async (root: string): Promise<boolean> =>
    root === EMPTY_LEDGER_ROOT || (await retainedRoots()).has(root);

  const persistLocked = async (ledger: LedgerValue): Promise<void> => {
    const roots = await retainedRoots();
    if (roots.has(ledger.root)) return;
    const change = newChange();
    /** Stored counts read so far; `undefined` for a node not held. */
    const stored = new Map<NodeHash, number | undefined>();
    const newLeaves: OutRefHex[] = [];
    // Each listed hash is one reference: the root's own, then one per parent.
    let level: NodeHash[] = [ledger.root];
    while (level.length > 0) {
      const next: NodeHash[] = [];
      for (const slice of slices(level, READ_BATCH)) {
        const unread = slice.filter((hash) => !stored.has(hash));
        for (const [hash, refs] of await readCounts(unread, (hashes) =>
          storage.nodeRefs(hashes),
        ))
          stored.set(hash, refs);
        for (const hash of slice) {
          const written = change.putNodes.get(hash);
          if (written !== undefined) {
            written.refs += 1;
            continue;
          }
          const refs = change.nodeRefs.get(hash) ?? stored.get(hash);
          if (refs !== undefined) {
            // Held, so complete below: it gains this reference only.
            change.nodeRefs.set(hash, refs + 1);
            continue;
          }
          const node = overlayNode(ledger.overlay, hash);
          if (node === undefined) throw new LedgerNodeMissing(hash);
          change.putNodes.set(hash, { node, refs: 1 });
          if (node.__kind === "Leaf") newLeaves.push(node.key);
          else next.push(...children(node));
        }
      }
      level = next;
    }
    const storedOutputs = await readCounts(newLeaves, (outRefs) =>
      storage.outputRefs(outRefs),
    );
    for (const outRef of newLeaves) {
      const written = change.putOutputs.get(outRef);
      if (written !== undefined) {
        written.refs += 1;
        continue;
      }
      const refs = change.outputRefs.get(outRef) ?? storedOutputs.get(outRef);
      if (refs !== undefined) {
        change.outputRefs.set(outRef, refs + 1);
        continue;
      }
      // A leaf the trie moved is a new node whose output the overlay does not
      // carry: it must still be held.
      const output = overlayOutput(ledger.overlay, outRef);
      if (output === undefined) throw new LedgerOutputMissing(outRef);
      change.putOutputs.set(outRef, { output, refs: 1 });
    }
    change.addRoots.push(ledger.root);
    await storage.write(change);
    roots.add(ledger.root);
  };

  /** Drops the retained roots `roots` does not name, queueing each whose
   * count reaches zero. */
  const replaceRetained = async (roots: ReadonlySet<string>): Promise<void> => {
    const held = await retainedRoots();
    const dropped = [...held].filter((root) => !roots.has(root));
    if (dropped.length === 0) return;
    const change = newChange();
    const counts = await readCounts(dropped, (hashes) =>
      storage.nodeRefs(hashes),
    );
    for (const root of dropped) {
      change.removeRoots.push(root);
      const refs = counts.get(root);
      if (refs === undefined) continue;
      change.nodeRefs.set(root, refs - 1);
      if (refs === 1) change.queueDead.push(root);
    }
    await storage.write(change);
    for (const root of dropped) held.delete(root);
  };

  /**
   * Deletes one batch of the dead queue: each queued node whose count is
   * still zero, with the reference it held on each child (a branch) or on its
   * output (a leaf). Children reaching zero join the queue, outputs reaching
   * zero are deleted, and a queued node a persist picked up again leaves the
   * queue. Answers whether the queue had anything.
   */
  const drainBatch = async (): Promise<boolean> => {
    const queued = await storage.deadNodes(DRAIN_BATCH);
    if (queued.length === 0) return false;
    const change = newChange();
    /** Current counts; `undefined` once not held. */
    const counts = await readCounts(queued, (hashes) =>
      storage.nodeRefs(hashes),
    );
    const doomed = queued.filter((hash) => counts.get(hash) === 0);
    const nodes = await storage.readNodes(doomed);
    const below = nodes.flatMap((node) =>
      node === undefined ? [] : children(node),
    );
    for (const [hash, refs] of await readCounts(
      below.filter((hash) => !counts.has(hash)),
      (hashes) => storage.nodeRefs(hashes),
    ))
      counts.set(hash, refs);
    const outputs = await readCounts(
      nodes.flatMap((node) => (node?.__kind === "Leaf" ? [node.key] : [])),
      (outRefs) => storage.outputRefs(outRefs),
    );
    const touched = new Set<NodeHash>(queued);
    for (const [index, hash] of doomed.entries()) {
      change.deleteNodes.push(hash);
      counts.set(hash, undefined);
      const node = nodes[index];
      if (node?.__kind === "Leaf") {
        const refs = outputs.get(node.key);
        if (refs === undefined) continue;
        if (refs === 1) {
          change.deleteOutputs.push(node.key);
          change.outputRefs.delete(node.key);
          outputs.set(node.key, undefined);
        } else {
          change.outputRefs.set(node.key, refs - 1);
          outputs.set(node.key, refs - 1);
        }
        continue;
      }
      for (const child of node === undefined ? [] : children(node)) {
        const refs = counts.get(child);
        if (refs === undefined) continue;
        counts.set(child, refs - 1);
        change.nodeRefs.set(child, refs - 1);
        touched.add(child);
      }
    }
    const queuedNow = new Set(queued);
    for (const hash of touched) {
      const dead = counts.get(hash) === 0;
      if (dead && !queuedNow.has(hash)) change.queueDead.push(hash);
      if (!dead && queuedNow.has(hash)) change.unqueueDead.push(hash);
    }
    await storage.write(change);
    return true;
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
      await writeLock(() => persistLocked(ledger));
    },

    retain: async (roots) => {
      // Before the first await, so both run when `retain` is called: `roots`
      // is copied, and the replacement is queued on the write lock ahead of
      // every write called later.
      const requested = new Set(roots);
      await writeLock(() => replaceRetained(requested));
      while (await writeLock(drainBatch));
      const held = await retainedRoots();
      const notRetained = new Set<string>();
      for (const root of requested)
        if (root !== EMPTY_LEDGER_ROOT && !held.has(root))
          notRetained.add(root);
      return { notRetained };
    },
  };
};
