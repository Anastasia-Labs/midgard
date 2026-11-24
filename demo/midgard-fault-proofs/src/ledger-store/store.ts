/**
 * `LedgerStore`: ledgers addressed by root over one storage adapter.
 *
 * Held nodes and outputs are reference counted (see `LedgerStorage`). A held
 * node's children are all held: a child is deleted only after its last
 * parent is, since each parent's reference is in the child's count. So
 * `persist` and `adopt` never look below a held node.
 */
import { type LedgerEntry, LedgerRootMismatchError } from "../ledger-at.js";
import { snapshotBatches } from "./batches.js";
import { EMPTY_LEDGER_ROOT } from "./engine.js";
import {
  LedgerMutationRefused,
  LedgerNodeMissing,
  LedgerNotRetained,
  LedgerOutputMissing,
  LedgerSnapshotRefused,
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
   * Writes the ledger of a snapshot and retains its root, once that root is
   * `expectedRoot`. `entries` is read as a stream and written in bounded
   * batches, so memory is bounded by one batch and the trie's depth. Refuses
   * with `LedgerRootMismatchError`, `LedgerSnapshotRefused` (an out-ref
   * listed twice) or `LedgerEntryNotCanonical`, retaining nothing: earlier
   * batches' roots were written unreferenced and queued dead, so no retained
   * root reaches them and the next `retain` deletes them. With `current`, the
   * retaining write also moves the current-root marker, as `persist` does.
   * Every other write waits until `admit` ends.
   */
  admit(
    entries: Iterable<LedgerEntry> | AsyncIterable<LedgerEntry>,
    expectedRoot: string,
    options?: LedgerPersistOptions,
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
   *
   * With `current`, the same write also moves the storage's current-root
   * marker to `ledger.root` (see `LedgerStorageChange.currentRoot`), and
   * persisting a retained root writes the marker alone.
   */
  persist(ledger: Ledger, options?: LedgerPersistOptions): Promise<void>;
  /**
   * Takes in the ledger with `root` from nodes its caller already holds, and
   * moves the storage's current-root marker to `root` (see
   * `LedgerStorageChange.currentRoot`), in one atomic write: the walk, the
   * reference counts and the retained root are those `persist` would give,
   * with each node the storage does not hold taken from `input.nodes` and
   * each new leaf's output the storage does not hold from `input.outputs`.
   * Supplied nodes the walk does not reach are not written. A node neither
   * held nor supplied is refused with `LedgerNodeMissing`, an output with
   * `LedgerOutputMissing`, and nothing is written. Adopting a retained root
   * (or the empty one) writes the marker and nothing else.
   */
  adopt(root: string, input: LedgerAdoption): Promise<void>;
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

export type LedgerPersistOptions = {
  /** Move the storage's current-root marker to the persisted root. */
  readonly current?: boolean;
};

/** What `adopt` takes the nodes and outputs the storage does not hold from. */
export type LedgerAdoption = {
  readonly nodes: ReadonlyMap<NodeHash, StoredNode>;
  /** Outputs of new leaves; none for storage that holds every output. */
  readonly outputs?: ReadonlyMap<OutRefHex, Buffer>;
};

/** Where a write takes each node and output the storage does not hold. */
type Unheld = {
  readonly node: (hash: NodeHash) => StoredNode | undefined;
  readonly output: (outRef: OutRefHex) => Buffer | undefined;
};

/** Takes what a write does not hold from `ledger`'s overlay. */
const overlayOf = (ledger: LedgerValue): Unheld => ({
  node: (hash) => overlayNode(ledger.overlay, hash),
  output: (outRef) => overlayOutput(ledger.overlay, outRef),
});

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

/** Resolves after one turn of the event loop, in which timers and I/O run. */
const nextTurn = (): Promise<void> =>
  new Promise((resolve) => setImmediate(resolve));

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

  /**
   * The change that writes the nodes reachable from `root` that the storage
   * does not hold, and the outputs of the new leaves, taking each from
   * `unheld`, and retains `root`. With `retain` false, `root` must not be
   * held: it is written with no reference and queued dead instead, so it is
   * stored for reading but the next drain deletes it.
   */
  const rootChange = async (root: NodeHash, unheld: Unheld, retain = true) => {
    const change = newChange();
    /** Stored counts read so far; `undefined` for a node not held. */
    const stored = new Map<NodeHash, number | undefined>();
    const newLeaves: OutRefHex[] = [];
    // Each listed hash is one reference: the root's own, then one per parent.
    let level: NodeHash[] = [root];
    while (level.length > 0) {
      // In key order: storage keeps neighbouring keys together, so a sorted
      // batch reads far fewer of its blocks than a scattered one.
      const unread = level.filter((hash) => !stored.has(hash)).sort();
      for (const [hash, refs] of await readCounts(unread, (hashes) =>
        storage.nodeRefs(hashes),
      ))
        stored.set(hash, refs);
      const next: NodeHash[] = [];
      for (const hash of level) {
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
        const node = unheld.node(hash);
        if (node === undefined) throw new LedgerNodeMissing(hash);
        change.putNodes.set(hash, { node, refs: 1 });
        if (node.__kind === "Leaf") newLeaves.push(node.key);
        else next.push(...children(node));
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
      // A leaf the trie moved is a new node whose output `unheld` need not
      // carry: it must still be held.
      const output = unheld.output(outRef);
      if (output === undefined) throw new LedgerOutputMissing(outRef);
      change.putOutputs.set(outRef, { output, refs: 1 });
    }
    if (retain) change.addRoots.push(root);
    else {
      change.putNodes.get(root)!.refs -= 1;
      change.queueDead.push(root);
    }
    return change;
  };

  /**
   * Writes `rootChange(root, unheld)` unless `root` is retained, in which case
   * nothing is written. With `moveMarker`, the same write moves the
   * current-root marker to `root`, and a retained root writes the marker
   * alone.
   */
  const writeRootLocked = async (
    root: NodeHash,
    unheld: Unheld,
    moveMarker: boolean,
  ): Promise<void> => {
    const roots = await retainedRoots();
    const held = root === EMPTY_LEDGER_ROOT || roots.has(root);
    if (held && !moveMarker) return;
    const change = held ? newChange() : await rootChange(root, unheld);
    await storage.write(moveMarker ? { ...change, currentRoot: root } : change);
    if (!held) roots.add(root);
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

  /**
   * Inserts each batch into the last batch's ledger read back from storage,
   * so no batch's nodes outlive its write. A batch before the last writes
   * its root unretained (nothing when storage holds it), kept from drains by
   * the write lock; the last batch's root is checked, then written and
   * retained. The event loop gets a turn between batches, as in `retain`.
   */
  const admitLocked = async (
    entries: Iterable<LedgerEntry> | AsyncIterable<LedgerEntry>,
    expectedRoot: string,
    moveMarker: boolean,
  ): Promise<Ledger> => {
    let root = EMPTY_LEDGER_ROOT;
    for await (const batch of snapshotBatches(entries)) {
      const built = (await new LedgerValue(root, owner, undefined, source)
        .apply(batch.entries.map((entry) => ({ type: "insert", ...entry })))
        .catch((error: unknown) => {
          if (error instanceof LedgerMutationRefused)
            throw new LedgerSnapshotRefused(error.outRef);
          throw error;
        })) as LedgerValue;
      root = built.root;
      if (batch.last && root !== expectedRoot)
        throw new LedgerRootMismatchError(
          "ledger snapshot",
          expectedRoot,
          root,
        );
      if (batch.last) await writeRootLocked(root, overlayOf(built), moveMarker);
      else if ((await storage.nodeRefs([root]))[0] === undefined)
        await storage.write(await rootChange(root, overlayOf(built), false));
      if (!batch.last) await nextTurn();
    }
    return new LedgerValue(root, owner, undefined, source);
  };

  return {
    at: async (root) => {
      if (!(await retained(root))) throw new LedgerNotRetained(root);
      return new LedgerValue(root, owner, undefined, source);
    },

    admit: async (entries, expectedRoot, options) =>
      await writeLock(() =>
        admitLocked(entries, expectedRoot, options?.current === true),
      ),

    persist: async (ledger, options) => {
      if (!(ledger instanceof LedgerValue) || ledger.owner !== owner)
        throw new Error("persist: the ledger does not belong to this store");
      const moveMarker = options?.current === true;
      await writeLock(() =>
        writeRootLocked(ledger.root, overlayOf(ledger), moveMarker),
      );
    },

    adopt: async (root, input) => {
      const unheld: Unheld = {
        node: (hash) => input.nodes.get(hash),
        output: (outRef) => input.outputs?.get(outRef),
      };
      await writeLock(() => writeRootLocked(root, unheld, true));
    },

    retain: async (roots) => {
      // Before the first await, so both run when `retain` is called: `roots`
      // is copied, and the replacement is queued on the write lock ahead of
      // every write called later.
      const requested = new Set(roots);
      await writeLock(() => replaceRetained(requested));
      // The event loop gets a turn between batches: over storage whose calls
      // never wait, a long deletion would otherwise hold timers and I/O off
      // until it ends.
      while (await writeLock(drainBatch)) await nextTurn();
      const held = await retainedRoots();
      const notRetained = new Set<string>();
      for (const root of requested)
        if (root !== EMPTY_LEDGER_ROOT && !held.has(root))
          notRetained.add(root);
      return { notRetained };
    },
  };
};
